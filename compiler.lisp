;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defun lisp (exp)
  (switch ((<- :type exp) :test 'equal)
    ("num" (lisp-atom exp))
    ("str" (lisp-atom exp))
    ("bool" (lisp-atom exp))
    ("var" (lisp-var exp))
    ("binary" (lisp-binary exp))
    ("assign" (lisp-assign exp))
    ("let" (lisp-let exp))
    ("lambda" (lisp-lambda exp))
    ("if" (lisp-if exp))
    ("prog" (lisp-prog exp))
    ("call" (lisp-call exp))
    (otherwise (error "I don't know how to make-lisp ~A" exp))))

(defun lisp-atom (exp)
  (<- :value exp))

(defun make-var (name)
  (intern (string-upcase name) 'minilang-runtime))

(defun lisp-var (exp)
  (make-var (<- :value exp)))

(defun lisp-binary (exp)
  (list (make-var (<- :operator exp))
        (lisp (<- :left exp))
        (lisp (<- :right exp))))

(defun lisp-assign (exp)
  (list 'setf (lisp (<- :left exp)) (lisp (<- :right exp))))

(defun lisp-lambda (exp)
  (let ((body (list 'lambda
                    (mapcar 'make-var (<- :vars exp))
                    (lisp (<- :body exp)))))
    `@(if (<- :name exp)
          (list 'setf (make-var (<- :name exp)) body)
          body)))

;; lisp-let could be implemented using CL's LET* instead of recurring to lambdas
(defun lisp-let (exp)
  (if (zerop (length (<- :vars exp)))
      (lisp (<- :body exp))
      (let ((iife
             (list
              :type "call"
              :func (list
                     :type "lambda"
                     :vars (list
                            (<- :name (nth 0 (<- :vars exp))))
                     :body (list
                            :type "let"
                            :vars (subseq (<- :vars exp) 1)
                            :body (<- :body exp)))
              :args (list (or (<- :def (nth 0 (<- :vars exp)))
                              +false+)))))
        (lisp iife))))

(defun lisp-if (exp)
  (list 'if (lisp (<- :cond exp))
        (lisp (<- :then exp))
        (lisp (or (<- :else exp) +false+))))

(defun lisp-prog (exp)
  (cons 'progn (mapcar 'lisp (<- :prog exp))))

(defun lisp-call (exp)
  (list 'apply
        (lisp (<- :func exp))
        (cons 'list (mapcar 'lisp (<- :args exp)))))

(defun make-lisp (exp)
  `(,@(lisp exp)))

(defun compile-to-lambda (source)
  (eval (list 'lambda '() (make-lisp (parse source)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage minilang-runtime
    (:documentation "Holds minilang runtime definitions")))

(defun install-primitives ()
  (defparameter minilang-runtime::import
    (lambda (name)
      (eval `(defparameter ,(make-var name)
               (find-symbol (string-upcase ,name) 'cl)))))
  (defparameter minilang-runtime::print 'prin1)
  (defparameter minilang-runtime::println (lambda (x) (format t "~A~%" x) x))
  (defparameter minilang-runtime::cons #'cons)
  (defparameter minilang-runtime::car #'car)
  (defparameter minilang-runtime::cdr #'cdr)
  (defun minilang-runtime::== (a b) (equal a b))
  (defun minilang-runtime::!= (a b) (not (equal a b)))
  (format t "~%Minilang primitives installed.~%"))

;; warnings are muffled to prevent spam about undefined variables,
;; but there could be a better way to handle this
(defun run (source)
  (locally
      (declare #+sbcl(sb-ext:muffle-conditions cl:warning))
    (handler-bind
        ((cl:warning #'muffle-warning))
      (funcall (compile-to-lambda source)))))
