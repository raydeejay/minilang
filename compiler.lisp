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
  (intern (string-upcase name)))

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

;; hack!!
(sb-ext:without-package-locks
  (defparameter print 'prin1)
  (defparameter println (lambda (x) (format t "~A~%" x) x))
  (defparameter cons #'cons)
  (defparameter car #'car)
  (defparameter cdr #'cdr)
  (defun == (a b) (equal a b))
  (defun != (a b) (not (equal a b))))

;; warnings are muffled to prevent spam about undefined variables,
;; but there could be a better way to handle this
(defun run (source)
  (locally
      (declare #+sbcl(sb-ext:muffle-conditions cl:warning))
    (handler-bind
        ((cl:warning #'muffle-warning))
      (funcall (compile-to-lambda source)))))
