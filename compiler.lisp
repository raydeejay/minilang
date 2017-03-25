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
  (list 'funcall (make-var (<- :operator exp))
        (lisp (<- :left exp))
        (lisp (<- :right exp))))

(defun lisp-assign (exp)
  (list 'setf (lisp (<- :left exp)) (lisp (<- :right exp))))

(defun lisp-lambda (exp)
  (let ((result (list 'lambda
                      (mapcar 'make-var (<- :vars exp))
                      (lisp (<- :body exp)))))
    (if (<- :name exp)
        `(let (,(make-var (<- :name exp)))
           (setf ,(make-var (<- :name exp)) ,result))
        result)))

;; is it really necessary to do this...?
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
