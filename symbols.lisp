;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(define-primitive symbols ()
  (let ((package (find-package :minilang-runtime)))
    (loop :for s :being :the :symbols :of package
       :for (n visibility)
       := (multiple-value-list (find-symbol (string s)
                                            package))
       :when (equal visibility :internal)
       :collect s)))

(define-primitive symb (name)
  (intern (string-upcase name)))

(define-primitive lit (name)
  (intern (string-upcase name) (find-package :minilang-runtime)))

(define-primitive setprop (symb prop value)
  (setf (get (lit symb) (symb prop)) value))

(define-primitive prop (symb prop)
  (get (lit symb) (symb prop)))

(define-primitive push! (obj stack)
  (push obj (symbol-value (lit stack))))
(defparameter minilang-runtime::push 'push!)

(define-primitive pop! (stack)
  (pop (symbol-value (lit stack))))
(defparameter minilang-runtime::pop 'pop!)
