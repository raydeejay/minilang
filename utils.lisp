;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; helper
(defun <- (key plist)
  (assoc-value (plist-alist plist) key :test 'equal))

(let ((gensym-counter 0))
  (defun gensym% (&optional (name "") (prefix "α"))
    (format nil  "~A_~A~D" prefix name (incf gensym-counter))))

(defun dismiss (&rest args)
  "Takes any number of arguments and simply ignores them, doing absolutely nothing."
  (declare (ignore args)))

(defun to-radians (degrees)
  (/ (* degrees pi) 180.0))

(defun to-degrees (radians)
  (/ (* radians 180.0) pi))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage minilang-runtime
    (:documentation "Holds minilang runtime definitions"))

  (defmacro define-primitive (name args &body body)
    (let ((symb (intern (string-upcase name) :minilang-runtime)))
      `(defparameter ,symb
         (defun ,name ,args ,@body))))

  (defmacro define-primitive-alias (alias name)
    (let ((symb (intern (string-upcase name) :minilang-runtime))
          (a-symb (intern (string-upcase alias) :minilang-runtime)))
      `(defparameter ,a-symb ,symb))))

