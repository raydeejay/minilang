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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage minilang-runtime
    (:documentation "Holds minilang runtime definitions")))
