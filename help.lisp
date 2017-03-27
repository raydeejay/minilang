;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(define-primitive doc! (symb text)
  (setf (get (lit symb) 'doc) text))
(doc! "doc!" "Sets the documentation for a symbol.")

(define-primitive help (&optional symb)
  (if symb
      (doc symb)
      (let ((symbols (symbols)))
        (format t "Symbols:~%~%")
        (format t "~{~19A ~}" (sort (loop :for s :in symbols
                                       :when (and (boundp s)
                                                  (not (get s 'buried)))
                                       :collect (format nil "~A" s))
                                    'string-lessp)))))
(doc! "help" "Without a parameter, displays a list of unburied symbols. With a string parameter, it will display the documentation for that symbol, if there's any available.")

(defun print-documentation (symb)
  (format t "~A~%" (get (lit symb) 'doc)))

(defun doc (symb &optional (print T))
  (let ((d (get (lit symb) 'doc)))
    (when print
      (if d
          (progn (print-documentation symb))
          (progn (format t "No documentation for ~A" symb))))
    (if d T NIL)))
