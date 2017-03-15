;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; character input stream
(defclass input-stream ()
  ((input :accessor input :initarg :input)
   (pos :accessor pos :initarg :position :initform 0)
   (line :accessor line :initarg :line :initform 1)
   (column :accessor column :initarg :column :initform 0)))

(defmethod next ((stream input-stream))
  (with-slots (input pos line column)
      stream
    (let ((ch (char input pos)))
      (incf pos)
      (if (char= ch #\Newline)
          (progn (incf line) (setf column 0))
          (incf column))
      ch)))

(defmethod peek ((stream input-stream))
  (with-slots (input pos)
      stream
    (when (< pos (length input))
      (char input pos))))

(defmethod eof ((stream input-stream))
  (null (peek stream)))

(defmethod croak ((stream input-stream) message)
  (with-slots (line column)
      stream
    (error "~A (~D:~D)" message line column)))
