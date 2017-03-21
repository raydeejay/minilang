;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; tokenizer
(defparameter *keywords*
  (list "let"
        "if" "then" "else"
        "lambda" "λ"
        "true" "false"))

;; helper functions used by `TOKEN-STREAM'
(defun is-keyword (word)
  (when (member word *keywords* :test 'equal) T))

(defun is-digit (ch)
  (when (member ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) T))

(defun is-id-start (ch)
  (or (char<= #\A ch #\Z)
      (char<= #\a ch #\z)
      (char= ch #\λ)
      (char= ch #\_)))

(defun is-id (ch)
  (or (is-id-start ch)
      (member ch '(#\? #\! #\- #\< #\> #\= #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))

(defun is-op-char (ch)
  (when (member ch '(#\+ #\- #\* #\/ #\% #\= #\& #\| #\< #\> #\!)) T))

(defun is-punc (ch)
  (when (member ch '(#\, #\; #\( #\) #\[ #\] #\{ #\})) T))

(defun is-whitespace (ch)
  (when (member ch '(#\Space #\Tab #\Newline)) T))

;; token-stream
(defclass token-stream ()
  ((input    :accessor input    :initarg :input)
   (current  :accessor current  :initarg :current :initform nil)))

(defmethod next ((stream token-stream))
  (with-slots (current)
      stream
    (let ((tok current))
      (setf current nil)
      (or tok (read-next stream)))))

(defmethod peek ((stream token-stream))
  (with-slots (current)
      stream
    (or current (setf current (read-next stream)))))

(defmethod eof ((stream token-stream))
  (null (peek stream)))

(defmethod croak ((stream token-stream) message)
  (with-slots (input)
      stream
    (croak input message)))

(defmethod read-while ((stream token-stream) predicate)
  (with-slots (input)
      stream
    (loop :while (and (not (eof input))
                      (funcall predicate (peek input)))
       :collecting (next input) :into str
       :finally (return (coerce str 'string)))))

;; methods to read each token kind
;; what would be a good representation for a token?
;; list
;; dictionary
;; object
(defmethod read-number ((stream token-stream))
  (let (has-dot)
    (let ((number (read-while stream
                              (lambda (ch)
                                (if (char= ch #\.)
                                    (if has-dot
                                        nil
                                        (setf has-dot t))
                                    (is-digit ch))))))
      ;; return { type: "num", value: parseFloat(number) }
      (list :type "num" :value (parse-float number)))))

(defmethod read-identifier ((stream token-stream))
  (let ((id (read-while stream 'is-id)))
    (list :type (if (is-keyword id) "kw" "var")
          :value id)))

;; note that this does not return an AST node
(defmethod read-escaped ((stream token-stream) end-char)
  (with-slots (input)
      stream
    (loop :with escaped
       :initially (next input)
       :while (not (eof input))
       :for ch := (next input)
       :if escaped
       :collect ch :into str
       :and :do (setf escaped nil)
       :else :if (char= ch #\\)
       :do (setf escaped t)
       :else :if (char= ch end-char)
       :do (loop-finish)
       :else
       :collect ch :into str
       :finally (return (coerce str 'string)))))

(defmethod read-string ((stream token-stream))
  (list :type "str" :value (read-escaped stream #\")))

(defmethod skip-comment ((stream token-stream))
  (read-while stream (lambda (ch) (char/= ch #\Newline)))
  (with-slots (input)
      stream
    (next input)))

(defmethod read-next ((stream token-stream))
  (read-while stream 'is-whitespace)
  (with-slots (input)
      stream
    (if (eof input)
        nil
        (let ((ch (peek input)))
          (cond ((char= ch #\#)
                 (skip-comment stream)
                 (read-next stream))
                ((char= ch #\") (read-string stream))
                ((is-digit ch) (read-number stream))
                ((is-id-start ch) (read-identifier stream))
                ((is-punc ch)
                 (list :type "punc"
                       :value (next input)))
                ((is-op-char ch)
                 (list :type "op"
                       :value (read-while stream 'is-op-char)))
                (t (croak input
                          (format nil
                                  "Can't handle character: ~A"
                                  ch))))))))
