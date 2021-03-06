;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; the node for boolean false, defined here because it's used all around
(defparameter minilang-runtime::nil nil)
(defvar +true+ (list :type "bool" :value T))
(defvar +false+ (list :type "bool" :value 'minilang-runtime::nil))

(defparameter *expression-separator* #\.)
(defparameter *element-separator* #\,)
;; parser
(defparameter *precedences*
  (list "=" 1 ;; ??
        ":=" 1
        "||" 2
        "&&" 3
        "<" 7 ">" 7 "<=" 7 ">=" 7 "==" 7 "!=" 7
        "+" 10 "-" 10
        "*" 20 "/" 20 "%" 20))

(defclass parser ()
  ((input :accessor input :initarg :input)))

(defmethod at-punc ((stream parser) &optional ch)
  (with-slots (input)
      stream
    (let ((tok (peek input)))
      (and tok
           (equal (<- :type tok) "punc")
           (or (null ch)
               (equal (<- :value tok) ch))
           tok))))

(defmethod at-kw ((stream parser) &optional kw)
  (with-slots (input)
      stream
    (let ((tok (peek input)))
      (and tok
           (equal (assoc-value (plist-alist tok) :type) "kw")
           (or (null kw)
               (equal (assoc-value (plist-alist tok) :value) kw))
           tok))))

(defmethod at-op ((stream parser) &optional op)
  (with-slots (input)
      stream
    (let ((tok (peek input)))
      (and tok
           (equal (<- :type tok) "op")
           (or (null op)
               (equal (<- :value tok) op))
           tok))))

(defmethod skip-punc ((stream parser) ch)
  (with-slots (input)
      stream
    (if (at-punc stream ch)
        (next input)
        (croak input
               (format nil "Expecting punctuation '~A', got '~A'" ch (peek input))))))

(defmethod skip-kw ((stream parser) kw)
  (with-slots (input)
      stream
    (if (at-kw stream kw)
        (next input)
        (croak input
               (format nil "Expecting keyword, got '~A'" kw)))))

(defmethod skip-op ((stream parser) op)
  (with-slots (input)
      stream
    (if (at-op stream op)
        (next input)
        (croak input
               (format nil "Expecting operator, got '~A'" op)))))

(defmethod unexpected ((stream parser))
  (with-slots (input)
      stream
    (croak input (format nil "Unexpected token '~A'" (peek input)))))

(defmethod maybe-binary ((stream parser) left my-prec)
  (with-slots (input)
      stream
    (let ((tok (at-op stream)))
      (when tok
        (let ((his-prec (<- (<- :value tok)
                            *precedences*)))
          (when (> (the fixnum his-prec) (the fixnum my-prec))
            (next input)
            (return-from maybe-binary
              (maybe-binary stream
                            (switch ((<- :value tok) :test 'equal)
                              ("&&"
                               (list :type "if"
                                     :cond left
                                     :then (maybe-binary stream
                                                         (parse-atom stream)
                                                         his-prec)))
                              ;; need gensym?
                              ;; need a var name unrepresentable in minilang?
                              ("||"
                               (list :type "let"
                                     :vars (list (list :name "Ω"
                                                       :def left))
                                     :body (list :type "if"
                                                 :cond (list :type "var"
                                                             :value "Ω")
                                                 :then (list :type "var"
                                                             :value "Ω")
                                                 :else (maybe-binary stream
                                                                     (parse-atom stream)
                                                                     his-prec))))
                              (otherwise
                               (list :type (if (equal (<- :value tok) ":=")
                                               "assign"
                                               "binary")
                                     :operator (<- :value tok)
                                     :left left
                                     :right (maybe-binary stream
                                                          (parse-atom stream)
                                                          his-prec))))
                            my-prec)))))
      left)))

(defmethod delimited ((stream parser) start stop delimiter parser-fn)
  (with-slots (input)
      stream
    (loop :with first := T
       :initially (when start
                    (if (is-punc start)
                        (skip-punc stream start)
                        (skip-op stream start)))
       :while (not (eof input))
       :when (if (is-punc stop)
                 (at-punc stream stop)
                 (at-op stream stop)) :do (loop-finish)
       :if first :do (setf first nil)
       :else :do (skip-punc stream delimiter)
       :when (if (is-punc stop)
                 (at-punc stream stop)
                 (at-op stream stop)) :do (loop-finish)
       :collect (funcall parser-fn stream) :into exp
       :finally (progn
                  (if (is-punc stop)
                      (skip-punc stream stop)
                      (skip-op stream stop))
                  (return exp)))))

;; specific parsing methods
(defmethod parse-call ((stream parser) func)
  (list :type "call"
        :func func
        :args (delimited stream #\( #\) #\, 'parse-expression)))

(defmethod parse-varname ((stream parser))
  (with-slots (input)
      stream
    (let ((var-name (next input)))
      (when (not (equal (<- :type var-name) "var"))
        (croak input "Expecting variable name"))
      (<- :value var-name))))

(defmethod parse-if ((stream parser))
  (skip-kw stream "if")
  (let ((cond (parse-expression stream)))
    (when (not (at-punc stream #\{))
      (skip-kw stream "then"))
    (let ((then (parse-expression stream)))
      (append (list :type "if"
                    :cond cond
                    :then then)
              (when (at-kw stream "else")
                (next (slot-value stream 'input))
                (list :else (parse-expression stream)))))))

(defmethod delimited-varnames ((stream parser) start stop)
  (delimited stream start stop
             *element-separator*
             'parse-varname))

(defmethod parse-lambda ((stream parser))
  (with-slots (input)
      stream
    (list :type "lambda"
          :name (if (equal (<- :type (peek input)) "var")
                    (<- :value (next input))
                    nil)
          :vars (delimited-varnames stream #\( #\))
          :body (parse-expression stream))))

(defmethod parse-bool ((stream parser))
  (with-slots (input)
      stream
    (if (equal (<- :value (next input)) "false")
        +FALSE+
        +TRUE+)))

;; hmmm...
(defmethod maybe-call ((stream parser) expr)
  (let ((expr% (funcall (the function expr))))
    (if (at-punc stream #\()
        (parse-call stream expr%)
        expr%)))

(defmethod parse-atom ((stream parser))
  (with-slots (input)
      stream
    (maybe-call stream
                (lambda ()
                  (block inner
                    (cond ((at-punc stream #\()
                           (next input)
                           (let ((exp (parse-expression stream)))
                             (skip-punc stream #\))
                             (return-from inner exp)))
                          ((at-punc stream #\{)
                           (return-from inner (parse-prog stream)))
                          ((at-punc stream #\[)
                           (return-from inner (parse-block stream)))
                          ((at-kw stream "let")
                           (return-from inner (parse-let stream)))
                          ((at-kw stream "if")
                           (return-from inner (parse-if stream)))
                          ((or (at-kw stream "true")
                               (at-kw stream "false"))
                           (return-from inner (parse-bool stream)))
                          ((or (at-kw stream "lambda")
                               (at-kw stream "λ"))
                           (next input)
                           (return-from inner (parse-lambda stream))))
                    (let ((tok (next input)))
                      (when (member (<- :type tok)
                                    '("var" "num" "str")
                                    :test 'equal)
                        (return-from inner tok)))
                    (unexpected stream))))))

(defmethod parse-prog ((stream parser))
  (let ((prog (delimited stream #\{ #\} *expression-separator* 'parse-expression)))
    (declare (list prog))
    (case (length prog)
      (0 +false+)
      (1 (car prog))
      (otherwise (list :type "prog" :prog prog)))))

(defmethod parse-block ((stream parser))
  (with-slots (input)
      stream
    (next input)                        ; skip #\[
    (let (has-params)
      (list :type "lambda"
            :name (when (at-op stream ":")
                    (skip-op stream ":")
                    (<- :value (next input)))
            :vars (when (at-op stream "|")
                    (setf has-params T)
                    ;; (delimited stream #\| #\|
                    ;;            *element-separator*
                    ;;            'parse-varname)
                    (delimited-varnames stream "|" "|"))
            :body (list :type "prog"
                        :prog (delimited stream
                                         nil
                                         #\]
                                         *expression-separator*
                                         'parse-expression))))))

(defmethod parse-expression ((stream parser))
  (maybe-call stream (lambda ()
                       (maybe-binary stream
                                     (parse-atom stream)
                                     0))))

(defmethod delimited-vardefs ((stream parser) start stop)
  (delimited stream start stop
             *element-separator*
             'parse-vardef))


(defmethod parse-let ((stream parser))
  (skip-kw stream "let")
  (with-slots (input)
      stream
    (if (equal (<- :type (peek input)) "var")
        (let* ((name (<- :value (next input)))
               (defs (delimited stream #\( #\) #\, 'parse-vardef)))
          (list :type "call"
                :func (list :type "lambda"
                            :name name
                            :vars (mapcar (the function (curry '<- :name)) defs)
                            :body (parse-expression stream))
                :args (mapcar (lambda (def)
                                (or (<- :def def)
                                    +false+))
                              defs)))
        (list :type "let"
              :vars (delimited stream #\( #\) #\, 'parse-vardef)
              :body (parse-expression stream)))))

(defmethod parse-vardef ((stream parser))
  (with-slots (input)
      stream
    (let ((name (parse-varname stream)) def)
      (when (at-op stream "=")
        (next input)
        (setf def (parse-expression stream)))
      (list :name name :def def))))

(defmethod parse-toplevel ((stream parser))
  (with-slots (input)
      stream
    (loop :while (not (eof input))
       :collecting (parse-expression stream) :into prog
       :when (not (eof input))
       :do (skip-punc stream *expression-separator*)
       :finally (return (list :type "prog" :prog prog)))))

(defun parse (source)
  "Convenience function. Wraps the source in the necessary objects,
then tells the parser to parse."
  (parse-toplevel
   (make-instance 'parser
                  :input (make-instance 'token-stream
                                        :input (make-instance 'input-stream
                                                              :input source)))))


