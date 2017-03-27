;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defun prelude ()
  "Standard library. To be moved to its own file later."
  (if (probe-file "prelude.mini")
      (read-file-into-string "prelude.mini")
      (error "Couldn't read PRELUDE.MINI")))

(defun install-primitives ()
  (declare (function minilang-runtime::import)
           (function minilang-runtime::import-op))
  (format t "~%Installing Minilang primitives.~%")
  (defparameter minilang-runtime::import-op
    (lambda (name)
      (eval `(defparameter ,(make-var name)
               (find-symbol (string-upcase ,name) 'cl)))))
  (defparameter minilang-runtime::import
    (lambda (name)
      (eval `(defparameter ,(make-var name)
               (lambda (&rest args)
                 (apply (find-symbol (string-upcase ,name)
                                     'cl)
                        args))))))
  ;; import these functions straight from CL
  ;; note that && and || are not imported as ops because an IF/LET AST
  ;; node is generated for them
  (mapc minilang-runtime::import-op '("+" "-" "*" "/" "<" ">" "<=" ">="))
  (mapc minilang-runtime::import '("sqrt" "expt" "max" "min"
                                   "cons" "car" "cdr" "list"
                                   "sleep" "reverse"))
  ;; these operators require some massaging
  (defparameter minilang-runtime::% (lambda (a b) (mod a b)))
  (defparameter minilang-runtime::== (lambda (a b) (equal a b)))
  (defparameter minilang-runtime::!= (lambda (a b) (not (equal a b))))
  ;; functions
  (defparameter minilang-runtime::gensym 'gensym%)
  (define-primitive symb (name)
    (intern (string-upcase name)))
  (define-primitive lit (name)
    (intern (string-upcase name) (find-package :minilang-runtime)))
  (define-primitive bury (name)
    (setf (get name 'buried) T))
  (define-primitive unbury (name)
    (setf (get name 'buried) NIL))
  (defparameter minilang-runtime::print (lambda (v) (princ v)))
  (defparameter minilang-runtime::println (lambda (x) (format t "~A~%" x) x))
  (defparameter minilang-runtime::maps
    (lambda (fn string)
      (map 'string fn string))
    "Map a function over a string.")
  (defparameter minilang-runtime::time (lambda (fn &rest args)
                                         (let ((in (get-internal-real-time)))
                                           (apply fn args)
                                           (- (get-internal-real-time) in)))
    "Returns the time elapsed in milliseconds.")
  ;; hooks for the REPL
  (defparameter minilang-runtime::load (lambda (filename)
                                         (if (probe-file filename)
                                             (eval
                                              (make-lisp
                                               (opt
                                                (parse
                                                 (read-file-into-string filename)))))
                                             (format t "Could not find ~A." filename)))
    "Load minilang source from a file.")
  (defparameter minilang-runtime::quit (lambda () (throw 'quit t)))
  (defparameter minilang-runtime::restart (lambda () (throw 'quit (repl)))))

;; warnings are muffled to prevent spam about undefined variables,
;; but there could be a better way to handle this
(defun compile-to-lambda (source)
  (eval (list 'lambda '() (make-lisp (opt (parse source))))))

(defun run (source)
  "Convenience function to run a string of code from the Lisp REPL."
  (locally
      (declare #+sbcl(sb-ext:muffle-conditions cl:warning))
    (handler-bind
        ((cl:warning #'muffle-warning))
      (funcall (the function (compile-to-lambda source))))))

(defun repl ()
  (catch 'quit
    (let ((prompt ">"))
      (locally
          (declare #+sbcl(sb-ext:muffle-conditions cl:warning))
        (handler-bind
            ((cl:warning #'muffle-warning))
          (loop :initially
             (install-primitives)
             (eval (make-lisp (opt (parse (prelude)))))
             (format t "Welcome to the minilang REPL~%")
             (format t "~%~A " prompt)
             :with last-result := ""
             :with edit-mode := nil
             :with source := ""
             :for input := (read-line)
             :do
             (cond ((and (not edit-mode) (equal input "edit"))
                    (setf edit-mode t
                          prompt "+"))
                   ((and edit-mode (zerop (length (the string input))))
                    (setf edit-mode nil
                          prompt ">"
                          source (concatenate 'string
                                              source
                                              input
                                              (list #\Newline))))
                   (t (setf source (concatenate 'string
                                                source
                                                input
                                                (list #\Newline)))))
             (if edit-mode
                 (format t "~A " prompt)
                 (progn
                   (handler-case
                       (progn (format t "~%~S~%~A "
                                      (setf last-result
                                            (eval
                                             (make-lisp
                                              (opt
                                               (parse source)))))
                                      prompt)
                              (setf source ""))
                     (unbound-variable (ex)
                       (format t "~A~%~A " ex prompt)
                       (setf source "")
                       nil)
                     (sb-int:simple-program-error (ex)
                       (princ (string-capitalize
                               (format nil "~A~%~A " ex prompt)
                               :end 1))
                       (setf source "")
                       nil))))))))))


(define-primitive help (&optional symb)
  (if symb
      (doc symb)
      (let ((symbols (let ((package (find-package :minilang-runtime)))
                       (loop :for s :being :the :symbols :of package
                          :for (n visibility)
                          := (multiple-value-list (find-symbol (string s)
                                                               package))
                          :when (equal visibility :internal)
                          :collect s))))
        (format t "Symbols:~%~%")
        (format t "~{~19A ~}" (sort (loop :for s :in symbols
                                       :when (and (boundp s)
                                                  (not (get s 'buried)))
                                       :collect (format nil "~A" s))
                                    'string-lessp)))))

(define-primitive doc! (symb text)
  (setf (get (lit symb) 'doc) text))
(doc! "doc!" "Sets the documentation for a symbol.")

(defun print-documentation (symb)
  (format t "~A~%" (get (lit symb) 'doc)))

(defun doc (symb &optional (print T))
  (let ((d (get (lit symb) 'doc)))
    (when print
      (if d
          (progn (print-documentation symb))
          (progn (format t "No documentation for ~A" symb))))
    (if d T NIL)))
