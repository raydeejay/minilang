;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defun prelude ()
  "Standard library. To be moved to its own file later."
  (let ((default-prelude "# minilang standard library

foreach := lambda(lst, f)
            if lst != nil {
              f(car(lst)).
              foreach(cdr(lst), f)
            }.

# a tail recursive version of range that is TCO'ed by SBCL
range := lambda(a,b)
           (lambda range% (a, b, acc)
             if a <= b
               then range% (a + 1, b, cons(a, acc))
               else reverse(acc)) (a, b, nil).

# a generator version of range
irange := lambda (a, b)
  let (n = a - 1)
    lambda()
      if n < b then n := n + 1

"))
    (if (probe-file "prelude.mini")
        (read-file-into-string "prelude.mini")
        default-prelude)))

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
                                   "cons" "car" "cdr" "list"))
  ;; these operators require some massaging
  (defparameter minilang-runtime::% (lambda (a b) (mod a b)))
  (defparameter minilang-runtime::== (lambda (a b) (equal a b)))
  (defparameter minilang-runtime::!= (lambda (a b) (not (equal a b))))
  ;; functions
  (defparameter minilang-runtime::gensym 'gensym%)
  (defparameter minilang-runtime::reverse (lambda (v) (reverse v)))
  (defparameter minilang-runtime::print (lambda (v) (princ v)))
  (defparameter minilang-runtime::println (lambda (x) (format t "~A~%" x) x))
  ;; hooks for the REPL
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
