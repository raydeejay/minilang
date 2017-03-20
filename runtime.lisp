;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defun install-primitives ()
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
  (mapc minilang-runtime::import-op '("+" "-" "*" "/" "<" ">" "<=" ">="))
  (mapc minilang-runtime::import '("sqrt" "expt" "max" "min"
                                   "cons" "car" "cdr" "list"))
  ;; these require some massaging
  (defparameter minilang-runtime::gensym 'gensym%)
  (defparameter minilang-runtime::== (lambda (a b) (equal a b)))
  (defparameter minilang-runtime::!= (lambda (a b) (not (equal a b))))
  (defparameter minilang-runtime::reverse (lambda (v) (reverse v)))
  (defparameter minilang-runtime::print (lambda (v) (princ v)))
  (defparameter minilang-runtime::println (lambda (x) (format t "~A~%" x) x))
  ;; hooks for the REPL
  (defparameter minilang-runtime::quit (lambda () (throw 'quit t)))
  (defparameter minilang-runtime::restart (lambda () (throw 'quit (crepl))))
  ;; print a message for some reason... :D
  (format t "~%Minilang primitives installed.~%"))

;; (defun install-cps-primitives ()
;;   (defparameter minilang-runtime::import-op
;;     (lambda (name)
;;       (eval `(defparameter ,(make-var name)
;;                (find-symbol (string-upcase ,name) 'cl)))))
;;   (defparameter minilang-runtime::import
;;     (lambda (name)
;;       (eval `(defparameter ,(make-var name)
;;                (lambda (k &rest args)
;;                  (funcall k (apply (find-symbol (string-upcase ,name)
;;                                                 'cl)
;;                                    args)))))))
;;   ;; import these functions straight from CL
;;   (mapc minilang-runtime::import-op '("+" "-" "*" "/" "<" ">" "<=" ">="))
;;   (mapc minilang-runtime::import '("sqrt" "expt" "max" "min"
;;                                    "cons" "car" "cdr" "list"))
;;   ;; these require some massaging
;;   (defparameter minilang-runtime::gensym 'gensym%)
;;   (defparameter minilang-runtime::== (lambda (a b) (equal a b)))
;;   (defparameter minilang-runtime::!= (lambda (a b) (not (equal a b))))
;;   (defprimitive "print" (lambda (v) (princ v)))
;;   (defprimitive "println" (lambda (x) (format t "~A~%" x) x))
;;   ;; hooks for the REPL
;;   (defprimitive "quit" (lambda () (throw 'quit t)))
;;   (defprimitive "restart" (lambda () (throw 'quit (crepl))))
;;   ;; print a message for some reason... :D
;;   (format t "~%Minilang primitives installed.~%"))

(defun defprimitive (name body)
  (eval `(defparameter ,(make-var name)
           (lambda (k &rest args)
             (funcall k (apply ,body args))))))

;; warnings are muffled to prevent spam about undefined variables,
;; but there could be a better way to handle this
(defun run (source)
  (locally
      (declare #+sbcl(sb-ext:muffle-conditions cl:warning))
    (handler-bind
        ((cl:warning #'muffle-warning))
      (funcall (compile-to-lambda source)))))

(defun crepl ()
  (catch 'quit
    (let ((prompt ">"))
      (locally
          (declare #+sbcl(sb-ext:muffle-conditions cl:warning))
        (handler-bind
            ((cl:warning #'muffle-warning))
          (loop :initially
             (install-primitives)
             (eval (make-lisp (parse (prelude))))
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
                   ((and edit-mode (zerop (length input)))
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
                 (progn (format t "~%~S~%~A "
                                ;; (setf last-result (run source))
                                (setf last-result
                                      (eval (make-lisp (opt (parse source)))))
                                prompt)
                        (setf source "")))))))))
