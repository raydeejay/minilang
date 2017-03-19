;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defun lisp (exp)
  (switch ((<- :type exp) :test 'equal)
    ("num" (lisp-atom exp))
    ("str" (lisp-atom exp))
    ("bool" (lisp-atom exp))
    ("var" (lisp-var exp))
    ("binary" (lisp-binary exp))
    ("assign" (lisp-assign exp))
    ("let" (lisp-let exp))
    ("lambda" (lisp-lambda exp))
    ("if" (lisp-if exp))
    ("prog" (lisp-prog exp))
    ("call" (lisp-call exp))
    (otherwise (error "I don't know how to make-lisp ~A" exp))))

(defun lisp-atom (exp)
  (<- :value exp))

(defun make-var (name)
  (intern (string-upcase name) 'minilang-runtime))

(defun lisp-var (exp)
  (make-var (<- :value exp)))

(defun lisp-binary (exp)
  (list 'funcall (make-var (<- :operator exp))
        (lisp (<- :left exp))
        (lisp (<- :right exp))))

(defun lisp-assign (exp)
  (list 'setf (lisp (<- :left exp)) (lisp (<- :right exp))))

(defun lisp-lambda (exp)
  (let ((result (list 'lambda
                      (mapcar 'make-var (<- :vars exp))
                      (lisp (<- :body exp)))))
    (if (<- :name exp)
        (list 'setf (make-var (<- :name exp)) result)
        result)))

(defun lisp-let (exp)
  (if (zerop (length (<- :vars exp)))
      (lisp (<- :body exp))
      (let ((iife
             (list
              :type "call"
              :func (list
                     :type "lambda"
                     :vars (list
                            (<- :name (nth 0 (<- :vars exp))))
                     :body (list
                            :type "let"
                            :vars (subseq (<- :vars exp) 1)
                            :body (<- :body exp)))
              :args (list (or (<- :def (nth 0 (<- :vars exp)))
                              +false+)))))
        (lisp iife))))

(defun lisp-if (exp)
  (list 'if (lisp (<- :cond exp))
        (lisp (<- :then exp))
        (lisp (or (<- :else exp) +false+))))

(defun lisp-prog (exp)
  (cons 'progn (mapcar 'lisp (<- :prog exp))))

(defun lisp-call (exp)
  (list 'apply
        (lisp (<- :func exp))
        (cons 'list (mapcar 'lisp (<- :args exp)))))

(defun make-lisp (exp)
  `(,@(lisp exp)))

(defun compile-to-lambda (source)
  (eval (list 'lambda '() (make-lisp (parse source)))))

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
  (defparameter minilang-runtime::print (lambda (v) (princ v)))
  (defparameter minilang-runtime::println (lambda (x) (format t "~A~%" x) x))
  ;; hooks for the REPL
  (defparameter minilang-runtime::quit (lambda () (throw 'quit t)))
  (defparameter minilang-runtime::restart (lambda () (throw 'quit (crepl))))
  ;; print a message for some reason... :D
  (format t "~%Minilang primitives installed.~%"))

(defun install-cps-primitives ()
  (defparameter minilang-runtime::import-op
    (lambda (name)
      (eval `(defparameter ,(make-var name)
               (find-symbol (string-upcase ,name) 'cl)))))
  (defparameter minilang-runtime::import
    (lambda (name)
      (eval `(defparameter ,(make-var name)
               (lambda (k &rest args)
                 (funcall k (apply (find-symbol (string-upcase ,name)
                                                'cl)
                                   args)))))))
  ;; import these functions straight from CL
  (mapc minilang-runtime::import-op '("+" "-" "*" "/" "<" ">" "<=" ">="))
  (mapc minilang-runtime::import '("sqrt" "expt" "max" "min"
                                   "cons" "car" "cdr" "list"))
  ;; these require some massaging
  (defparameter minilang-runtime::gensym 'gensym%)
  (defparameter minilang-runtime::== (lambda (a b) (equal a b)))
  (defparameter minilang-runtime::!= (lambda (a b) (not (equal a b))))
  (defprimitive "print" (lambda (v) (princ v)))
  (defprimitive "println" (lambda (x) (format t "~A~%" x) x))
  ;; hooks for the REPL
  (defprimitive "quit" (lambda () (throw 'quit t)))
  (defprimitive "restart" (lambda () (throw 'quit (crepl))))
  ;; print a message for some reason... :D
  (format t "~%Minilang primitives installed.~%"))

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

(defun crepl (&key cps)
  (catch 'quit
    (let ((prompt ">"))
      (locally
          (declare #+sbcl(sb-ext:muffle-conditions cl:warning))
        (handler-bind
            ((cl:warning #'muffle-warning))
          (loop :initially
             (if cps
                 (install-cps-primitives)
                 (install-primitives))
             (if (not cps)
                 (eval (make-lisp (parse (prelude))))
                 (eval (make-lisp (cps (parse (prelude))))))
             (format t "Welcome to the minilang REPL~%")
             (if cps (format t "CPS mode on."))
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
                                      (eval (make-lisp (if cps
                                                           (cps (parse source))
                                                           (parse source)))))
                                prompt)
                        (setf source "")))))))))
