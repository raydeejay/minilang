;;;; minilang.lisp

(in-package #:minilang)

;; parser
(defparameter *precedences*
  (list "=" 1
        "||" 2
        "&&" 3
        "<" 7 ">" 7 "<=" 7 ">=" 7 "==" 7 "!=" 7
        "+" 10 "-" 10
        "*" 20 "/" 20 "%" 20))

;; sample code
(defun test-program ()
  (let* ((code "sum = lambda(x, y) x + y; print(sum(2, 3));")
         (ast (parse code))
         (env (base-env)))
    (evaluate ast env)))

(defun base-env ()
  (let ((env (make-instance 'environment)))
    (def env "print" (lambda (callback txt)
                       (format t "~A" txt)
                       (funcall callback nil)))
    (def env "println" (lambda (callback txt)
                         (format t "~A~%" txt)
                         (funcall callback nil)))
    (def env "quit" (lambda (callback)
                      (declare (ignore callback))
                      (throw 'quit t)))
    env))

(defun prelude ()
  "Standard library. To be moved to its own file later."
  "# minilang standard library

foreach = lambda(lst, f)
            if lst != nil {
              f(car(lst)).
              foreach(cdr(lst), f).
            }.

#range = lambda(a, b)
#          if a <= b then cons(a, range(a + 1, b))
#                    else NIL;

range = lambda(a, b, acc)
          if a <= b then range(a + 1, b, cons(a, acc))
                    else reverse(acc).

# a generator version of range
irange = lambda (a, b)
  let (n = a - 1)
    lambda()
      if n < b then n = n + 1

")

(defun dismiss (&rest args)
  "Takes any number of arguments and simply ignores them, doing absolutely nothing."
  (declare (ignore args)))

;; TODO: catch errors and don't crash the REPL
(defun repl ()
  (catch 'quit
    (loop :with env := (base-env)
       :initially (evaluate (parse (prelude)) env 'dismiss)
       :initially (format t "Welcome to the minilang REPL~%> ")
       :doing (format t "~A~%> "
                      (evaluate (parse (read-line))
                                env
                                (lambda (x)
                                  (fresh-line)
                                  x))))))
