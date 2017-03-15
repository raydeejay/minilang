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
    (def env "print" (lambda (txt) (format t "~A" txt)))
    (def env "println" (lambda (txt) (format t "~A~%" txt)))
    (def env "quit" (lambda () (throw 'quit t)))
    env))

(defun prelude ()
  "Standard library. To be moved to its own file later."
  "# minilang standard library

# cons cells
cons = lambda(x, y)
         lambda(a, i, v)
           if a == \"get\"
              then if i == 0 then x else y
              else if i == 0 then x = v else y = v;

car = lambda(cell) cell(\"get\", 0);
cdr = lambda(cell) cell(\"get\", 1);
set-car! = lambda(cell, val) cell(\"set\", 0, val);
set-cdr! = lambda(cell, val) cell(\"set\", 1, val);

# NIL can be a real cons this time
NIL = cons(0, 0);
set-car!(NIL, NIL);
set-cdr!(NIL, NIL);

foreach = lambda(list, f)
            if list != NIL {
              f(car(list));
              foreach(cdr(list), f);
            };

range = lambda(a, b)
          if a <= b then cons(a, range(a + 1, b))
                    else NIL;
")

(defun repl ()
  (catch 'quit
    (loop :with env := (base-env)
       :initially (evaluate (parse (prelude)) env)
       (format t "Welcome to the minilang REPL~%> ")
       :doing (format t "~A~%> "
                      (evaluate (parse (read-line)) env)))))
