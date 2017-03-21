;;;; minilang.lisp

(in-package #:minilang)

;; (defun base-env ()
;;   (let ((env (make-instance 'environment)))
;;     (def env "print" (lambda (callback txt)
;;                        (format t "~A" txt)
;;                        (funcall callback nil)))
;;     (def env "println" (lambda (callback txt)
;;                          (format t "~A~%" txt)
;;                          (funcall callback nil)))
;;     (def env "quit" (lambda (callback)
;;                       (declare (ignore callback))
;;                       (throw 'quit t)))
;;     env))

;; interpreted repl
;; TODO: catch errors and don't crash the REPL
;; (defun repl ()
;;   (catch 'quit
;;     (loop :with env := (base-env)
;;        :initially (evaluate (parse (prelude)) env 'dismiss)
;;        :initially (format t "Welcome to the minilang REPL~%> ")
;;        :doing (format t "~A~%> "
;;                       (evaluate (parse (read-line))
;;                                 env
;;                                 (lambda (x)
;;                                   (fresh-line)
;;                                   x))))))
