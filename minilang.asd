;;;; minilang.asd

(asdf:defsystem #:minilang
  :serial t
  :description "Describe minilang here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:parse-float #:arnesi #:lispbuilder-sdl #:bordeaux-threads)
  :components ((:file "package")
               (:file "utils")
               (:file "input-stream")
               (:file "token-stream")
               (:file "parser")
               (:file "env")
               ;; (:file "interp")
               (:file "compiler")
               ;; (:file "cps")
               (:file "optimizer")
               (:file "runtime")
               (:file "display")
               (:file "minilang")))

