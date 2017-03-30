;;;; minilang.asd

(asdf:defsystem #:minilang
  :serial t
  :description "A small programming language"
  :author "Sergi Reyner <sergi.reyner@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:parse-float
               #:arnesi
               #:sdl2
               #:cl-opengl
               #:bordeaux-threads
               #:cl-svg
               #:cl-devil
               #:cl-ilut)
  :components ((:file "package")
               (:file "utils")
               (:file "input-stream")
               (:file "token-stream")
               (:file "parser")
               ;; (:file "env")
               (:file "compiler")
               (:file "optimizer")
               (:file "symbols")
               (:file "help")
               (:file "runtime")
               (:file "textures")
               (:file "display")
               (:file "trail")
               (:file "turtle")
               (:file "minilang")))

