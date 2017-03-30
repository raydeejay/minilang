;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defparameter *firetex* nil)

(defun load-textures ()
  (let ((tex (gl:gen-texture)))
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d
                      :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d
                      :texture-min-filter :linear)
                     
    (il:with-init
      (ilut:renderer :opengl)
      (let ((img (il:gen-image)))
        (il:with-bound-image img
          (il:load-image "/home/raydj/lisp/minilang/fire.png")
          (il:convert-image :rgba :unsigned-byte)
          (gl:tex-image-2d :texture-2d 0 :rgba
                           (il:image-width) (il:image-height)
                           0 :rgba :unsigned-byte
                           (il:get-data))
          (setf *firetex* (list tex
                                (il:image-width)
                                (il:image-height))))))
    (gl:bind-texture :texture-2d 0)
    (gl:disable :texture-2d)))

(defun draw-textures (texture)
  (let ((id (first texture))
        (width (second texture))
        (height (third texture)))
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d id)
    (gl:color 1 1 1)
    (gl:with-primitives :quads
      (gl:tex-coord 0 0)
      (gl:vertex 0 0)
      (gl:tex-coord 0 1)
      (gl:vertex 0 height)
      (gl:tex-coord 1 1)
      (gl:vertex width height)
      (gl:tex-coord 1 0)
      (gl:vertex width 0))
    (gl:bind-texture :texture-2d 0)
    (gl:disable :texture-2d)))
