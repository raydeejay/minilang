;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defparameter *display-surface* nil)

(let ((random-color sdl:*white*)
      display-thread)
  (defun open-display% ()
    (sdl:with-init ()
      (sdl:window 640 400 :title-caption "Minilang Display")
      (setf (sdl:frame-rate) 60)
      (setf *display-surface* (sdl:create-surface 640 400 ))
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event () (sdl:push-quit-event))
        (:idle
         ()
         ;; Change the color of the box if the left mouse button is depressed
         (when (sdl:mouse-left-p)
           (setf random-color (sdl:color :r (random 255)
                                         :g (random 255)
                                         :b (random 255))))
         ;; Clear the display each game loop
         (sdl:clear-display sdl:*black*)
         ;; draw the display
         (sdl:draw-surface *display-surface*)
         ;; Draw the box having a center at the mouse x/y coordinates.
         (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x)
                                                      (sdl:mouse-y)
                                                      20 20)
                       :color random-color)
         ;; Redraw the display
         (sdl:update-display)))))

  (defun open-display ()
    (setf display-thread
          (make-thread 'open-display%
                       :name "Minilang Display")))

  (defun close-display ()
    (sdl:push-quit-event))

  (defun eval-in-display (fn)
    (when display-thread
      (interrupt-thread display-thread fn *display-surface*))))


(defparameter minilang-runtime::open-display #'open-display)
(defparameter minilang-runtime::close-display #'close-display)
(defparameter minilang-runtime::eval-in-display #'eval-in-display)

(defparameter minilang-runtime::line
  (lambda (x y xt yt)
    (eval-in-display
     (lambda (s)
       (sdl:draw-line-* x y xt yt
                        :surface s
                        :color sdl:*white*)))))

(defparameter minilang-runtime::rectangle
  (lambda (x y w h)
    (eval-in-display
     (lambda (s)
       (sdl:draw-rectangle-* x y w h
                             :surface s
                             :color sdl:*white*)))))
