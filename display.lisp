;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defparameter *display-surface* nil)
(defparameter *display-thread* nil)
(defparameter *display-width* 640)
(defparameter *display-height* 400)

(defparameter *trail* nil
  "Holds a list of lines to redraw each frame.")

;; accounting for the inverted Y axis and operating on the fourth quadrant
;; not quite sure how... other than by swapping SIN and COS in the usual formula
(defun rotate-point (p degrees)
  (let ((f (to-radians degrees)))
    (cons (+ (* (car p) (cos f)) (* (cdr p) (sin f)))
          (- (* (cdr p) (cos f)) (* (car p) (sin f))))))

(defun to-origin (p anchor)
  (cons (- (car p) (car anchor))
        (- (cdr p) (cdr anchor))))

(defun from-origin (p anchor)
  (cons (+ (car p) (car anchor))
        (+ (cdr p) (cdr anchor))))

(defun gl-setup (&optional (width *display-width*) (height *display-height*))
  "Set up 1:1 pixel ortho matrix"
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 -1 1)
  (gl:matrix-mode :modelview))

(defun draw-turtle ()
  (when (visible *turtle*)
    (let ((anchor (cons (x *turtle*)
                        (y *turtle*))))
      (labels ((transform (p)
                 (from-origin
                  (rotate-point
                   (to-origin p anchor)
                   (heading *turtle*))
                  anchor)))
        (let ((p1 (transform
                   (cons (car anchor)
                         (- (cdr anchor) 4))))
              (p2 (transform
                   (cons (+ 13 (car anchor))
                         (cdr anchor))))
              (p3 (transform
                   (cons (car anchor)
                         (+ 4 (cdr anchor))))))
          (gl:with-primitive :triangles
            (apply 'gl:color (color *turtle*))
            (gl:vertex (car p1) (cdr p1))
            (gl:vertex (car p2) (cdr p2))
            (gl:vertex (car p3) (cdr p3)))
          (gl:flush))))))

(defun idle-func (win)
  (gl:load-identity)
  (gl-setup *display-width* *display-height*)

  ;; clear the display
  (gl:clear-color 0.0 0.0 1.0 1.0)
  (gl:clear :color-buffer :depth-buffer)

  ;; redraw lines
  (gl:line-width 1)
  (gl:with-primitives :lines
    (mapc (lambda (coords)
            (let ((color (fifth coords)))
              (gl:color (first color) (second color) (third color)))
            (gl:vertex (first coords) (second coords))
            (gl:vertex (third coords) (fourth coords)))
          *trail*))
  (gl:flush)

  ;; draw turtle
  (draw-turtle)

  ;; stuff
  (sdl2:gl-swap-window win))

(defun open-display% ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Minilang Display"
                           :w *display-width* :h *display-height*
                           :flags '(:shown :opengl))
      (sdl2:with-renderer (renderer win)
        (sdl2:with-gl-context (gl-context win)
          (init-turtle)
          (setf *trail* nil)

          (sdl2:gl-make-current win gl-context)
          (gl-setup *display-width* *display-height*)
          (gl:clear-color 0.0 0.0 1.0 1.0)
          (gl:clear :color-buffer :depth-buffer)

          (sdl2:with-event-loop (:method :poll)
            (:quit () t)

            ;; (:keydown
            ;;  (:keysym keysym)
            ;;  (let ((scancode (sdl2:scancode-value keysym))
            ;;        (sym (sdl2:sym-value keysym))
            ;;        (mod-value (sdl2:mod-value keysym)))
            ;;    (cond
            ;;      ((sdl2:scancode= scancode :scancode-w)
            ;;       (format t "~a~%" "WALK"))
            ;;      ((sdl2:scancode= scancode :scancode-s)
            ;;       (sdl2:show-cursor))
            ;;      ((sdl2:scancode= scancode :scancode-h)
            ;;       (sdl2:hide-cursor)))
            ;;    (format t "Key sym: ~a, code: ~a, mod: ~a~%"
            ;;            sym
            ;;            scancode
            ;;            mod-value)))

            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym)
                                   :scancode-escape)
               (sdl2:push-event :quit)))

            (:idle
             ()
             (idle-func win)
             (sleep 0.01))))))))        ; naively 100fps?

(define-primitive open-display ()
  (setf *display-thread*
        (make-thread 'open-display%
                     :name "Minilang Display")))

(define-primitive close-display ()
  (sdl2:push-quit-event))

;; lines include the end point, too (yes?)
;; so we compensate here, or maybe we should compensate somewhere else...
(define-primitive line (x y xt yt)
  "Draw a line."
  (push (list x y xt yt (color *turtle*))
        *trail*))

;; we need to compensate for rectangles too?
(define-primitive rectangle (x y w h)
  "Draw a line."
  (mapcar (lambda (p) (push p *trail*))
          `(,(list x         y        (+ x w)  y        (color *turtle*))
             ,(list x        y        x        (+ y h)  (color *turtle*))
             ,(list (+ x w)  (+ y h)  (+ x w)  y        (color *turtle*))
             ,(list (+ x w)  (+ y h)  x        (+ y h)  (color *turtle*)))))

;; Compensating the missing pixel/point/whatever
(define-primitive plot (x y)
  "Draw a single point."
  (push (list (1- x) (1- y) x y (color *turtle*))
        *trail*))

(define-primitive clear ()
  (sdl2:in-main-thread ()
    (gl:clear :color-buffer)
    (draw-turtle)
    (setf *trail* nil)))
