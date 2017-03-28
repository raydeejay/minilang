;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defparameter *display-window* nil)
(defparameter *display-thread* nil)
(defparameter *display-width* 640)
(defparameter *display-height* 400)

(defparameter *paper* (list 0 0 0 1)
  "RGBA color of the background of the drawing field.")

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

(defun idle-func (win)
  (gl:load-identity)
  (gl-setup *display-width* *display-height*)

  ;; clear the display
  (destructuring-bind (r g b a)
      *paper*
    (gl:clear-color r g b a))
  (gl:clear :color-buffer :depth-buffer)

  ;; redraw lines
  (loop :for node :in *trail* :doing
     (mapc (lambda (coords)
             (destructuring-bind (r g b)
                 (color node)
               (gl:color r g b))
             (gl:line-width (width node))
             (gl:with-primitives (primitive node)
               (gl:vertex (first coords) (second coords))
               (gl:vertex (third coords) (fourth coords))))
           (vertices node)))
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
      (setf *display-window* win)
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-gl-context (gl-context win)
          (init-turtle)
          (setf *trail* (list (make-instance 'lines-node)))

          (sdl2:gl-make-current win gl-context)
          (gl-setup *display-width* *display-height*)
          (destructuring-bind (r g b a)
              *paper*
            (gl:clear-color r g b a))
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
;; so do compensate here, or maybe we should compensate somewhere else...?
;; (define-primitive line (x y xt yt)
;;   "Draw a line."
;;   (push (list x y xt yt (color *turtle*) (pen-width *turtle*))
;;         *trail*))

;; we need to compensate for rectangles too?
;; (define-primitive rectangle (x y w h)
;;   "Draw a rectangle."
;;   (mapcar (lambda (p) (push p *trail*))
;;           `(,(list  x        y        (+ x w)  y        (color *turtle*) (pen-width *turtle*))
;;              ,(list x        y        x        (+ y h)  (color *turtle*) (pen-width *turtle*))
;;              ,(list (+ x w)  (+ y h)  (+ x w)  y        (color *turtle*) (pen-width *turtle*))
;;              ,(list (+ x w)  (+ y h)  x        (+ y h)  (color *turtle*) (pen-width *turtle*)))))

;; Compensating the missing pixel/point/whatever
;; (define-primitive plot (x y)
;;   "Draw a single point."
;;   (push (list (1- x) (1- y) x y (color *turtle*) (pen-width *turtle*))
;;         *trail*))

(define-primitive clear ()
  (sdl2:in-main-thread ()
    (gl:clear-color (first *paper*) (second *paper*) (third *paper*) (fourth *paper*))
    (gl:clear :color-buffer)
    (draw-turtle)
    (setf *trail* (list (make-instance 'lines-node)))))

(define-primitive paper (r g b)
  (setf *paper* (list r g b 1)))

;; quick and dirty
(define-primitive save-display (filename)
  ;; doesn't work...
  (sdl2:raise-window *display-window*)
  (uiop/run-program:run-program (format nil
                                        "gnome-screenshot -w -B -f \"~A\""
                                        filename)))

(define-primitive save-svg (filename)
  (let ((scene (svg:make-svg-toplevel 'svg:svg-1.2-toplevel
                                      :width *display-width*
                                      :height *display-height*)))
    (multiple-value-bind (r g b a)
        (values-list *paper*)
      (svg:draw scene (:rect :x 0 :y 0
                             :width *display-width*
                             :height *display-height*
                             :fill (format nil "rgb(~D,~D,~D,~D)"
                                           (* 255 r)
                                           (* 255 g)
                                           (* 255 b)
                                           a))))
    (mapc (lambda (line)
            (let ((color (fifth line)))
              (multiple-value-bind (r g b)
                  (values-list color)
                ;; (title "title")
                ;; (desc "desc")
                (svg:draw scene (:line :x1 (first line) :y1 (second line)
                                       :x2 (third line) :y2 (fourth line)
                                       :stroke (format nil "rgba(~D,~D,~D,1)"
                                                       (* 255 r)
                                                       (* 255 g)
                                                       (* 255 b))
                                       :stroke-width (sixth line)
                                       :opacity 1)))))
          *trail*)
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (svg:stream-out s scene))))
