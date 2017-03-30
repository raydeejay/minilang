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
  (declare (ignore win))
  ;; redraw lines
  (loop :for turtle :in *turtles*
     :initially (progn (gl:load-identity)
                       (gl-setup *display-width* *display-height*)
                       ;; clear the display
                       (destructuring-bind (r g b a)
                           *paper*
                         (gl:clear-color r g b a))
                       (gl:clear :color-buffer :depth-buffer))

     :do (loop :for node :in (nodes (trail turtle))
            :doing (destructuring-bind (r g b)
                       (color node)
                     (gl:color r g b)
                     (gl:line-width (width node)))

            :when (has-partial-shape-p node)
            :do (gl:with-primitives :line-strip
                  (mapc (lambda (v) (apply 'gl:vertex v))
                        (reverse (partial-shape node))))

            :do (gl:with-primitives (primitive node)
                  (mapc (lambda (v) (apply 'gl:vertex v))
                        (complete-shapes node)))

            :do (draw-textures *firetex*))

     :finally (progn (draw-turtle))))


(defun handle-keydown (keysym)
  (let ((scancode (sdl2:scancode-value keysym))
        ;; (sym (sdl2:sym-value keysym))
        ;; (mod-value (sdl2:mod-value keysym))
        )
    (cond
      ((sdl2:scancode= scancode :scancode-w)
       (forward 10))
      ((sdl2:scancode= scancode :scancode-s)
       (back 10))
      ((sdl2:scancode= scancode :scancode-a)
       (left 15))
      ((sdl2:scancode= scancode :scancode-d)
       (right 15))
      ((sdl2:scancode= scancode :scancode-backspace)
       (undo (trail *turtle*)))
      ((sdl2:scancode= scancode :scancode-1)
       (ink 1 0 0))
      ((sdl2:scancode= scancode :scancode-2)
       (ink 0 1 0))
      ((sdl2:scancode= scancode :scancode-3)
       (ink 1 1 0))
      ((sdl2:scancode= scancode :scancode-4)
       (ink 0 0 1))
      ((sdl2:scancode= scancode :scancode-5)
       (ink 1 0 1))
      ((sdl2:scancode= scancode :scancode-6)
       (ink 0 1 1))
      ((sdl2:scancode= scancode :scancode-7)
       (ink 1 1 1))
      ((sdl2:scancode= scancode :scancode-0)
       (ink 0 0 0))
      ((sdl2:scancode= scancode :scancode-u)
       (pen-up))
      ((sdl2:scancode= scancode :scancode-d)
       (pen-down))
      ((sdl2:scancode= scancode :scancode-f5)
       (setf *turtle* (car *turtles*))
       (setf *turtles* (list *turtle*))
       (home)
       (point-to 90)
       (clear)))
    ;; (format t "Key sym: ~a, code: ~a, mod: ~a~%"
    ;;         sym
    ;;         scancode
    ;;         mod-value)
    ))

(defun handle-keyup (keysym)
  (when (sdl2:scancode= (sdl2:scancode-value keysym)
                        :scancode-escape)
    (sdl2:push-event :quit)))

(defun handle-mouse (button)
  (declare (ignore button))
  (multiple-value-bind (x y)
      (sdl2:mouse-state)
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer)
    (gl:enable :scissor-test)
    (gl:scissor x (- *display-height* y) 1 1)
    (render-turtles-for-picking)
    (let ((c (1- (elt (gl:read-pixels x (- *display-height* y) 1 1
                                      :rgba :unsigned-byte)
                      0))))
      (when (not (minusp c))
        (let ((*turtle* (nth c *turtles*)))
          ;; action here
          (forward 40)))))
  (gl:scissor 0 0 *display-width* *display-height*)
  (gl:disable :scissor-test))

(defun open-display% ()
  (sdl2:with-init
    (:everything)
    (sdl2:with-window (win :title "Minilang Display"
                           :w *display-width* :h *display-height*
                           :flags '(:shown :opengl))
      (setf *display-window* win)
      (sdl2:with-gl-context (gl-context win)
        (setf *turtles* nil)
        (init-turtle)

        (sdl2:gl-make-current win gl-context)
        (gl-setup *display-width* *display-height*)
        (destructuring-bind (r g b a)
            *paper*
          (gl:clear-color r g b a))
        (gl:clear :color-buffer :depth-buffer)
        (load-textures)
        
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)

          (:keydown
           (:keysym keysym)
           (handle-keydown keysym))

          (:keyup
           (:keysym keysym)
           (handle-keyup keysym))

          (:mousebuttondown
           (:button button)
           (handle-mouse button))

          (:idle
           ()
           (idle-func win)
           (sdl2:gl-swap-window win)
           (sleep 0.001)))))))          ; naively 100fps?

(define-primitive open-display ()
  (setf *display-thread*
        (make-thread 'open-display%
                     :name "Minilang Display")))

(define-primitive close-display ()
  (sdl2:push-quit-event))

;; lines include the end point, too (yes?)
;; so do compensate here, or maybe we should compensate somewhere else...?
(define-primitive line (x y xt yt)
  "Draw a line."
  ;; terminate the current node with relatively extreme prejudice :D
  (add (trail *turtle*) 'lines-node)
  (setf (vertices (top (trail *turtle*))) (list (list x y))) ;hack
  (add (top (trail *turtle*)) (list xt yt))
  (add (trail *turtle*) 'lines-node))

;; we need to compensate for rectangles too?
;; (define-primitive rectangle (x y w h)
;;   "Draw a rectangle."
;;   (mapcar (lambda (p) (push p (trail *turtle*)))
;;           `(,(list  x        y        (+ x w)  y        (color *turtle*) (pen-width *turtle*))
;;              ,(list x        y        x        (+ y h)  (color *turtle*) (pen-width *turtle*))
;;              ,(list (+ x w)  (+ y h)  (+ x w)  y        (color *turtle*) (pen-width *turtle*))
;;              ,(list (+ x w)  (+ y h)  x        (+ y h)  (color *turtle*) (pen-width *turtle*)))))

;; Compensating the missing pixel/point/whatever
;; (define-primitive plot (x y)
;;   "Draw a single point."
;;   (push (list (1- x) (1- y) x y (color *turtle*) (pen-width *turtle*))
;;         (trail *turtle*)))

(define-primitive clear ()
  (sdl2:in-main-thread ()
    (destructuring-bind (r g b a)
        *paper*
      (gl:clear-color r g b a))
    (gl:clear :color-buffer)
    (draw-turtle)
    (loop :for turtle :in *turtles* :do
       (setf (trail turtle)
             (make-instance 'trail
                            :nodes (list (make-instance 'lines-node)))))))

(define-primitive paper (r g b &optional (a 1.0))
  (setf *paper* (list r g b a)))

(define-primitive save-png (filename)
  (il:with-init
    (sdl2:in-main-thread ()
      (ilut:renderer :opengl)
      (let ((screenshot (il:gen-image)))
        (il:with-bound-image screenshot
          (ilut:gl-screen)
          (il:save-image filename))))))

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
    ;; (title "title")
    ;; (desc "desc")
    (loop :for turtle :in *turtles* :do
       (mapc (lambda (line)
               (let ((color (fifth line)))
                 (multiple-value-bind (r g b)
                     (values-list color)
                   (svg:draw scene (:line :x1 (first line) :y1 (second line)
                                          :x2 (third line) :y2 (fourth line)
                                          :stroke (format nil "rgba(~D,~D,~D,1)"
                                                          (* 255 r)
                                                          (* 255 g)
                                                          (* 255 b))
                                          :stroke-width (sixth line)
                                          :opacity 1)))))
             (trail turtle)))
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (svg:stream-out s scene))))
