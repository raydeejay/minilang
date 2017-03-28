;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defclass turtle ()
  ((x         :accessor x         :initarg :x         :initform 320)
   (y         :accessor y         :initarg :y         :initform 200)
   (heading   :accessor heading   :initarg :heading   :initform 90)
   (color     :accessor color     :initarg :color     :initform (list 1 1 1))
   (pen-width :accessor pen-width :initarg :pen-width :initform 1)
   (visible   :accessor visible   :initarg :visible   :initform T)
   (pen       :accessor pen       :initarg :pen       :initform T)
   (primitive :accessor primitive :initarg :primitive :initform :lines)))

(defun make-turtle ()
    (make-instance 'turtle))

(defparameter *turtle* nil)

(defun init-turtle ()
  (setf *turtle* (make-turtle)))

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


;; turtle primitives
(define-primitive forward (n)
  (let ((dx (* n (cos (to-radians (heading *turtle*)))))
        (dy (* n (sin (to-radians (heading *turtle*))))))
    ;; calculate the new coordinates and call goto
    ;; decrement because the Y axis is backwards
    (let ((new-x (+ (x *turtle*) dx))
          (new-y (- (y *turtle*) dy)))
      (goto new-x new-y))))

(define-primitive back (n)
  (forward (- n)))

(define-primitive left (n)
  (incf (heading *turtle*) n))

(define-primitive right (n)
  (decf (heading *turtle*) n))

(define-primitive-alias fd forward)
(define-primitive-alias bk back)
(define-primitive-alias lt left)
(define-primitive-alias rt right)

(define-primitive point-to (n)
  (setf (heading *turtle*) n))

;; Y axis is inverted
(define-primitive point-at (x y)
  (let ((dx (- x (x *turtle*)))
        (dy (- (y *turtle*) y)))
    (setf (heading *turtle*)
          (/ (* (atan dy dx) 180) pi))))

(define-primitive home ()
  (point-at 320 200)
  (goto 320 200))

(define-primitive goto (x y)
  (if (pen *turtle*)
      (add (top *trail*)
           (list x y))
      (add *trail* 'lines-node))
  (setf (x *turtle*) x
        (y *turtle*) y))
(doc! "goto" "Tell the turtle to go to a specific pair of coordinates. This will produce a drawing if the pen is down.")

(define-primitive coords ()
  (cons (x *turtle*) (y *turtle*)))

(define-primitive hide ()
  (setf (visible *turtle*) NIL))
(doc! "hide" "Tell the turtle to hide itself.")

(define-primitive show ()
  (setf (visible *turtle*) T))
(doc! "show" "Tell the turtle to display itself on screen.")

(define-primitive pen-up ()
  (setf (pen *turtle*) NIL))
(doc! "pen-up" "Tell the turtle to lift the pen up. Further movement will not produce a drawing.")

(define-primitive pen-down ()
  (setf (pen *turtle*) T))
(doc! "pen-down" "Tell the turtle to place the pen down. Further movement will produce a drawing in the turtle's current ink color.")

(define-primitive ink (r g b)
  (let ((color (list r g b)))
    (setf (color *turtle*) color)
    (push (make-instance 'lines-node
                         :color color
                         :width (pen-width *turtle*))
          (nodes *trail*))))
(doc! "ink" "Sets the color of the turtle and its trail. Takes red, green, and blue components as parameters, ranging from 0 to 1. See also the global COLORS.")

(define-primitive pen-size (n)
  (setf (pen-width *turtle*) n)
  (push (make-instance 'lines-node
                       :width n
                       :color (color *turtle*))
        (nodes *trail*)))

(define-primitive shape (shape)
  (switch (shape :test 'equal)
    ("lines" (setf (primitive *turtle*) :line-strip)
             (add *trail* 'lines-node))
    ("triangles" (setf (primitive *turtle*) :triangles)
                 (add *trail* 'triangles-node))
    (otherwise (error "~A is not a valid argument for SHAPE." shape))))

(define-primitive-alias ht hide)
(define-primitive-alias st show)
(define-primitive-alias pu pen-up)
(define-primitive-alias pd pen-down)
(define-primitive-alias ps pen-size)
