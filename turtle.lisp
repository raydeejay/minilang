;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defclass turtle ()
  ((x         :accessor x         :initarg :x         :initform 320)
   (y         :accessor y         :initarg :y         :initform 200)
   (heading   :accessor heading   :initarg :heading   :initform 90)
   (color     :accessor color     :initarg :color     :initform (list 1 1 1))
   (pen-width :accessor pen-width :initarg :pen-width :initform 1)
   (visible   :accessor visible   :initarg :visible   :initform T)
   (pen       :accessor pen       :initarg :pen       :initform T)))

(defun make-turtle ()
    (make-instance 'turtle))

(defparameter *turtle* nil)
(defparameter *trail* nil
  "Holds a list of lines to redraw each frame.")

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
        (dy (* n (sin (to-radians (heading *turtle*)))))
        (old-x (x *turtle*))
        (old-y (y *turtle*)))
    ;; decrement because the Y axis is backwards,
    ;; this should be dealt with using a transform function...
    (incf (x *turtle*) dx)
    (decf (y *turtle*) dy)

    ;; add current line to trail
    (when (and (pen *turtle*)
               (or (/= old-x (x *turtle*))
                   (/= old-y (y *turtle*))))
      (push (list (x *turtle*) (y *turtle*)
                  old-x old-y
                  (color *turtle*)
                  (pen-width *turtle*))
            *trail*))))

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
  ;; fake it
  ;; (line (x *turtle*) (y *turtle*) 320 200)
  (goto 320 200))

(define-primitive goto (x y)
  (when (pen *turtle*)
    (push (list (x *turtle*) (y *turtle*) x y (color *turtle*) (pen-width *turtle*))
          *trail*))
  (setf (x *turtle*) x
        (y *turtle*) y))

(define-primitive coords ()
  (cons (x *turtle*) (y *turtle*)))

(define-primitive hide ()
  (setf (visible *turtle*) NIL))

(define-primitive show ()
  (setf (visible *turtle*) T))

(define-primitive pen-up ()
  (setf (pen *turtle*) NIL))

(define-primitive pen-down ()
  (setf (pen *turtle*) T))

(define-primitive ink (r g b)
  (setf (color *turtle*) (list r g b)))

(define-primitive pen-size (n)
  (setf (pen-width *turtle*) n))

(define-primitive-alias ht hide)
(define-primitive-alias st show)
(define-primitive-alias pu pen-up)
(define-primitive-alias pd pen-down)
(define-primitive-alias ps pen-size)
