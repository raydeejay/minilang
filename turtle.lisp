;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; backward

;; move +/-
;; turn +/-

;; point-at absolutes

(defclass turtle ()
  ((x       :accessor x       :initarg :x :initform 300)
   (y       :accessor y       :initarg :y :initform 200)
   (heading :accessor heading :initarg :heading :initform 270)
   (color   :accessor color   :initarg :color :initform sdl:*white*)
   (visible :accessor visible :initarg :visible :initform T)
   (pen :accessor pen :initarg :pen :initform T)))

(defun make-turtle ()
    (make-instance 'turtle))

(defparameter *turtle* nil)
(defparameter *turtle-icon* nil)
(defparameter *turtle-surface* nil)

(defun init-turtle ()
  (setf *turtle* (make-turtle)
        *turtle-icon* (sdl:create-surface 16 16))
  (sdl:draw-filled-trigon (sdl:point :x 3 :y 15)
                          (sdl:point :x 8 :y 1)
                          (sdl:point :x 12 :y 15)
                          :color (color *turtle*)
                          :surface *turtle-icon*))

;; this probably shouldn't be exposed
(define-primitive draw ()
  (eval-in-display
   (lambda (s)
     (sdl:draw-surface-at-*
      (sdl-gfx:rotate-surface (- 270 (heading *turtle*))
                              :surface *turtle-icon*)
      (- (x *turtle*) 8)
      (- (y *turtle*) 8)
      :surface s))))

;; (defparameter minilang-runtime::fd
;;   (defun fd (n)
;;     (let ((dx (* n (cos (sdl:to-radian (heading *turtle*)))))
;;           (dy (* n (sin (sdl:to-radian (heading *turtle*))))))
;;       (incf (x *turtle*) (truncate dx))
;;       (incf (y *turtle*) (truncate dy)))))

(define-primitive forward (n)
  (let ((dx (* n (cos (sdl:to-radian (heading *turtle*)))))
        (dy (* n (sin (sdl:to-radian (heading *turtle*))))))
    (let ((old-x (x *turtle*))
          (old-y (y *turtle*)))
      (incf (x *turtle*) (truncate dx))
      (incf (y *turtle*) (truncate dy))
      (when (pen *turtle*)
        (line old-x old-y (x *turtle*) (y *turtle*))))))

(define-primitive back (n)
  (let ((dx (* n (cos (sdl:to-radian (heading *turtle*)))))
        (dy (* n (sin (sdl:to-radian (heading *turtle*))))))
    (decf (x *turtle*) (truncate dx))
    (decf (y *turtle*) (truncate dy))))

(define-primitive left (n)
  (decf (heading *turtle*) n))

(define-primitive right (n)
  (incf (heading *turtle*) n))

(define-primitive-alias fd forward)
(define-primitive-alias bk back)
(define-primitive-alias lt left)
(define-primitive-alias rt right)

(define-primitive point-to (n)
  (setf (heading *turtle*) n))

(define-primitive goto (x y)
  (setf (x *turtle*) (truncate x)
        (y *turtle*) (truncate y)))

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

(define-primitive-alias ht hide)
(define-primitive-alias st show)
(define-primitive-alias pu pen-up)
(define-primitive-alias pd pen-down)
