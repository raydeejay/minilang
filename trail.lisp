;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; trail
(defclass trail ()
  ((nodes :accessor nodes :initarg :nodes :initform nil)))

(defmethod add ((trail trail) node-class)
  ;; remove top node if it's empty
  (when (emptyp (car (nodes trail)))
    (pop (nodes trail)))
  (push (make-instance node-class
                       :color (color *turtle*)
                       :width (pen-width *turtle*))
        (nodes trail)))

(defmethod top ((trail trail))
  (car (nodes trail)))


(defclass node ()
  ((primitive :accessor primitive :initarg :primitive)
   (vertices  :accessor vertices  :initarg :vertices  :initform nil)
   (color     :accessor color     :initarg :color     :initform '(1 1 1))
   (width     :accessor width     :initarg :width     :initform 1)))

(defmethod add ((node node) vertex)
  (when (emptyp (vertices node))
    ;; add the current position to begin with
    (push (list (x *turtle*) (y *turtle*))
          (vertices node)))
  (push vertex (vertices node)))

(defmethod emptyp ((node node))
  (emptyp (vertices node)))

(defmethod setup ((node node))
  (destructuring-bind (r g b)
      (color node)
    (gl:color r g b))
  (gl:line-width (width node)))


(defclass points-node (node)
  ((primitive :reader primitive :initform :points)))


(defclass lines-node (node)
  ((primitive :reader primitive :initform :line-strip)))


(defclass triangles-node (node)
  ((primitive :reader   primitive :initform :triangles)))



(defparameter *trail* (list (make-instance 'lines-node))
  "Holds a list of nodes to redraw each frame.")
