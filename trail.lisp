;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)
;; a trail has nodes

;; each kind of node has
;; - a primitive
;; - a setup method which executes the appropriate commands

;; each node has
;; - a list of elements

;; for each in trail
;;   read primitive

(defclass node ()
  ((primitive :accessor primitive :initarg :primitive)
   (vertices  :accessor vertices  :initarg :vertices  :initform nil)
   (color     :accessor color     :initarg :color     :initform '(1 1 1))
   (width     :accessor width     :initarg :width     :initform 1)))

(defclass lines-node (node)
  ((primitive :reader primitive :initform :lines)))

(defclass triangles-node (node)
  ((primitive :reader   primitive :initform :triangles)
   (fill?     :accessor fill?     :initform T)))

(defparameter *trail* (list (make-instance 'lines-node))
  "Holds a list of nodes to redraw each frame.")

(defmethod add ((node node) vertex)
  (with-slots (vertices)
      node
    (push vertex vertices)))

(defmethod setup ((node node))
  (with-slots (color width)
      node
    (destructuring-bind (r g b)
        (color node)
      (gl:color r g b))
    (gl:line-width width)))

(defmethod setup ((node triangles-node))
  (with-slots (fill?)
      node
    (when fill?)
    ;; something
    )
  (call-next-method))
