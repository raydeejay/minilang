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

(defmethod add ((node node) vertex)
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
  ((primitive :reader primitive :initform :lines)))


(defclass triangles-node (node)
  ((primitive :reader   primitive :initform :triangles)))



(defparameter *trail* (list (make-instance 'lines-node))
  "Holds a list of nodes to redraw each frame.")
