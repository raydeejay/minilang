;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; trail
(defclass trail ()
  ((nodes :accessor nodes :initarg :nodes :initform nil)))

(defmethod add ((trail trail) node-class)
  ;; remove top node if it's empty
  (when (emptyp (top trail))
    (pop (nodes trail)))
  (push (make-instance node-class
                       :color (color *turtle*)
                       :width (pen-width *turtle*))
        (nodes trail)))

(defmethod top ((trail trail))
  (car (nodes trail)))

(defparameter *trail* (list (make-instance 'lines-node))
  "Holds a list of nodes to redraw each frame.")


;; the base node
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

;; points (WIP)
(defclass points-node (node)
  ((primitive :reader primitive :initform :points)))


;; lines
(defclass lines-node (node)
  ((primitive :reader primitive :initform :line-strip)))

(defmethod has-partial-shape-p ((node lines-node))
  (= 1 (length (vertices node))))

(defmethod partial-shape ((node lines-node))
  ;; stub
  )

(defmethod complete-shapes ((node lines-node))
  (vertices node))


;; triangles
(defclass triangles-node (node)
  ((primitive :reader   primitive :initform :triangles)))

(defmethod has-partial-shape-p ((node triangles-node))
  (plusp (mod (length (vertices node)) 3)))

(defmethod partial-shape ((node triangles-node))
  (subseq (vertices node)
          0
          (mod (length (vertices node)) 3)))

(defmethod complete-shapes ((node triangles-node))
  (subseq (vertices node)
          (mod (length (vertices node)) 3)))
