;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

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

(defmethod undo ((node node))
  (when (not (emptyp (vertices node)))
    (pop (vertices node))))

(defmethod setup ((node node))
  (destructuring-bind (r g b)
      (color node)
    (gl:color r g b))
  (gl:line-width (width node)))

(defmethod normalize ((node node))
  (when (has-partial-shape-p node)
    (setf (vertices node)
          (nthcdr (length (partial-shape node))
                  (vertices node)))))

(defmethod top ((node node))
  (car (vertices node)))

;; points (WIP)
(defclass points-node (node)
  ((primitive :reader primitive :initform :points)))


;; lines
(defclass lines-node (node)
  ((primitive :reader primitive :initform :line-strip)))

(defmethod has-partial-shape-p ((node lines-node))
  (= 1 (length (vertices node))))

(defmethod normalize ((node lines-node))
  (when (has-partial-shape-p node)
    (setf (vertices node) nil)))

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


;; quads
(defclass quads-node (node)
  ((primitive :reader   primitive :initform :quads)))

(defmethod has-partial-shape-p ((node quads-node))
  (plusp (mod (length (vertices node)) 4)))

(defmethod partial-shape ((node quads-node))
  (subseq (vertices node)
          0
          (mod (length (vertices node)) 4)))

(defmethod complete-shapes ((node quads-node))
  (subseq (vertices node)
          (mod (length (vertices node)) 4)))


;; trail
(defclass trail ()
  ((nodes :accessor nodes :initarg :nodes :initform (list (make-instance 'lines-node)))))

(defmethod add ((trail trail) node-class)
  ;; remove top node if it's empty
  (when (emptyp (top trail))
    (pop (nodes trail)))
  (push (make-instance node-class
                       :color (color *turtle*)
                       :width (pen-width *turtle*))
        (nodes trail)))

(defmethod undo ((trail trail))
  ;; if the top node is empty, get rid of it
  (when (and (emptyp (top trail))
             (> (length (nodes trail)) 1))
    (pop (nodes trail)))

  ;; undo the last move if there is one
  (undo (top trail))

  ;; if the top node is not empty, relocate the turtle to that place
  (when (not (emptyp (top trail)))
    (destructuring-bind (x y)
        (top (top trail))
      (setf (x *turtle*) x
            (y *turtle*) y))))

(defmethod top ((trail trail))
  (car (nodes trail)))


(defparameter *trail* (list (make-instance 'trail))
  "Holds a list of nodes to redraw each frame.")

