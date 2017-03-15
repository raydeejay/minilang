;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; environment
(defclass environment ()
  ((parent :accessor parent :initarg :parent :initform nil)
   (vars   :accessor vars   :initarg :vars :initform (make-hash-table :test 'equal))))

(defmethod extend ((env environment))
  "Creates a subscope."
  (make-instance 'environment :parent env))

(defmethod lookup ((env environment) name)
  "Find the scope where the variable with the given name is defined."
  (loop :with scope := env
     :while scope
     :when (gethash name (vars scope))
     :return scope
     :do (setf scope (parent scope))))

(defmethod getv ((env environment) name)
  "Get the current value of a variable. Throws an error if the
  variable is not defined."
  ;; this has a bug because values can be null... can't they?
  (loop :with scope := env
     :while scope
     :when (gethash name (vars scope))
     :return it
     :do (setf scope (parent scope))
     :finally (error "Undefined variable: ~A" name)))

(defmethod setv ((env environment) name value)
  "Set the value of a variable. This needs to lookup the actual scope
  where the variable is defined. If it's not found and we're not in
  the global scope, throws an error."
  (let ((scope (lookup env name)))
    ;; let's not allow defining globals from a nested environment
    (if (and (null scope) (parent env))
        (error "Globals can only be defined at toplevel: ~A" name)
        (setf (gethash name (vars (or scope env)))
              value))))

(defmethod def ((env environment) name value)
  "Creates (or shadows, or overwrites) a variable in the current scope."
  (setf (gethash name (vars env)) value))
