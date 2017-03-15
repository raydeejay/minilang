;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; interpreter
(defun evaluate (exp env callback)
  (switch ((<- :type exp) :test 'equal)
    ("num"
     (funcall callback (<- :value exp)))
    ("str"
     (funcall callback (<- :value exp)))
    ("bool"
     (funcall callback (<- :value exp)))
    ("var"
     (funcall callback (getv env (<- :value exp))))
    ("assign"
     (if (not (equal (<- :type (<- :left exp)) "var"))
         (error "Cannot assign to ~A" (<- :left exp))
         (evaluate (<- :right exp)
                   env
                   (lambda (right)
                     (setv env
                           (<- :value (<- :left exp))
                           right)
                     (funcall callback env)))))
    ("binary"
     (evaluate (<- :left exp)
               env
               (lambda (left)
                 (evaluate (<- :right exp)
                           env
                           (lambda (right)
                             (funcall callback (apply-op (<- :operator exp)
                                                         left
                                                         right)))))))
    ("lambda"
     (funcall callback (make-lambda env exp)))
    ("let"
     (labels ((loop% (env i)
                 (if (< i (length (<- :vars exp)))
                     (let ((v (nth i (<- :vars exp))))
                       (if (<- :def v)
                           (evaluate (<- :def v)
                                     env
                                     (lambda (value)
                                       (let ((scope (extend env)))
                                         (def scope (<- :name v) value)
                                         (loop% scope (1+ i)))))
                           (let ((scope (extend env)))
                             (def scope (<- :name v) nil)
                             (loop% scope (1+ i)))))
                     (evaluate (<- :body exp) env callback))))
       (loop% env 0)))
    ("if"
     (evaluate (<- :cond exp)
               env
               (lambda (cond)
                 (if cond
                     (evaluate (<- :then exp) env callback)
                     (if (<- :else exp)
                         (evaluate (<- :else exp) env callback)
                         (funcall callback nil))))))
    ("prog"
     (labels ((loop% (last i)
                 (if (< i (length (<- :prog exp)))
                     (evaluate (nth i (<- :prog exp))
                               env
                               (lambda (val)
                                 (loop% val (1+ i))))
                     (funcall callback last))))
       (loop% nil 0)))
    ("call"
     (evaluate (<- :func exp)
               env
               (lambda (func)
                 (labels ((loop% (args i)
                             (if (< i (length (<- :args exp)))
                                 (evaluate (nth i (<- :args exp))
                                           env
                                           (lambda (arg)
                                             (loop% (append args
                                                            (list arg))
                                                (1+ i))))
                                 (apply func args))))
                   (loop% (list callback) 0)))))
    (otherwise (error "I don't know how to evaluate ~A"
                      (<- :type exp)))))

(defun apply-op (op a b)
  (labels ((num (x)
             (if (not (numberp x))
                 (error "Expected number but got ~A" x)
                 x))
           (div (x)
             (if (zerop (num x))
                 (error "Divide by zero")
                 x)))
    (switch (op :test 'equal)
      ("+" (+ (num a) (num b)))
      ("-" (- (num a) (num b)))
      ("*" (* (num a) (num b)))
      ("/" (/ (num a) (div b)))
      ("%" (mod (num a) (div b)))
      ("<" (< (num a) (num b)))
      (">" (> (num a) (num b)))
      ("<=" (<= (num a) (num b)))
      (">=" (>= (num a) (num b)))
      ("==" (equal a b))
      ("!=" (not (equal a b)))
      ("&&" (and a b))
      ("||" (or a b))
      (t (error "Can't apply operator ~A" op)))))

;; I know... but I can't figure out how to do it properly right now
(defun make-lambda (env exp)
  (labels ((lambda% (callback &rest args)
             (let ((params (<- :vars exp))
                   (scope (extend env)))
               (when (<- :name exp)
                 (def scope (<- :name exp) #'lambda%))
               (loop :for name :in params
                  :for n :from 0
                  :do (def scope name (nth n args))
                  :finally (return (evaluate (<- :body exp)
                                             scope
                                             callback))))))
    #'lambda%))

