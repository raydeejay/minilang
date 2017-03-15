;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; interpreter
(defun evaluate (exp env)
  (switch ((<- :type exp) :test 'equal)
    ("num"
      (<- :value exp))
    ("str"
      (<- :value exp))
    ("bool"
      (<- :value exp))
    ("var"
     (getv env (<- :value exp)))
    ("assign"
     (if (not (equal (<- :type (<- :left exp)) "var"))
         (error "Cannot assign to ~A" (<- :left exp))
         (setv env
               (<- :value (<- :left exp))
               (evaluate (<- :right exp) env))))
    ("binary"
     (apply-op (<- :operator exp)
               (evaluate (<- :left exp) env)
               (evaluate (<- :right exp) env)))
    ("lambda"
     (make-lambda env exp))
    ("let"
     (loop :for v :in (<- :vars exp)
        :do (let ((scope (extend env)))
              (def scope (<- :name v)
                (if (<- :def v)
                    (evaluate (<- :def v) env)
                    nil))
              (setf env scope)))
     (evaluate (<- :body exp) env))
    ("if"
     (if (evaluate (<- :cond exp) env)
         (evaluate (<- :then exp) env)
         (if (<- :else exp)
             (evaluate (<- :else exp) env))))
    ("prog"
     (loop :with val := nil
        :for subexp :in (<- :prog exp)
        :do (setf val (evaluate subexp env))
        :finally (return val)))
    ("call"
     (let ((func (evaluate (<- :func exp) env)))
       (apply func
              (mapcar (lambda (arg) (evaluate arg env))
                      (<- :args exp)))))
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
  (let ((lambda-env (if (<- :name exp) (extend env) env)))
    (if (<- :name exp)
        (def lambda-env (<- :name exp)
          (lambda (&rest args)
            (let ((params (<- :vars exp))
                  (scope (extend lambda-env)))
              (loop :for name :in params
                 :for n :from 0
                 :do (def scope name (nth n args))
                 :finally (return (evaluate (<- :body exp)
                                            scope))))))
        (lambda (&rest args)
            (let ((params (<- :vars exp))
                  (scope (extend lambda-env)))
              (loop :for name :in params
                 :for n :from 0
                 :do (def scope name (nth n args))
                 :finally (return (evaluate (<- :body exp)
                                            scope))))))))

