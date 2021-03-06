;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defun is-constant (exp)
  (member (<- :type exp) '("num" "str" "bool") :test 'equal))

(defun has-side-effects (exp)
  (switch ((<- :type exp) :test 'equal)
    ("call" T)
    ("assign" T)
    ("num" nil)
    ("str" nil)
    ("bool" nil)
    ("var" nil)
    ("lambda" nil)
    ("binary" (or (has-side-effects (<- :left exp))
                  (has-side-effects (<- :right exp))))
    ("if" (or (has-side-effects (<- :cond exp))
              (has-side-effects (<- :then exp))
              (<- :else exp)
              (has-side-effects (<- :else exp))))
    ("let" (loop :for v :in (<- :vars exp)
              :when (and (<- :def v)
                         (has-side-effects (<- :def v)))
              :return T
              :finally (return (has-side-effects (<- :body exp)))))
    ("prog" (loop :for p :in (<- :prog exp)
              :when (has-side-effects p)
               :return T
               :finally (return nil)))))

(defun opt (exp)
  (switch ((<- :type exp) :test 'equal)
    ("num" exp)
    ("str" exp)
    ("bool" exp)
    ("var" exp)
    ("binary" (opt-binary exp))
    ("prog" (opt-prog exp))
    ("if" (opt-if exp))
    (otherwise exp)))

(defun opt-binary (exp)
  (labels ((num (exp)
             (if (not (equal (<- :type exp) "num"))
                 (error "Not a number ~A" exp)
                 (<- :value exp)))
           (div (exp)
             (if (zerop (num exp))
                 (error "Divide by zero")
                 (<- :value exp))))
    ;; this should be destructive perhaps?
    (let ((exp (list :type "binary"
                     :operator (<- :operator exp)
                     :left (opt (<- :left exp))
                     :right (opt (<- :right exp)))))
      (if (and (is-constant (<- :left exp))
               (is-constant (<- :right exp)))
          (switch ((<- :operator exp) :test 'equal)
            ("+"
             ;; (increase-changed)
             (list :type "num" :value (+ (num (<- :left exp))
                                         (num (<- :right exp)))))
            ("-"
             ;; (increase-changed)
             (list :type "num" :value (- (num (<- :left exp))
                                         (num (<- :right exp)))))
            ("*"
             ;; (increase-changed)
             (list :type "num" :value (* (num (<- :left exp))
                                         (num (<- :right exp)))))
            ("/"
             ;; (increase-changed)
             (list :type "num" :value (/ (num (<- :left exp))
                                         (div (<- :right exp)))))
            ("%"
             ;; (increase-changed)
             (list :type "num" :value (mod (num (<- :left exp))
                                           (div (<- :right exp)))))
            ("<"
             ;; (increase-changed)
             (list :type "bool" :value (< (num (<- :left exp))
                                          (num (<- :right exp)))))
            (">"
             ;; (increase-changed)
             (list :type "bool" :value (> (num (<- :left exp))
                                          (num (<- :right exp)))))
            ("<="
             ;; (increase-changed)
             (list :type "bool" :value (<= (num (<- :left exp))
                                          (num (<- :right exp)))))
            (">="
             ;; (increase-changed)
             (list :type "bool" :value (>= (num (<- :left exp))
                                          (num (<- :right exp)))))
            (otherwise exp))
          exp))))

;; there's room for improvement below here
(defun opt-prog (exp)
  (labels ((loop% (body)
              (declare (list body))
              (cond ((zerop (length body))
                     +FALSE+)
                    ((= 1 (length body))
                     (opt (car body)))
                    ((not (has-side-effects (car body)))
                     (loop% (cdr body)))
                    (t (list :type "prog"
                             :prog (loop :for p :in body
                                      :collecting (opt p)))))))
    (loop% (<- :prog exp))))

(defun opt-if (exp)
  (let ((optimized-cond (opt (<- :cond exp)))
        (optimized-then (opt (<- :then exp)))
        (optimized-else (opt (<- :else exp))))
    (cond ((equal optimized-cond +TRUE+)
           optimized-then)
          ((or (equal optimized-cond +FALSE+)
               ;; hmm...
               (equal optimized-cond
                      (list :type "bool" :value nil)))
           (or optimized-else +FALSE+))
          ((and (is-constant optimized-cond)
                (not (equal optimized-cond +FALSE+)))
           optimized-then)
          (t (let ((new-node (list :type "if"
                                   :cond optimized-cond
                                   :then optimized-then)))
               (if optimized-else
                   (append new-node (list :else optimized-else))
                   new-node))))))
