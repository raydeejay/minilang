;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

(defun cps (exp &optional (k 'identity))
  (switch ((<- :type exp) :test equal)
    ("num" (cps-atom exp k))
    ("str" (cps-atom exp k))
    ("bool" (cps-atom exp k))
    ("var" (cps-atom exp k))
    ("assign" (cps-binary exp k))
    ("binary" (cps-binary exp k))
    ("let" (cps-let exp k))
    ("lambda" (cps-lambda exp k))
    ("if" (cps-if exp k))
    ("prog" (cps-prog exp k))
    ("call" (cps-call exp k))
    (otherwise (error "Don't know how to CPS ~A." exp))))

(defun cps-atom (exp k)
  (funcall k exp))

(defun cps-binary (exp k)
  (cps (<- :left exp)
       (lambda (left)
         (cps (<- :right exp)
              (lambda (right)
                (funcall k (list :type (<- :type exp)
                                 :operator (<- :operator exp)
                                 :left left
                                 :right right)))))))

(defun cps-let (exp k)
  (if (zerop (length (<- :vars exp)))
      (cps (<- :body exp) k)
      (cps (list :type "call"
                 :args (list (or (<- :def (car (<- :vars exp))) +FALSE+))
                 :func (list :type "lambda"
                             :vars (<- :name (car (<- :vars exp)))
                             :body (list :type "let"
                                         :vars (cdr (<- :vars exp))
                                         :body (<- :body exp))))
           k)))

(defun cps-lambda (exp k)
  (let* ((cont (gensym% "κ"))
         (body (cps (<- :body exp)
                    (lambda (body)
                      (list :type "call"
                            :func (list :type "var" :value cont)
                            :args (list body))))))
    (funcall k (list :type "lambda"
                     :name (<- :name exp)
                     ;; :vars (cons cont (ensure-list (<- :vars exp)))
                     :vars (cons cont (<- :vars exp))
                     :body body))))

(defun cps-if (exp k)
  (cps (<- :cond exp)
       (lambda (cond)
         (list :type "if"
               :cond cond
               :then (cps (<- :then exp) k)
               :else (cps (or (<- :else exp) +FALSE+) k)))))

(defun cps-call (exp k)
  (cps (<- :func exp)
       (lambda (func)
         (labels ((loop% (args i)
                     (if (= i (length (<- :args exp)))
                         (list :type "call"
                               :func func
                               :args args)
                         (cps (nth i (<- :args exp))
                              (lambda (value)
                                (loop% (append args (list value)) (1+ i)))))))
           (loop% (list (make-continuation k)) 0)))))

(defun make-continuation (k)
  (let ((cont (gensym% "R")))
    (list :type "lambda"
          :vars (list cont)
          :body (funcall k (list :type "var"
                                 :value cont)))))

(defun cps-prog (exp k)
  (labels ((loop% (body)
              (cond ((zerop (length body))
                     (funcall k +FALSE+))
                    ((= 1 (length body))
                     (cps (car body) k)) ; it appears this must be a list sometimes
                    (t (cps (car body)
                            (lambda (first)
                              (list :type "prog"
                                    :prog (cons first (list (loop% (cdr body)))))))))))
    (loop% (<- :prog exp))))

(defun cps-run (ast)
  (eval (make-lisp ast)))
