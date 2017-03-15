;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:minilang)

;; helper
(defun <- (key plist)
  (assoc-value (plist-alist plist) key :test 'equal))
