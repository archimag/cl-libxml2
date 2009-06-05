;;; packages.lisp

(defpackage :libxml2.xfactory
  (:nicknames :xfactory)
  (:use :cl :iter)
  (:export #:xfactory
           #:element-maker))