;;; packages.lisp

(defpackage :libxml2.xfactory
  (:nicknames :xfactory)
  (:use :cl :iter)
  (:export #:with-element-factory
           #:with-document-factory
           #:namespace
           #:attribute
           #:text))