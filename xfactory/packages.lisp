;;; packages.lisp

(defpackage :libxml2.xfactory
  (:nicknames :xfactory)
  (:use :cl :xtree)
  (:export #:build-documnet
           #:build-element
           #:build-document-fragment))