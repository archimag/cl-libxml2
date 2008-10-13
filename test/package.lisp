;; package.lisp

(defpackage :libxml2-test
  (:use :cl :iter :libxml2 :lift)
  (:export
   #:run-libxml2-test))
