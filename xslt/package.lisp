;; packages.lisp

(defpackage :libxml2.xslt
  (:use :cl :cffi :libxml2.private :libxml2.tree :iter)
  (:nicknames :xslt)
  (:export
   :stylesheet
   :parse-stylesheet
   :with-stylesheet
   :defxsl
   :stylesheet-set-param
   :stylesheet-remove-param
   :stylesheet-clear-params
   :transform
   :with-transform-result
   :register-exslt-extensions
   :define-xslt-element
   :*lisp-xslt-elements*
   :with-xslt-elements))
