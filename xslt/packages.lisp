;; packages.lisp

(defpackage :libxml2.xslt
  (:use :cl :cffi :libxml2.private :libxml2.tree :iter)
  (:export
   :stylesheet
   :parse-stylesheet
   :with-stylesheet
   :stylesheet-set-param
   :stylesheet-remove-param
   :stylesheet-clear-params
   :transform
   :with-transfom-result
   :register-exslt-extensions
   :define-xslt-element
   :with-xslt-elements))
