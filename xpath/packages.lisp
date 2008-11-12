;; packages.lisp

(defpackage :libxml2.xpath
  (:use :cl :cffi :iter :libxml2.private :libxml2.tree #+sbcl :sb-ext)
  (:export
   :compiled-expression
   :compile-expression
   :with-compiled-expression
   :node-set
   :node-set-length
   :node-set-at
   :xpath-result
   :xpath-result-type
   :xpath-result-value
   :eval-expression
   :eval-expression-as-string
   :eval-expression-as-number
   :eval-expression-as-boolean
   :with-xpath-result
   :*default-ns-map*
   ))