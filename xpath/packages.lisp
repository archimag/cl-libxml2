;; packages.lisp

(defpackage :libxml2.xpath
  (:use :cl :cffi :iter :libxml2.private :libxml2.tree)
  (:export
   :compiled-expression
   :compile-expression
   :node-set
   :node-set-length
   :node-set-at
   :xpath-result
   :xpath-result-type
   :xpath-result-value
   :eval-xpath-expression
   :with-xpath-result
   ))