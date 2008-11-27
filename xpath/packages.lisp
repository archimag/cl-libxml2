;; packages.lisp

(defpackage :libxml2.xpath
  (:use :cl :cffi :iter :libxml2.private :libxml2.tree #+sbcl :sb-ext :metabang.bind)
  (:nicknames :xpath)
  (:export
   :compiled-expression
   :compile-expression
   :with-compiled-expression
   :node-set
   :node-set-length
   :node-set-at
   
   :xpath-object
   :xpath-object-type
   :xpath-object-value
   :with-xpath-object

   :eval-expression

   :find-string
   :find-number
   :find-boolean
   :find-single-node

   :*default-ns-map*
   :getpath

   :xpath-parser-context
   :with-xpath-functions
   :define-xpath-function
   ))