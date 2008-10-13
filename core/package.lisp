;; package.lisp

(defpackage :cl-libxlm2.0.dev
  (:nicknames :libxml2)
  (:use :cl :iter :cffi)
  (:export
   :parse


   :document-root-element
   :with-document
   :process-xinclude

   :node-type
   :document
   :parent
   :text-content
   :first-child
   :last-child
   :next-sibling
   :prev-sibling
   :local-name
   :namespace-uri
   :namespace-prefix

   :node-filter
   :find-node

   :attribute-value
   :remove-attribute
   ))