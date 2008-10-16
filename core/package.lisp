;; package.lisp

(defpackage :cl-libxlm2.0.dev
  (:nicknames :libxml2)
  (:use :cl :iter :cffi)
  (:export
   :node
   :document
   :ns
   :attribute

   :make-element
   
   :parse
   :serialize
   :release

   ;;:document-root-element
   :with-document
   :process-xinclude

   :node-type
   :root
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