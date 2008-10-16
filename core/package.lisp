;; package.lisp

(defpackage :libxml2.tree
  (:use :cl :iter :cffi)
  (:export
   :node
   :document
   :ns
   :attribute

   :make-document
   :make-element
   
   :parse
   :serialize
   :release

   ;;:document-root-element
   :with-document
   :with-parse-document
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