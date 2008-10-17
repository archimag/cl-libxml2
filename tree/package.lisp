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
   :make-text
   :make-comment
   :make-process-instruction
   :copy
   
   :parse
   :serialize
   :release

   ;;:document-root-element
   :with-document
   :with-parse-document
   :process-xinclude


   :insert-child-before
   :insert-child-after
   :append-child

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