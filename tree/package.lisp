;; package.lisp

(defpackage :libxml2.private
  (:use :cl)
  (:export
   :pointer))

(defpackage :libxml2.tree
  (:use :cl :iter :cffi :libxml2.private)
  (:export
   :node
   :document
   :ns
   :attribute

   ;;:pointer

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

(defpackage :libxml2.xslt
  (:use :cl :cffi :libxml2.private :libxml2.tree)
  ;;(:import  :libxml2.tree::pointer)
  (:export
   :stylesheet
   :parse-stylesheet
   :with-stylesheet
   :transform
   :with-transfom-result))