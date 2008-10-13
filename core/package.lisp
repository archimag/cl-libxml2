;; package.lisp

(defpackage :cl-libxlm2.0.dev
  (:nicknames :libxml2)
  (:use :cl :iter :cffi)
  (:export
   :parse

   :document
   :document-root-element
   :with-document
   :process-xinclude

   :node

   :node-type

   :parent
   :text-content
   :first-child
   :last-child
   :next-sibling
   :prev-sibling
   :local-name

   :node-filter
   :find-node

   :attribute-value
   ))