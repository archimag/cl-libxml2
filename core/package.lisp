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
   :node-tag
   :node-next
   :node-prev
   :node-first
   :node-last
   :node-parent
   :node-text-content

   :node-filter
   :find-node
   ))