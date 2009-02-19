;; package.lisp

(defpackage :libxml2.private
  (:use :cl)
  (:export
   :pointer
   :%xmlDocPtr
   :%xmlNodeSetPtr
   :%xmlDictPtr
   :%xmlHashTablePtr
   :%charPtr
   :%xmlCharPtr
   :%xmlNodePtr
   :%xmlNsPtr
   :defwrapper
   :wrapper-slot-value
   :define-libxml2-function
   ))

(defpackage :libxml2.tree
  (:use :cl :iter :cffi :libxml2.private :metabang.bind)
  (:nicknames :xtree)
  (:export
   :version
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

   :element-p
   :attribute-p
   :text-p
   :comment-p
   :process-instruction-p
   
   :parse
   :with-custom-resolvers
   :resolve-string
   :resolve-file/url
   :resolve-stream
   :serialize
   :release

   :document-properties
   :append-document-property

   :base-url
   
   :with-libxml2-object
   :with-parse-document
   :with-fake-document
   :defxml
   :process-xinclude


   :insert-child-before
   :insert-child-after
   :append-child
   :prepend-child

   :remove-child
   :replace-child
   :detach

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
   :with-attributes


   ;; errors
   :xmlerror
   :error-message
   :error-domain
   :error-level
   :last-error
   :reset-error
   :libxml2-error
   :init-error-handling
   :reset
   :with-simple-reset
   ))
