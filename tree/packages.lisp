;;; package.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:libxml2.private
  (:use #:cl)
  (:export #:pointer
           #:%xmlDocPtr
           #:%xmlNodeSetPtr
           #:%xmlDictPtr
           #:%xmlHashTablePtr
           #:%charPtr
           #:%xmlCharPtr
           #:%xmlNodePtr
           #:%xmlNsPtr
           #:defwrapper
           #:wrapper-slot-value
           #:define-libxml2-function))

(defpackage #:libxml2.tree
  (:use #:cl #:iter #:cffi #:libxml2.private #:metabang.bind)
  (:nicknames #:xtree)
  (:export #:node
           #:document
           #:ns

           #:make-document
           #:make-element
           #:make-child-element
           #:make-text
           #:make-child-text
           #:make-comment
           #:make-process-instruction
           #:make-ns
           #:copy

           #:element-p
           #:attribute-p
           #:text-p
           #:comment-p
           #:process-instruction-p
   
           #:parse
           #:parse-options
           #:with-custom-resolvers
           #:resolve-string
           #:resolve-file/url
           #:resolve-stream
           #:serialize
           #:release

           #:base-url
   
           #:with-libxml2-object
           #:with-object
           #:with-parse-document
           #:with-fake-document
           #:defxml
           #:process-xinclude

           #:insert-child-before
           #:insert-child-after
           #:append-child
           #:prepend-child

           #:remove-child
           #:replace-child
           #:detach

           #:node-type
           #:root
           #:parent
           #:text-content
           #:first-child
           #:last-child
           #:next-sibling
           #:prev-sibling
           #:local-name
           #:namespace-uri
           #:namespace-prefix

           #:node-filter
           #:find-node

           #:attribute-value
           #:remove-attribute
           #:with-attributes

           #:encode-entitites
           #:encode-special-chars

           ;; errors
           #:xmlerror
           #:error-message
           #:error-domain
           #:error-level
           #:last-error
           #:reset-error
           #:libxml2-error
           #:init-error-handling
           #:reset
           #:with-simple-reset))
