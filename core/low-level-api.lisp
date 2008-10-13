;; core.lisp

(in-package :libxml2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define and load libxml2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library libxml2
  (:unix (:or "libxml2.so"))
  (t (:default "libxml2")))

(with-simple-restart (skip "Skip loading foreign library libxml2.")
  (use-foreign-library libxml2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcenum %xmlElementType
  (:xml-element-node 1)
  (:xml-attribute-node 2)
  (:xml-text-node 3)
  (:xml-cdata-section-node  4)
  (:xml-entity-refODE  5)
  (:xml-entity-node 6)
  (:xml-pi-node 7)
  (:xml-comment-node 8)
  (:xml-document-node 9)
  (:xml-document-type-node  10)
  (:xml-document-frag-node  11)
  (:xml-notation-node 12)
  (:xml-html-document-node  13)
  (:xml-dtd-node 14)
  (:xml-element-decl 15)
  (:xml-attribute-decl 16)
  (:xml-entity-decl 17)
  (:xml-namespace-decl 18)
  (:xml-xinclude-start 19)
  (:xml-xinclude-end 20)
  (:xml-docb-document-node  21))

(defctype %xmlDocPtr :pointer)

(defctype %xmlNodePtr :pointer)

(defcstruct %xmlNode
  "libxml2 xmlNode struct"
  (%_private :pointer)
  (%type %xmlElementType)
  (%name :pointer)
  (%children %xmlNodePtr)
  (%last %xmlNodePtr)
  (%parent %xmlNodePtr)
  (%next %xmlNodePtr)
  (%prev %xmlNodePtr)
  (%doc %xmlDocPtr)
  (%ns :pointer)
  (%content :pointer)
  (%properties :pointer)
  (%nsDef :pointer)
  (%psvi :pointer)
  (%line :unsigned-short)
  (%extra :unsigned-short))

(defcfun ("xmlReadFile" %xmlReadFile)  %xmlDocPtr
  (filename :pointer)
  (encoding :pointer)
  (options :int))

(defcfun ("xmlFreeDoc" %xmlFreeDoc) :void
  (doc %xmlDocPtr))

(defcfun ("xmlDocGetRootElement" %xmlDocGetRootElement) %xmlNodePtr
  (doc %xmlDocPtr))


(defcfun ("xmlXIncludeProcessTree" %xmlXIncludeProcessTree) :int
  (node %xmlNodePtr))

  
(defcfun ("xmlGetProp" %xmlGetProp) :pointer
  (node %xmlNodePtr)
  (name :pointer))

