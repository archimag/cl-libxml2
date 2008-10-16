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

(defctype %xmlChar :pointer)

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


;; struct _xmlNs {
;;     struct _xmlNs *	next	: next Ns link for this node
;;     xmlNsType	type	: global or local
;;     const xmlChar *	href	: URL for the namespace
;;     const xmlChar *	prefix	: prefix for the namespace
;;     void *	_private	: application data
;;     struct _xmlDoc *	context	: normally an xmlDoc
;; }
(defctype %xmlNsType %xmlElementType)

(defctype %xmlNsPtr :pointer)

(defcstruct %xmlNs
  "libxml2 xmlNs struct"
  (%next %xmlNsPtr)
  (%type %xmlNsType)
  (%href :pointer)
  (%prefix :pointer)
  (%_private :pointer)
  (%context %xmlDocPtr))

(defctype %xmlAttrPtr :pointer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlSearchNs" %xmlSearchNs) %xmlNsPtr
  (doc %xmlDocPtr)
  (node %xmlNodePtr)
  (prefix %xmlChar))

(defcfun ("xmlSearchNsByHref" %xmlSearchNsByHref) %xmlNsPtr
  (doc %xmlDocPtr)
  (node %xmlNodePtr)
  (href %xmlChar))

(defcfun ("xmlNewNs" %xmlNewNs) %xmlNsPtr
  (node %xmlNodePtr)
  (href %xmlChar)
  (prefix %xmlChar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlReadFile" %xmlReadFile)  %xmlDocPtr
  (filename :pointer)
  (encoding :pointer)
  (options :int))

(defcfun ("xmlReadMemory" %xmlReadMemory) %xmlDocPtr
  (buffer %xmlChar)
  (size :int)
  (uri %xmlChar)
  (encoding %xmlChar)
  (options :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlSaveFile" %xmlSaveFile) :int
  (filename :pointer)
  (doc %xmlDocPtr))

(defcfun ("xmlFreeDoc" %xmlFreeDoc) :void
  (doc %xmlDocPtr))

(defcfun ("xmlDocGetRootElement" %xmlDocGetRootElement) %xmlNodePtr
  (doc %xmlDocPtr))


(defcfun ("xmlXIncludeProcessTree" %xmlXIncludeProcessTree) :int
  (node %xmlNodePtr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; atrribute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlGetNsProp" %xmlGetNsProp) %xmlChar
  (node %xmlNodePtr)
  (name %xmlChar)
  (uri %xmlChar))
    
(defcfun ("xmlGetProp" %xmlGetProp) :pointer
  (node %xmlNodePtr)
  (name :pointer))

(defcfun ("xmlSetNsProp" %xmlSetNsProp) %xmlAttrPtr
  (node %xmlNodePtr)
  (ns %xmlNsPtr)
  (name %xmlChar)
  (value %xmlChar))

(defcfun ("xmlSetProp" %xmlSetProp) :pointer
  (node %xmlNodePtr)
  (name :pointer)
  (value :pointer))

;; (defcfun ("xmlUnsetNsProp" %xmlUnsetNsProp) :int
;;   (node %xmlNodePtr)
;;   (ns %xmlNsPtr)
;;   (name %xmlChar))

;; (defcfun ("xmlUnsetProp" %xmlUnsetProp) :int
;;   (node %xmlNodePtr)
;;   (name %xmlChar))

(defcfun ("xmlRemoveProp" %xmlRemoveProp) :int
  (attr %xmlAttrPtr))

;; (defcfun ("xmlFreeProp" %xmlFreeProp) :void
;;   (attr %xmlAttrPtr))

(defcfun ("xmlHasProp" %xmlHasProp) %xmlAttrPtr
  (node %xmlNodePtr)
  (name %xmlChar))

(defcfun ("xmlHasNsProp" %xmlHasNsProp) %xmlAttrPtr
  (node %xmlNodePtr)
  (name %xmlChar)
  (href %xmlChar))