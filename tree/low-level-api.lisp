;; core.lisp

(in-package :libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define and load libxml2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library libxml2
  (:unix (:or "libxml2.so"))
  (t (:default "libxml2")))

(with-simple-restart (skip "Skip loading foreign library libxml2.")
  (use-foreign-library libxml2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype %xmlCharPtr :pointer)
(defctype %xmlNsPtr :pointer)
(defctype %xmlDocPtr :pointer)
(defctype %xmlNodePtr :pointer)
(defctype %xmlAttrPtr :pointer)
(defctype %xmlNodeSetPtr :pointer)
(defctype %xmlDictPtr :pointer)
(defctype %xmlHashTablePtr :pointer)
(defctype %charPtr :pointer)



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

(defctype %xmlNsType %xmlElementType)

;; fuck! fuck! fuck!
;; (defcfun ("xmlFree" %xmlFree) :void
;;    (ptr :pointer))
(defun %xmlFree (ptr)
  (foreign-funcall "free" :pointer ptr :void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xmlDoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; struct _xmlDoc
(defcstruct %xmlDoc
  ;; void *	_private	: application data
  (%_private :pointer)
  ;; xmlElementType	type	: XML_DOCUMENT_NODE, must be second !
  (%type %xmlElementType)   
  ;; char *	name	: name/filename/URI of the document
  (%name %xmlCharPtr)
  ;; struct _xmlNode *	children	: the document tree
  (%children %xmlNodePtr)
  ;; struct _xmlNode *	last	: last child link
  (%last %xmlNodePtr)
  ;; struct _xmlNode *	parent	: child->parent link
  (%parent %xmlNodePtr)
  ;; struct _xmlNode *	next	: next sibling link
  (%next %xmlNodePtr)
  ;; struct _xmlNode *	prev	: previous sibling link
  (%prev %xmlNodePtr)
  ;; struct _xmlDoc *	doc	: autoreference to itself End of common p
  (%doc %xmlDocPtr)
  ;; int	compression	: level of zlib compression
  (%compressiong :int)
  ;; int	standalone	: standalone document (no external refs)
  (%stanalone :int)
  ;; struct _xmlDtd *	intSubset	: the document internal subset
  (%intSubset :pointer)
  ;; struct _xmlDtd *	extSubset	: the document external subset
  (extSubset :pointer)
  ;; struct _xmlNs *	oldNs	: Global namespace, the old way
  (%ns %xmlNsPtr)
  ;; const xmlChar *	version	: the XML version string
  (%version %xmlCharPtr)
  ;; const xmlChar *	encoding	: external initial encoding, if any
  (%encoding %xmlCharPtr)
  ;; void *	ids	: Hash table for ID attributes if any
  (%ids :pointer)
  ;; void *	refs	: Hash table for IDREFs attributes if any
  (%refs :pointer)
  ;; const xmlChar *	URL	: The URI for that document
  (%url %xmlCharPtr)
  ;; int	charset	: encoding of the in-memory content actua
  (%charset %xmlCharPtr)
  ;; struct _xmlDict *	dict	: dict used to allocate names or NULL
  (%dict %xmlDictPtr)
  ;; void *	psvi	: for type/PSVI informations
  (%psvi :pointer)
  ;; int	parseFlags	: set of xmlParserOption used to parse th
  (%parseFlags :int)
  ;; int	properties	: set of xmlDocProperties for this docume
  (%properties :int))


(defcfun ("xmlReadFile" %xmlReadFile)  %xmlDocPtr
  (filename :pointer)
  (encoding :pointer)
  (options :int))

(defcfun ("xmlReadDoc" %xmlReadDoc) %xmlDocPtr
  (cur %xmlCharPtr)
  (base-url %xmlCharPtr)
  (encoding %xmlCharPtr)
  (options :int))

;; (defcfun ("xmlReadMemory" %xmlReadMemory) %xmlDocPtr
;;   (buffer %xmlCharPtr)
;;   (size :int)
;;   (uri %xmlCharPtr)
;;   (encoding %xmlCharPtr)
;;   (options :int))

(defcfun ("xmlSaveFile" %xmlSaveFile) :int
  (filename :pointer)
  (doc %xmlDocPtr))

(defcfun ("xmlDocDumpFormatMemoryEnc" %xmlDocDumpFormatMemoryEnc) :void
  (doc %xmlDocPtr)
  (doc_txt_ptr :pointer)
  (doc_txt_len :pointer)
  (txt_encoding :pointer)
  (format :int))

(defcfun ("xmlDocDumpFormatMemory" %xmlDocDumpFormatMemory) :void
  (doc %xmlDocPtr)
  (doc_txt_ptr :pointer)
  (doc_txt_len :pointer)
  (format :int))


(defcfun ("xmlFreeDoc" %xmlFreeDoc) :void
  (doc %xmlDocPtr))

(defcfun ("xmlDocGetRootElement" %xmlDocGetRootElement) %xmlNodePtr
  (doc %xmlDocPtr))

(defcfun ("xmlDocSetRootElement" %xmlDocSetRootElement) %xmlNodePtr
  (doc %xmlDocPtr)
  (root %xmlNodePtr))

(defcfun ("xmlNewDoc" %xmlNewDoc) %xmlDocPtr
  (version %xmlCharPtr))

(defcfun ("xmlCopyDoc" %xmlCopyDoc) %xmlDocPtr
  (doc %xmlDocPtr)
  (recursive :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xmlNode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; struct _xmlNode
(defcstruct %xmlNode
  ;; void *	_private	: application data
  (%private :pointer)
  ;; xmlElementType	type	: type number, must be second !
  (%type %xmlElementType)
  ;; const xmlChar *	name	: the name of the node, or the entity
  (%name %xmlCharPtr)
  ;; struct _xmlNode *	children	: parent->childs link
  (%children %xmlNodePtr)
  ;; struct _xmlNode *	last	: last child link
  (%last %xmlNodePtr)
  ;; struct _xmlNode *	parent	: child->parent link
  (%parent %xmlNodePtr)
  ;; struct _xmlNode *	next	: next sibling link
  (%next %xmlNodePtr)
  ;; struct _xmlNode *	prev	: previous sibling link
  (%prev %xmlNodePtr)
  ;; struct _xmlDoc *	doc	: the containing document End of common p
  (%doc %xmlDocPtr)
  ;; xmlNs *	ns	: pointer to the associated namespace
  (%ns %xmlNsPtr)
  ;; xmlChar *	content	: the content
  (%content %xmlCharPtr)
  ;; struct _xmlAttr *	properties	: properties list
  (%properties %xmlAttrPtr)
  ;; xmlNs *	nsDef	: namespace definitions on this node
  (%nsDef %xmlNsPtr)
  ;; void *	psvi	: for type/PSVI informations
  (%psvi :pointer)
  ;; unsigned short	line	: line number
  (%line :unsigned-short)
  ;; unsigned short	extra	: extra data for XPath/XSLT
  (%extra :unsigned-short))

(defcfun ("xmlNewNode" %xmlNewNode) %xmlNodePtr
  (ns %xmlNsPtr)
  (name %xmlCharPtr))

(defcfun ("xmlNewText" %xmlNewText) %xmlNodePtr
  (content %xmlCharPtr))

(defcfun ("xmlNewComment" %xmlNewComment) %xmlNodePtr
  (content %xmlCharPtr))

(defcfun ("xmlNewPI" %xmlNewPI) %xmlNodePtr
  (name %xmlCharPtr)
  (content %xmlCharPtr))

(defcfun ("xmlCopyNode" %xmlCopyNode) %xmlNodePtr
  (node %xmlNodePtr)
  (extended :int))

(defcfun ("xmlXIncludeProcessTree" %xmlXIncludeProcessTree) :int
  (node %xmlNodePtr))

(defcfun ("xmlSetTreeDoc" %xmlSetTreeDoc) :void
  (tree %xmlNodePtr)
  (doc %xmlDocPtr))

(defcfun ("xmlFreeNode" %xmlFreeNode) :void
  (node %xmlNodePtr))

(defcfun ("xmlUnlinkNode" %xmlUnlinkNode) :void
  (node %xmlNodePtr))

(defcfun ("xmlAddChild" %xmlAddChild) %xmlNodePtr
  (parent %xmlNodePtr)
  (child %xmlNodePtr))

(defcfun ("xmlAddNextSibling" %xmlAddNextSibling) %xmlNodePtr
  (cur %xmlNodePtr)
  (elem %xmlNodePtr))

(defcfun ("xmlAddPrevSibling" %xmlAddPrevSibling) %xmlNodePtr
  (cur %xmlNodePtr)
  (elem %xmlNodePtr))


(defcfun ("xmlReplaceNode" %xmlReplaceNode) %xmlNodePtr
  (old %xmlNodePtr)
  (cur %xmlNodePtr))
  

(defcfun ("xmlNodeGetBase" %xmlNodeGetBase) %xmlCharPtr
  (doc %xmlDocPtr)
  (cur %xmlNodePtr))

(defcfun ("xmlDocumentGetBase" %xmlDocumentGetBase) %xmlCharPtr
  (doc %xmlDocPtr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xmlNs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; struct _xmlNs
(defcstruct %xmlNs
  ;; struct _xmlNs *	next	: next Ns link for this node
  (%next %xmlNsPtr)
  ;; xmlNsType	type	: global or local
  (%type %xmlNsType)
  ;; const xmlChar *	href	: URL for the namespace
  (%href %xmlCharPtr)
  ;; const xmlChar *	prefix	: prefix for the namespace  
  (%prefix %xmlCharPtr)
  ;; void *	_private	: application data
  (%_private :pointer)
  ;; struct _xmlDoc *	context	: normally an xmlDoc  
  (%context %xmlDocPtr))


(defcfun ("xmlSearchNs" %xmlSearchNs) %xmlNsPtr
  (doc %xmlDocPtr)
  (node %xmlNodePtr)
  (prefix %xmlCharPtr))

(defcfun ("xmlSearchNsByHref" %xmlSearchNsByHref) %xmlNsPtr
  (doc %xmlDocPtr)
  (node %xmlNodePtr)
  (href %xmlCharPtr))

(defcfun ("xmlNewNs" %xmlNewNs) %xmlNsPtr
  (node %xmlNodePtr)
  (href %xmlCharPtr)
  (prefix %xmlCharPtr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; atrribute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcenum %xmlAttributeType
  (:xml-attribute-cdata  1)
  (:xml-attribute-id  2)
  (:xml-attribute-idref  3)
  (:xml-attribute-idrefs  4)
  (:xml-attribute-entity  5)
  (:xml-attribute-entities  6)
  (:xml-attribute-nmtoken  7)
  (:xml-attribute-nmtokens  8)
  (:xml-attribute-enumeration  9)
  (:xml-attribute-notation  10))


;; struct _xmlAttr
(defcstruct %xmlAttr
  ;; void *	_private	: application data
  (%_private :pointer)
  ;; xmlElementType	type	: XML_ATTRIBUTE_NODE, must be second !
  (%type %xmlElementType)
  ;; const xmlChar *	name	: the name of the property
  (%name %xmlCharPtr)
  ;; struct _xmlNode *	children	: the value of the property
  (%children %xmlNodePtr)
  ;; struct _xmlNode *	last	: NULL
  (%last %xmlNodePtr)
  ;; struct _xmlNode *	parent	: child->parent link
  (%parent %xmlNodePtr)
  ;; struct _xmlAttr *	next	: next sibling link
  (%next %xmlAttrPtr)
  ;; struct _xmlAttr *	prev	: previous sibling link
  (%prev %xmlAttrPtr)
  ;; struct _xmlDoc *	doc	: the containing document
  (%doc %xmlDocPtr)
  ;; xmlNs *	ns	: pointer to the associated namespace
  (%ns %xmlNsPtr)
  ;; xmlAttributeType	atype	: the attribute type if validating
  (%atype %xmlAttributeType)
  ;; void *	psvi	: for type/PSVI informations
  (%psvi :pointer))


(defcfun ("xmlGetNsProp" %xmlGetNsProp) %xmlCharPtr
  (node %xmlNodePtr)
  (name %xmlCharPtr)
  (uri %xmlCharPtr))
    
(defcfun ("xmlGetProp" %xmlGetProp) :pointer
  (node %xmlNodePtr)
  (name :pointer))

(defcfun ("xmlSetNsProp" %xmlSetNsProp) %xmlAttrPtr
  (node %xmlNodePtr)
  (ns %xmlNsPtr)
  (name %xmlCharPtr)
  (value %xmlCharPtr))

(defcfun ("xmlSetProp" %xmlSetProp) :pointer
  (node %xmlNodePtr)
  (name :pointer)
  (value :pointer))

(defcfun ("xmlRemoveProp" %xmlRemoveProp) :int
  (attr %xmlAttrPtr))


(defcfun ("xmlHasProp" %xmlHasProp) %xmlAttrPtr
  (node %xmlNodePtr)
  (name %xmlCharPtr))

(defcfun ("xmlHasNsProp" %xmlHasNsProp) %xmlAttrPtr
  (node %xmlNodePtr)
  (name %xmlCharPtr)
  (href %xmlCharPtr))

  

