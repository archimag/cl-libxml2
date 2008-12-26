;;; document.lisp

(in-package #:libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; document
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

(defwrapper document %xmlDoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; release/impl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlFreeDoc" %xmlFreeDoc) :void
  (doc %xmlDocPtr))

(defmethod release/impl ((doc document))
  (%xmlFreeDoc (pointer doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlDocGetRootElement" %xmlDocGetRootElement) %xmlNodePtr
  (doc %xmlDocPtr))

(define-libxml2-function ("xmlDocSetRootElement" %xmlDocSetRootElement) %xmlNodePtr
  (doc %xmlDocPtr)
  (root %xmlNodePtr))

;; (defgeneric root (obj))

;; (defmethod root ((doc document))
;;   (make-instance 'node :pointer (%xmlDocGetRootElement (pointer doc))))

;; (defmethod root ((node node))
;;   (let ((doc (document node)))
;;     (if doc
;;         (root doc)
;;         (let ((parent (parent node)))
;;           (if parent
;;               (root parent)
;;               node)))))

(defun root (doc)
  (make-instance 'node :pointer (%xmlDocGetRootElement (pointer doc))))

(defun (setf root) (node doc)
  (%xmlDocSetRootElement (pointer doc)
                         (pointer node)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlCopyDoc" %xmlCopyDoc) %xmlDocPtr
  (doc %xmlDocPtr)
  (recursive :int))

(defmethod copy ((doc document))
  (make-instance 'document
                 :pointer (%xmlCopyDoc (pointer doc) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlNewDoc" %xmlNewDoc) %xmlDocPtr
  (version %xmlCharPtr))

(defun make-document (document-element)
  (let ((%doc (%xmlNewDoc (null-pointer))))
    (%xmlDocSetRootElement %doc (pointer document-element))
    (make-instance 'document
                   :pointer %doc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base-url
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod base-url ((doc document))
  (let ((%str (wrapper-slot-value doc '%url)))
    (unless (null-pointer-p %str)
      (puri:parse-uri (foreign-string-to-lisp %str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process-xinclude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlXIncludeProcess" %xmlXIncludeProcess) :int
  (doc %xmlDocPtr))

(defmethod process-xinclude ((doc document))
  (%xmlXIncludeProcess (pointer doc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun document (node)
  (wrapper-slot-wrapper node '%doc 'document))
