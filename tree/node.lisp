;;; node.lisp

(in-package #:libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defwrapper node %xmlNode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; release/impl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlFreeNode" %xmlFreeNode) :void
  (node %xmlNodePtr))

(defmethod release/impl ((node node))
  (%xmlFreeNode (pointer node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlCopyNode" %xmlCopyNode) %xmlNodePtr
  (node %xmlNodePtr)
  (extended :int))

(defmethod copy ((node node))
  (make-instance 'node
                 :pointer (%xmlCopyNode (pointer node) 1)))



(defun wrapper-slot-node (node slot)
  (wrapper-slot-wrapper node slot 'node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlNewNode" %xmlNewNode) %xmlNodePtr
  (ns %xmlNsPtr)
  (name %xmlCharPtr))

(defun make-element (name &optional href prefix)
  (let ((%node (with-foreign-string (%name name)
                 (%xmlNewNode (null-pointer) 
                              %name))))
    (if href
        (setf (foreign-slot-value %node
                                  '%xmlNode
                                  '%ns)
              (gp:with-garbage-pool ()
                (%xmlNewNs %node
                           (gp:cleanup-register (foreign-string-alloc href) #'foreign-string-free)
                           (if prefix
                               (gp:cleanup-register (foreign-string-alloc prefix)  #'foreign-string-free)
                               (null-pointer))))))
    (make-instance 'node
                   :pointer %node)))
                
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlNewText" %xmlNewText) %xmlNodePtr
  (content %xmlCharPtr))

(defun make-text (data)
  (make-instance 'node
                 :pointer (with-foreign-string (%data data)
                            (%xmlNewText %data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlNewComment" %xmlNewComment) %xmlNodePtr
  (content %xmlCharPtr))

(defun make-comment (data)
  (make-instance 'node
                 :pointer (with-foreign-string (%data data)
                            (%xmlNewComment %data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-process-instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlNewPI" %xmlNewPI) %xmlNodePtr
  (name %xmlCharPtr)
  (content %xmlCharPtr))

(defun make-process-instruction (name content)
  (make-instance 'node
                 :pointer (with-foreign-strings ((%name name) (%content content))
                            (%xmlNewPI %name %content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-node-p (name node-type)
  `(defun ,name (node &key throw-error)
     (if (eql (node-type node) ,node-type)
         t
         (if throw-error
             (error (format nil "node is not ~A" ,node-type))))))

(def-node-p element-p :xml-element-node)
(def-node-p attribute-p :xml-attribute-node)
(def-node-p text-p :xml-element-text)
(def-node-p comment-p :xml-comment-node)
(def-node-p process-instruction-p :xml-pi-node)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

