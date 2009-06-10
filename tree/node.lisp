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

(define-libxml2-function ("xmlFreeNode" %xmlFreeNode) :void
  (node %xmlNodePtr))

(defmethod release/impl ((node node))
  (%xmlFreeNode (pointer node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlCopyNode" %xmlCopyNode) %xmlNodePtr
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

(define-libxml2-function ("xmlNewNode" %xmlNewNode) %xmlNodePtr
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

(defun make-child-element (parent name &optional href prefix)
  (let* ((ns (if prefix
                 (search-ns-by-prefix parent prefix)
                 (search-ns-by-href parent href)))
         (node (if ns
                   (with-foreign-string (%name name)
                     (make-instance 'node
                                    :pointer (%xmlNewNode (pointer ns)
                                                          %name)))
                   (make-element name href prefix))))
    (append-child parent node)))
                     

               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlNewText" %xmlNewText) %xmlNodePtr
  (content %xmlCharPtr))

(defun make-text (data)
  (make-instance 'node
                 :pointer (with-foreign-string (%data data)
                            (%xmlNewText %data))))

(define-libxml2-function ("xmlNodeAddContent" %xmlNodeAddContent) :void
  (parent %xmlNodePtr)
  (content %xmlCharPtr))

(defun make-child-text (node content)
  (with-foreign-string (%content content)
    (%xmlNodeAddContent (pointer node)
                        %content)))
                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlNewComment" %xmlNewComment) %xmlNodePtr
  (content %xmlCharPtr))

(defun make-comment (data)
  (make-instance 'node
                 :pointer (with-foreign-string (%data data)
                            (%xmlNewComment %data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-process-instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlNewPI" %xmlNewPI) %xmlNodePtr
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
             (error (format nil "node (~A) is not ~A" (node-type node) ,node-type))))))

(def-node-p element-p :xml-element-node)
(def-node-p document-node-p :xml-document-node)
(def-node-p attribute-p :xml-attribute-node)
(def-node-p text-p :xml-element-text)
(def-node-p comment-p :xml-comment-node)
(def-node-p process-instruction-p :xml-pi-node)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local-name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun local-name (node)
  (cffi:foreign-string-to-lisp
   (wrapper-slot-value node '%name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node-type (node)
  (wrapper-slot-value node '%type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; next-sibling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-sibling (node)
  (wrapper-slot-node node '%next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prev-sibling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prev-sibling (node)
  (wrapper-slot-node node '%prev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first-child
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first-child (node)
  (wrapper-slot-node node '%children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; last-child
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun last-child (node)
  (wrapper-slot-node node '%last))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parent (node)
  (wrapper-slot-node node '%parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlNodeGetContent" %xmlNodeGetContent) %xmlCharPtr
  (cur %xmlNodePtr))

(defun text-content (node)
  (let ((%content (%xmlNodeGetContent (pointer node))))
    (unless (null-pointer-p %content)
      (unwind-protect
           (foreign-string-to-lisp %content)
        (%xmlFree %content)))))

(define-libxml2-function ("xmlNodeSetContent" %xmlNodeSetContent) :void
  (cur %xmlNodePtr)
  (content %xmlCharPtr))

(defun (setf text-content) (content node)
  (with-foreign-string (%content content)
    (%xmlNodeSetContent (pointer node)
                        %content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base-url
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlNodeGetBase" %xmlNodeGetBase) %xmlCharPtr
  (doc %xmlDocPtr)
  (cur %xmlNodePtr))

(defmethod base-url ((node node))
  (let ((%str (%xmlNodeGetBase (pointer (document node))
                               (pointer node))))
    (unwind-protect
         (puri:parse-uri (foreign-string-to-lisp %str))
      (%xmlFree %str))))


(define-libxml2-function ("xmlNodeSetBase" %xmlNodeSetBase) :void
  (node %xmlNodePtr)
  (uri %xmlCharPtr))

(defgeneric (setf base-url) (uri obj))

(defmethod (setf base-url) ((uri string) (node node))
  (with-foreign-string (%uri uri)
    (%xmlNodeSetBase (pointer node)
                     %uri))
  uri)
                   
(defmethod (setf base-url) ((uri puri:uri) (node node))
  (setf (base-url node)
        (format nil "~A" uri)))

(defmethod (setf base-url) ((empty (eql nil)) (node node))
  (xtree:remove-attribute node "base" "http://www.w3.org/XML/1998/namespace"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process-xinclude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlXIncludeProcessTree" %xmlXIncludeProcessTree) :int
  (node %xmlNodePtr))

(define-libxml2-function ("xmlXIncludeProcessTreeFlags" %xmlXIncludeProcessTreeFlags) :int
  (node %xmlNodePtr)
  (flags :int))

(defmethod process-xinclude ((node node) &optional options)
  (if options
      (%xmlXIncludeProcessTreeFlags (pointer node) options)
      (%xmlXIncludeProcessTree (pointer node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FOR var IN-... WITH ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node-filter (&key type local-name ns filter)
  (if (or type local-name)
      (lambda (node)
        (and (or (not type)
                 (eql (node-type node) type))
             (or (not local-name)
                 (string= (local-name node) local-name))
             (or (not ns)
                 (string= (namespace-uri node) ns))
             (or (not filter)
                 (funcall filter node))))))

(defun find-node (first filter)
  (if first
    (if filter
        (if (funcall filter first)
            first
            (find-node (next-sibling first) filter))
        first)))


;;; FOR var IN-CHILD-NODES node WITH ()

(defmacro-driver (for var in-child-nodes node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node (first-child ,node) filter-fun) then (find-node (next-sibling ,var) filter-fun))
     (while ,var))))


;;; FOR var IN-NEXT-SIBLINGS node WITH ()

(defmacro-driver (for var in-next-siblings node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node (next-sibling ,node) filter-fun) then (find-node (next-sibling ,var) filter-fun))
     (while ,var))))

;;; FOR var IN-NEXT-SIBLINGS-FROM node WITH ()

(defmacro-driver (for var in-next-siblings-from node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node ,node filter-fun) then (find-node (next-sibling ,var) filter-fun))
     (while ,var))))

;;; FOR var IN-PREV-SIBLING node WITH ()

(defmacro-driver (for var in-prev-siblings node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node (prev-sibling ,node) filter-fun) then (find-node (prev-sibling ,var) filter-fun))
     (while ,var))))

;;; FOR var IN-PREV-SIBLING-FROM node WITH ()

(defmacro-driver (for var in-prev-siblings-from node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node ,node filter-fun) then (find-node (prev-sibling ,var) filter-fun))
     (while ,var))))




(defun pointer-to-node (ptr)
  (unless (null-pointer-p ptr)
    (make-instance 'node
                   :pointer ptr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert-child-before
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlAddPrevSibling" %xmlAddPrevSibling) %xmlNodePtr
  (cur %xmlNodePtr)
  (elem %xmlNodePtr))

(defun insert-child-before (new-child ref-child)
  (pointer-to-node (%xmlAddPrevSibling (pointer ref-child)
                                       (pointer new-child))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert-child-after
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlAddNextSibling" %xmlAddNextSibling) %xmlNodePtr
  (cur %xmlNodePtr)
  (elem %xmlNodePtr))

(defun insert-child-after (new-child ref-child)
  (pointer-to-node (%xmlAddNextSibling (pointer ref-child)
                                       (pointer new-child))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; append-child
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlAddChild" %xmlAddChild) %xmlNodePtr
  (parent %xmlNodePtr)
  (child %xmlNodePtr))

(defun append-child (parent node)
  (pointer-to-node (%xmlAddChild (pointer parent)
                                 (pointer node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prepend-child
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepend-child (parent node)
  (let ((first (first-child parent)))
    (if first
        (insert-child-before node first)
        (append-child parent node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; detach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlUnlinkNode" %xmlUnlinkNode) :void
  (node %xmlNodePtr))

(defun detach (node)
  (%xmlUnlinkNode (pointer node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remove-child
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-child (child)
  (detach child)
  (release child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replace-child
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlReplaceNode" %xmlReplaceNode) %xmlNodePtr
  (old %xmlNodePtr)
  (cur %xmlNodePtr))

(defun replace-child (old-child new-child &key (delete t))
  (let ((%old (%xmlReplaceNode (pointer old-child)
                               (pointer new-child))))
    (if delete
        (%xmlFreeNode %old)
        (pointer-to-node %old))))
