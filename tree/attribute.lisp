;;; attribute.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; attribute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defcenum %xmlAttributeType
;;   (:xml-attriubte-unknown 0)
;;   (:xml-attribute-cdata  1)
;;   (:xml-attribute-id  2)
;;   (:xml-attribute-idref  3)
;;   (:xml-attribute-idrefs  4)
;;   (:xml-attribute-entity  5)
;;   (:xml-attribute-entities  6)
;;   (:xml-attribute-nmtoken  7)
;;   (:xml-attribute-nmtokens  8)
;;   (:xml-attribute-enumeration  9)
;;   (:xml-attribute-notation  10))


;; ;; struct _xmlAttr
;; (defcstruct %xmlAttr
;;   ;; void *	_private	: application data
;;   (%_private :pointer)
;;   ;; xmlElementType	type	: XML_ATTRIBUTE_NODE, must be second !
;;   (%type %xmlElementType)
;;   ;; const xmlChar *	name	: the name of the property
;;   (%name %xmlCharPtr)
;;   ;; struct _xmlNode *	children	: the value of the property
;;   (%children %xmlNodePtr)
;;   ;; struct _xmlNode *	last	: NULL
;;   (%last %xmlNodePtr)
;;   ;; struct _xmlNode *	parent	: child->parent link
;;   (%parent %xmlNodePtr)
;;   ;; struct _xmlAttr *	next	: next sibling link
;;   (%next %xmlAttrPtr)
;;   ;; struct _xmlAttr *	prev	: previous sibling link
;;   (%prev %xmlAttrPtr)
;;   ;; struct _xmlDoc *	doc	: the containing document
;;   (%doc %xmlDocPtr)
;;   ;; xmlNs *	ns	: pointer to the associated namespace
;;   (%ns %xmlNsPtr)
;;   ;; xmlAttributeType	atype	: the attribute type if validating
;;   (%atype %xmlAttributeType)
;;   ;; void *	psvi	: for type/PSVI informations
;;   (%psvi :pointer))


;; (defwrapper attribute %xmlAttr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; attribute-value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlGetNsProp" %xmlGetNsProp) %xmlCharPtr
  (node %xmlNodePtr)
  (name %xmlCharPtr)
  (uri %xmlCharPtr))

(defun attribute-value (element name &optional uri)
  (identity (foreign-string-to-lisp 
             (with-foreign-string (%name name)
               (if uri
                   (with-foreign-string (%uri uri)
                     (%xmlGetNsProp (pointer element) %name %uri))
                   (%xmlGetNsProp (pointer element) %name (null-pointer)))))))

(define-libxml2-function ("xmlSetNsProp" %xmlSetNsProp) %xmlAttrPtr
  (node %xmlNodePtr)
  (ns %xmlNsPtr)
  (name %xmlCharPtr)
  (value %xmlCharPtr))


(defun (setf attribute-value) (value element name &optional uri)
  (with-foreign-strings ((%name name) (%value value))
    (if uri
        (let ((ns (or (search-ns-by-href element uri)
                      (make-ns element uri))))
          (%xmlSetNsProp (pointer element) (pointer ns) %name %value))
      (%xmlSetNsProp (pointer element) (null-pointer) %name %value)))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remove-attribute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlHasNsProp" %xmlHasNsProp) %xmlAttrPtr
  (node %xmlNodePtr)
  (name %xmlCharPtr)
  (href %xmlCharPtr))

(define-libxml2-function ("xmlRemoveProp" %xmlRemoveProp) :int
  (attr %xmlAttrPtr))

(defun remove-attribute (element name &optional uri)
  (let ((%attr (with-foreign-string (%name name)
                  (if uri
                      (with-foreign-string (%uri uri)
                        (%xmlHasNsProp (pointer element) %name %uri))
                      (%xmlHasNsProp (pointer element) %name (null-pointer))))))
    (unless (null-pointer-p %attr)
      (= 0 (%xmlRemoveProp %attr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-attributes ((&rest entries) element &body body)
  (alexandria:once-only (element)
    `(symbol-macrolet
	 ,(mapcar (lambda (entry)
		    (destructuring-bind (var name &optional uri)
			entry
		      `(,var (attribute-value ,element ,name ,uri))))
		  entries)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iter (FOR (value name href)  IN-NODE-ATTRIBUTES node )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro-driver (for attr in-attributes node)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd %attr first (foreign-slot-value (pointer ,node) '%xmlNode '%properties) then (foreign-slot-value %attr '%xmlAttr '%next))
       (while (not (null-pointer-p %attr)))
       (for ,attr = (list (foreign-string-to-lisp 
                           (foreign-slot-value (foreign-slot-value %attr 
                                                                   '%xmlAttr 
                                                                   '%children) 
                                               '%xmlNode 
                                               '%content))
                          (foreign-string-to-lisp (foreign-slot-value %attr '%xmlAttr '%name))
                          (let ((%ns (foreign-slot-value %attr '%xmlAttr '%ns)))
                            (unless (null-pointer-p %ns)
                              (foreign-string-to-lisp (foreign-slot-value %ns
                                                                          '%xmlNs
                                                                          '%href)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add-extra-namespace (element prefix uri)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-extra-namespace (element href prefix)
  (with-foreign-strings ((%href href) (%prefix prefix))
                         (%xmlNewNs (pointer element)
                                    %href
                                    %prefix)))

  