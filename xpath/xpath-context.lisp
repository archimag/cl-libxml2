;;; xpath-context.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.xpath)


(defctype %xmlXPathAxisPtr :pointer)
(defctype %xmlXPathTypePtr :pointer)
(defctype %xmlXPathVariableLookupFunc :pointer)
(defctype %xmlStructuredErrorFunc :pointer)
(defctype %xmlXPathVariableLookupFunc :pointer)
(defctype %xmlXPathFuncLookupFunc :pointer)
(defctype %xmlStructuredErrorFunc :pointer)

(defcstruct %xmlXPathContext 
  ;; xmlDocPtr	doc	: The current document
  (%doc %xmlDocPtr)
  ;; xmlNodePtr	node	: The current node
  (%node	%xmlNodePtr)
  ;; int	nb_variables_unused	: unused (hash table)
  (%nb_variables_unused :int)
  ;; int	max_variables_unused	: unused (hash table)
  (%max_variables_unused :int)
  ;; xmlHashTablePtr	varHash	: Hash table of defined variables
  (%varHash	%xmlHashTablePtr)
  ;; int	nb_types	: number of defined types
  (%nb_types :int)
  ;; int	max_types	: max number of types
  (%max_types :int)
  ;; xmlXPathTypePtr	types	: Array of defined types
  (%types	%xmlXPathTypePtr)
  ;; int	nb_funcs_unused	: unused (hash table)
  (%nb_funcs_unused :int)
  ;; int	max_funcs_unused	: unused (hash table)
  (%max_funcs_unused :int)
  ;; xmlHashTablePtr	funcHash	: Hash table of defined funcs
  (%funcHash	%xmlHashTablePtr)
  ;; int	nb_axis	: number of defined axis
  (%nb_axis :int)
  ;; int	max_axis	: max number of axis
  (%max_axis :int)
  ;; xmlXPathAxisPtr	axis	: Array of defined axis the namespace nod
  (%axis	%xmlXPathAxisPtr)
  ;; xmlNsPtr *	namespaces	: Array of namespaces
  (%namespaces %xmlNsPtr)
  ;; int	nsNr	: number of namespace in scope
  (%nsNr :int)
  ;; void *	user	: function to free extra variables
  (%user :pointer)
  ;; int	contextSize	: the context size
  (%contextSize :int)
  ;; int	proximityPosition	: the proximity position extra stuff for
  (%proximityPosition :int)
  ;; int	xptr	: is this an XPointer context?
  (%xptr :int)
  ;; xmlNodePtr	here	: for here()
  (%here	%xmlNodePtr)
  ;; xmlNodePtr	origin	: for origin() the set of namespace decla
  (%origin	%xmlNodePtr)
  ;; xmlHashTablePtr	nsHash	: The namespaces hash table
  (%nsHash	%xmlHashTablePtr)
  ;; xmlXPathVariableLookupFunc	varLookupFunc	: variable lookup func
  (%varLookupFunc	%xmlXPathVariableLookupFunc)
  ;; void *	varLookupData	: variable lookup data Possibility to lin
  (%varLookupData :pointer)
  ;; void *	extra	: needed for XSLT The function name and U
  (%extra :pointer)
  ;; const xmlChar *	function
  (%function %xmlCharPtr)
  ;; const xmlChar *	functionURI	: function lookup function and data
  (%functionURI %xmlCharPtr)
  ;; xmlXPathFuncLookupFunc	funcLookupFunc	: function lookup func
  (%funcLookupFunc	%xmlXPathFuncLookupFunc)
  ;; void *	funcLookupData	: function lookup data temporary namespac
  (%funcLookupData :pointer)
  ;; xmlNsPtr *	tmpNsList	: Array of namespaces
  (%tmpNsList %xmlNsPtr)
  ;; int	tmpNsNr	: number of namespaces in scope error rep
  (%tmpNsNr :int)
  ;; void *	userData	: user specific data block
  (%userData :pointer)
  ;; xmlStructuredErrorFunc	error	: the callback in case of errors
  (%error	%xmlStructuredErrorFunc)
  ;; xmlError	lastError	: the last error
  (%lastError	xtree::%xmlError)
  ;; xmlNodePtr	debugNode	: the source node XSLT dictionary
  (%debugNode	%xmlNodePtr)
  ;; xmlDictPtr	dict	: dictionary if any
  (%dict	%xmlDictPtr)
  ;; int	flags	: flags to control compilation Cache for
  (%flags :int)
  ;; void *	cache
  (%cache :pointer))

(defctype %xmlXPathContextPtr :pointer)

(define-libxml2-function ("xmlXPathNewContext" %xmlXPathNewContext) %xmlXPathContextPtr
  (doc %xmlDocPtr))

(define-libxml2-function ("xmlXPathFreeContext" %xmlXPathFreeContext) :void
  (ctxt %xmlXPathContextPtr))

(define-libxml2-function ("xmlXPathRegisterNs" %xmlXPathRegisterNs) :int
  (ctxt %xmlXPathContextPtr ctxt)
  (prefix %xmlCharPtr)
  (ns_uri %xmlCharPtr))

(define-libxml2-function ("xmlXPathRegisterFuncNS" %xmlXPathRegisterFuncNS) :int
  (ctxt %xmlXPathContextPtr)
  (name %xmlCharPtr)
  (ns-uri %xmlCharPtr)
  (xpath-function :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-%context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *lisp-xpath-functions*)

(defvar *private-xpath-context*)

(defmacro with-%context ((var doc node ns-map) &body body)
  `(gp:with-garbage-pool (xpath-context-pool)
     (let ((,var (if (boundp '*private-xpath-context*) *private-xpath-context*
                     (gp:cleanup-register (%xmlXPathNewContext (pointer ,doc))
                                          #'%xmlXPathFreeContext
                                          xpath-context-pool))))
       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
       (if (boundp 'libxml2.xpath::*lisp-xpath-functions*)
           (gp:with-garbage-pool ()
             (iter (for (func name ns) in *lisp-xpath-functions*)
                   (%xmlXPathRegisterFuncNS ,var
                                            (gp:cleanup-register (foreign-string-alloc (eval name))
                                                                 #'foreign-string-free)
                                            (if ns
                                                (gp:cleanup-register (foreign-string-alloc (eval ns))
                                                                     #'foreign-string-free)
                                                (null-pointer))
                                            (get-callback func)))))
       (if ,node
           (setf (foreign-slot-value %ctxt
                                     '%xmlXPathContext
                                     '%node)
                 (pointer ,node)))
       (if ,ns-map
           (iter (for (prefix uri) in ,ns-map)
                 (with-foreign-strings ((%prefix prefix) (%uri uri))
                   (%xmlXPathRegisterNs ,var
                                        %prefix
                                        %uri))))
       ,@body)))

  
