;;; extensions.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.xslt)

(defctype %xsltTemplatePtr :pointer)
(defctype %xsltStackElemPtr :pointer)
(defctype %xsltRuntimeExtraPtr :pointer)
(defctype %xsltDocumentPtr :pointer)
(defctype %xsltTransformCachePtr :pointer)

(defcenum %xsltOutputType
  (:xslt-output-xml 0)
  (:xslt-output-html 1)
  (:xslt-output-text 2))

(defcenum %xsltTransformState
  (:xslt-state-ok 0)
  (:xslt-state-error 1)
  (:xslt-state-stopped 2))

;;struct _xsltTransformContext {
(defcstruct %xsltTransformContext
  ;; the stylesheet used
  (%style %xsltStylesheetPtr)
  ;; the type of output
  (%type %xsltOutputType)
  ;; the current template
  (%templ %xsltTemplatePtr)
  ;; Nb of templates in the stack
  (%templNr :int)
  ;; Size of the templtes stack
  (%templMax :int)
  ;; the template stack
  (%templTab %xsltTemplatePtr)
  ;; the current variable list
  (%vars %xsltStackElemPtr)
  ;; Nb of variable list in the stack
  (%varsNr :int)
  ;; Size of the variable list stack
  (%varsMax :int)
  ;; the variable list stack
  (%varsTab %xsltStackElemPtr )
  ;; * Extensions *
  (%varsBase :int)
  ;; the extension functions
  (%extFunctions libxml2.tree::%xmlHashTablePtr)
  ;; the extension elements  
  (%extElements libxml2.tree::%xmlHashTablePtr)
  ;; the extension data  
  (%extInfos libxml2.tree::%xmlHashTablePtr)
  ;; the current mode  
  (%mode %xmlCharPtr)
  ;; the current mode URI  
  (%modeURI %xmlCharPtr)
  ;; the document list  
  (%docList %xsltDocumentPtr)
  ;; the current source document; can be NUL  
  (%document %xsltDocumentPtr)
  ;; the current node being processed  
  (%node %xmlNodePtr)
  ;; the current node list %xmlNodePtr curren  
  (%nodeList libxml2.tree::%xmlNodeSetPtr)
  ;; the resulting document  
  (%output %xmlDocPtr)
  ;; the insertion node  
  (%insert %xmlNodePtr)
  ;; the XPath context  
  (%xpathCtxt libxml2.xpath::%xmlXPathContextPtr)
  ;; * Global variables *  
  (%state %xsltTransformState)
  ;; the global variables and params  
  (%globalVars libxml2.tree::%xmlHashTablePtr)
  ;; the instruction in the stylesheet  
  (%inst %xmlNodePtr)
  ;; should XInclude be processed  
  (%xinclude :int)
  ;; the output URI if known  
  (outputFile :pointer)
  ;; is this run profiled  
  (%profile :int)
  ;; the current profiled value  
  (%prof :long)
  ;; Nb of templates in the stack  
  (%profNr :int)
  ;; Size of the templtaes stack  
  (%profMax :int)
  ;; the profile template stack  
  (%profTab :pointer)
  ;; user defined data  
  (%private :pointer)
  ;; the number of extras used  
  (%extrasNr :int)
  ;; the number of extras allocated  
  (%extrasMax :int)
  ;; extra per runtime informations  
  (%extras %xsltRuntimeExtraPtr)
  ;; the stylesheet docs list  
  (%styleList %xsltDocumentPtr)
  ;; the security preferences if any  
  (%sec :pointer)
  ;; a specific error handler  
  (%error :pointer)
  ;; context for the error handler  
  (%errctx :pointer)
  ;; * handling of temporary Result Value Tre  
  (%sortfunc :pointer)
  ;; list of RVT without persistance  
  (%tmpRVT %xmlDocPtr)
  ;; list of persistant RVTs  
  (%persistRVT %xmlDocPtr)
  ;; * Speed optimization when coalescing tex  
  (%ctxtflags :int)
  ;; last text node content  
  (%lasttext %xmlCharPtr)
  ;; last text node size  
  (%lasttsize :uint)
  ;; * Per Context Debugging *  
  (%lasttuse :uint)
  ;; the context level debug status  
  (%debugStatus :int)
  ;; pointer to the variable holding the mas  
  (%traceCode :pointer)
  ;; * dictionnary: shared between stylesheet  
  (%parserOptions :int)
  (%dict libxml2.tree::%xmlDictPtr)
  ;; * all document text strings are internal  
  (%tmpDoc %xmlDocPtr)
  (%internalized :int)
  (%nbKeys :int)
  (%hasTemplKeyPatterns :int)
  ;; the Current Template Rule  
  (%currentTemplateRule %xsltTemplatePtr)
  (%initialContextNode %xmlNodePtr)
  (%initialContextDoc %xmlDocPtr)
  (%cache %xsltTransformCachePtr)
  ;; the current variable item  
  (%contextVariable :pointer)
  ;; list of local tree fragments; will be f  
  (%localRVT %xmlDocPtr)
  (%localRVTBase %xmlDocPtr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom xpath funstions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xsltRegisterExtFunction" %xsltRegisterExtFunction) :int
  (ctxt %xsltTransformContextPtr)
  (name %xmlCharPtr)
  (URI %xmlCharPtr)
  (function :pointer))


(defun register-xpath-extensions (ctxt function-traits)
  (with-garbage-pool ()
    (iter (for (func name ns) in function-traits)
          (%xsltRegisterExtFunction ctxt
                                    (cleanup-register (foreign-string-alloc (eval name))
                                                         #'foreign-string-free)
                                    (if ns
                                        (cleanup-register (foreign-string-alloc (eval ns))
                                                             #'foreign-string-free)
                                        (null-pointer))
                                    (get-callback func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom xsl elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xsltRegisterExtElement" %xsltRegisterExtElement) :int
  (ctxt %xsltTransformContextPtr)
  (name %xmlCharPtr)
  (URI %xmlCharPtr)
  (function :pointer))

(defmacro define-xslt-element (name (style-node input-node output-parent) &body body)
  `(defcallback ,name :void ((%ctxt %xsltTransformContextPtr) (%node %xmlNodePtr) (%inst %xmlNodePtr) (comp :pointer))
     (declare (ignore comp))
     (let ((,style-node (make-instance 'node :pointer %inst))
           (,input-node (make-instance 'node :pointer %node))
           (,output-parent (make-instance 'node
                                          :pointer (foreign-slot-value %ctxt '%xsltTransformContext '%insert)))
           (xpath::*private-xpath-context* (foreign-slot-value %ctxt '%xsltTransformContext '%xpathCtxt)))
       ,@body)))

(defun register-xslt-elements (ctxt element-traits)
  (with-garbage-pool ()
    (iter (for (func name ns) in element-traits)
          (%xsltRegisterExtElement ctxt
                                    (cleanup-register (foreign-string-alloc (eval name))
                                                      #'foreign-string-free)
                                    (if ns
                                        (cleanup-register (foreign-string-alloc (eval ns))
                                                          #'foreign-string-free)
                                        (null-pointer))
                                    (get-callback func)))))

(defvar *lisp-xslt-elements*)

(defmacro with-xslt-elements ((&rest els) &body body)
  `(let ((*lisp-xslt-elements* (if (boundp '*lisp-xslt-elements*)
                                   (concatenate 'list
                                                ',els
                                                *lisp-xslt-elements*)
                                   ',els)))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-transform-context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-libxml2-function ("xsltNewTransformContext" %xsltNewTransformContext) %xsltTransformContextPtr
  (style %xsltStylesheetPtr)
  (doc libxml2.tree::%xmlDocPtr))

(define-libxml2-function ("xsltFreeTransformContext" %xsltFreeTransformContext) :void
  (ctxt %xsltTransformContextPtr))

(defmacro with-transform-context ((var (style doc)) &body body)
  `(let  ((,var (%xsltNewTransformContext (pointer ,style)
                                          (pointer ,doc))))
     (unwind-protect
          (progn (if (boundp 'libxml2.xpath::*lisp-xpath-functions*)
                     (register-xpath-extensions ,var libxml2.xpath::*lisp-xpath-functions*))
                 (if (boundp '*lisp-xslt-elements*)
                     (register-xslt-elements ,var *lisp-xslt-elements*))
                 ,@body)
       (%xsltFreeTransformContext ,var))))
