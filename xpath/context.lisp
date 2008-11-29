;; context.lisp

(in-package #:libxml2.xpath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xpath-parser-context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct %xmlXPathParserContext
  ;; const xmlChar *	cur	: the current char being parsed
  (%cur %xmlCharPtr)
  ;; const xmlChar *	base	: the full expression
  (%base %xmlCharPtr)
  ;; int	error	: error code
  (%error :int)
  ;; xmlXPathContextPtr	context	: the evaluation context
  (%context %xmlXPathContextPtr)
  ;; xmlXPathObjectPtr	value	: the current value
  (%value %xmlXPathObjectPtr)
  ;; int	valueNr	: number of values stacked
  (%valueNr :int)
  ;; int	valueMax	: max number of values stacked
  (%valueMax :int)
  ;; xmlXPathObjectPtr *	valueTab	: stack of values
  (%valueTab %xmlXPathObjectPtr)
  ;; xmlXPathCompExprPtr	comp	: the precompiled expression
  (%comp %xmlXPathCompExprPtr)
  ;; int	xptr	: it this an XPointer expression
  (%xptr :int)
  ;; xmlNodePtr	ancestor	: used for walking preceding axis
  (%ancestor %xmlNodePtr))

(defctype %xmlXPathParserContextPtr :pointer)

(libxml2.tree::defwrapper xpath-parser-context %xmlXPathParserContext)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-register-xpath-funtions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-xpath-functions ((&rest funcs) &body body)
  `(let ((*lisp-xpath-functions* ',funcs))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define-xpath-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *parser-context*)

;;; value-push

(defcfun ("valuePush" %valuePush) :int
  (ctxt %xmlXPathContextPtr)
  (value %xmlXPathObjectPtr))

(defun value-push (val &optional (ctxt *parser-context*))
  (%valuePush ctxt
              (make-xpath-object val)))

;;; value-pop

(defcfun ("valuePop" %valuePop) %xmlXPathObjectPtr
  (ctxt %xmlXPathContextPtr))

(defun value-pop (&optional (ctxt *parser-context*))
  (xpath-object-value (gp:object-register (make-instance 'xpath-object
                                                         :pointer (%valuePop ctxt)))))

;;; define-xpath-function
                         
(defmacro define-xpath-function (name (&rest args) &body body)
  (let ((bindings (if args
                      (list (list args '(reverse (iter (for i from 0 below %nargs)
                                                      (collect (value-pop %ctxt))))))))
        (ignore-nargs (unless args '(declare (ignore %nargs)))))
  `(defcallback ,name :void ((%ctxt %xmlXPathParserContextPtr) (%nargs :int))
     ,ignore-nargs
     (gp:with-garbage-pool ()
         (bind ,bindings
           (value-push (let ((*parser-context* (make-instance 'xpath-parser-context
                                                              :pointer %ctxt)))
                         ,@body)
                       %ctxt))))))



