;; context.lisp

(in-package #:libxml2.xpath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xpath-parser-context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(libxml2.tree::defwrapper xpath-parser-context %xmlXPathParserContext)


;;; with-register-xpath-funtions

(defmacro with-xpath-functions ((&rest funcs) &body body)
  `(let ((*lisp-xpath-functions* ',funcs))
     ,@body))

;;; defxpathfun

(defvar *parser-context*)


(defun value-push (val &optional (ctxt *parser-context*))
  (%valuePush ctxt
              (make-xpath-object val)))

(defun value-pop (&optional (ctxt *parser-context*))
  (xpath-object-value (gp:object-register (make-instance 'xpath-object
                                                         :pointer (%valuePop ctxt)))))

                         
(defmacro define-xpath-function (name (&rest args) &body body)
  (let ((bindings (if args
                      (list (list args '(reverse (iter (for i from 0 below %nargs)
                                                      (collect (value-pop %ctxt))))))))
        (ignore-nargs (unless args '(declare (ignore %nargs))))
        )
  `(defcallback ,name :void ((%ctxt %xmlXPathParserContextPtr) (%nargs :int))
     ,ignore-nargs
     (gp:with-garbage-pool ()
         (bind ,bindings
           (value-push (let ((*parser-context* (make-instance 'xpath-parser-context
                                                              :pointer %ctxt)))
                         ,@body)
                       %ctxt))))))



