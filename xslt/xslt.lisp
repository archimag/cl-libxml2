;; xslt.lisp

(in-package #:libxml2.xslt)

;;; Define and load libxslt

(define-foreign-library libxslt
  (:unix (:or "libxslt.so"))
  (t (:default "libxslt")))

(define-foreign-library libexslt
  (:unix (:or "libexslt.so"))
  (t (:default "libexslt")))


(with-simple-restart (skip "Skip loading foreign library libxslt.")
  (use-foreign-library libxslt)
  (use-foreign-library libexslt))

(define-libxml2-function ("xsltInit" %xsltInit) :void)
(define-libxml2-function ("exsltRegisterAll" register-exslt-extensions) :void)

(%xsltInit)
;;(%exsltRegisterAll)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype %xsltStylesheetPtr :pointer)
(defctype %xsltTransformContextPtr :pointer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xsltSetGenericErrorFunc" %xsltSetGenericErrorFunc) :void
  (ctx :pointer)
  (handler :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library cl_libxml2
  (:unix (:or "cl_libxml2.so"))
  (t (:default "cl_libxml2")))

(use-foreign-library cl_libxml2)


(defcallback %generic-error-handler :void ((message :string))
  (if (boundp 'xtree::*libxml2-errors*)
      (push (make-instance 'xtree:xmlerror
                           :message message
                           :domain :xml-from-xslt
                           :level :xml-err-fatal)
            xtree::*libxml2-errors*)))


(%xsltSetGenericErrorFunc (callback %generic-error-handler)
                         (cffi:foreign-symbol-pointer "cl_libxml2_error_func"))


