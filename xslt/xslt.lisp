;;; xslt.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.xslt)

;;; Define and load libxslt

(define-foreign-library libxslt
  (:darwin "libxslt.dylib")
  (:unix (:or "libxslt.so" "libxslt.so.1"))
  (t (:default "libxslt")))

(define-foreign-library libexslt
  (:darwin "libexslt.dylib")
  (:unix (:or "libexslt.so" "libexslt.so.0"))
  (t (:default "libexslt")))

(define-foreign-library cllibxml2
  (:darwin "cllibxml2.dylib")
  (:unix (:or "cllibxml2.so"))
  (t (:default "cllibxml2")))

(with-simple-restart (skip "Skip loading foreign library libxslt.")
  (use-foreign-library libxslt)
  (use-foreign-library libexslt)
  (use-foreign-library cllibxml2))

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

(defcallback %generic-error-handler :void ((message :string))
  (if (boundp 'xtree::*libxml2-errors*)
      (push (make-instance 'xtree:xmlerror
                           :message message
                           :domain :xml-from-xslt
                           :level :xml-err-fatal)
            xtree::*libxml2-errors*)))


(%xsltSetGenericErrorFunc (callback %generic-error-handler)
                         (cffi:foreign-symbol-pointer "cl_libxml2_error_func"))


