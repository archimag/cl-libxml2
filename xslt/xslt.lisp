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

(defcfun ("xsltInit" %xsltInit) :void)
(defcfun ("exsltRegisterAll" register-exslt-extensions) :void)

(%xsltInit)
;;(%exsltRegisterAll)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype %xsltStylesheetPtr :pointer)
(defctype %xsltTransformContextPtr :pointer)
