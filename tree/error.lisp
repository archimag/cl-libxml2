;; error.lisp

(in-package #:libxml2.tree)

;; xmlErrorLevel
(defcenum %xmlErrorLevel
  (:xml-err-none 0)
  (:xml-err-warning 1) ;; A simple warning
  (:xml-err-error 2)   ;; A recoverable error
  (:xml-err-fatal 3))  ;; A fatal error

;; xmlErrorDomain
(defcenum %xmlErrorDomain
  (:xml-from-none  0)
  (:xml-from-parser 1)        ;; The XML parser
  (:xml-from-tree 2)          ;; The tree module
  (:xml-from-namespace 3)     ;; The XML Namespace module
  (:xml-from-dtd 4)           ;; The XML DTD validation with parser contex
  (:xml-from-html 5)          ;; The HTML parser
  (:xml-from-memory 6)        ;; The memory allocator
  (:xml-from-output 7)        ;; The serialization code
  (:xml-from-io 8)            ;; The Input/Output stack
  (:xml-from-ftp 9)           ;; The FTP module
  (:xml-from-http 10)         ;; The HTTP module
  (:xml-from-xinclude 11)     ;; The XInclude processing
  (:xml-from-xpath 12)        ;; The XPath module
  (:xml-from-xpointer 13)     ;; The XPointer module
  (:xml-from-regexp 14)       ;; The regular expressions module
  (:xml-from-datatype 15)     ;; The W3C XML Schemas Datatype module
  (:xml-from-schemasp 16)     ;; The W3C XML Schemas parser module
  (:xml-from-schemasv 17)     ;; The W3C XML Schemas validation module
  (:xml-from-relaxngp 18)     ;; The Relax-NG parser module
  (:xml-from-relaxngv 19)     ;; The Relax-NG validator module
  (:xml-from-catalog 20)      ;; The Catalog module
  (:xml-from-c14n 21)         ;; The Canonicalization module
  (:xml-from-xslt 22)         ;; The XSLT engine from libxslt
  (:xml-from-valid 23)        ;; The XML DTD validation with valid context
  (:xml-from-check 24)        ;; The error checking module
  (:xml-from-writer 25)       ;; The xmlwriter module
  (:xml-from-module 26)       ;; The dynamically loaded module modul
  (:xml-from-i18n 27)         ;; The module handling character conversion
  (:xml-from-schematronv 28)) ;; The Schematron validator module

;; _xmlError
(defcstruct %xmlError
  (%domain %xmlErrorDomain) ;; What part of the library raised this er
  (%code %xmlErrorLevel)    ;; The error code, e.g. an xmlParserError
  (%message  %xmlCharPtr)   ;; human-readable informative error messag
  (%level %xmlErrorLevel)   ;; how consequent is the error
  (%file %xmlCharPtr)       ;; the filename
  (%line :int)              ;; the line number if available
  (%str1 %xmlCharPtr)       ;; extra string information
  (%str2 %xmlCharPtr)       ;; extra string information
  (%str3 %xmlCharPtr)       ;; extra string information
  (%int1 :int)              ;; extra number information
  (%int2 :int)              ;; column number of the error or 0 if N/A
  (%ctxt :pointer)          ;; the parser context if available
  (%node :pointer))         ;; the node in the tree

;;
(defctype %xmlErrorPtr :pointer)
   
(defcfun ("xmlGetLastError" %xmlGetLastError) %xmlErrorPtr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xmlerror
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper xmlerror %xmlError)

;;; last-error

(defun last-error ()
  (make-libxml2-cffi-object-wrapper/impl (%xmlGetLastError) 'xmlerror))

;;; error-message

(defun error-message (err)
  (metatilities:strip-whitespace (foreign-string-to-lisp (wrapper-slot-value err '%message))))

;;; error-domain

(defun error-domain (err)
  (wrapper-slot-value err '%domain))

;;; error-level

(defun error-level (err)
  (wrapper-slot-value err '%level))

(defmethod print-object ((err xmlerror) stream)
  (format stream
          "~A ~A: ~A"
          (case (error-domain err)
            (:xml-from-parser "XML parser")
            (:xml-from-xpath "XPath")
            (:xml-from-xslt "XSLT engine")
            (otherwise (error-domain err)))
          (case (error-level err)
            (:xml-err-warning "warning")
            (:xml-err-error   "recoverable error")
            (:xml-err-fatal   "fatal error")
            (otherwise        "none error"))
          (error-message err)))

;;; reset error

(defcfun ("xmlResetError" %xmlResetError) :void
  (err %xmlErrorPtr))

(defun reset-error (err)
  (%xmlResetError (pointer err)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structred error 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition libxml2-error (error)
  ((xmlerror :initarg :xmlerror :reader getxmlerror)))

(defmethod print-object ((err libxml2-error) stream)
  (print-object (getxmlerror err) stream))

(defcallback %structured-error-handler :void ((userdata :pointer) (%err %xmlErrorPtr))
  (declare (ignore userdata))
  (let ((err (make-instance 'xmlerror :pointer %err)))
    (if (eql (error-level err) :xml-err-fatal)
        (error 'libxml2-error :xmlerror err)
        (restart-case (error 'libxml2-error :xmlerror err)
          (reset () (reset-error err))))))

(defcfun ("xmlSetStructuredErrorFunc" %xmlSetStructuredErrorFunc) :void
  (ctx :pointer)
  (handler :pointer))

(defun init-error-handling ()
  (%xmlSetStructuredErrorFunc (null-pointer)
                              (callback %structured-error-handler)))

(init-error-handling)