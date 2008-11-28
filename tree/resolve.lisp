;; resolve.lisp

(in-package #:libxml2.tree)

(defctype %xmlExternalEntityLoader :pointer)
(defctype %xmlParseInputPtr :pointer)
(defctype %xmlParserCtxPtr :pointer)
(defctype %xmlParserInputBufferPtr	:pointer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-custom-resolvers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlGetExternalEntityLoader" %xmlGetExternalEntityLoader) %xmlExternalEntityLoader)

(defcfun ("xmlSetExternalEntityLoader" %xmlSetExternalEntityLoader) :void
  (loader %xmlExternalEntityLoader))

(defvar *resolvers*)
(defvar *default-resolvers* nil)

(defparameter *default-external-resolver* (%xmlGetExternalEntityLoader))

(defcallback %custom-external-resolver :pointer ((%url :pointer) (%id :pointer) (%context :pointer))
  (or (and (or (if (boundp '*resolvers*) *resolvers*)
               *default-resolvers*)
           (let ((url (unless (null-pointer-p %url) (puri:parse-uri (foreign-string-to-lisp %url))))
                 (id  (unless (null-pointer-p %id) (foreign-string-to-lisp %id))))
             (iter (for resolver in *resolvers*)
                   (for res = (funcall resolver url id %context))
                   (finding res such-that res))))
      (foreign-funcall-pointer *default-external-resolver*
                               ()
                               :pointer %url
                               :pointer %id
                               :pointer %context
                               :pointer)))

(%xmlSetExternalEntityLoader (callback %custom-external-resolver))

(defmacro with-custom-resolvers ((&rest resolvers) &body body)
  `(let ((*resolvers* (list ,@resolvers))
         (*stream-for-xml-parse*))     
     (gp:with-garbage-pool () ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resolve-file/url
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlNewInputFromFile" %xmlNewInputFromFile) %xmlParseInputPtr
  (ctxt %xmlParserCtxPtr)
  (filename %xmlCharPtr))

(defun resolve-file/url (filename %ctxt)
  (with-foreign-string (%filename filename)
    (%xmlNewInputFromFile %ctxt
                          %filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resolve-string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlNewStringInputStream" %xmlNewStringInputStream) %xmlParseInputPtr
  (ctxt %xmlParserCtxPtr)
  (buffer %xmlCharPtr))

(defun resolve-string (str %ctxt)
  (%xmlNewStringInputStream %ctxt
                            (gp:cleanup-register (foreign-string-alloc str) #'foreign-string-free)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resolve-stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcenum %xmlCharEncoding
  (:xml-char-encoding-error -1) ;; = -1 : No char encoding detected
  (:xml-char-encoding-none 0) ;; = 0 : No char encoding detected)
  (:xml-char-encoding-utf8 1) ;; = 1 : UTF-8
  (:xml-char-encoding-utf16le 2) ;; = 2 : UTF-16 little endian
  (:xml-char-encoding-utf16be 3) ;; = 3 : UTF-16 big endian
  (:xml-char-encoding-ucs4le 4) ;; = 4 : UCS-4 little endian
  (:xml-char-encoding-ucs4be 5) ;; = 5 : UCS-4 big endian
  (:xml-char-encoding-ebcdic 6) ;; = 6 : EBCDIC uh!
  (:xml-char-encoding-ucs4-2143 7) ;; = 7 : UCS-4 unusual ordering
  (:xml-char-encoding-ucs4-3412 8) ;; = 8 : UCS-4 unusual ordering
  (:xml-char-encoding-ucs2 9) ;; = 9 : UCS-2
  (:xml-char-encoding-8859-1 10) ;; = 10 : ISO-8859-1 ISO Latin 1
  (:xml-char-encoding-8859-2 11) ;; = 11 : ISO-8859-2 ISO Latin 2
  (:xml-char-encoding-8859-3 12) ;; = 12 : ISO-8859-3
  (:xml-char-encoding-8859-4 13) ;; = 13 : ISO-8859-4
  (:xml-char-encoding-8859-5 14) ;; = 14 : ISO-8859-5
  (:xml-char-encoding-8859-6 15) ;; = 15 : ISO-8859-6
  (:xml-char-encoding-8859-7 16) ;; = 16 : ISO-8859-7
  (:xml-char-encoding-8859-8 17) ;; = 17 : ISO-8859-8
  (:xml-char-encoding-8859-9 18) ;; = 18 : ISO-8859-9
  (:xml-char-encoding-2022-jp 19) ;; = 19 : ISO-2022-JP
  (:xml-char-encoding-shift-jis 20) ;; = 20 : Shift-JIS
  (:xml-char-encoding-euc-jp 21) ;; = 21 : EUC-JP
  (:xml-char-encoding-ascii 22) ;; = 22 : pure ASCII
  )

(defcfun ("xmlNewIOInputStream" %xmlNewIOInputStream) %xmlParseInputPtr
  (ctxt %xmlParserCtxPtr)
  (input %xmlParserInputBufferPtr)
  (enc %xmlCharEncoding))

(defcfun ("xmlParserInputBufferCreateIO" %xmlParserInputBufferCreateIO) %xmlParserInputBufferPtr
  (ioread :pointer)
  (ioclose :pointer)
  (ioctx :pointer)
  (enc %xmlCharEncoding))


(defun resolve-stream (stream %ctxt)
  (setq *stream-for-xml-parse* stream)
  (%xmlNewIOInputStream %ctxt
                        (%xmlParserInputBufferCreateIO (%stream-reader-callback stream)
                                                       (null-pointer)
                                                       (null-pointer)
                                                       :xml-char-encoding-none)
                        :xml-char-encoding-none))
  