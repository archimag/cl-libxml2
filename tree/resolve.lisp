;; resolve.lisp

(in-package #:libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cffi declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype %xmlExternalEntityLoader :pointer)
(defctype %xmlParseInputPtr :pointer)
(defctype %xmlParserCtxPtr :pointer)
(defctype %xmlParserInputBufferPtr	:pointer)

(defcfun ("xmlGetExternalEntityLoader" %xmlGetExternalEntityLoader) %xmlExternalEntityLoader)

(defcfun ("xmlSetExternalEntityLoader" %xmlSetExternalEntityLoader) :void
  (loader %xmlExternalEntityLoader))

;; (defcfun ("xmlNewInputStream" %xmlNewInputStream) %xmlParseInputPtr
;;   (ctxt %xmlParserCtxPtr))

(defcfun ("xmlNewInputFromFile" %xmlNewInputFromFile) %xmlParseInputPtr
  (ctxt %xmlParserCtxPtr)
  (filename %xmlCharPtr))

(defcfun ("xmlNewStringInputStream" %xmlNewStringInputStream) %xmlParseInputPtr
  (ctxt %xmlParserCtxPtr)
  (buffer %xmlCharPtr))


(defcfun ("xmlParserInputBufferCreateIO" %xmlParserInputBufferCreateIO) %xmlParserInputBufferPtr
  (ioread :pointer)
  (ioclose :pointer)
  (ioctx :pointer)
  (enc %xmlCharEncoding))


(defcfun ("xmlNewIOInputStream" %xmlNewIOInputStream) %xmlParseInputPtr
  (ctxt %xmlParserCtxPtr)
  (input %xmlParserInputBufferPtr)
  (enc %xmlCharEncoding))

;; (defcfun ("xmlFreeParserInputBuffer" %xmlFreeParserInputBuffer) :void
;;   (in %xmlParserInputBufferPtr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom resolvers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun resolve-file/url (filename %ctxt)
  (with-foreign-string (%filename filename)
    (%xmlNewInputFromFile %ctxt
                          %filename)))

(defun resolve-string (str %ctxt)
  (%xmlNewStringInputStream %ctxt
                            (gp:cleanup-register (foreign-string-alloc str) #'foreign-string-free)))

(defun resolve-stream (stream %ctxt)
  (setq *stream-for-xml-parse* stream)
  (%xmlNewIOInputStream %ctxt
                        (%xmlParserInputBufferCreateIO (%stream-reader-callback stream)
                                                       (null-pointer)
                                                       (null-pointer)
                                                       :xml-char-encoding-none)
                        :xml-char-encoding-none))
  
