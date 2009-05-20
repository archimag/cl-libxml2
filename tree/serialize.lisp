;; serialize.lisp

(in-package #:libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialize (obj target &key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((doc document) (filename pathname))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlSaveFile" %xmlSaveFile) :int
  (filename :pointer)
  (doc %xmlDocPtr))

(define-libxml2-function ("xmlSaveFileEnc" %xmlSaveFileEnc) :int
  (filename :pointer)
  (doc %xmlDocPtr)
  (encoding %xmlCharPtr))

(defmethod serialize ((doc document) (filename pathname) &key (encoding :utf-8))
  (with-foreign-strings ((%path (format nil "~A" filename))
                         (%encoding (format nil "~A" encoding)))
    (%xmlSaveFileEnc %path
                     (pointer doc)
                     %encoding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((doc document) (s (eql :to-string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlDocDumpFormatMemoryEnc" %xmlDocDumpFormatMemoryEnc) :void
  (doc %xmlDocPtr)
  (doc_txt_ptr :pointer)
  (doc_txt_len :pointer)
  (txt_encoding :pointer)
  (format :int))

(defmethod serialize ((doc document) (s (eql :to-string)) &key (encoding :utf-8))
  (with-foreign-string (%encoding (format nil "~A" encoding))
    (with-foreign-pointer (%xml-string (foreign-type-size :pointer))
      (with-foreign-pointer (%xml-string-len (foreign-type-size :pointer))
        (%xmlDocDumpFormatMemoryEnc (pointer doc)
                                    %xml-string
                                    %xml-string-len
                                    %encoding
                                    1)
        (let ((%ptr (mem-ref %xml-string :pointer)))
          (unwind-protect 
               (foreign-string-to-lisp %ptr)
            (%xmlFree %ptr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((doc document) (stream stream))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *stream-for-xml-serialize*)

(defcallback %write-binary-stream :int ((context :pointer) (buffer :pointer) (len :int))
  (declare (ignore context))
  (iter (for pos from 0 below len)
        (write-byte (mem-aref buffer :uchar pos) *stream-for-xml-serialize*))
  len)

(defcallback %write-string-stream :int ((context :pointer) (buffer :pointer) (len :int))
  (declare (ignore context))
  (write (foreign-string-to-lisp buffer :count len) :stream *stream-for-xml-serialize*)
  len)

(defun %stream-writer-callback (stream)
  (if (subtypep (stream-element-type stream)
                'character)
      (cffi:callback %write-string-stream)
      (cffi:callback %write-binary-stream)))

(define-libxml2-function ("xmlSaveFileTo" %xmlSaveFileTo) :int
  (buf %xmlOutputBufferPtr)
  (cur %xmlDocPtr)
  (encoding %xmlCharPtr))

(define-libxml2-function ("xmlOutputBufferCreateIO" %xmlOutputBufferCreateIO) %xmlOutputBufferPtr
  (iowrite :pointer)
  (ioclose :pointer)
  (ioctx :pointer)
  (encoder :pointer))

(define-libxml2-function ("xmlOutputBufferClose" %xmlOutputBufferClose) :int
  (buf %xmlOutputBufferPtr))

(defctype %xmlCharEncodingHandlerPtr :pointer)

(define-libxml2-function ("xmlFindCharEncodingHandler" %xmlFindCharEncodingHandler) %xmlCharEncodingHandlerPtr
  (name %xmlCharPtr))

(defmethod serialize ((doc document) (stream stream) &key (encoding :utf-8))
  (with-foreign-string (%encoding (format nil "~A" encoding))
    (let ((*stream-for-xml-serialize* stream))
      (%xmlSaveFileTo (%xmlOutputBufferCreateIO (%stream-writer-callback stream)
                                                (null-pointer)
                                                (null-pointer)
                                                (%xmlFindCharEncodingHandler %encoding))
                      (pointer doc)
                      %encoding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((el node) target)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((el node) target &key (encoding :utf-8))
  (case  (node-type el)
    (:xml-element-node (with-fake-document (doc el)
                         (serialize doc target)))
    (:xml-document-node (serialize (make-instance 'document
                                                  :pointer (pointer el))
                                   target
                                   :encoding encoding))
    (otherwise (error "Bad node type: ~A" (node-type el)))))
     
