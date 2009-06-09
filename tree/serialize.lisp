;; serialize.lisp

(in-package #:libxml2.tree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encoding-string (encoding)
  (if (keywordp encoding)
      (string-downcase (symbol-name encoding))
      encoding))

(defun format-flag (format)
  (if format
      1
      0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialize (obj target &key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((doc document) (filename pathname))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlSaveFormatFileEnc" %xmlSaveFormatFileEnc) :int
  (filename :pointer)
  (doc %xmlDocPtr)
  (encoding %xmlCharPtr)
  (format :int))

(defmethod serialize ((doc document) (filename pathname) &key (encoding :utf-8) (format nil))
  (with-foreign-strings ((%path (format nil "~A" filename))
                         (%encoding (encoding-string encoding)))
    (%xmlSaveFormatFileEnc %path
                           (pointer doc)
                           %encoding
                           (format-flag format))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((doc document) (s (eql :to-string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlDocDumpFormatMemoryEnc" %xmlDocDumpFormatMemoryEnc) :void
  (doc %xmlDocPtr)
  (doc_txt_ptr :pointer)
  (doc_txt_len :pointer)
  (txt_encoding :pointer)
  (format :int))

(defmethod serialize ((doc document) (s (eql :to-string)) &key (encoding :utf-8) (format nil))
  (with-foreign-string (%encoding (encoding-string encoding))
    (with-foreign-pointer (%xml-string (foreign-type-size :pointer))
      (with-foreign-pointer (%xml-string-len (foreign-type-size :pointer))
        (%xmlDocDumpFormatMemoryEnc (pointer doc)
                                    %xml-string
                                    %xml-string-len
                                    %encoding
                                    (format-flag format))
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
  (write-string (foreign-string-to-lisp buffer :count len) *stream-for-xml-serialize*)
  len)

(defun %stream-writer-callback (stream)
  (if (subtypep (stream-element-type stream)
                'character)
      (cffi:callback %write-string-stream)
      (cffi:callback %write-binary-stream)))

(define-libxml2-function ("xmlSaveFormatFileTo" %xmlSaveFormatFileTo) :int
  (buf %xmlOutputBufferPtr)
  (cur %xmlDocPtr)
  (encoding %xmlCharPtr)
  (format :int))

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

(defmethod serialize ((doc document) (stream stream) &key (encoding :utf-8) (format nil))
  (with-foreign-string (%encoding (format nil "~A" encoding))
    (let ((*stream-for-xml-serialize* stream))
      (%xmlSaveFormatFileTo (%xmlOutputBufferCreateIO (%stream-writer-callback stream)
                                                (null-pointer)
                                                (null-pointer)
                                                (%xmlFindCharEncodingHandler %encoding))
                      (pointer doc)
                      %encoding
                      (if format 1 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((el node) target)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((el node) target &key (encoding :utf-8) (format nil))
  (case  (node-type el)
    (:xml-element-node (with-fake-document (doc el)
                         (serialize doc target :format format)))
    (:xml-document-node (serialize (make-instance 'document
                                                  :pointer (pointer el))
                                   target
                                   :encoding encoding
                                   :format format))
    (otherwise (error "Bad node type: ~A" (node-type el)))))
     
