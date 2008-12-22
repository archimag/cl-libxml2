;; serialize.lisp

(in-package #:libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialize (obj target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((doc document) (filename pathname))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlSaveFile" %xmlSaveFile) :int
  (filename :pointer)
  (doc %xmlDocPtr))

(defmethod serialize ((doc document) (filename pathname))
  (with-foreign-string (%path (format nil "~A" filename))
    (%xmlSaveFile %path (pointer doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((doc document) (s (eql :to-string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlDocDumpFormatMemoryEnc" %xmlDocDumpFormatMemoryEnc) :void
  (doc %xmlDocPtr)
  (doc_txt_ptr :pointer)
  (doc_txt_len :pointer)
  (txt_encoding :pointer)
  (format :int))

(defmethod serialize ((doc document) (s (eql :to-string)))
  (with-foreign-string (%utf-8 "utf-8")
    (with-foreign-pointer (%xml-string (foreign-type-size :pointer))
      (with-foreign-pointer (%xml-string-len (foreign-type-size :pointer))
        (%xmlDocDumpFormatMemoryEnc (pointer doc)
                                    %xml-string
                                    %xml-string-len
                                    %utf-8 ;;(null-pointer)
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

(defmethod serialize ((doc document) (stream stream))
  (with-foreign-string (%utf-8 "utf-8")
    (let ((*stream-for-xml-serialize* stream))
      (%xmlSaveFileTo (%xmlOutputBufferCreateIO (%stream-writer-callback stream)
                                                (null-pointer)
                                                (null-pointer)
                                                (null-pointer))
                      (pointer doc)
                      %utf-8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize ((el node) target)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((el node) target)
  (element-p el :throw-error t)
  (let ((%doc (%xmlNewDoc (null-pointer))))
    (unwind-protect
         (progn 
           (setf (foreign-slot-value %doc '%xmlDoc '%children) (pointer el))
           (setf (foreign-slot-value %doc '%xmlDoc '%last) (pointer el))
           (serialize (make-instance 'document
                                     :pointer %doc)
                      target))
      (progn
        (setf (foreign-slot-value %doc '%xmlDoc '%children) (null-pointer))
        (setf (foreign-slot-value %doc '%xmlDoc '%last) (null-pointer))
        (%xmlFreeDoc %doc)))))