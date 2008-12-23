;; parse.lisp

(in-package #:libxml2.tree)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse (obj &key)
  (make-instance 'document
                 :pointer (parse/impl obj)))

(defgeneric parse/impl (obj &key)
  (:documentation "parse xml"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((path pathname))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlReadFile" %xmlReadFile)  %xmlDocPtr
  (filename :pointer)
  (encoding :pointer)
  (options :int))

(defmethod parse/impl ((path pathname) &key)
  (with-foreign-string (_path (format nil "~A" path))
    (%xmlReadFile _path (cffi:null-pointer) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((str string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlReadDoc" %xmlReadDoc) %xmlDocPtr
  (cur %xmlCharPtr)
  (base-url %xmlCharPtr)
  (encoding %xmlCharPtr)
  (options :int))

(defmethod parse/impl ((str string)  &key)
  (with-foreign-string (%str str)
    (%xmlReadDoc %str
                 (null-pointer)
                 (null-pointer)
                 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((uri puri))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod parse/impl ((uri puri:uri)  &key)
  (with-foreign-string (_path (format nil "~A" uri))
    (%xmlReadFile _path (cffi:null-pointer) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((octets (array unsigned-byte)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod parse/impl ((octets array) &key)
  (flexi-streams:with-input-from-sequence (in octets)
    (parse/impl in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((stream stream)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *stream-for-xml-parse*)

(defcallback %read-binary-stream :int ((context :pointer) (buffer :pointer) (len :int))
  (declare (ignore context))
  (if (and *stream-for-xml-parse*
           (input-stream-p *stream-for-xml-parse*))
      (iter (for pos from 0 below len)
            (for byte next (read-byte *stream-for-xml-parse* nil))
            (while byte)
            (setf (cffi:mem-ref buffer :uchar pos) byte)
            (finally (return pos)))
      -1))

(defcallback %read-string-stream :int ((context :pointer) (buffer :pointer) (len :int))
  (declare (ignore context))
  (if *stream-for-xml-parse*
      (let ((curpos 0))
        (iter (for ch next (read-char *stream-for-xml-parse* nil))
              (while ch)
              (for octets = (flex:string-to-octets (make-string 1 :initial-element ch) :external-format :utf-8))
              (if (< (+ (length octets)
                        curpos)
                     len)
                  (iter (for byte in-vector octets)
                        ;;(print (flex:octets-to-string #(byte)))
                        (setf (cffi:mem-ref buffer :uchar curpos) byte)
                        (incf curpos))
                  (progn
                    (unread-char ch *stream-for-xml-parse*)
                    (finish))))
        curpos)
      -1))

(defun %stream-reader-callback (stream)
  (if (subtypep (stream-element-type stream)
                'character)
      (cffi:callback %read-string-stream)
      (cffi:callback %read-binary-stream)))
          

(define-libxml2-function ("xmlReadIO" %xmlReadIO) %xmlDocPtr
  (ioread :pointer)
  (ioclose :pointer)
  (ioctx :pointer)
  (url %xmlCharPtr)
  (encoding %xmlCharPtr)
  (options :int))

(defmethod parse/impl ((stream stream) &key)
  (let ((*stream-for-xml-parse* stream))
    (with-foreign-string (%utf-8 "utf-8")
    (%xmlReadIO (%stream-reader-callback *stream-for-xml-parse*)
                (cffi:null-pointer)
                (cffi:null-pointer)
                (cffi:null-pointer)
                (cffi:null-pointer)
                0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-parse-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-parse-document ((var src) &rest body)
  `(let ((,var (parse ,src)))
     (unwind-protect
          (progn ,@body)
       (if ,var (release ,var)))))


