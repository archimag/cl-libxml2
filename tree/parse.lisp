;;; parse.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.tree)


;;;Enum xmlParserOption
(defbitfield %xmlParserOption
  (:xml-parse-recover 1)        ;; recover on errors
  (:xml-parse-noent 2)          ;; substitute entities
  (:xml-parse-dtdload 4)        ;; load the external subset
  (:xml-parse-dtdattr 8)        ;; default DTD attributes
  (:xml-parse-dtdvalid 16)      ;; validate with the DTD
  (:xml-parse-noerror 32)       ;; suppress error reports
  (:xml-parse-nowarning 64)     ;; suppress warning reports
  (:xml-parse-pedantic 128)     ;; pedantic error reporting
  (:xml-parse-noblanks 256)     ;; remove blank nodes
  (:xml-parse-sax1 512)         ;; use the SAX1 interface internally
  (:xml-parse-xinclude 1024)    ;; Implement XInclude substitition
  (:xml-parse-nonet 2048)       ;; Forbid network access
  (:xml-parse-nodict 4096)      ;; Do not reuse the context dictionnary
  (:xml-parse-nsclean 8192)     ;; remove redundant namespaces declarations
  (:xml-parse-nocdata 16384)    ;; merge CDATA as text nodes
  (:xml-parse-noxincnode 32768) ;; do not generate XINCLUDE START/END nodes
  (:xml-parse-compact 65536)    ;; compact small text nodes; no modification of the tree allowed afterwards (will possibly crash if you try to modify the tree)
  (:xml-parse-old10 131072)     ;; parse using XML-1.0 before update 5
  (:xml-parse-nobasefix 262144) ;; do not fixup XINCLUDE xml:base uris
  (:xml-parse-huge 524288)      ;; relax any hardcoded limit from the parser
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-options (&rest options)
  (foreign-bitfield-value '%xmlParserOption options))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse (obj &rest options)
  (let ((ptr (parse/impl obj
                         (foreign-bitfield-value '%xmlParserOption options))))
    (unless (null-pointer-p ptr)
      (make-instance 'document
                     :pointer ptr))))

(defgeneric parse/impl (obj options)
  (:documentation "parse xml"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((path pathname))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlReadFile" %xmlReadFile)  %xmlDocPtr
  (filename :pointer)
  (encoding :pointer)
  (options :int))

(defmethod parse/impl ((path pathname) options)
  (with-foreign-string (_path (format nil "~A" path))
    (%xmlReadFile _path (cffi:null-pointer) options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((str string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlReadDoc" %xmlReadDoc) %xmlDocPtr
  (cur %xmlCharPtr)
  (base-url %xmlCharPtr)
  (encoding %xmlCharPtr)
  (options :int))

(defmethod parse/impl ((str string) options)
  (with-foreign-string (%str str)
    (%xmlReadDoc %str
                 (null-pointer)
                 (null-pointer)
                 options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((uri puri))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod parse/impl ((uri puri:uri)  options)
  (with-foreign-string (_path (format nil "~A" uri))
    (%xmlReadFile _path (cffi:null-pointer) options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse ((octets (array unsigned-byte)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod parse/impl ((octets array) options)
  (flexi-streams:with-input-from-sequence (in octets)
    (parse/impl in options)))

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

(defmethod parse/impl ((stream stream) options)
  (let ((*stream-for-xml-parse* stream))
    (with-foreign-string (%utf-8 "utf-8")
    (%xmlReadIO (%stream-reader-callback *stream-for-xml-parse*)
                (cffi:null-pointer)
                (cffi:null-pointer)
                (cffi:null-pointer)
                (cffi:null-pointer)
                options))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-parse-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-parse-document ((var src &rest options) &body body)
  `(let ((,var (parse ,src ,@options)))
     (unwind-protect
          (progn ,@body)
       (if ,var (release ,var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defxml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defxml (var src)
  `(progn
     (if (and (boundp (quote ,var))
              (typep ,var 'xtree::libxml2-cffi-object-wrapper))
         (xtree:release ,var))
     (defparameter ,var (xtree:parse ,src))))
