;;; html.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.html)

(defctype %htmlDocPtr %xmlDocPtr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create-html-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("htmlNewDocNoDtD" %htmlNewDocNoDtD) %htmlDocPtr
  (uri %xmlCharPtr)
  (external-id %xmlCharPtr))


(defun create-html-document (&key (uri "http://www.w3.org/TR/REC-html40/loose.dtd") (external-id "-//W3C//DTD HTML 4.0 Transitional//EN"))
  "Creates a new HTML document. Do not initialize the DTD if uri and external-id is nil.
Params:
uri:          URI for the dtd, default -  http://www.w3.org/TR/REC-html40/loose.dtd
external-id:  the external ID of the DTD 
Returns: a new document
"
  (flet ((toforeign (str)
           (if str
               (gp:cleanup-register (foreign-string-alloc str) #'foreign-string-free)
               (null-pointer))))
    (gp:with-garbage-pool ()
      (make-instance 'document
                     :pointer (%htmlNewDocNoDtD (toforeign uri)
                                                (toforeign external-id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-p
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun html-p (doc)
  (eql (node-type doc) :xml-html-document-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; meta-encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("htmlGetMetaEncoding" %htmlGetMetaEncoding) %xmlCharPtr
  (doc %htmlDocPtr))

(defun meta-encoding (doc)
  "Encoding definition lookup in the Meta tags"
  (foreign-string-to-lisp (%htmlGetMetaEncoding (pointer doc))))

(define-libxml2-function ("htmlSetMetaEncoding" %htmlSetMetaEncoding) :int
  (doc %htmlDocPtr)
  (encoding %xmlCharPtr))

;;; (setf meta-encoding)

(defun (setf meta-encoding) (encoding doc)
  "Sets the current encoding in the Meta tags
NOTE: this will not change the document content encoding, just the META flag associated."
  (with-foreign-string (%encoding encoding)
    (%htmlSetMetaEncoding (pointer doc)
                          %encoding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defgeneric parse-html/impl (obj &key)
;;   (:documentation "parse html"))

(defgeneric parse-html (obj &key &allow-other-keys)
  (:documentation "parse html"))

(defmethod parse-html :around (obj &key &allow-other-keys)
  (let ((ptr (call-next-method)))
    (if (not (null-pointer-p ptr))
        (make-instance 'document
                       :pointer ptr))))

;;; parse-html ((str string) &key)

(define-libxml2-function ("htmlReadDoc" %htmlReadDoc) %xmlDocPtr
  (cur %xmlCharPtr)
  (base-url %xmlCharPtr)
  (encoding %xmlCharPtr)
  (options :int))

(defmethod parse-html ((str string)  &key)
  (with-foreign-string (%utf8 "utf-8")
    (with-foreign-string (%str str)
      (%htmlReadDoc %str
                    (null-pointer)
                    %utf8
                    0))))

;;; parse-html ((path pathname) &key)

(define-libxml2-function ("htmlReadFile" %htmlReadFile) %htmlDocPtr
  (filename %xmlCharPtr)
  (encoding %xmlCharPtr)
  (options :int))

(defmethod parse-html ((path pathname) &key encoding)
  (gp:with-garbage-pool ()
    (let ((%path (gp:cleanup-register (cffi:foreign-string-alloc (format nil "~A" path))
                                      #'cffi:foreign-string-free))
          (%encoding (if encoding
                         (gp:cleanup-register (cffi:foreign-string-alloc encoding)
                                              #'cffi:foreign-string-free)
                         (cffi:null-pointer))))
      (%htmlReadFile %path
                     %encoding
                     0))))

;;; parse-html ((uri puri:uri))

(defmethod parse-html ((uri puri:uri) &key encoding)
  (gp:with-garbage-pool ()
    (let ((%path (gp:cleanup-register (cffi:foreign-string-alloc (format nil "~A" uri))
                                      #'cffi:foreign-string-free))
          (%encoding (if encoding
                         (gp:cleanup-register (cffi:foreign-string-alloc encoding)
                                              #'cffi:foreign-string-free)
                         (cffi:null-pointer))))
      (%htmlReadFile %path
                     %encoding
                     0))))

;;; parse-html ((octets (array unsigned-byte)))

(defmethod parse-html ((octets array) &key encoding)
  (flexi-streams:with-input-from-sequence (in octets)
    (parse-html in :encoding encoding)))


;;; parse-html ((stream stream)

(define-libxml2-function ("htmlReadIO" %htmlReadIO) %htmlDocPtr
  (ioread :pointer)
  (ioclose :pointer)
  (ioctx :pointer)
  (url %xmlCharPtr)
  (encoding %xmlCharPtr)
  (options :int))

(defmethod parse-html ((stream stream) &key encoding)
  (let ((xtree::*stream-for-xml-parse* stream))
    (with-foreign-string (%utf-8 (or encoding "utf-8"))
      (%htmlReadIO (xtree::%stream-reader-callback xtree::*stream-for-xml-parse*)
                  (cffi:null-pointer)
                  (cffi:null-pointer)
                  (cffi:null-pointer)
                  (cffi:null-pointer)
                  0))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-parse-html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-parse-html ((var src &rest keys &key &allow-other-keys) &rest body)
  `(let ((,var (parse-html ,src ,@keys)))
     (unwind-protect
          (progn ,@body)
       (if ,var (release ,var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse-document-fragment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse-html-fragment (html)
  (:documentation "Parse HTML to document fragment"))

(defmethod parse-html-fragment ((html string))
  (with-parse-html (doc (format nil "<div>~A</div>" html))
    (let ((top (first-child (first-child (root doc))))
          (fragment (make-document-fragment)))
      (iter (for node in (all-childs top))
            (append-child fragment (detach node)))
      fragment)))

(defmacro with-parse-html-fragment ((var src) &rest body)
  `(with-object (,var (parse-html-fragment ,src))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize-html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialize-html (obj target &key))

;;; serialize-html (doc (filename apthname))

(define-libxml2-function ("htmlSaveFileEnc" %htmlSaveFileEnc) :int
  (filename %xmlCharPtr)
  (doc xtree::%xmlDocPtr)
  (encoding %xmlCharPtr))

(defmethod serialize-html (doc (filename pathname) &key)
  (with-foreign-string (%path (format nil "~A" filename))
    (%htmlSaveFileEnc %path
                      (pointer doc)
                      (%htmlGetMetaEncoding (pointer doc)))))


;;; serialize-html (doc :to-string)

(define-libxml2-function ("htmlDocDumpMemoryFormat" %htmlDocDumpMemoryFormat) :void
  (cur xtree::%xmlDocPtr)
  (mem %xmlCharPtr)
  (size :pointer)
  (format :int))

(defmethod serialize-html ((doc document) (s (eql :to-string)) &key)
  (with-foreign-pointer (%xml-string (foreign-type-size :pointer))
    (with-foreign-pointer (%xml-string-len (foreign-type-size :pointer))
      (%htmlDocDumpMemoryFormat (pointer doc)
                                    %xml-string
                                    %xml-string-len
                                    4)
      (let ((%ptr (mem-ref %xml-string :pointer)))
        (unwind-protect 
             (foreign-string-to-lisp %ptr)
          (xtree::%xmlFree %ptr))))))
  
;;; serialize-html (doc (stream stream))

(define-libxml2-function ("htmlDocContentDumpOutput" %htmlDocContentDumpOutput) :void
   (buf xtree::%xmlOutputBufferPtr)
   (cur %xmlDocPtr)
   (encoding %xmlCharPtr))


(defmethod serialize-html ((doc document) (stream stream) &key)
  (let ((xtree::*stream-for-xml-serialize* stream)
        (%buf (xtree::%xmlOutputBufferCreateIO (xtree::%stream-writer-callback stream)
                                               (null-pointer)
                                               (null-pointer)
                                               (null-pointer))))
    (%htmlDocContentDumpOutput %buf
                               (pointer doc)
                               (null-pointer))
    (xtree::%xmlOutputBufferClose %buf)))

