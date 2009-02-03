;; xhtml.lisp

(in-package :libxml2.html)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-p
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun html-p (doc)
  (find :xml-doc-html (wrapper-slot-value doc 'xtree::%properties)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; meta-encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("htmlGetMetaEncoding" %htmlGetMetaEncoding) %xmlCharPtr
  (doc %xmlDocPtr))

(defun meta-encoding (doc)
  (foreign-string-to-lisp (%htmlGetMetaEncoding (pointer doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse-html/impl (obj &key)
  (:documentation "parse html"))

(defun parse-html (obj &key)
  (make-instance 'document
                 :pointer (parse-html/impl obj)))

;;; parse-html ((str string))

(define-libxml2-function ("htmlReadDoc" %htmlReadDoc) %xmlDocPtr
  (cur %xmlCharPtr)
  (base-url %xmlCharPtr)
  (encoding %xmlCharPtr)
  (options :int))

(defmethod parse-html/impl ((str string)  &key)
  (with-foreign-string (%utf8 "utf-8")
    (with-foreign-string (%str str)
      (%htmlReadDoc %str
                    (null-pointer)
                    %utf8
                    0))))

(defmacro with-parse-html ((var src) &rest body)
  `(let ((,var (parse-html ,src)))
     (unwind-protect
          (progn ,@body)
       (if ,var (release ,var)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize-html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialize-html (obj target))

;;; serialize-html (doc (filename apthname))

(define-libxml2-function ("htmlSaveFileEnc" %htmlSaveFileEnc) :int
  (filename %xmlCharPtr)
  (doc xtree::%xmlDocPtr)
  (encoding %xmlCharPtr))

(defmethod serialize-html (doc (filename pathname) )
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

(defmethod serialize-html ((doc document) (s (eql :to-string)))
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
  