;; xhtml.lisp

(in-package :libxml2.xhtml)

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
  