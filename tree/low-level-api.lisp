;; core.lisp

(in-package :libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define and load libxml2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library libxml2
  (:unix (:or "libxml2.so"))
  (t (:default "libxml2")))

(with-simple-restart (skip "Skip loading foreign library libxml2.")
  (use-foreign-library libxml2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype %xmlCharPtr :pointer)
(defctype %xmlNsPtr :pointer)
(defctype %xmlDocPtr :pointer)
(defctype %xmlNodePtr :pointer)
(defctype %xmlAttrPtr :pointer)
(defctype %xmlNodeSetPtr :pointer)
(defctype %xmlDictPtr :pointer)
(defctype %xmlHashTablePtr :pointer)
(defctype %charPtr :pointer)
(defctype %xmlOutputBufferPtr :pointer)


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


(defcenum %xmlElementType
  (:xml-element-node 1)
  (:xml-attribute-node 2)
  (:xml-text-node 3)
  (:xml-cdata-section-node  4)
  (:xml-entity-refODE  5)
  (:xml-entity-node 6)
  (:xml-pi-node 7)
  (:xml-comment-node 8)
  (:xml-document-node 9)
  (:xml-document-type-node  10)
  (:xml-document-frag-node  11)
  (:xml-notation-node 12)
  (:xml-html-document-node  13)
  (:xml-dtd-node 14)
  (:xml-element-decl 15)
  (:xml-attribute-decl 16)
  (:xml-entity-decl 17)
  (:xml-namespace-decl 18)
  (:xml-xinclude-start 19)
  (:xml-xinclude-end 20)
  (:xml-docb-document-node  21))

(defctype %xmlNsType %xmlElementType)

;; fuck! fuck! fuck!
;; (defcfun ("xmlFree" %xmlFree) :void
;;    (ptr :pointer))
(defun %xmlFree (ptr)
  (foreign-funcall "free" :pointer ptr :void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xmlNode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlXIncludeProcessTree" %xmlXIncludeProcessTree) :int
  (node %xmlNodePtr))

(defcfun ("xmlSetTreeDoc" %xmlSetTreeDoc) :void
  (tree %xmlNodePtr)
  (doc %xmlDocPtr))

(defcfun ("xmlUnlinkNode" %xmlUnlinkNode) :void
  (node %xmlNodePtr))

(defcfun ("xmlAddChild" %xmlAddChild) %xmlNodePtr
  (parent %xmlNodePtr)
  (child %xmlNodePtr))

(defcfun ("xmlAddNextSibling" %xmlAddNextSibling) %xmlNodePtr
  (cur %xmlNodePtr)
  (elem %xmlNodePtr))

(defcfun ("xmlAddPrevSibling" %xmlAddPrevSibling) %xmlNodePtr
  (cur %xmlNodePtr)
  (elem %xmlNodePtr))


(defcfun ("xmlReplaceNode" %xmlReplaceNode) %xmlNodePtr
  (old %xmlNodePtr)
  (cur %xmlNodePtr))
  

(defcfun ("xmlNodeGetBase" %xmlNodeGetBase) %xmlCharPtr
  (doc %xmlDocPtr)
  (cur %xmlNodePtr))

(defcfun ("xmlNodeGetContent" %xmlNodeGetContent) %xmlCharPtr
  (cur %xmlNodePtr))

(defcfun ("xmlNodeSetContent" %xmlNodeSetContent) :void
  (cur %xmlNodePtr)
  (content %xmlCharPtr))


;; (defcfun ("xmlGetProp" %xmlGetProp) :pointer
;;   (node %xmlNodePtr)
;;   (name :pointer))


;; (defcfun ("xmlSetProp" %xmlSetProp) :pointer
;;   (node %xmlNodePtr)
;;   (name :pointer)
;;   (value :pointer))



;; (defcfun ("xmlHasProp" %xmlHasProp) %xmlAttrPtr
;;   (node %xmlNodePtr)
;;   (name %xmlCharPtr))

 
