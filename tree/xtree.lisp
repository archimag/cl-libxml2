;; xtree.lisp

(in-package #:libxml2.tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define and load libxml2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library libxml2
  (:unix (:or "libxml2.so" "libxml2.so.2"))
  (t (:default "libxml2")))

(with-simple-restart (skip "Skip loading foreign library libxml2.")
  (use-foreign-library libxml2))

(define-foreign-library cllibxml2
  (:unix (:or "cllibxml2.so"))
  (t (:default "cllibxml2")))

(use-foreign-library cllibxml2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlGetVersion" version) :int)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base forward declarations
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

(defcvar ("xmlFree" %xmlFreeVar) :pointer)

(defun %xmlFree (ptr)
  (foreign-funcall-pointer %xmlFreeVar nil :pointer ptr :void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; libxml2-cffi-obejct-wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass libxml2-cffi-object-wrapper ()
  ((pointer :initarg :pointer :reader pointer)))

(defmethod pointer ((obj (eql nil)))
  (null-pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defwrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric wrapper-slot-value (obj slot))
(defgeneric (setf wrapper-slot-value) (value obj slot))

(defmacro defwrapper (wrapper-name cffi-type)
  `(progn
     (defclass ,wrapper-name (libxml2-cffi-object-wrapper) ())
     (defmethod wrapper-slot-value ((obj ,wrapper-name) slot)
       (cffi:foreign-slot-value (pointer obj) (quote ,cffi-type) slot))
     (defmethod (setf wrapper-slot-value) (value (obj ,wrapper-name) slot)
       (setf (cffi:foreign-slot-value (pointer obj) (quote ,cffi-type) slot) value))))

(defun make-libxml2-cffi-object-wrapper/impl (%ptr wrapper-type)
  (unless (null-pointer-p %ptr)
    (make-instance wrapper-type :pointer %ptr)))

(defun wrapper-slot-wrapper (obj slot wrapper-type)
  (make-libxml2-cffi-object-wrapper/impl (wrapper-slot-value obj slot) 
                                         wrapper-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; release
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric release/impl (obj))

(defun release (obj)
  (if (slot-value obj 'pointer)
      (if obj (release/impl obj)))
  (setf (slot-value obj 'pointer) nil))

(gp:defcleanup libxml2-cffi-object-wrapper #'release)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-libxml2-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-libxml2-object ((var value) &rest body)
  `(let ((,var ,value))
     (unwind-protect 
          (progn ,@body)
       (if ,var (release ,var)))))

(defmacro with-object ((var value) &rest body)
  `(with-libxml2-object (,var ,value) ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric base-url (obj))

(defgeneric process-xinclude (obj &optional options))

(defgeneric copy (obj))
