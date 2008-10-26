;; stylesheet.lisp

(in-package #:libxml2.xslt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define and load libxslt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library libxslt
  (:unix (:or "libxslt.so"))
  (t (:default "libxslt")))

(with-simple-restart (skip "Skip loading foreign library libxslt.")
  (use-foreign-library libxslt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defctype %xsltStylesheetPtr :pointer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xsltParseStylesheetDoc" %xsltParseStylesheetDoc) %xsltStylesheetPtr
  (doc libxml2.tree::%xmlDocPtr))

(defcfun ("xsltParseStylesheetFile" %xsltParseStylesheetFile) %xsltStylesheetPtr
  (filename libxml2.tree::%xmlCharPtr))

(defcfun ("xsltFreeStylesheet" %xsltFreeStylesheet) :void
  (style %xsltStylesheetPtr))

(defcfun ("xsltApplyStylesheet" %xsltApplyStylesheet) libxml2.tree::%xmlDocPtr
  (style %xsltStylesheetPtr)
  (doc libxml2.tree::%xmlDocPtr)
  (args :pointer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass stylesheet (libxml2.tree::libxml2-cffi-object-wrapper) ())
;;  ((pointer :initarg :pointer :reader pointer)))


(defmethod release/impl ((style stylesheet))
  (%xsltFreeStylesheet  (pointer style)))


;;; parse-stylesheet

(defgeneric parse-stylesheet (obj))

(defmethod parse-stylesheet ((filename pathname))
  (with-foreign-string (%filename (format nil "~A" filename))
    (make-instance 'stylesheet
                   :pointer (%xsltParseStylesheetFile %filename))))

(defmethod parse-stylesheet ((doc document))
  (make-instance 'stylesheet
                 :pointer (%xsltParseStylesheetDoc (pointer doc))))

(defmethod parse-stylesheet (obj)
  (with-parse-document (doc obj)
    (parse-stylesheet doc)))

;;; with-stylesheet

(defmacro with-stylesheet ((style obj) &rest body)
  `(let ((,style (parse-stylesheet ,obj)))
     (unwind-protect
          (progn ,@body)
       (release ,style))))

;;; transform

(defun transform (style doc &rest args)
  (declare (ignore args))
  (make-instance 'document
                 :pointer (%xsltApplyStylesheet (pointer style)
                                                (pointer doc)
                                                (null-pointer))))

(defmacro with-transfom-result ((res (style doc)) &rest body)
  `(let ((,res (transform ,style ,doc)))
     (unwind-protect
          (progn ,@body)
       (release ,res))))