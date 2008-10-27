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

(defclass stylesheet (libxml2.tree::libxml2-cffi-object-wrapper)
  ((params :initform nil)))

(defun stylesheet-set-param (style name value &optional (isstring t))
  (unless (slot-value style 'params)
    (setf (slot-value style 'params) (make-hash-table :test 'equal)))
  (setf (gethash name (slot-value style 'params))
        (if isstring
            (format nil "\"~A\"" value)
            value)))

(defun stylesheet-remove-param (style name)
  (let ((params (slot-value style 'params)))
    (if params
        (remhash name params))))

(defun stylesheet-clear-params (style)
  (setf (slot-value style 'params) nil))

(defmethod libxml2.tree::release/impl ((style stylesheet))
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

(defun transform (style doc)
  (let ((params (slot-value style 'params)))
    (if params
        (let ((array-length (1+ (* 2 (hash-table-count params)))))
          (with-foreign-object (%array :pointer array-length)
            (iter (for (name value) in-hashtable params)
                  (for i upfrom 0 by 2)
                  (setf (mem-aref %array :pointer i)
                        (foreign-string-alloc name))
                  (setf (mem-aref %array :pointer (1+ i))
                        (foreign-string-alloc value)))
            (setf (mem-aref %array :pointer (1- array-length))
                  (null-pointer))
            (unwind-protect
                 (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (%xsltApplyStylesheet (pointer style)
                                                                                            (pointer doc)
                                                                                            %array)
                                                                      'document)
              (iter (for i from 0 below (1- array-length))
                    (foreign-string-free (mem-aref %array :pointer i))))))
        (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (%xsltApplyStylesheet (pointer style)
                                                                                   (pointer doc)
                                                                                   (null-pointer))
                                                             'document))))

(defmacro with-transfom-result ((res (style doc)) &rest body)
  `(let ((,res (transform ,style ,doc)))
     (unwind-protect
          (progn ,@body)
       (release ,res))))