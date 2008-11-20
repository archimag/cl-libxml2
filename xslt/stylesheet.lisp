;; stylesheet.lisp

(in-package #:libxml2.xslt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define and load libxslt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library libxslt
  (:unix (:or "libxslt.so"))
  (t (:default "libxslt")))

(define-foreign-library libexslt
  (:unix (:or "libexslt.so"))
  (t (:default "libexslt")))


(with-simple-restart (skip "Skip loading foreign library libxslt.")
  (use-foreign-library libxslt)
  (use-foreign-library libexslt))

(defcfun ("xsltInit" %xsltInit) :void)
(defcfun ("exsltRegisterAll" %exsltRegisterAll) :void)

(%xsltInit)
(%exsltRegisterAll)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defctype %xsltStylesheetPtr :pointer)
(defctype %xsltTransformContextPtr :pointer)

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

(defcfun ("xsltNewTransformContext" %xsltNewTransformContext) %xsltTransformContextPtr
  (style %xsltStylesheetPtr)
  (doc libxml2.tree::%xmlDocPtr))

(defcfun ("xsltFreeTransformContext" %xsltFreeTransformContext) :void
  (ctxt %xsltTransformContextPtr))

(defcfun ("xsltApplyStylesheetUser" %xsltApplyStylesheetUser) libxml2.tree::%xmlDocPtr
  (style %xsltStylesheetPtr)
  (doc libxml2.tree::%xmlDocPtr)
  (args :pointer)
  (output :pointer)
  (profile :pointer)
  (userCtxt %xsltTransformContextPtr))

(defcfun ("xsltRegisterExtFunction" %xsltRegisterExtFunction) :int
  (ctxt %xsltTransformContextPtr)
  (name %xmlCharPtr)
  (URI %xmlCharPtr)
  (function :pointer))

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
  (gp:with-garbage-pool ()  
    (let* ((params (slot-value style 'params))
           (array-length)
           (%array (null-pointer))
           (%ctxt (null-pointer)))
      (if params          
          (progn
            (setq array-length
                  (1+ (* 2 (hash-table-count params))))
            (setq %array
                  (gp:cleanup-register (foreign-alloc :pointer :count array-length) #'foreign-free))
            (iter (for (name value) in-hashtable params)
                  (for i upfrom 0 by 2)
                  (setf (mem-aref %array :pointer i)
                        (gp:cleanup-register (foreign-string-alloc name) #'foreign-string-free))
                  (setf (mem-aref %array :pointer (1+ i))
                        (gp:cleanup-register (foreign-string-alloc value) #'foreign-string-free)))
            (setf (mem-aref %array :pointer (1- array-length))
                  (null-pointer))))
      (if (boundp 'libxml2.xpath::*lisp-xpath-functions*)
          (progn
            (setq %ctxt
                  (%xsltNewTransformContext (pointer style)
                                            (pointer doc)))
            (iter (for (name func &key ns) in libxml2.xpath::*lisp-xpath-functions*)
                  (%xsltRegisterExtFunction %ctxt
                                            (gp:cleanup-register (foreign-string-alloc (eval name))
                                                                 #'foreign-string-free)
                                            (if ns
                                                (gp:cleanup-register (foreign-string-alloc (eval ns))
                                                                     #'foreign-string-free)
                                                (null-pointer))
                                            (get-callback func)))))
      (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (%xsltApplyStylesheetUser (pointer style)
                                                                                     (pointer doc)
                                                                                     %array
                                                                                     (null-pointer)
                                                                                     (null-pointer)
                                                                                     %ctxt)
                                                           'document))))

(defmacro with-transfom-result ((res (style doc)) &rest body)
  `(let ((,res (transform ,style ,doc)))
     (unwind-protect
          (progn ,@body)
       (release ,res))))