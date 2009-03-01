;; stylesheet.lisp

(in-package #:libxml2.xslt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stylesheet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass stylesheet (libxml2.tree::libxml2-cffi-object-wrapper)
  ((params :initform nil)))

(define-libxml2-function ("xsltFreeStylesheet" %xsltFreeStylesheet) :void
  (style %xsltStylesheetPtr))

(defmethod libxml2.tree::release/impl ((style stylesheet))
  (%xsltFreeStylesheet  (pointer style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylesheet-set-param
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stylesheet-set-param (style name value &optional (isstring t))
  (unless (slot-value style 'params)
    (setf (slot-value style 'params) (make-hash-table :test 'equal)))
  (setf (gethash name (slot-value style 'params))
        (if isstring
            (format nil "\"~A\"" value)
            value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylesheet-remove-param
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stylesheet-remove-param (style name)
  (let ((params (slot-value style 'params)))
    (if params
        (remhash name params))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylesheet-clear-params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stylesheet-clear-params (style)
  (setf (slot-value style 'params) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse-stylesheet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-stylesheet (obj)
  (let ((%style (parse-stylesheet/impl obj)))
    (if %style
        (make-instance 'stylesheet :pointer %style)
        (error "This is not stylesheet"))))


(defgeneric parse-stylesheet/impl (obj))

;;; parse-stylehseet ((filename pathname))

(define-libxml2-function ("xsltParseStylesheetFile" %xsltParseStylesheetFile) %xsltStylesheetPtr
  (filename libxml2.tree::%xmlCharPtr))

(defmethod parse-stylesheet/impl ((filename pathname))
  (with-foreign-string (%filename (format nil "~A" filename))
    (%xsltParseStylesheetFile %filename)))

;;; parse-stylesheet ((doc document))

(define-libxml2-function ("xsltParseStylesheetDoc" %xsltParseStylesheetDoc) %xsltStylesheetPtr
  (doc libxml2.tree::%xmlDocPtr))

(defmethod parse-stylesheet/impl ((doc document))
  (%xsltParseStylesheetDoc (pointer doc)))

;;; parse-stylesheet (obj)

(defmethod parse-stylesheet/impl (obj)
  (gp:with-garbage-pool ()
    (let* ((doc (gp:object-register (parse obj)))
           (%style (parse-stylesheet/impl (parse obj))))
      (unless (null-pointer-p %style)
        (progn (gp:cancel-object-cleanup doc)
               %style)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-stylesheet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-stylesheet ((style obj) &rest body)
  `(let ((,style (parse-stylesheet ,obj)))
     (unwind-protect
          (progn ,@body)
       (release ,style))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defxsl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defxsl (var src)
  (if (boundp (quote var))
      (xtree:release var))
  `(defparameter ,var (xslt:parse-stylesheet ,src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-xsl-params (params)
  (if params
      (let* ((array-length (1+ (* 2 (hash-table-count params))))
             (%array (gp:cleanup-register (foreign-alloc :pointer
                                                         :count array-length
                                                         :initial-element (null-pointer))
                                          #'foreign-free)))
        (iter (for (name value) in-hashtable params)
              (for i upfrom 0 by 2)
              (setf (mem-aref %array :pointer i)
                    (gp:cleanup-register (foreign-string-alloc name) #'foreign-string-free))
              (setf (mem-aref %array :pointer (1+ i))
                    (gp:cleanup-register (foreign-string-alloc value) #'foreign-string-free)))
        %array)
      (null-pointer)))
    
(define-libxml2-function ("xsltApplyStylesheetUser" %xsltApplyStylesheetUser) libxml2.tree::%xmlDocPtr
  (style %xsltStylesheetPtr)
  (doc libxml2.tree::%xmlDocPtr)
  (args :pointer)
  (output :pointer)
  (profile :pointer)
  (userCtxt %xsltTransformContextPtr))

(defgeneric transform (style obj))

;;; transform (style (doc document))

(defmethod transform (style (doc document))
  (gp:with-garbage-pool ()  
    (with-transform-context (%ctxt (style doc))
      (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (%xsltApplyStylesheetUser (pointer style)
                                                                                     (pointer doc)
                                                                                     (prepare-xsl-params (slot-value style 'params))
                                                                                     (null-pointer)
                                                                                     (null-pointer)
                                                                                     %ctxt)
                                                           'document))))
;;; transform (style (el node))

(defmethod transform (style (el node))
  (with-fake-document (doc el)
    (transform style doc)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-tranform-result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-transform-result ((res (style doc)) &rest body)
  `(let ((,res (transform ,style ,doc)))
     (unwind-protect
          (progn ,@body)
       (release ,res))))

