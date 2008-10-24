;; objects.lisp

(in-package #:libxml2.tree)

(defclass libxml2-cffi-object-wrapper ()
  ((pointer :initarg :pointer :reader pointer)))

(defgeneric wrapper-slot-value (obj slot))

(defgeneric release (obj) )
(defgeneric copy (obj))


(defmacro defwrapper (wrapper-name cffi-type)
  `(progn
     (defclass ,wrapper-name (libxml2-cffi-object-wrapper) ())
     (defmethod wrapper-slot-value ((obj ,wrapper-name) slot)
       (cffi:foreign-slot-value (pointer obj) (quote ,cffi-type) slot))))

(defmacro with-libxml2-object ((var value) &rest body)
  `(let ((,var ,value))
     (unwind-protect
          (progn ,@body)
       (release ,var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper node %xmlNode)

(defmethod release ((node node))
  (%xmlFreeNode (pointer node)))

(defmethod copy ((node node))
  (make-instance 'node
                 :pointer (%xmlCopyNode (pointer node) 1)))

(defun wrapper-slot-node (node slot)
  (wrapper-slot-wrapper node slot 'node))

(defun make-element (name &optional href prefix)
  (let ((%node (with-foreign-string (%name name)
                 (%xmlNewNode (null-pointer) 
                              %name))))
    (if href
        (let ((%ns (if prefix
                       (with-foreign-strings ((%href href) (%prefix prefix))
                         (%xmlNewNs %node
                                    %href
                                    %prefix))
                       (with-foreign-string (%href href)
                         (%xmlNewNs %node
                                    %href
                                    (null-pointer))))))
          (setf (foreign-slot-value %node
                                    '%xmlNode
                                    '%ns)
                %ns)))
    (make-instance 'node
                   :pointer %node)))
                
                
(defun make-text (data)
  (make-instance 'node
                 :pointer (with-foreign-string (%data data)
                            (%xmlNewText %data))))

(defun make-comment (data)
  (make-instance 'node
                 :pointer (with-foreign-string (%data data)
                            (%xmlNewComment %data))))

(defun make-process-instruction (name content)
  (make-instance 'node
                 :pointer (with-foreign-strings ((%name name) (%content content))
                            (%xmlNewPI %name %content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper document %xmlDoc)

(defmethod release ((doc document))
  (%xmlFreeDoc (pointer doc)))

(defmethod copy ((doc document))
  (make-instance 'document
                 :pointer (%xmlCopyDoc (pointer doc) 1)))

;; (defun make-document (name &optional href prefix)
;;   (let* ((%doc (%xmlNewDoc (null-pointer)))
;;          (%node (with-foreign-string (%name name)
;;                   (%xmlNewNode (null-pointer) %name)))
;;          (%ns (if href
;;                   (if prefix
;;                       (with-foreign-strings ((%href href) (%prefix prefix))
;;                         (%xmlNewNs %node %href %prefix))
;;                       (with-foreign-string (%href href)
;;                         (%xmlNewNs %node %href (null-pointer))))
;;                   (null-pointer))))
;;     (setf (foreign-slot-value %node '%xmlNode '%ns) %ns)
;;     (%xmlDocSetRootElement %doc %node)
;;     (make-instance 'document
;;                    :pointer %doc)))

(defun make-document (document-element)
  (let ((%doc (%xmlNewDoc (null-pointer))))
    (%xmlDocSetRootElement %doc (pointer document-element))
    (make-instance 'document
                   :pointer %doc)))


(defmacro with-parse-document ((var src) &rest body)
  `(with-libxml2-object (,var (parse ,src)) ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; attribute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper attribute %xmlAttr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper ns %xmlNs)

(defun generate-ns-prefix (element)
  (iter (for i from 1)
        (for prefix = (format nil "ns_~A" i))
        (finding prefix such-that (null-pointer-p (with-foreign-string (%prefix prefix)
                                                    (%xmlSearchNs (pointer (document element))
                                                                  (pointer element)
                                                                  %prefix))))))

(defun make-ns (element href &optional prefix)
  (make-instance 'ns
                 :pointer (with-foreign-strings ((%href href) (%prefix (or prefix (generate-ns-prefix element))))
                            (%xmlNewNs (pointer element)
                                       %href
                                       %prefix))))


(defun search-ns-by-href (element href)
  (let ((%ns (with-foreign-string (%href href)
               (%xmlSearchNsByHref (pointer (document element))
                                   (pointer element)
                                   %href))))
    (unless (null-pointer-p %ns)
      (make-instance 'ns :pointer %ns))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrapper-slot-wrapper (obj slot wrapper-type)
  (let ((ptr (wrapper-slot-value obj slot)))
    (unless (null-pointer-p ptr)
      (make-instance wrapper-type :pointer ptr))))
