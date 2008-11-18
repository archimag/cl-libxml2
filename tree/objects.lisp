;; objects.lisp

(in-package #:libxml2.tree)

(defclass libxml2-cffi-object-wrapper ()
  ((pointer :initarg :pointer :reader pointer)))

(defgeneric wrapper-slot-value (obj slot))
(defgeneric set-wrapper-slot-value (obj slot value))

(defgeneric release (obj) )
(defgeneric release/impl (obj))
(defgeneric copy (obj))


(defmacro defwrapper (wrapper-name cffi-type)
  `(progn
     (defclass ,wrapper-name (libxml2-cffi-object-wrapper) ())
     (defmethod wrapper-slot-value ((obj ,wrapper-name) slot)
       (cffi:foreign-slot-value (pointer obj) (quote ,cffi-type) slot))
     (defmethod set-wrapper-slot-value ((obj ,wrapper-name) slot value)
       (setf (cffi:foreign-slot-value (pointer obj) (quote ,cffi-type) slot) value))))

(defsetf wrapper-slot-value set-wrapper-slot-value)

(defmacro with-libxml2-object ((var value) &rest body)
  `(unwind-protect
        (let ((,var ,value))
          ,@body)
     (if ,value (release ,value))))

(gp:defcleanup libxml2-cffi-object-wrapper #'release)

(defmethod release ((obj libxml2-cffi-object-wrapper))
  (release/impl obj)
  (setf (slot-value obj 'pointer) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper node %xmlNode)

(defmethod release/impl ((node node))
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
        (setf (foreign-slot-value %node
                                  '%xmlNode
                                  '%ns)
              (gp:with-garbage-pool ()
                (%xmlNewNs %node
                           (gp:cleanup-register (foreign-string-alloc href) #'foreign-string-free)
                           (if prefix
                               (gp:cleanup-register (foreign-string-alloc prefix)  #'foreign-string-free)
                               (null-pointer))))))
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

(defmethod release/impl ((doc document))
  (%xmlFreeDoc (pointer doc)))

(defmethod copy ((doc document))
  (make-instance 'document
                 :pointer (%xmlCopyDoc (pointer doc) 1)))

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

(defun make-libxml2-cffi-object-wrapper/impl (%ptr wrapper-type)
  (unless (null-pointer-p %ptr)
    (make-instance wrapper-type :pointer %ptr)))

(defun wrapper-slot-wrapper (obj slot wrapper-type)
  (make-libxml2-cffi-object-wrapper/impl (wrapper-slot-value obj slot) wrapper-type))
