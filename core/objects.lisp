;; objects.lisp

(in-package #:libxml2)

(defclass libxml2-cffi-object-wrapper ()
  ((pointer :initarg :pointer :reader pointer)))

(defgeneric wrapper-slot-value (obj slot))

(defgeneric release (obj) )


(defmacro defwrapper (wrapper-name cffi-type)
  `(progn
     (defclass ,wrapper-name (libxml2-cffi-object-wrapper) ())
     (defmethod wrapper-slot-value ((obj ,wrapper-name) slot)
       (cffi:foreign-slot-value (pointer obj) (quote ,cffi-type) slot))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper node %xmlNode)

(defun wrapper-slot-node (node slot)
  (wrapper-slot-wrapper node slot 'node))

(defun make-element (doc name &optional href)
  (let ((ns (if  href
                 (search-ns-by-href (root doc) href)
                 (wrapper-slot-wrapper (root doc) '%ns 'ns))))
    (print ns)
    (if ns
        (let ((%node (with-foreign-string (%name name)
                      (%xmlNewNode (pointer ns) %name))))
          (%xmlSetTreeDoc %node (pointer doc))
          (make-instance 'node :pointer %node))
        (let ((%node (with-foreign-string (%name name)
                      (%xmlNewNode (null-pointer) %name))))
          (%xmlSetTreeDoc %node (pointer doc))
          (if href
              (with-foreign-string (%href href)
                (%xmlNewNs %node %href (null-pointer))))
          (make-instance 'node :pointer %node)
          ))))
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper document %xmlDoc)

(defmethod release ((doc document))
  (%xmlFreeDoc (pointer doc)))

(defmacro with-document ((var src) &rest body)
  `(let ((,var  ,src))
     (unwind-protect
          (progn ,@body)
       (release ,var))))



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
  (with-foreign-strings ((%href href) (%prefix (or prefix (generate-ns-prefix element))))
    (%xmlNewNs (pointer element)
               %href
               %prefix)))

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
