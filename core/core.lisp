;; core.lisp 

(in-package #:libxml2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass node ()
  ((impl :initarg :impl :initform nil :reader impl)))

(defun node-type (node)
  (cffi:foreign-slot-value (impl node)
                           '%xmlnode
                           '%type))

(defun slot-node (node slot)
  (let ((ptr (cffi:foreign-slot-value (impl node)
                                      '%xmlnode
                                      slot)))
    (unless (cffi:null-pointer-p ptr)
      (make-instance 'node
                     :impl ptr))))


(defun node-tag (node)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value (impl node)
                            '%xmlnode
                            '%name)))

(defun node-next (node)
  (slot-node node '%next))n

(defun node-prev (node)
  (slot-node node '%prev))

(defun node-first (node)
  (slot-node node '%children))

(defun node-last (node)
  (slot-node node '%last))

(defun node-parent (node)
  (slot-node node '%parent))

(defun node-text-content (node)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value (impl node)
                            '%xmlnode
                            '%content)))

(defmacro node-filter (&key type tag)
  (if (or type tag)
      `(lambda (node)
         (and (or (not ,type)
                  (eql (node-type node) ,type))
              (or (not ,tag)
                  (string= (node-tag node) ,tag))))))

(defun find-node (first filter)
  (if first
    (if filter
        (if (funcall filter first)
            first
            (find-node (node-next first) filter))
        first)))

(defmacro-driver (for var in-child-nodes node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (initially (setq ,var (node-first ,node)))
     (,kwd ,var next (find-node (node-next ,var)
                                (node-filter ,@filter)))
     (while ,var))))
                     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass document ()
  ((impl :initarg :impl :initform nil :accessor impl)))

(defun document-free (doc)
  (%xmlFreeDoc (impl doc)))

(defun document-root-element (doc)
  (make-instance 'node
                 :impl (%xmlDocGetRootElement (impl doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse (obj)
  (:documentation "parse xml"))

(defmethod parse ((path pathname))
  (with-foreign-string (_path (format nil "~A" path))
    (make-instance 'document
                   :impl (%xmlReadFile _path
                                      (cffi:null-pointer)
                                      0))))

(defmacro with-document ((var src) &rest body)
  `(let ((,var  ,src))
     (unwind-protect
          (progn ,@body)
       (document-free ,var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xinclude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric process-xinclude (obj))

(defmethod process-xinclude ((node node))
  (%xmlXincludeProcessTree (impl node)))

(defmethod process-xinclude ((doc document))
  (process-xinclude (document-root-element doc)))
