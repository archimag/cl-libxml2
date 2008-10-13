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


(defun local-name (node)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value (impl node)
                            '%xmlnode
                            '%name)))

(defun next-sibling (node)
  (slot-node node '%next))n

(defun prev-sibling (node)
  (slot-node node '%prev))

(defun first-child (node)
  (slot-node node '%children))

(defun last-child (node)
  (slot-node node '%last))

(defun parent (node)
  (slot-node node '%parent))

(defun text-content (node)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value (impl node)
                            '%xmlnode
                            '%content)))

(defmacro node-filter (&key type local-name)
  (if (or type local-name)
      `(lambda (node)
         (and (or (not ,type)
                  (eql (node-type node) ,type))
              (or (not ,local-name)
                  (string= (local-name node) ,local-name))))))

(defun find-node (first filter)
  (if first
    (if filter
        (if (funcall filter first)
            first
            (find-node (next-sibling first) filter))
        first)))

(defmacro-driver (for var in-child-nodes node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (initially (setq ,var (first-child ,node)))
     (,kwd ,var next (find-node (next-sibling ,var)
                                (node-filter ,@filter)))
     (while ,var))))
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; attribute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun attribute-value (element name &optional uri)
  (declare (ignore uri))
  (foreign-string-to-lisp (with-foreign-string (%name name)
                            (%xmlGetProp (impl element) %name))))
    
  

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
