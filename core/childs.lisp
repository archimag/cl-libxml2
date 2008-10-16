;; childs.lisp

(in-package #:libxml2)

(defun node-type (node)
  (wrapper-slot-value node '%type))
      
(defun local-name (node)
  (cffi:foreign-string-to-lisp
   (wrapper-slot-value node '%name)))

(defun namespace-uri (node)
   (let ((%ns (wrapper-slot-value node '%ns)))
     (unless (null-pointer-p %ns)
       (cffi:foreign-string-to-lisp (cffi:foreign-slot-value %ns '%xmlNs '%href)))))

(defun namespace-prefix (node)
   (let ((ns (wrapper-slot-wrapper node '%ns 'ns)))
     (if ns (cffi:foreign-string-to-lisp
              (wrapper-slot-value ns '%prefix)))))

(defgeneric root (obj))

(defmethod root ((doc document))
  (wrapper-slot-node doc '%children))

(defmethod root ((node node))
  (let ((doc (document node)))
    (if doc
        (root doc)
        (let ((parent (parent node)))
          (if parent
              (root parent)
              node)))))

(defun next-sibling (node)
  (wrapper-slot-node node '%next))

(defun prev-sibling (node)
  (wrapper-slot-node node '%prev))

(defun first-child (node)
  (wrapper-slot-node node '%children))

(defun last-child (node)
  (wrapper-slot-node node '%last))

(defun parent (node)
  (wrapper-slot-node node '%parent))

(defun document (node)
  (wrapper-slot-wrapper node '%doc 'document))

(defun text-content (node)
  (cffi:foreign-string-to-lisp
   (wrapper-slot-value node '%content)))

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

(defun process-xinclude (node)
  (%xmlXincludeProcessTree node))
