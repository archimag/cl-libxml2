;; childs.lisp

(in-package #:libxml2.tree)

;;; node-type

(defun node-type (node)
  (wrapper-slot-value node '%type))

;;; local-name

(defun local-name (node)
  (cffi:foreign-string-to-lisp
   (wrapper-slot-value node '%name)))

;;; namespace-uri

(defun namespace-uri (node)
   (let ((%ns (wrapper-slot-value node '%ns)))
     (unless (null-pointer-p %ns)
       (cffi:foreign-string-to-lisp (cffi:foreign-slot-value %ns '%xmlNs '%href)))))

;;; namespace-prefix

(defun namespace-prefix (node)
   (let ((ns (wrapper-slot-wrapper node '%ns 'ns)))
     (if ns (cffi:foreign-string-to-lisp
              (wrapper-slot-value ns '%prefix)))))


;;; base-url

(defgeneric base-url (obj))

(defmethod base-url ((doc document))
  (let ((%str (wrapper-slot-value doc '%url)))
    (unless (null-pointer-p %str)
      (puri:parse-uri (foreign-string-to-lisp %str)))))

(defmethod base-url ((node node))
  (let ((%str (%xmlNodeGetBase (pointer (document node))
                               (pointer node))))
    (unwind-protect
         (puri:parse-uri (foreign-string-to-lisp %str))
      (%xmlFree %str))))
                   

;;; root

(defgeneric root (obj))

(defmethod root ((doc document))
  (make-instance 'node :pointer (%xmlDocGetRootElement (pointer doc))))

(defmethod root ((node node))
  (let ((doc (document node)))
    (if doc
        (root doc)
        (let ((parent (parent node)))
          (if parent
              (root parent)
              node)))))

;;; next-sibling

(defun next-sibling (node)
  (wrapper-slot-node node '%next))

;;; prev-sibling

(defun prev-sibling (node)
  (wrapper-slot-node node '%prev))

;;; first-child

(defun first-child (node)
  (wrapper-slot-node node '%children))

;;; last-child

(defun last-child (node)
  (wrapper-slot-node node '%last))

;;; parent

(defun parent (node)
  (wrapper-slot-node node '%parent))

;;; document

(defun document (node)
  (wrapper-slot-wrapper node '%doc 'document))

;;; test-contetn

(defun text-content (node)
  (cffi:foreign-string-to-lisp
   (wrapper-slot-value node '%content)))

;;; process-xinclude

(defgeneric process-xinclude (obj))

(defmethod process-xinclude ((node node))
  (%xmlXincludeProcessTree (pointer node)))

(defmethod process-xinclude ((doc document))
  (process-xinclude (root doc)))

;;; FOR var IN-... WITH ()

(defun node-filter (&key type local-name ns filter)
  (if (or type local-name)
      (lambda (node)
        (and (or (not type)
                 (eql (node-type node) type))
             (or (not local-name)
                 (string= (local-name node) local-name))
             (or (not ns)
                 (string= (namespace-uri node) ns))
             (or (not filter)
                 (funcall filter node))))))

(defun find-node (first filter)
  (if first
    (if filter
        (if (funcall filter first)
            first
            (find-node (next-sibling first) filter))
        first)))


;;; FOR var IN-CHILD-NODES node WITH ()

(defmacro-driver (for var in-child-nodes node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node (first-child ,node) filter-fun) then (find-node (next-sibling ,var) filter-fun))
     (while ,var))))


;;; FOR var IN-NEXT-SIBLINGS node WITH ()

(defmacro-driver (for var in-next-siblings node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node (next-sibling ,node) filter-fun) then (find-node (next-sibling ,var) filter-fun))
     (while ,var))))

;;; FOR var IN-NEXT-SIBLINGS-FROM node WITH ()

(defmacro-driver (for var in-next-siblings-from node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node ,node filter-fun) then (find-node (next-sibling ,var) filter-fun))
     (while ,var))))

;;; FOR var IN-PREV-SIBLING node WITH ()

(defmacro-driver (for var in-prev-siblings node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node (prev-sibling ,node) filter-fun) then (find-node (prev-sibling ,var) filter-fun))
     (while ,var))))

;;; FOR var IN-PREV-SIBLING-FROM node WITH ()

(defmacro-driver (for var in-prev-siblings-from node &optional with filter)
  (let ((kwd (if generate 'generate 'for)))
  `(progn
     (with filter-fun = (node-filter ,@filter))
     (,kwd ,var first (find-node ,node filter-fun) then (find-node (prev-sibling ,var) filter-fun))
     (while ,var))))



