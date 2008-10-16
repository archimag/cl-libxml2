;; core.lisp 

(in-package #:libxml2)


(defun pointer-or-nil-if-null (ptr)
  (unless (null-pointer-p ptr) ptr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node-type (node)
  (cffi:foreign-slot-value node '%xmlnode '%type))

(defun slot-node (node slot)
  (let ((ptr (cffi:foreign-slot-value node '%xmlnode slot)))
    (unless (cffi:null-pointer-p ptr) ptr)))
      
(defun local-name (node)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value node '%xmlnode '%name)))

(defun namespace-uri (node)
   (let ((%ns (slot-node node '%ns)))
     (if %ns (cffi:foreign-string-to-lisp
              (cffi:foreign-slot-value %ns '%xmlNs '%href)))))

(defun namespace-prefix (node)
   (let ((%ns (slot-node node '%ns)))
     (if %ns (cffi:foreign-string-to-lisp
              (cffi:foreign-slot-value %ns '%xmlNs '%prefix)))))

(defun next-sibling (node)
  (slot-node node '%next))

(defun prev-sibling (node)
  (slot-node node '%prev))

(defun first-child (node)
  (slot-node node '%children))

(defun last-child (node)
  (slot-node node '%last))

(defun parent (node)
  (slot-node node '%parent))

(defun document (node)
  (slot-node node '%doc))

(defun text-content (node)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value node '%xmlnode '%content)))

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
  (foreign-string-to-lisp 
   (with-foreign-string (%name name)
     (if uri
         (with-foreign-string (%uri uri)
           (%xmlGetNsProp element %name %uri))
         (%xmlGetProp element %name)))))

(defun generate-ns-prefix (element)
  (iter (for i from 1)
        (for prefix = (format nil "ns_~A" i))
        (finding prefix such-that (null-pointer-p (with-foreign-string (%prefix prefix)
                                                    (%xmlSearchNs (document element)
                                                                  element
                                                                  %prefix))))))
                                           
(defun set-attribute-value (element name &optional uri value)
  (with-foreign-strings ((%name name) (%value (or value uri)))
    (if value
        (let ((%ns (with-foreign-string (%href uri)
                     (or (pointer-or-nil-if-null (%xmlSearchNsByHref (document element)
                                                                     element
                                                                     %href))
                         (with-foreign-string (%prefix (generate-ns-prefix element))
                           (%xmlNewNs element
                                      %href
                                      %prefix))))))
          (%xmlSetNsProp element %ns %name %value))
        (%xmlSetProp element %name %value)))
  (or value uri))

(defsetf attribute-value set-attribute-value)


(defun remove-attribute (element name &optional uri)
  (let ((%attr (with-foreign-string (%name name)
                 (if uri
                     (%xmlHasProp element %name)
                     (with-foreign-string (%uri uri)
                       (%xmlHasNsProp element %name %uri))))))
    (print (format nil "attr is ~A" (not (null-pointer-p %attr))))
    (unless (null-pointer-p %attr)
      (= 0 (%xmlRemoveProp %attr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun document-free (doc)
  (%xmlFreeDoc doc))

(defun document-root-element (doc)
  (%xmlDocGetRootElement doc))

(defun document-save (doc filename)
  (with-foreign-string (%path (format nil "~A" filename))
    (%xmlSaveFile %path doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse (obj)
  (:documentation "parse xml"))

(defmethod parse ((path pathname))
  (with-foreign-string (_path (format nil "~A" path))
    (%xmlReadFile _path (cffi:null-pointer) 0)))

(defmethod parse ((str string))
  (with-foreign-strings ((%buffer str) (%utf-8 "utf-8") (%ns "http://www.sample.org"))
    (%xmlReadMemory %buffer
                    (print (cffi::foreign-string-length %buffer))
                    %ns
                    %utf-8
                    1)))

(defmacro with-document ((var src) &rest body)
  `(let ((,var  ,src))
     (unwind-protect
          (progn ,@body)
       (document-free ,var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xinclude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defgeneric process-xinclude (obj))

;; (defmethod process-xinclude ((node node))
;;   (%xmlXincludeProcessTree node))

;; (defmethod process-xinclude ((doc document))
;;   (process-xinclude (document-root-element doc)))

(defun process-xinclude (node)
  (%xmlXincludeProcessTree node))
