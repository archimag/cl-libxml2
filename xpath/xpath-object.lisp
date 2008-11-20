;; xpath-object.lisp

(in-package #:libxml2.xpath)

;;; xpath-object

(defwrapper xpath-object %xmlXPathObject)

(defmethod libxml2.tree::release/impl ((result xpath-object))
  (%xmlXPathFreeObject (pointer result)))

(defun xpath-object-type (res)
  (wrapper-slot-value res '%type))

(defun xpath-object-value (res)  
  (case (xpath-object-type res)
    (:xpath-undefined nil)
    (:xpath-boolean (> (wrapper-slot-value res '%boolval) 0))
    (:xpath-number  (wrapper-slot-value res '%floatval))
    (:xpath-string (foreign-string-to-lisp (wrapper-slot-value res '%stringval)))
    (:xpath-nodeset (libxml2.tree::wrapper-slot-wrapper res '%nodesetval 'node-set))
    (otherwise nil)))

;;; with-xpath-object

(defmacro with-xpath-object ((res (obj expr &optional (ns-map '*default-ns-map*))) &rest body)
  `(let ((,res (eval-expression ,obj ,expr :ns-map ,ns-map)))
     (unwind-protect
          (progn ,@body)
       (if ,res (release ,res)))))

;;; make-xpath-object

(defgeneric make-xpath-object (obj))

(defmethod make-xpath-object ((val number))
  (%xmlXPathNewFloat (coerce val 'double-float)))

(defmethod make-xpath-object ((val string))
  (with-foreign-string (%str val)
    (%xmlXPathNewString %str)))

(defmethod make-xpath-object ((val node))
  (%xmlXPathNewNodeSet (pointer val)))

(defmethod make-xpath-object ((val node-set))
  (%xmlXPathNewNodeSetList (pointer val)))

(defmethod make-xpath-object (val)
  (%xmlXPathNewBoolean (if val 1 0)))
