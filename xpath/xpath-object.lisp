;; xpath-object.lisp

(in-package #:libxml2.xpath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xpath-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcenum %xmlXPathObjectType
  (:xpath-undefined  0)
  (:xpath-nodeset  1)
  (:xpath-boolean  2)
  (:xpath-number  3)
  (:xpath-string  4)
  (:xpath-point  5)
  (:xpath-range  6)
  (:xpath-locationset  7)
  (:xpath-users  8)
  (:xpath-xslt-tree 9))

(defcstruct %xmlXPathObject 
  (%type %xmlXPathObjectType)
  (%nodesetval %xmlNodeSetPtr)
  (%boolval :int)
  (%floatval :double)
  (%stringval %xmlCharPtr)
  (%user :pointer)
  (%index :int)
  (%user2 :pointer)
  (%index2 :int))

(defctype %xmlXPathObjectPtr :pointer)

(defwrapper xpath-object %xmlXPathObject)

(defcfun ("xmlXPathFreeObject" %xmlXPathFreeObject) :void
  (obj %xmlXPathObjectPtr))

(defmethod libxml2.tree::release/impl ((result xpath-object))
  (%xmlXPathFreeObject (pointer result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xpath-object-type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xpath-object-type (res)
  (wrapper-slot-value res '%type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xpath-object-value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xpath-object-value (res)  
  (case (xpath-object-type res)
    (:xpath-undefined nil)
    (:xpath-boolean (> (wrapper-slot-value res '%boolval) 0))
    (:xpath-number  (wrapper-slot-value res '%floatval))
    (:xpath-string (unless (= 0 (mem-ref (wrapper-slot-value res '%stringval) :uchar))
                     (foreign-string-to-lisp (wrapper-slot-value res '%stringval))))
    (:xpath-nodeset (libxml2.tree::wrapper-slot-wrapper res '%nodesetval 'node-set))
    (otherwise nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-xpath-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-xpath-object ((res (obj expr &optional (ns-map '*default-ns-map*))) &rest body)
  `(let ((,res (eval-expression ,obj ,expr :ns-map ,ns-map)))
     (unwind-protect
          (progn ,@body)
       (if ,res (release ,res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-xpath-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric make-xpath-object (obj))

;;; make-xpath-object ((val number))

(defcfun ("xmlXPathNewFloat" %xmlXPathNewFloat) %xmlXPathObjectPtr
  (val :double))

(defmethod make-xpath-object ((val number))
  (%xmlXPathNewFloat (coerce val 'double-float)))

;;;make-xpath-object ((val string))

(defcfun ("xmlXPathNewString" %xmlXPathNewString) %xmlXPathObjectPtr
  (val %xmlCharPtr))

(defmethod make-xpath-object ((val string))
  (with-foreign-string (%str val)
    (%xmlXPathNewString %str)))

;;; make-xpath-object ((val node))

(defcfun ("xmlXPathNewNodeSet" %xmlXPathNewNodeSet) %xmlXPathObjectPtr
  (val %xmlNodePtr))

(defmethod make-xpath-object ((val node))
  (%xmlXPathNewNodeSet (pointer val)))

;;; make-xpath-object ((val node-set))

(defcfun ("xmlXPathNewNodeSetList" %xmlXPathNewNodeSetList) %xmlXPathObjectPtr
  (val %xmlNodeSetPtr))

(defmethod make-xpath-object ((val node-set))
  (%xmlXPathNewNodeSetList (pointer val)))

;;; make-xpath-object (val)

(defcfun ("xmlXPathNewBoolean" %xmlXPathNewBoolean) %xmlXPathObjectPtr
  (val :int))

(defmethod make-xpath-object (val)
  (%xmlXPathNewBoolean (if val 1 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xpath-object-cast
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric xpath-object-cast (obj type))

;;; def-xo-cast

(defmacro def-xo-cast ((type xpath-type) &body impl)
  `(defmethod xpath-object-cast (obj (tp (eql (quote ,type))))
     (if (eql (xpath-object-type obj) ,xpath-type)
         (xpath-object-value obj)
         (unless (eql (xpath-object-type obj) :xpath-undefined)
           ,@impl))))

;;; xpath-object-cast (obj (type (eql 'string)))

(defcfun ("xmlXPathCastToString" %xmlXPathCastToString) %xmlCharPtr
  (val %xmlXPathObjectPtr))

(def-xo-cast (string :xpath-string)
  (let ((%str (%xmlXPathCastToString (pointer obj))))
    (unwind-protect
         (let ((str (foreign-string-to-lisp %str)))
           (unless (string= str "") str))
      (libxml2.tree::%xmlFree %str))))

;;; xpath-object-cast (obj (type (eql 'number)))

(defcfun ("xmlXPathCastToNumber" %xmlXPathCastToNumber) :double
  (val %xmlXPathObjectPtr))

(def-xo-cast (number :xpath-number)
  (%xmlXPathCastToNumber (pointer obj)))

;;; xpath-object-cast (obj (type (eql 'boolean)))

(defcfun ("xmlXPathCastToBoolean" %xmlXPathCastToBoolean) :int
    (val %xmlXPathObjectPtr))

(def-xo-cast (boolean :xpath-boolean)
  (equal 1 (%xmlXPathCastToBoolean (pointer obj))))
        