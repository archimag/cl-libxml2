;; objects.lisp

(in-package #:libxml2.tree)

(defclass libxml2-cffi-object-wrapper ()
  ((pointer :initarg :pointer :reader pointer)))

(defgeneric wrapper-slot-value (obj slot))

;;(defgeneric set-wrapper-slot-value (obj slot value))

(defgeneric release/impl (obj))
(defgeneric copy (obj))
(defgeneric (setf wrapper-slot-value) (value obj slot))


(defmethod pointer ((obj (eql nil)))
  (null-pointer))

(defmacro defwrapper (wrapper-name cffi-type)
  `(progn
     (defclass ,wrapper-name (libxml2-cffi-object-wrapper) ())
     (defmethod wrapper-slot-value ((obj ,wrapper-name) slot)
       (cffi:foreign-slot-value (pointer obj) (quote ,cffi-type) slot))
     (defmethod (setf wrapper-slot-value) (value (obj ,wrapper-name) slot)
       (setf (cffi:foreign-slot-value (pointer obj) (quote ,cffi-type) slot) value))))


(defmacro with-libxml2-object ((var value) &rest body)
  `(unwind-protect
        (let ((,var ,value))
          ,@body)
     (if ,value (release ,value))))

(defun release (obj)
  (release/impl obj)
  (setf (slot-value obj 'pointer) nil))

(gp:defcleanup libxml2-cffi-object-wrapper #'release)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-libxml2-cffi-object-wrapper/impl (%ptr wrapper-type)
  (unless (null-pointer-p %ptr)
    (make-instance wrapper-type :pointer %ptr)))

(defun wrapper-slot-wrapper (obj slot wrapper-type)
  (make-libxml2-cffi-object-wrapper/impl (wrapper-slot-value obj slot) wrapper-type))
