;; parse.lisp

(in-package #:libxml2.tree)


(defgeneric parse (obj)
  (:documentation "parse xml"))

;;; parse ((path pathname))

(defmethod parse ((path pathname))
  (with-foreign-string (_path (format nil "~A" path))
    (make-instance 'document
                   :pointer (%xmlReadFile _path (cffi:null-pointer) 0))))

;;; parse ((str string))

(defmethod parse ((str string))
  (with-foreign-string (%str str)
    (make-instance 'document
                   :pointer (%xmlReadDoc %str
                                         (null-pointer)
                                         (null-pointer)
                                         0))))

;;; parse ((uri puri))

(defmethod parse ((uri puri:uri))
  (with-foreign-string (_path (format nil "~A" uri))
    (make-instance 'document
                   :pointer (%xmlReadFile _path (cffi:null-pointer) 0))))
