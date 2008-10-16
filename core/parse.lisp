;; parse.lisp

(in-package #:libxml2)


(defgeneric parse (obj)
  (:documentation "parse xml"))

(defmethod parse ((path pathname))
  (with-foreign-string (_path (format nil "~A" path))
    (make-instance 'document
                   :pointer (%xmlReadFile _path (cffi:null-pointer) 0))))

(defmethod parse ((str string))
  (with-foreign-strings ((%buffer str))
    (make-instance 'document
                   :pointer (%xmlReadMemory %buffer
                                          (print (cffi::foreign-string-length %buffer))
                                          (null-pointer)
                                          (null-pointer)
                                          1))))
