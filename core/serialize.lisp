;; serialize.lisp

(in-package #:libxml2.tree)

(defgeneric serialize (obj target))

(defmethod serialize ((doc document) (filename pathname))
  (with-foreign-string (%path (format nil "~A" filename))
    (%xmlSaveFile %path (pointer doc))))
