;; serialize.lisp

(in-package #:libxml2.tree)

(defgeneric serialize (obj target))

;;; serialize ((doc document) (filename pathname))

(defmethod serialize ((doc document) (filename pathname))
  (with-foreign-string (%path (format nil "~A" filename))
    (%xmlSaveFile %path (pointer doc))))

;;; serialize ((doc document) (s (eql :to-string)))

(defmethod serialize ((doc document) (s (eql :to-string)))
  (with-foreign-string (%utf-8 "utf-8")
    (with-foreign-pointer (%xml-string (foreign-type-size :pointer))
      (with-foreign-pointer (%xml-string-len (foreign-type-size :pointer))
        (%xmlDocDumpFormatMemoryEnc (pointer doc)
                                    %xml-string
                                    %xml-string-len
                                    %utf-8 ;;(null-pointer)
                                    1)
        (let ((%ptr (mem-ref %xml-string :pointer)))
          (unwind-protect 
               (foreign-string-to-lisp %ptr)
            (%xmlFree %ptr)))))))


;;; serialize ((el node) target)

(defmethod serialize ((el node) target)
  (element-p el :throw-error t)
  (let ((%doc (%xmlNewDoc (null-pointer))))
    (unwind-protect
         (progn 
           (setf (foreign-slot-value %doc '%xmlDoc '%children) (pointer el))
           (setf (foreign-slot-value %doc '%xmlDoc '%last) (pointer el))
           (serialize (make-instance 'document
                                     :pointer %doc)
                      target))
      (progn
        (setf (foreign-slot-value %doc '%xmlDoc '%children) (null-pointer))
        (setf (foreign-slot-value %doc '%xmlDoc '%last) (null-pointer))
        (%xmlFreeDoc %doc)))))