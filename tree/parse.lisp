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


(defvar *stream-for-xml-parse*)

;;; parse ((octets (array unsigned-byte)))

(defmethod parse ((octets array))
  (flexi-streams:with-input-from-sequence (in octets)
    (parse in)))

;;; parse ((stream stream))

(defcallback %read-binary-stream :int ((context :pointer) (buffer :pointer) (len :int))
  (declare (ignore context))
  (if (and *stream-for-xml-parse*
           (input-stream-p *stream-for-xml-parse*))
      (iter (for pos from 0 below len)
            (for byte next (read-byte *stream-for-xml-parse* nil))
            (while byte)
            (setf (cffi:mem-ref buffer :uchar pos) byte)
            (finally (return pos)))
      -1))

(defcallback %read-string-stream :int ((context :pointer) (buffer :pointer) (len :int))
  (declare (ignore context))
  (if *stream-for-xml-parse*
      (let ((curpos 0))
        (iter (for ch next (read-char *stream-for-xml-parse* nil))
              (while ch)
              (for octets = (flex:string-to-octets (make-string 1 :initial-element ch) :external-format :utf-8))
              (if (< (+ (length octets)
                        curpos)
                     len)
                  (iter (for byte in-vector octets)
                        (setf (cffi:mem-ref buffer :uchar curpos) byte)
                        (incf curpos))
                  (progn
                    (unread-char ch *stream-for-xml-parse*)
                    (finish))))
        curpos)
      -1))
          
(defmethod parse ((stream stream))
  (let ((*stream-for-xml-parse* stream))
    (make-instance 'document
                   :pointer (%xmlReadIO (if (subtypep (stream-element-type *stream-for-xml-parse*)
                                                      'character)
                                            (cffi:callback %read-string-stream)
                                            (cffi:callback %read-binary-stream))
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        0))))
                                            