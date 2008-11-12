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

;;; parse ((stream stream))

(defcallback %read-binary-stream :int ((context :pointer) (buffer :pointer) (len :int))
  (declare (ignore context))
  (if *stream-for-xml-parse*
      (let ((arr (make-array len :initial-element nil))
            (length nil))
        (read-sequence arr *stream-for-xml-parse*)
        (setq length (or (position nil arr) len))
        (iter (for pos from 0 below length)
              (setf (cffi:mem-ref buffer :uchar pos)
                    (aref arr pos)))
        length)
      -1))


(defmethod parse ((stream stream))
  (let ((*stream-for-xml-parse* stream))
    (make-instance 'document
                   :pointer (%xmlReadIO (cffi:callback %read-binary-stream)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        0))))
                                        
(defcallback %read-string-stream :int ((context :pointer) (buffer :pointer) (len :int))
  (declare (ignore context))
  (if *stream-for-xml-parse*
      (let ((curpos 0))
        (iter (for ch next (handler-case
                               (read-char *stream-for-xml-parse*)
                             (end-of-file nil)))
              (while ch)
              (for octets = (flexi-streams:string-to-octets (make-array 1 :initial-element ch) :external-format :utf-8))
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

(defmethod parse ((stream string-stream))
  (let ((*stream-for-xml-parse* stream))
    (make-instance 'document
                   :pointer (%xmlReadIO (cffi:callback %read-string-stream)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        0))))
    