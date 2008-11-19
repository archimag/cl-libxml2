;; parse.lisp

(in-package #:libxml2.tree)


(defvar *default-resolvers* nil)

(defgeneric parse (obj &key)
  (:documentation "parse xml"))


;;(defmethod parse :before (obj &key (resolvers *default-resolvers*)))

;;; parse ((path pathname))

(defmethod parse ((path pathname) &key)
  (with-foreign-string (_path (format nil "~A" path))
    (make-instance 'document
                   :pointer (%xmlReadFile _path (cffi:null-pointer) 0))))

;;; parse ((str string))

(defmethod parse ((str string)  &key)
  (with-foreign-string (%str str)
    (make-instance 'document
                   :pointer (%xmlReadDoc %str
                                         (null-pointer)
                                         (null-pointer)
                                         0))))

;;; parse ((uri puri))

(defmethod parse ((uri puri:uri)  &key)
  (with-foreign-string (_path (format nil "~A" uri))
    (make-instance 'document
                   :pointer (%xmlReadFile _path (cffi:null-pointer) 0))))


(defvar *stream-for-xml-parse*)

;;; parse ((octets (array unsigned-byte)))

(defmethod parse ((octets array) &key)
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

(defun %stream-reader-callback (stream)
  (if (subtypep (stream-element-type stream)
                'character)
      (cffi:callback %read-string-stream)
      (cffi:callback %read-binary-stream)))
          
(defmethod parse ((stream stream) &key)
  (let ((*stream-for-xml-parse* stream))
    (make-instance 'document
                   :pointer (%xmlReadIO (%stream-reader-callback *stream-for-xml-parse*)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)
                                        0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custor resolver support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *resolvers* nil)

(defparameter *default-external-resolver* (%xmlGetExternalEntityLoader))


(defcallback %custom-external-resolver :pointer ((%url :pointer) (%id :pointer) (%context :pointer))
  (or (and (or *resolvers*
               *default-resolvers*)
           (let ((url (unless (null-pointer-p %url) (puri:parse-uri (foreign-string-to-lisp %url))))
                 (id  (unless (null-pointer-p %id) (foreign-string-to-lisp %id))))
             (iter (for resolver in *resolvers*)
                   (for res = (funcall resolver url id %context))
                   (finding res such-that res))))
      (foreign-funcall-pointer *default-external-resolver*
                               ()
                               :pointer %url
                               :pointer %id
                               :pointer %context
                               :pointer)))

(%xmlSetExternalEntityLoader (callback %custom-external-resolver))

(defmacro with-custom-resolvers ((&rest resolvers) &body body)
  `(let ((*resolvers* (list ,@resolvers))
         (*stream-for-xml-parse*))
     (gp:with-garbage-pool () ,@body)))


(defun resolve-file/url (filename %ctxt)
  (with-foreign-string (%filename filename)
    (%xmlNewInputFromFile %ctxt
                          %filename)))

(defun resolve-string (str %ctxt)
  (%xmlNewStringInputStream %ctxt
                            (gp:cleanup-register (foreign-string-alloc str) #'foreign-string-free)))

(defun resolve-stream (stream %ctxt)
  (setq *stream-for-xml-parse* stream)
  (%xmlNewIOInputStream %ctxt
                        (%xmlParserInputBufferCreateIO (%stream-reader-callback stream)
                                                                            (null-pointer)
                                                                            (null-pointer)
                                                                            :xml-char-encoding-none)
                        :xml-char-encoding-none))
  
