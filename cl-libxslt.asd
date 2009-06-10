;; cl-libxslt.asd

(defpackage :cl-libxslt-system
  (:use :cl :asdf))

(in-package :cl-libxslt-system)

(defsystem :cl-libxslt
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:cl-libxml2)
  :components
  ((:module :xslt
            :components
            ((:file "package")
             (:file "xslt" :depends-on ("package"))
             (:file "extensions" :depends-on ("xslt"))
             (:file "stylesheet" :depends-on ("extensions"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxslt))))
  (operate 'load-op 'cl-libxslt)
  (operate 'load-op 'cl-libxslt-test)
  (operate 'test-op 'cl-libxslt-test :force t))

;;; tests

(defsystem :cl-libxslt-test
  :depends-on (#:cl-libxslt #:cl-libxml2-test)
  :components
  ((:module :test
            :components
            ((:file "libxslt")))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxslt-test))))
  (operate 'load-op 'cl-libxslt-test)
  (operate 'test-op 'cl-libxml2-test))
