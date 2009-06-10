;; cl-libxml2-xslt.asd

(defpackage :cl-libxslt-system
  (:use :cl :asdf))

(in-package :cl-libxslt-system)

(defsystem :cl-libxml2-xslt
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:cl-libxml2)
  :components
  ((:module :xslt
            :components
            ((:file "package")
             (:file "xslt" :depends-on ("package"))
             (:file "extensions" :depends-on ("xslt"))
             (:file "stylesheet" :depends-on ("extensions"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxml2-xslt))))
  (operate 'load-op 'cl-libxml2-xslt)
  (operate 'load-op 'cl-libxml2-test)
  (operate 'load-op 'cl-libxml2-xslt-test)
  (operate 'test-op 'cl-libxml2-xslt-test :force t))

;;; tests

(defsystem :cl-libxml2-xslt-test
  :depends-on (#:cl-libxml2-xslt #:lift)
  :components
  ((:module :test
            :components
            ((:file "libxslt")))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxml2-xslt-test))))
  (operate 'load-op 'cl-libxml2-test)
  (operate 'load-op 'cl-libxml2-xslt-test)
  (operate 'test-op 'cl-libxml2-test))
