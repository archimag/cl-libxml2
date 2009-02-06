

(defsystem :cl-libxml2-xslt-test
  :depends-on (#:cl-libxml2-xslt #:cl-libxml2-test)
  :components
  ((:module :test
            :components
            ((:file "libxslt" )))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxml2-xslt-test))))
  (operate 'load-op 'cl-libxml2-xslt-test )
  (operate 'test-op 'cl-libxml2-test))
