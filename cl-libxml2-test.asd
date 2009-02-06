
(defsystem :cl-libxml2-test
  :depends-on (#:cl-libxml2 #:lift)
  :components
  ((:module :test
            :components
            ((:file "libxml2")))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxml2-test))))
  (operate 'load-op 'cl-libxml2-test )
  (let ((failtures (funcall (intern (symbol-name 'failures) :lift)
                            (funcall (intern (symbol-name 'run-libxml2-tests) :libxml2.test)))))
    (if failtures
        (error "test-op failed: ~A" failtures))))
