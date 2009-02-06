
(defsystem :cl-libxml2-test
  :depends-on (#:cl-libxml2 #:lift)
  :components
  ((:module :test
            :components
            ((:file "libxml2")))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxml2-test))))
  (operate 'load-op 'cl-libxml2-test )
  (let* ((test-results (funcall (intern (symbol-name 'run-libxml2-tests) :libxml2.test)))
         (errors (funcall (intern (symbol-name 'errors) :lift) test-results))
         (failures (funcall (intern (symbol-name 'failures) :lift) test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures)))))
