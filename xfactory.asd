;;; cl-libxml2-xfactory.asd

(defpackage :xfactory-system
  (:use :cl :asdf))

(in-package :xfactory-system)

(defsystem :xfactory
    :depends-on (#:cl-libxml2)
    :components ((:module :xfactory
                          :components ((:file "packages")
                                       (:file "xfactory" :depends-on ("packages"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'xfactory))))
  (operate 'load-op 'xfactory-test)
  (operate 'test-op 'xfactory-test :force t))


;;; tests

(defsystem :xfactory-test
  :depends-on (#:xfactory #:lift)
  :components
  ((:module :test
            :components
            ((:file "xfactory")))))

(defmethod perform ((o test-op) (c (eql (find-system 'xfactory-test))))
  (operate 'load-op 'xfactory-test )
  (let* ((test-results (funcall (intern (symbol-name 'run-xfactory-tests) :xfactory.test)))
         (errors (funcall (intern (symbol-name 'errors) :lift) test-results))
         (failures (funcall (intern (symbol-name 'failures) :lift) test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures)))))
