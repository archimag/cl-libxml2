;; cl-libxml2.asd

(defsystem :cl-libxml2
  :depends-on (#:cffi #:iterate #:puri #:flexi-streams #:alexandria #:garbage-pools #:metabang-bind)
  :components
  ((:module :tree
            :serial t
            :components
            ((:file "packages")
             (:file "xtree" :depends-on ("packages"))
             (:file "error" :depends-on ("xtree"))
             (:file "namespace" :depends-on ("error"))
             (:file "attribute" :depends-on ("error"))
             (:file "node" :depends-on ("namespace"))
             (:file "document" :depends-on ("node"))
             (:file "parse" :depends-on ("document"))
             (:file "serialize" :depends-on ("document"))
             (:file "resolve" :depends-on ("parse"))))
   (:module :xpath
            :components
            ((:file "package")
             (:file "node-set" :depends-on ("package"))
             (:file "xpath-object" :depends-on ("node-set"))
             (:file "xpath-context" :depends-on ("package"))
             (:file "expression" :depends-on ("xpath-object" "xpath-context"))
             (:file "extensions" :depends-on ("expression")))
            :depends-on ("tree"))
   (:module :html
            :components
            ((:file "package")
             (:file "html" :depends-on ("package")))
            :depends-on ("tree"))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxml2))))
  (operate 'load-op 'cl-libxml2-test)
  (operate 'test-op 'cl-libxml2-test :force t))

;;; tests

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
