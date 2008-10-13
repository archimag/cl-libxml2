
(defsystem :cl-libxml2-test
  :version "0.0.0"
  :depends-on (#:cl-libxml2 #:lift)
  :components
  ((:module :test
            :components
            ((:file "package")
             (:file "test" :depends-on ("package"))))))
