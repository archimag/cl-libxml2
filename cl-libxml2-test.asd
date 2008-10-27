
(defsystem :cl-libxml2-test
  :version "0.0.0"
  :depends-on (#:cl-libxml2 #:cl-libxml2-xslt #:lift)
  :components
  ((:module :test
            :components
            ((:file "test" )))))
