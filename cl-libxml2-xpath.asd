;; cl-libxml2-xpath.asd

(defsystem :cl-libxml2-xpath
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:cl-libxml2)
  :components
  ((:module :xpath
            :components
            ((:file "packages")
             (:file "low-level-api" :depends-on ("packages"))))))