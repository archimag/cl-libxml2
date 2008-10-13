;; cl-libxml2.asd

(defsystem :cl-libxml2
  :version "0.0.0"
  :depends-on (#:cffi #:iterate)
  :components
  ((:module :core
            :components
            ((:file "package")
             (:file "low-level-api" :depends-on ("package"))
             (:file "core" :depends-on ("low-level-api"))))))