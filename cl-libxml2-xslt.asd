;; cl-libxml2-xslt.asd

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