;; cl-libxml2.asd

(defsystem :cl-libxml2
  :version "0.0.0"
  :depends-on (#:cffi #:iterate)
  :components
  ((:module :tree
            :components
            ((:file "package")
             (:file "low-level-api" :depends-on ("package" ))
             (:file "objects" :depends-on ("low-level-api"))
             (:file "childs" :depends-on ("objects"))
             (:file "attribute" :depends-on ("objects"))
             (:file "parse" :depends-on ("objects"))
             (:file "serialize" :depends-on ("objects"))
             ))))
             ;;(:file "core" :depends-on ("low-level-api"))))))