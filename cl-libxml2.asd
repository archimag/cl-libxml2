;; cl-libxml2.asd

(defsystem :cl-libxml2
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:puri)
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
             (:file "stylesheet" :depends-on ("low-level-api" "objects" ))
             ))))
             ;;(:file "core" :depends-on ("low-level-api"))))))