;; cl-libxml2.asd

(defsystem :cl-libxml2
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:puri #:flexi-streams #:alexandria)
  :components
  ((:module :tree
            :components
            ((:file "packages")
             (:file "low-level-api" :depends-on ("packages" ))
             (:file "objects" :depends-on ("low-level-api"))
             (:file "childs" :depends-on ("objects"))
             (:file "attribute" :depends-on ("objects"))
             (:file "parse" :depends-on ("objects"))
             (:file "serialize" :depends-on ("objects"))))
   (:module :xpath
            :components
            ((:file "packages")
             (:file "low-level-api" :depends-on ("packages"))
             (:file "expression" :depends-on ("low-level-api")))
            :depends-on ("tree"))))
