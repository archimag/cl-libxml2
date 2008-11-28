;; cl-libxml2.asd

(defsystem :cl-libxml2
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:puri #:flexi-streams #:alexandria #:garbage-pools #:metabang-bind)
  :components
  ((:module :tree
            :components
            ((:file "packages")
             (:file "low-level-api" :depends-on ("packages" ))
             (:file "objects" :depends-on ("low-level-api"))
             (:file "childs" :depends-on ("document"))
             (:file "attribute" :depends-on ("objects"))
             (:file "parse" :depends-on ("document"))
             (:file "serialize" :depends-on ("document"))
             (:file "add-remove" :depends-on ("objects"))
             (:file "resolve" :depends-on ("parse"))
             (:file "document" :depends-on ("objects"))))
   (:module :xpath
            :components
            ((:file "packages")
             (:file "low-level-api" :depends-on ("packages"))
             (:file "node-set" :depends-on ("low-level-api"))
             (:file "xpath-object" :depends-on ("node-set"))
             (:file "expression" :depends-on ("xpath-object"))
             (:file "context" :depends-on ("expression")))
            :depends-on ("tree"))))
