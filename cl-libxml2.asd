;; cl-libxml2.asd

(defsystem :cl-libxml2
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:puri #:flexi-streams #:alexandria #:garbage-pools #:metabang-bind)
  :components
  ((:module :tree
            :components
            ((:file "packages")
             (:file "xtree" :depends-on ("packages"))
             (:file "namespace" :depends-on ("xtree"))
             (:file "attribute" :depends-on ("xtree"))
             (:file "node" :depends-on ("namespace"))
             (:file "document" :depends-on ("node"))
             (:file "parse" :depends-on ("document"))
             (:file "serialize" :depends-on ("document"))
             (:file "resolve" :depends-on ("parse"))))
   (:module :xpath
            :components
            ((:file "packages")
             (:file "low-level-api" :depends-on ("packages"))
             (:file "node-set" :depends-on ("low-level-api"))
             (:file "xpath-object" :depends-on ("node-set"))
             (:file "expression" :depends-on ("xpath-object"))
             (:file "context" :depends-on ("expression")))
            :depends-on ("tree"))))
