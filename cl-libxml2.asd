;; cl-libxml2.asd

(defsystem :cl-libxml2
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:puri #:flexi-streams #:alexandria #:garbage-pools #:metabang-bind #:metatilities)
  :components
  ((:module :tree
            :components
            ((:file "packages")
             (:file "xtree" :depends-on ("packages"))
             (:file "error" :depends-on ("xtree"))
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
             (:file "node-set" :depends-on ("packages"))
             (:file "xpath-object" :depends-on ("node-set"))
             (:file "xpath-context" :depends-on ("packages"))
             (:file "expression" :depends-on ("xpath-object" "xpath-context"))
             (:file "extensions" :depends-on ("expression")))
            :depends-on ("tree"))))
