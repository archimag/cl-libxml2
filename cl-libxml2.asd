;; cl-libxml2.asd

(defsystem :cl-libxml2
  :depends-on (#:cffi #:iterate #:puri #:flexi-streams #:alexandria #:garbage-pools #:metabang-bind #:metatilities)
  :components
  ((:module :tree
            :serial t
            :components
            ((:file "package")
             (:file "xtree" :depends-on ("package"))
             (:file "error" :depends-on ("xtree"))
             (:file "namespace" :depends-on ("error"))
             (:file "attribute" :depends-on ("error"))
             (:file "node" :depends-on ("namespace"))
             (:file "document" :depends-on ("node"))
             (:file "parse" :depends-on ("document"))
             (:file "serialize" :depends-on ("document"))
             (:file "resolve" :depends-on ("parse"))))
   (:module :xpath
            :components
            ((:file "package")
             (:file "node-set" :depends-on ("package"))
             (:file "xpath-object" :depends-on ("node-set"))
             (:file "xpath-context" :depends-on ("package"))
             (:file "expression" :depends-on ("xpath-object" "xpath-context"))
             (:file "extensions" :depends-on ("expression")))
            :depends-on ("tree"))
   (:module :html
            :components
            ((:file "package")
             (:file "xhtml" :depends-on ("package")))
            :depends-on ("tree"))))
