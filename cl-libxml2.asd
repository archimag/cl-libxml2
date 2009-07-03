;;; cl-libxml2.asd
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage :cl-libxml2-system
  (:use :cl :asdf))

(in-package :cl-libxml2-system)

(defsystem :cl-libxml2
  :depends-on (#:cffi #:iterate #:puri #:flexi-streams #:alexandria #:garbage-pools #:metabang-bind)
  :components
  ((:module :tree
            :serial t
            :components
            ((:file "packages")
             (:file "xtree" :depends-on ("packages"))
             (:file "error" :depends-on ("xtree"))
             (:file "namespace" :depends-on ("error"))
             (:file "attribute" :depends-on ("error"))
             (:file "node" :depends-on ("namespace" "entities"))
             (:file "document" :depends-on ("node"))
             (:file "parse" :depends-on ("document"))
             (:file "serialize" :depends-on ("document"))
             (:file "resolve" :depends-on ("parse"))
             (:file "entities" :depends-on ("packages"))))
   (:module :xpath
            :components
            ((:file "packages")
             (:file "node-set" :depends-on ("packages"))
             (:file "xpath-object" :depends-on ("node-set"))
             (:file "xpath-context" :depends-on ("packages"))
             (:file "expression" :depends-on ("xpath-object" "xpath-context"))
             (:file "extensions" :depends-on ("expression")))
            :depends-on ("tree"))
   (:module :html
            :components
            ((:file "packages")
             (:file "html" :depends-on ("packages")))
            :depends-on ("tree"))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxml2))))
  (operate 'load-op 'cl-libxml2-test)
  (operate 'test-op 'cl-libxml2-test :force t))

;;; tests

(defsystem :cl-libxml2-test
  :depends-on (#:cl-libxml2 #:lift)
  :components
  ((:module :test
            :components
            ((:file "libxml2")))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-libxml2-test))))
  (operate 'load-op 'cl-libxml2-test )
  (let* ((test-results (funcall (intern (symbol-name 'run-libxml2-tests) :libxml2.test)))
         (errors (funcall (intern (symbol-name 'errors) :lift) test-results))
         (failures (funcall (intern (symbol-name 'failures) :lift) test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures))
        (print test-results))))
