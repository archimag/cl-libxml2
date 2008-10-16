;; test.lisp

(defpackage :libxml2-test
  (:use :cl :iter :libxml2.tree :lift)
  (:export
   #:run-libxml2-test))

(in-package #:libxml2-test)

(defun path-to-data-file (name)
  (merge-pathnames (pathname (concatenate 'string "data/" name))
                   (asdf:component-pathname  (asdf:find-component (asdf:find-system  "cl-libxml2-test") "test"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite libxml2.tree-test () ())

;;; make-document

(addtest (libxml2.tree-test)
  make-document-1
  (ensure-same '("root" nil nil)
               (with-document (doc (make-document "root"))
                 (list (local-name (root doc))
                       (namespace-uri (root doc))
                       (namespace-prefix (root doc))))))

(addtest (libxml2.tree-test)
  make-document-2
  (ensure-same '("root" "www.sample.org" nil)
               (with-document (doc (make-document "root" "www.sample.org"))
                 (list (local-name (root doc))
                       (namespace-uri (root doc))
                       (namespace-prefix (root doc))))))

(addtest (libxml2.tree-test)
  make-document-3
  (ensure-same '("root" "www.sample.org" "my")
               (with-document (doc (make-document "root" "www.sample.org" "my"))
                 (list (local-name (root doc))
                       (namespace-uri (root doc))
                       (namespace-prefix (root doc))))))


;;; ITERM (FOR child IN-CHILD-NODES node WITH ())
(addtest (libxml2.tree-test)
  iter-child-nodes-1
  (ensure-same '("foo" "bar" "zoo")
               (with-document (doc (parse (path-to-data-file "1.xml")))
                 (iter (for child in-child-nodes (root doc) with (:type :xml-element-node))
                       (collect (local-name child))))))

(addtest (libxml2.tree-test)
  (ensure-same '("foo" "bar" "zoo")
               (with-parse-document (doc  "<root> <foo/><bar/><zoo/></root>")
                 (iter (for child in-child-nodes (root doc) with (:type :xml-element-node))
                       (collect (local-name child))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-libxml2-test ()
  (run-tests :suite 'libxml2.tree-test))


