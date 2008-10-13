;; test.lisp

(in-package #:libxml2-test)

(defun path-to-data-file (name)
  (merge-pathnames (pathname (concatenate 'string "data/" name))
                   (asdf:component-pathname  (asdf:find-component (asdf:find-system  "cl-libxml2-test") "test"))))


(deftestsuite libxml2-core-test ()
  ()
  (:tests
   ((ensure-same '("foo" "bar" "zoo")
                 (with-document (doc (parse (path-to-data-file "1.xml")))
                   (iter (for child in-child-nodes (document-root-element doc) with (:type :xml-element-node))
                         (collect (node-tag child))))))))


(defun run-libxml2-test ()
  (run-tests :suite 'libxml2-core-test))
