;; test.lisp

(defpackage :libxml2-test
  (:use :cl :iter :libxml2.tree :lift)
  (:export
   #:run-libxml2-test))

(in-package #:libxml2-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite libxml2.tree-test () ())

;;; make-element

(addtest (libxml2.tree-test)
  make-element-1
  (ensure-same '(:xml-element-node "root" nil nil)
               (with-libxml2-object (el (make-element "root"))
                 (list (node-type el)
                       (local-name el)
                       (namespace-uri el)
                       (namespace-prefix el)))))
               

(addtest (libxml2.tree-test)
  make-element-2
  (ensure-same '(:xml-element-node "root" "www.sample.org" nil)
               (with-libxml2-object (el (make-element "root" "www.sample.org"))
                 (list (node-type el)
                       (local-name el)
                       (namespace-uri el)
                       (namespace-prefix el)))))

(addtest (libxml2.tree-test)
  make-element-3
  (ensure-same '(:xml-element-node "root" "www.sample.org" "my")
               (with-libxml2-object (el (make-element "root" "www.sample.org" "my"))
                 (list (node-type el)
                       (local-name el)
                       (namespace-uri el)
                       (namespace-prefix el)))))

;;; make-comment

(addtest (libxml2.tree-test)
  make-comment-1
  (ensure-same '(:xml-comment-node "Test comment")
               (with-libxml2-object (comm (make-comment "Test comment"))
                 (list (node-type comm)
                       (text-content comm)))))

;;; make-process-instruction

(addtest (libxml2.tree-test)
  make-process-instruction-1
  (ensure-same '(:xml-pi-node "my-pi" "Test PI content")
               (with-libxml2-object (p-i (make-process-instruction "my-pi" "Test PI content"))
                 (list (node-type p-i)
                       (local-name p-i)
                       (text-content p-i)))))

;;; make-text

(addtest (libxml2.tree-test)
  make-text-1
  (ensure-same '(:xml-text-node "Test text data")
               (with-libxml2-object (text (make-text "Test text data"))
                 (list (node-type text)
                       (text-content text)))))

;;; make-document

(addtest (libxml2.tree-test)
  make-document-1
  (ensure-same '(:xml-document-node :xml-element-node "root" "www.sample.org" "my")
               (with-libxml2-object (doc (make-document (make-element "root" "www.sample.org" "my")))
                 (list (node-type doc)
                       (node-type (root doc))
                       (local-name (root doc))
                       (namespace-uri (root doc))
                       (namespace-prefix (root doc))))))

;;; parse (str string)

(addtest (libxml2.tree-test)
  parse-string-1
  (ensure-same '("root" "www.sample.org" "my" "test attibute")
               (with-parse-document (doc "<my:root xmlns:my=\"www.sample.org\" attr=\"test attibute\" />")
                 (list (local-name (root doc))
                       (namespace-uri (root doc))
                       (namespace-prefix (root doc))
                       (attribute-value (root doc) "attr")))))


;;; attribute-value

(addtest (libxml2.tree-test)
  attribute-value-1
  (ensure-same "test attrbiute value"
               (with-libxml2-object (el (make-element "root"))
                 (setf (attribute-value el "value") "test attrbiute value")
                 (attribute-value el "value"))))

(addtest (libxml2.tree-test)
  attribute-value-2
  (ensure-same "test attrbiute value"
               (with-parse-document (doc "<root />")
                 (setf (attribute-value (root doc) "value" "www.sample.org") "test attrbiute value")
                 (attribute-value (root doc) "value" "www.sample.org"))))

(addtest (libxml2.tree-test)
  attribute-value-3
  (ensure-different "test attrbiute value"
                    (with-parse-document (doc "<root />")
                      (setf (attribute-value (root doc) "value" "www.sample.org") "test attrbiute value")
                      (attribute-value (root doc) "value"))))


;;; remove-attribute

(addtest (libxml2.tree-test)
  remove-attribute-1
  (ensure-null (with-parse-document (doc "<root attr=\"value\" />")
                 (remove-attribute (root doc) "attr")
                 (attribute-value (root doc) "attr"))))

(addtest (libxml2.tree-test)
  remove-attribute-2
  (ensure (with-parse-document (doc "<root my:attr=\"value\" xmlns:my=\"www.sample.org\" />")
                 (remove-attribute (root doc) "attr")
                 (attribute-value (root doc) "attr" "www.sample.org"))))

(addtest (libxml2.tree-test)
  remove-attribute-3
  (ensure-null (with-parse-document (doc "<root my:attr=\"value\" xmlns:my=\"www.sample.org\" />")
                 (remove-attribute (root doc) "attr" "www.sample.org")
                 (attribute-value (root doc) "attr" "www.sample.org"))))


;;; ITERM (FOR child IN-CHILD-NODES node WITH ())

(addtest (libxml2.tree-test)
  (ensure-same '("foo" "bar" "zoo")
               (with-parse-document (doc  "<root>Hello <foo/>World<bar/><zoo/>Buy!</root>")
                 (iter (for child in-child-nodes (root doc) with (:type :xml-element-node))
                       (collect (local-name child))))))

;;; ITERM (FOR child IN-NEXT-SIBLINGS-FROM node WITH ())

;;(addtest (libxml2.tree

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-libxml2-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-libxml2-test ()
  (run-tests :suite 'libxml2.tree-test))


