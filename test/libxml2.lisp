;; test.lisp

(defpackage :libxml2.test
  (:use :cl :iter :libxml2.tree :lift :libxml2.xpath :metabang.bind)
  (:export :libxml2-test
           :run-libxml2-tests))

(in-package #:libxml2.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; libxml2-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite libxml2-test () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite tree-test (libxml2-test) ())

;;; make-element

(addtest (tree-test)
  make-element-1
  (ensure-same '(:xml-element-node "root" nil nil)
               (with-libxml2-object (el (make-element "root"))
                 (list (node-type el)
                       (local-name el)
                       (namespace-uri el)
                       (namespace-prefix el)))))

(addtest (tree-test)
  make-element-2
  (ensure-same '(:xml-element-node "root" "www.sample.org" nil)
               (with-libxml2-object (el (make-element "root" "www.sample.org"))
                 (list (node-type el)
                       (local-name el)
                       (namespace-uri el)
                       (namespace-prefix el)))))

(addtest (tree-test)
  make-element-3
  (ensure-same '(:xml-element-node "root" "www.sample.org" "my")
               (with-libxml2-object (el (make-element "root" "www.sample.org" "my"))
                 (list (node-type el)
                       (local-name el)
                       (namespace-uri el)
                       (namespace-prefix el)))))

;;; make-child-element

(addtest (tree-test)
  make-child-element-1
  (ensure-same '("root" :xml-element-node  "child")
               (with-libxml2-object (el (make-element "root"))
                 (let ((child (make-child-element el "child")))
                   (list (local-name (parent child))
                         (node-type child)
                         (local-name child))))))

(addtest (tree-test)
  make-child-element-2
  (ensure-same '("child" "www.sample.org" "my")
               (with-libxml2-object (el (make-element "root" "www.sample.org" "my"))
                 (let ((child (make-child-element el "child" "www.sample.org")))
                 (list (local-name child)
                       (namespace-uri child)
                       (namespace-prefix child))))))

(addtest (tree-test)
  make-child-element-3
  (ensure-same '("child" "www.sample.org" "my2")
               (with-libxml2-object (el (make-element "root" "www.sample.org" "my"))
                 (let ((child (make-child-element el "child" "www.sample.org" "my2")))
                 (list (local-name child)
                       (namespace-uri child)
                       (namespace-prefix child))))))

;;; make-comment

(addtest (tree-test)
  make-comment-1
  (ensure-same '(:xml-comment-node "test comment")
               (with-libxml2-object (comm (make-comment "test comment"))
                 (list (node-type comm)
                       (text-content comm)))))

;;; make-process-instruction

(addtest (tree-test)
  make-process-instruction-1
  (ensure-same '(:xml-pi-node "my-pi" "test pi content")
               (with-libxml2-object (p-i (make-process-instruction "my-pi" "test pi content"))
                 (list (node-type p-i)
                       (local-name p-i)
                       (text-content p-i)))))

;;; make-text

(addtest (tree-test)
  make-text-1
  (ensure-same '(:xml-text-node "test text data")
               (with-libxml2-object (text (make-text "test text data"))
                 (list (node-type text)
                       (text-content text)))))

;;; make-document

(addtest (tree-test)
  make-document-1
  (ensure-same '(:xml-document-node :xml-element-node "root" "www.sample.org" "my")
               (with-libxml2-object (doc (make-document (make-element "root" "www.sample.org" "my")))
                 (list (node-type doc)
                       (node-type (root doc))
                       (local-name (root doc))
                       (namespace-uri (root doc))
                       (namespace-prefix (root doc))))))

;;; parse (str string)

(addtest (tree-test)
  parse-string-1
  (ensure-same '("root" "www.sample.org" "my" "test attibute")
               (with-parse-document (doc "<my:root xmlns:my=\"www.sample.org\" attr=\"test attibute\" />")
                 (list (local-name (root doc))
                       (namespace-uri (root doc))
                       (namespace-prefix (root doc))
                       (attribute-value (root doc) "attr")))))

;;; parse (stream string-stream)

(addtest (tree-test)
  parse-string-stream-1
  (ensure-same 1000
               (with-input-from-string (in (concatenate 'string
                                                        (iter (repeat 1000)
                                                              (reducing "<tag />"
                                                                        by (lambda (sum val) (concatenate 'string sum val))
                                                                        initial-value "<root>"))
                                                        "</root>"))
                 (with-parse-document (doc in)
                   (iter (for node in-child-nodes (root doc) with (:type :xml-element-node :local-name "tag"))
                         (counting node))))))

(addtest (tree-test)
  parse-string-stream-2
  (ensure-same 1000
               (with-input-from-string (in (concatenate 'string
                                                        (iter (repeat 1000)
                                                              (reducing "<тэг/>"
                                                                        by (lambda (sum val) (concatenate 'string sum val))
                                                                        initial-value "<корень>"))
                                                        "</корень>"))
                 (with-parse-document (doc in)
                   (iter (for node in-child-nodes (root doc) with (:type :xml-element-node :local-name "тэг"))
                         (counting node))))))
                                           
;;; parse (stream stream)

(addtest (tree-test)
  parse-stream-1
  (ensure-same 1000
               (flexi-streams:with-input-from-sequence
                   (in (flexi-streams:string-to-octets 
                        (concatenate 'string
                                     (iter (repeat 1000)
                                           (reducing "<tag />"
                                                     by (lambda (sum val) (concatenate 'string sum val))
                                                     initial-value "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<root>"))
                                     "</root>")
                        :external-format :utf-8))
                 (with-parse-document (doc in)
                   (iter (for node in-child-nodes (root doc) with (:type :xml-element-node :local-name "tag"))
                         (counting node))))))

(addtest (tree-test)
  parse-stream-2
  (ensure-same 1000
               (flexi-streams:with-input-from-sequence
                   (in (flexi-streams:string-to-octets 
                        (concatenate 'string
                                     (iter (repeat 1000)
                                           (reducing "<тэг />"
                                                     by (lambda (sum val) (concatenate 'string sum val))
                                                     initial-value "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<корень>"))
                                     "</корень>")
                        :external-format :utf-8))
                 (with-parse-document (doc in)
                   (iter (for node in-child-nodes (root doc) with (:type :xml-element-node :local-name "тэг"))
                         (counting node))))))

(addtest (tree-test)
  parse-stream-3
  (ensure-same 1000
               (flexi-streams:with-input-from-sequence
                   (in (flexi-streams:string-to-octets 
                        (concatenate 'string
                                     (iter (repeat 1000)
                                           (reducing "<тэг />"
                                                     by (lambda (sum val) (concatenate 'string sum val))
                                                     initial-value "<?xml version=\"1.0\" encoding=\"windows-1251\"?>
<корень>"))
                                     "</корень>")
                        :external-format :windows-1251))
                 (with-parse-document (doc in)
                   (iter (for node in-child-nodes (root doc) with (:type :xml-element-node :local-name "тэг"))
                         (counting node))))))

;;; parse (octets stream)

(addtest (tree-test)
  parse-array-1
  (ensure-same '("root" "www.sample.org" "my" "test attibute")
               (with-parse-document (doc (flexi-streams:string-to-octets "<my:root xmlns:my=\"www.sample.org\" attr=\"test attibute\" />"))
                 (list (local-name (root doc))
                       (namespace-uri (root doc))
                       (namespace-prefix (root doc))
                       (attribute-value (root doc) "attr")))))

;;; setf text-content

(addtest (tree-test)
  setf-text-content-1
  (ensure-same '((:xml-text-node) "Hello world")
               (with-parse-document (doc "<root><a /></root>")
                 (setf (text-content (root doc)) "Hello world")
                 (list (iter (for node in-child-nodes (root doc))
                             (collect (node-type node)))
                       (text-content (root doc))))))

;;; insert-child-before

(addtest (tree-test)
  insert-child-before-1
  (ensure-same '("a" "node" "b" "c")
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (insert-child-before (make-element "node")
                                      (iter (for node in-child-nodes (root doc))
                                            (finding node such-that (string= "b" (local-name node)))))
                 (iter (for node in-child-nodes (root doc))
                       (collect (local-name node))))))

;;; insert-child-after

(addtest (tree-test)
  insert-child-after-1
  (ensure-same '("a" "b" "node" "c")
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (insert-child-after (make-element "node")
                                      (iter (for node in-child-nodes (root doc))
                                            (finding node such-that (string= "b" (local-name node)))))
                 (iter (for node in-child-nodes (root doc))
                       (collect (local-name node))))))

;;; append-child

(addtest (tree-test)
  append-child-1
  (ensure-same '("a" "b" "c")
               (with-parse-document (doc "<root />")
                 (append-child (root doc) (make-element "a"))
                 (append-child (root doc) (make-element "b"))
                 (append-child (root doc) (make-element "c"))
                 (iter (for node in-child-nodes (root doc) with (:type :xml-element-node))
                       (collect (local-name node))))))

;;; prepend-child

(addtest (tree-test)
  prepend-child-1
  (ensure-same '("c" "b" "a")
               (with-parse-document (doc "<root />")
                 (prepend-child (root doc) (make-element "a"))
                 (prepend-child (root doc) (make-element "b"))
                 (prepend-child (root doc) (make-element "c"))
                 (iter (for node in-child-nodes (root doc) with (:type :xml-element-node))
                       (collect (local-name node))))))

;;; remove-child

(addtest (tree-test)
  remove-child-1
  (ensure-same '("a" "b" "c")
               (with-parse-document (doc "<root><a /><node /><b /><node /><c /><node /></root>")
                 (iter (for node in (iter (for node in-child-nodes (root doc)
                                                with (:type :xml-element-node :local-name "node"))
                                          (collect node)))
                       (remove-child node))
                 (iter (for node in-child-nodes (root doc))
                       (collect (local-name node))))))
                       
;;; replace-child

(addtest (tree-test)
  replace-child-1
  (ensure-same '("a1" "b1")
               (with-parse-document (doc "<root><a0 /><b0 /></root>")
                 (bind (((node1 node2) (iter (for node in-child-nodes (root doc))
                                             (collect node))))
                   (replace-child node1 (make-element "a1"))
                   (replace-child node2 (make-element "b1")))
                 (iter (for node in-child-nodes (root doc))
                       (collect (local-name node))))))
                                              
;;; detach

(addtest (tree-test)
   detach-1
   (ensure-same '(("a" "c") ("a" "c" "b"))
                (with-parse-document (doc "<root><a /><b /><c /></root>")
                  (let ((b-node (iter (for node in-child-nodes (root doc))
                                      (finding node such-that (string= "b" (local-name node))))))
                    (detach b-node)
                    (list (iter (for node in-child-nodes (root doc))
                                (collect (local-name node)))
                          (progn (append-child (root doc) b-node)
                                 (iter (for node in-child-nodes (root doc))
                                       (collect (local-name node)))))))))
                 
;;; attribute-value

(addtest (tree-test)
  attribute-value-1
  (ensure-same "test attribute value"
               (with-parse-document (doc "<root attr=\"test attribute value\" />")
                 (attribute-value (root doc) "attr"))))

(addtest (tree-test)
  attribute-value-2
  (ensure-same "test attribute value"
               (with-parse-document (doc "<root xmlns:my=\"www.sample.org\" my:attr=\"test attribute value\" />")
                 (attribute-value (root doc) "attr" "www.sample.org"))))


(addtest (tree-test)
  attribute-value-3
  (ensure-same "test attrbiute value"
               (with-libxml2-object (el (make-element "root"))
                 (setf (attribute-value el "value") "test attrbiute value")
                 (attribute-value el "value"))))

(addtest (tree-test)
  attribute-value-4
  (ensure-same "test attrbiute value"
               (with-parse-document (doc "<root />")
                 (setf (attribute-value (root doc) "value" "www.sample.org") "test attrbiute value")
                 (attribute-value (root doc) "value" "www.sample.org"))))

(addtest (tree-test)
  attribute-value-5
  (ensure-different "test attrbiute value"
                    (with-parse-document (doc "<root />")
                      (setf (attribute-value (root doc) "value" "www.sample.org") "test attrbiute value")
                      (attribute-value (root doc) "value"))))


;;; remove-attribute

(addtest (tree-test)
  remove-attribute-1
  (ensure-null (with-parse-document (doc "<root attr=\"value\" />")
                 (remove-attribute (root doc) "attr")
                 (attribute-value (root doc) "attr"))))

(addtest (tree-test)
  remove-attribute-2
  (ensure (with-parse-document (doc "<root my:attr=\"value\" xmlns:my=\"www.sample.org\" />")
                 (remove-attribute (root doc) "attr")
                 (attribute-value (root doc) "attr" "www.sample.org"))))

(addtest (tree-test)
  remove-attribute-3
  (ensure-null (with-parse-document (doc "<root my:attr=\"value\" xmlns:my=\"www.sample.org\" />")
                 (remove-attribute (root doc) "attr" "www.sample.org")
                 (attribute-value (root doc) "attr" "www.sample.org"))))

;;; with-attributes

(addtest (tree-test)
  with-attributes-1
  (ensure-same '("val1" "val2" "val3")
               (with-parse-document (doc "<root attr1=\"val1\" attr2=\"val2\" attr3=\"val3\" />")
                 (with-attributes ((attr1 "attr1")
                                   (attr2 "attr2")
                                   (attr3 "attr3")) (root doc)
                   (list attr1 attr2 attr3)))))

(addtest (tree-test)
  with-attributes-2
  (ensure-same '("val1" "val2" "val3")
               (with-parse-document (doc "<root xmlns:my1=\"www.sample1.org\" xmlns:my2=\"www.sample2.org\" attr1=\"val1\" my1:attr2=\"val2\" my2:attr3=\"val3\" />")
                 (with-attributes ((attr1 "attr1")
                                   (attr2 "attr2" "www.sample1.org")
                                   (attr3 "attr3" "www.sample2.org")) (root doc)
                   (list attr1 attr2 attr3)))))
                     


;;; ITERM (FOR child IN-CHILD-NODES node WITH ())

(addtest (tree-test)
  (ensure-same '("foo" "bar" "zoo")
               (with-parse-document (doc  "<root>Hello <foo/>World<bar/><zoo/>Buy!</root>")
                 (iter (for child in-child-nodes (root doc) with (:type :xml-element-node))
                       (collect (local-name child))))))

;;; ITERM (FOR child IN-NEXT-SIBLINGS node WITH ())

(addtest (tree-test)
  in-next-siblings
  (ensure-same '("bar" "zoo")
               (with-parse-document (doc  "<root><foo/><bar/><zoo/></root>")
                 (iter (for child in-next-siblings (first-child (root doc)) with (:type :xml-element-node))
                       (collect (local-name child))))))

;;; ITERM (FOR child IN-NEXT-SIBLINGS-FROM node WITH ())

(addtest (tree-test)
  in-next-siblings-from
  (ensure-same '("foo" "bar" "zoo")
               (with-parse-document (doc  "<root><foo/><bar/><zoo/></root>")
                 (iter (for child in-next-siblings-from (first-child (root doc)) with (:type :xml-element-node))
                       (collect (local-name child))))))


;;; ITERM (FOR child IN-PREV-SIBLINGS node WITH ())

(addtest (tree-test)
  in-prev-siblings
  (ensure-same '("bar" "foo")
               (with-parse-document (doc  "<root><foo/><bar/><zoo/></root>")
                 (iter (for child in-prev-siblings (last-child (root doc)) with (:type :xml-element-node))
                       (collect (local-name child))))))

;;; ITERM (FOR child IN-PREV-SIBLINGS-FROM node WITH ())

(addtest (tree-test)
  in-prev-siblings-from
  (ensure-same '("zoo" "bar" "foo")
               (with-parse-document (doc  "<root><foo/><bar/><zoo/></root>")
                 (iter (for child in-prev-siblings-from (last-child (root doc)) with (:type :xml-element-node))
                       (collect (local-name child))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xpath-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite xpath-test (libxml2-test) ())

;;; xpath-object-type

(addtest (xpath-test)
  xpath-object-type-1
  (ensure-same :xpath-nodeset
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-object (res (doc "//node()"))
                   (xpath-object-type res)))))


(addtest (xpath-test)
  xpath-object-type-2
  (ensure-same :xpath-number
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-object (res (doc "count(//node())"))
                   (xpath-object-type res)))))

(addtest (xpath-test)
  xpath-object-type-3
  (ensure-same :xpath-string
               (with-parse-document (doc "<root attr=\"Test\" />")
                 (with-xpath-object (res (doc "string(/root/@attr)"))
                   (xpath-object-type res)))))

;;; find-string

(addtest (xpath-test)
  find-string-1
  (ensure-same "a"
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (find-string doc "local-name(/root/node())"))))

(addtest (xpath-test)
  find-string-2
  (ensure-same "test value"
               (with-parse-document (doc "<root attr=\"test value\"/>")
                 (find-string doc "/root/@attr"))))

(addtest (xpath-test)
  find-string-3
  (ensure-same "value1"
               (with-parse-document (doc "<root attr1=\"value1\" attr2=\"value2\" />")
                 (find-string doc "/root/@*"))))

(addtest (xpath-test)
  find-string-4
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (find-string doc "/root/node()"))))

(addtest (xpath-test)
  find-string-5
  (ensure-same "value"
               (with-parse-document (doc "<root xmlns:my=\"www.sample.org\" my:attr=\"value\" />")
                 (find-string doc "/root/@my:attr" :ns-map '(("my" "www.sample.org"))))))

(addtest (xpath-test)
  find-string-6
  (ensure-null (with-parse-document (doc "<root xmlns:my=\"www.sample.org\" my:attr=\"value\" />")
                 (find-string doc "/root/@my:attr"))))

(addtest (xpath-test)
  find-string-7
  (ensure-null (with-parse-document (doc "<root xmlns:my=\"www.sample.org\" my:attr=\"value\" />")
                   (find-string doc "/root/@attr"))))

;;; eval-expressiong-as-number

(addtest (xpath-test)
  find-number-1
  (ensure-same 3.0d0
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (find-number doc "count(/root/node())"))))

(addtest (xpath-test)
  find-number-2
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (find-number doc "/root/node()"))))

;;; eval-expressiong-as-boolean

(addtest (xpath-test)
  find-boolean-1
  (ensure (with-parse-document (doc "<root attr=\"\" />")
            (find-boolean doc "/root/@*"))))

(addtest (xpath-test)
  find-boolean-2
  (ensure (with-parse-document (doc "<root><a /><b /><c /></root>")
            (find-boolean doc "/root/node()"))))

(addtest (xpath-test)
  find-boolean-3
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (find-boolean doc "/root/text()"))))

(addtest (xpath-test)
  find-boolean-4
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (find-boolean doc "/root/@*"))))

;; find-single-node

(addtest (xpath-test)
  find-single-node-1
  (ensure-same "b"
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (local-name (find-single-node (root doc) "b")))))

(addtest (xpath-test)
  find-single-node-2
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (find-single-node (root doc) "test"))))

(addtest (xpath-test)
  find-single-node-3
  (ensure-same "val"
               (with-parse-document (doc "<root><a attr=\"val\"/><b /><c /></root>")
                 (text-content (find-single-node (root doc) "a/@attr")))))

;;; in-nodeset

(addtest (xpath-test)
  in-nodeset-1
  (ensure-same '("root" "a" "b" "c")
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-object (res (doc "//node()"))
                   (iter (for node in-nodeset (xpath-object-value res))
                         (collect (local-name node)))))))

(addtest (xpath-test)
  in-nodeset-2
  (ensure-same '("a" "b" "c")
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-object (res ((root doc) "node()"))
                   (iter (for node in-nodeset (xpath-object-value res))
                         (collect (local-name node)))))))

(addtest (xpath-test)
  in-nodeset-3
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-object (res ((root doc) "text()"))
                   (iter (for node in-nodeset (xpath-object-value res))
                         (collect (local-name node)))))))

;;; in-xpath-result

(addtest (xpath-test)
  in-xpath-result-1
  (ensure-same '("a" "b" "c")
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (iter (for node in-xpath-result "node()" on (root doc))
                       (collect (local-name node))))))

(addtest (xpath-test)
  in-xpath-result-2
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (iter (for node in-xpath-result "node()" on doc)
                       (collect (local-name node))))))

(addtest (xpath-test)
  in-xpath-result-3
  (ensure-same '("box")
               (with-parse-document (doc "<root xmlns:xul=\"http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul\"><a /><xul:box /><b /><c /></root>")
                 (iter (for node in-xpath-result "//xul:box" on doc)
                       (collect (local-name node))))))

(addtest (xpath-test)
  in-xpath-result-4
  (ensure-null (with-parse-document (doc "<root xmlns:my=\"www.sample.org\"><a /><my:box /><b /><c /></root>")
                 (iter (for node in-xpath-result "//box" on doc)
                       (collect (local-name node))))))

(addtest (xpath-test)
  in-xpath-result-5
  (ensure-null (with-parse-document (doc "<root xmlns:my=\"www.sample.org\"><a /><my:box /><b /><c /></root>")
                 (iter (for node in-xpath-result "//my:box" on doc)
                       (collect (local-name node))))))

;; compiled-expression

(addtest (xpath-test)
  compiled-expression-1
  (ensure-same '("a" "b" "c")
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-compiled-expression (expr "node()")
                   (iter (for node in-xpath-result expr on (root doc))
                       (collect (local-name node)))))))

;; getpath

(addtest (xpath-test)
  getpath-1
  (ensure-same "/root/foo/bar"
               (with-parse-document (doc "<root><foo><bar /></foo></root>")
                 (getpath (first-child (first-child (root doc)))))))

(addtest (xpath-test)
  getpath-2
  (ensure-same '("/root/@a" "/root/child/@a" "/root/child/@b" "/root/child/@c")
               (with-parse-document (doc "<root a=\"\" ><child a=\"\" b=\"\" c=\"\" /></root>")
                 (iter (for node in-xpath-result "//@*" on doc)
                       (collect (getpath node))))))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-resolve-tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite custom-resolve-test (libxml2-test) ())

(addtest (custom-resolve-test)
  with-custom-resolve-string-1
  (ensure-same "node"
               (with-custom-resolvers ((lambda (url id ctxt) (resolve-string "<node />" ctxt)))
                 (with-parse-document (doc "<root xmlns:xi=\"http://www.w3.org/2001/XInclude\"><xi:include href=\"/tmp/blank.xml\" /></root>")
                   (process-xinclude doc)
                   (local-name (find-single-node doc "/root/node()"))))))

(addtest (custom-resolve-test)
  with-custom-resolve-string-2
  (ensure-same '("node1" "node2" "include")
               (with-custom-resolvers ((lambda (url id ctxt)
                                         (if (eql (puri:uri-scheme url) :my1)
                                             (resolve-string "<node1 />" ctxt)))
                                       (lambda (url id ctxt)
                                         (if (eql (puri:uri-scheme url) :my2)
                                             (resolve-string "<node2 />" ctxt))))
                 (with-parse-document (doc "<root xmlns:xi=\"http://www.w3.org/2001/XInclude\">
<xi:include href=\"my1:doc\" />
<xi:include href=\"my2:doc\" />
<xi:include href=\"my3:doc\" />
</root>")
                   (process-xinclude doc)
                   (iter (for node in-child-nodes (root doc) with (:type :xml-element-node))
                         (collect (local-name node)))))))
                   
(addtest (custom-resolve-test)
  with-custom-resolve-stream-1
  (ensure-same "node"
               (with-input-from-string (in "<node />")
                 (with-custom-resolvers ((lambda (url id ctxt) (resolve-stream in ctxt)))
                   (with-parse-document (doc "<root xmlns:xi=\"http://www.w3.org/2001/XInclude\">
<xi:include href=\"/tmp/blank.xml\" />
</root>")
                     (process-xinclude doc)
                     (iter (for node in-child-nodes (root doc) with (:type :xml-element-node))
                           (finding (local-name node) such-that node)))))))

(addtest (custom-resolve-test)
  with-custom-resolve-stream-2
  (ensure-same "/root/foo/bar"
               (with-input-from-string (in1 "<foo><xi:include xmlns:xi=\"http://www.w3.org/2001/XInclude\" href=\"my2:data\" /></foo>")
                 (with-input-from-string (in2 "<bar />")
                   (with-custom-resolvers ((lambda (url id ctxt)
                                             (if (eql (puri:uri-scheme url) :my1)
                                                 (resolve-stream in1 ctxt)))
                                           (lambda (url id ctxt)
                                             (resolve-stream in2 ctxt)))
                     (with-parse-document (doc "<root xmlns:xi=\"http://www.w3.org/2001/XInclude\">
<xi:include href=\"my1:data\" />
</root>")
                       (process-xinclude doc)
                       (getpath (find-single-node doc "//bar"))
                       ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom xpath functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite custom-xpath-func-test (libxml2-test) ())

(define-xpath-function test-hello ()
  "Hello world")

(addtest (custom-xpath-func-test)
  custom-xpath-func-1
  (ensure-same '("Hello world" nil "Hello world")
               (with-xpath-functions ((test-hello "test-hello")
                                      (test-hello "test-hello" "www.sample.org"))
                 (with-parse-document (doc "<root />")
                   (list (find-string doc "test-hello()")
                         (find-string doc "my:test-hello()" :ns-map '(("my" "www.other-sample.org")))
                         (find-string doc "my:test-hello()" :ns-map '(("my" "www.sample.org"))))))))

(define-xpath-function test-echo (msg)
  msg)

(addtest (custom-xpath-func-test)
  custom-xpath-func-2
  (ensure-same '("Hello world" "Buy!")
               (with-xpath-functions ((test-echo "test-echo"))
                 (with-parse-document (doc "<root />")
                   (list (find-string doc "test-echo('Hello world')")
                         (find-string doc "test-echo('Buy!')"))))))
    
(define-xpath-function test-local-name (nodes)
  (local-name (node-set-at nodes 0)))
  
(addtest (custom-xpath-func-test)
  custom-xpath-func-3
  (ensure-same '("root" "test-node")
               (with-xpath-functions ((test-local-name "test-local-name"))
                 (with-parse-document (doc "<root><test-node /></root>")
                   (list (find-string doc "test-local-name(/root)")
                         (find-string doc "test-local-name(/root/test-node)"))))))

(define-xpath-function test-node-count (nodes)
  (node-set-length nodes))

(addtest (custom-xpath-func-test)
  custom-xpath-func-4
  (ensure-same '(3.0d0 0.0d0)
               (with-xpath-functions ((test-node-count "test-node-count"))
                 (with-parse-document (doc "<root><a /><b /><c /></root>")
                   (list (find-number doc "test-node-count(/root/node())")
                         (find-number doc "test-node-count(/root/text())"))))))


(define-xpath-function test-concate (&rest strs)
  (apply 'concatenate 'string strs))

(addtest (custom-xpath-func-test)
  custom-xpath-func-5
  (ensure-same '("hello world!" "hello" nil)
               (with-xpath-functions ((test-concate "test-concate"))
                 (with-parse-document (doc "<root />")
                   (list (find-string doc "test-concate('hello', ' ', 'world', '!')")
                         (find-string doc "test-concate('hello')")
                         (find-string doc "test-concate()"))))))


(addtest (custom-xpath-func-test)
  custom-xpath-func-6
  (ensure-same "Hello world"
               (with-xpath-functions ((test-hello "test-hello")
                                      (test-echo "test-echo"))
                 (with-parse-document (doc "<root />")
                   (find-string doc "test-echo(test-hello())")))))

(define-xpath-function test-node-name-concat (nodes)
  (iter (for node in-nodeset nodes)
        (reducing (local-name node)
                  by (lambda (s x) (concatenate 'string s x)))))

(addtest (custom-xpath-func-test)
  custom-xpath-func-7
  (ensure-same "abc"
               (with-xpath-functions ((test-node-name-concat "test-node-name-concat"))
                 (with-parse-document (doc "<root><a /><b /><c /></root>")
                   (find-string doc "test-node-name-concat(/root/node())")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-libxml2-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-libxml2-tests (&optional (test 'libxml2-test))
  (run-tests :suite test :report-pathname nil))
