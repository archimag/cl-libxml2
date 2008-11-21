;; test.lisp

(defpackage :libxml2.test
  (:use :cl :iter :libxml2.tree :lift :libxml2.xpath :libxml2.xslt)
  (:export
   #:run-libxml2-tests))

(in-package #:libxml2.test)

(register-exslt-extensions)

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

;;; make-comment

(addtest (tree-test)
  make-comment-1
  (ensure-same '(:xml-comment-node "Test comment")
               (with-libxml2-object (comm (make-comment "Test comment"))
                 (list (node-type comm)
                       (text-content comm)))))

;;; make-process-instruction

(addtest (tree-test)
  make-process-instruction-1
  (ensure-same '(:xml-pi-node "my-pi" "Test PI content")
               (with-libxml2-object (p-i (make-process-instruction "my-pi" "Test PI content"))
                 (list (node-type p-i)
                       (local-name p-i)
                       (text-content p-i)))))

;;; make-text

(addtest (tree-test)
  make-text-1
  (ensure-same '(:xml-text-node "Test text data")
               (with-libxml2-object (text (make-text "Test text data"))
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

         
                 
;;; attribute-value

(addtest (tree-test)
  attribute-value-1
  (ensure-same "Test attribute value"
               (with-parse-document (doc "<root attr=\"Test attribute value\" />")
                 (attribute-value (root doc) "attr"))))

(addtest (tree-test)
  attribute-value-2
  (ensure-same "Test attribute value"
               (with-parse-document (doc "<root xmlns:my=\"www.sample.org\" my:attr=\"Test attribute value\" />")
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
                 (attribute-node-value (find-single-node (root doc) "a/@attr")))))

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
;; xslt-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite xslt-test (libxml2-test) ())

(addtest (xslt-test)
  xslt-test-1
  (ensure-same '("result" "Hello world")
               (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">
    <xsl:template match=\"/\">
        <result>
            <xsl:value-of select=\".\" />
        </result>
    </xsl:template>
</xsl:stylesheet>")
                 (with-parse-document (doc "<root>Hello world</root>")
                   (with-transfom-result (res (style doc))
                     (list (local-name (root res))
                           (find-string (root res) "text()")))))))

(addtest (xslt-test)
  xslt-test-2
  (ensure-same '("result" "Hello world")
               (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">
    <xsl:param name=\"arg1\" />
    <xsl:template match=\"/\">
        <result>
            <xsl:value-of select=\"$arg1\" />
        </result>
    </xsl:template>
</xsl:stylesheet>")
                 (stylesheet-set-param style "arg1" "Hello world")
                 (with-parse-document (doc "<root />")
                   (with-transfom-result (res (style doc))
                     (list (local-name (root res))
                           (find-string (root res) "text()")))))))

(addtest (xslt-test)
  xslt-test-3
  (ensure-same '("result" "Hello world")
               (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">
    <xsl:param name=\"arg1\" />
    <xsl:template match=\"/\">
        <result>
            <xsl:value-of select=\"concat($arg1, ' ', $arg2)\" />
        </result>
    </xsl:template>
</xsl:stylesheet>")
                 (stylesheet-set-param style "arg1" "Hello")
                 (stylesheet-set-param style "arg2" "world")
                 (with-parse-document (doc "<root />")
                   (with-transfom-result (res (style doc))
                     (list (local-name (root res))
                           (find-string (root res) "text()")))))))

;;xmlns:str="http://exslt.org/strings"


(addtest (xslt-test)
  exslt-test-1
  (ensure-same '("result" "abc")
               (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:str=\"http://exslt.org/strings\" version=\"1.0\">
    <xsl:template match=\"/\">
        <result>
            <xsl:value-of select=\"str:concat(/root/node())\" />
        </result>
    </xsl:template>
</xsl:stylesheet>")
                 (with-parse-document (doc "<root><node>a</node><node>b</node><node>c</node></root>")
                   (with-transfom-result (res (style doc))
                     (list (local-name (root res))
                           (find-string (root res) "text()")))))))

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


(addtest (custom-resolve-test)
  with-custom-resolvers-xsl-1
  (ensure-same '("result" "Hello world")
               (with-custom-resolvers ((lambda (url id ctxt)
                                         (declare (ignore url id))
                                         (resolve-string "<node>Hello world</node>" ctxt)))
                 (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">
    <xsl:template match=\"/\">
        <result>
            <xsl:copy-of select=\"document('data')\" />
        </result>
    </xsl:template>
</xsl:stylesheet>")
                   (with-parse-document (doc "<root/>")
                     (with-transfom-result (res (style doc))
                       (list (local-name (root res))
                             (find-string (root res) "/result/node/text()"))))))))

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


(addtest (custom-xpath-func-test)
  custom-xpath-func-in-xsl-1
  (ensure-same '("Hello world" "Buy!")
               (with-xpath-functions ((test-echo "test-echo" "www.sample.org"))
                 (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:my=\"www.sample.org\"  version=\"1.0\">
    <xsl:template match=\"/\">
        <result>
            <node><xsl:value-of select=\"my:test-echo('Hello world')\" /></node>
            <node><xsl:value-of select=\"my:test-echo('Buy!')\" /></node>
        </result>
    </xsl:template>
</xsl:stylesheet>")
                   (with-parse-document (doc "<root/>")
                     (with-transfom-result (res (style doc))
                       (iter (for node in-child-nodes (root res) with (:type :xml-element-node))
                             (collect (find-string node "text()")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom xslt elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite custom-xslt-elements-test (libxml2-test) ())

;;; custom-xslt-element-1

(define-xslt-element hello-element (self input output)
  (declare (ignore self input))
  (append-child output (make-text "Hello world!")))

(addtest (custom-xslt-elements-test)
  custom-xslt-element-1
  (ensure-same "Hello world!"
               (with-xslt-elements ((hello-element "hello" "www.sample.org"))
                 (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:my=\"www.sample.org\"  extension-element-prefixes=\"my\" version=\"1.0\">
    <xsl:template match=\"/\">
        <result><my:hello /></result>
    </xsl:template>
</xsl:stylesheet>")
                   (with-parse-document (doc "<root/>")
                     (with-transfom-result (res (style doc))
                       (find-string res "/result/text()")))))))

(define-xslt-element my-copy-of (self input output)
   (iter (for node in-xpath-result (attribute-value self "select")  on input)
         (append-child output (copy node))))

;;; custom-xslt-element-2

(addtest (custom-xslt-elements-test)
  custom-xslt-element-2
  (ensure-same '("a" "c")
               (with-xslt-elements ((my-copy-of "copy-of" "www.sample.org"))
                 (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:my=\"www.sample.org\"  extension-element-prefixes=\"my\" version=\"1.0\">
    <xsl:template match=\"/root\">
        <result><my:copy-of select=\"node()[@attr]\" /></result>
    </xsl:template>
</xsl:stylesheet>")
                   (with-parse-document (doc "<root><a attr=\"1\"/><b /><c attr=\"2\"/><d /></root>")
                     (with-transfom-result (res (style doc))
                       (iter (for node in-child-nodes (root res) with (:type :xml-element-node))
                             (collect (local-name node)))))))))
                       ;;(find-string res "/result/text()")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-libxml2-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-libxml2-tests ()
  (run-tests :suite 'libxml2-test :report-pathname nil))
