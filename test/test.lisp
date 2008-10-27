;; test.lisp

(defpackage :libxml2.test
  (:use :cl :iter :libxml2.tree :lift :libxml2.xpath :libxml2.xslt)
  (:export
   #:run-libxml2-tests))

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

;;; xpath-result-type

(addtest (xpath-test)
  xpath-result-type-1
  (ensure-same :xpath-nodeset
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-result (res (doc "//node()"))
                   (xpath-result-type res)))))


(addtest (xpath-test)
  xpath-result-type-2
  (ensure-same :xpath-number
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-result (res (doc "count(//node())"))
                   (xpath-result-type res)))))

(addtest (xpath-test)
  xpath-result-type-3
  (ensure-same :xpath-string
               (with-parse-document (doc "<root attr=\"Test\" />")
                 (with-xpath-result (res (doc "string(/root/@attr)"))
                   (xpath-result-type res)))))

;;; eval-expression-as-string

(addtest (xpath-test)
  eval-expression-as-string-1
  (ensure-same "a"
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (eval-expression-as-string doc "local-name(/root/node())"))))

(addtest (xpath-test)
  eval-expression-as-string-2
  (ensure-same "test value"
               (with-parse-document (doc "<root attr=\"test value\"/>")
                 (eval-expression-as-string doc "/root/@attr"))))

(addtest (xpath-test)
  eval-expression-as-string-3
  (ensure-same "value1"
               (with-parse-document (doc "<root attr1=\"value1\" attr2=\"value2\" />")
                 (eval-expression-as-string doc "/root/@*"))))

(addtest (xpath-test)
  eval-expression-as-string-4
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (eval-expression-as-string doc "/root/node()"))))

(addtest (xpath-test)
  eval-expression-as-string-5
  (ensure-same "value"
               (with-parse-document (doc "<root xmlns:my=\"www.sample.org\" my:attr=\"value\" />")
                 (eval-expression-as-string doc "/root/@my:attr" :ns-map '(("my" "www.sample.org"))))))

(addtest (xpath-test)
  eval-expression-as-string-6
  (ensure-null (with-parse-document (doc "<root xmlns:my=\"www.sample.org\" my:attr=\"value\" />")
                 (eval-expression-as-string doc "/root/@my:attr"))))

(addtest (xpath-test)
  eval-expression-as-string-7
  (ensure-null (with-parse-document (doc "<root xmlns:my=\"www.sample.org\" my:attr=\"value\" />")
                 (eval-expression-as-string doc "/root/@attr"))))

;;; eval-expressiong-as-number

(addtest (xpath-test)
  eval-expression-as-number-1
  (ensure-same 3.0d0
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (eval-expression-as-number doc "count(/root/node())"))))

(addtest (xpath-test)
  eval-expression-as-number-2
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (eval-expression-as-number doc "/root/node()"))))

;;; eval-expressiong-as-boolean

(addtest (xpath-test)
  eval-expression-as-boolean-1
  (ensure (with-parse-document (doc "<root attr=\"\" />")
            (eval-expression-as-boolean doc "/root/@*"))))

(addtest (xpath-test)
  eval-expression-as-boolean-2
  (ensure (with-parse-document (doc "<root><a /><b /><c /></root>")
            (eval-expression-as-boolean doc "/root/node()"))))

(addtest (xpath-test)
  eval-expression-as-boolean-3
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (eval-expression-as-boolean doc "/root/text()"))))

(addtest (xpath-test)
  eval-expression-as-boolean-4
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (eval-expression-as-boolean doc "/root/@*"))))

;;; in-nodeset

(addtest (xpath-test)
  in-nodeset-1
  (ensure-same '("root" "a" "b" "c")
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-result (res (doc "//node()"))
                   (iter (for node in-nodeset (xpath-result-value res))
                         (collect (local-name node)))))))

(addtest (xpath-test)
  in-nodeset-2
  (ensure-same '("a" "b" "c")
               (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-result (res ((root doc) "node()"))
                   (iter (for node in-nodeset (xpath-result-value res))
                         (collect (local-name node)))))))

(addtest (xpath-test)
  in-nodeset-3
  (ensure-null (with-parse-document (doc "<root><a /><b /><c /></root>")
                 (with-xpath-result (res ((root doc) "text()"))
                   (iter (for node in-nodeset (xpath-result-value res))
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
                           (eval-expression-as-string (root res) "text()")))))))

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
                           (eval-expression-as-string (root res) "text()")))))))

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
                           (eval-expression-as-string (root res) "text()")))))))
                          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-libxml2-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-libxml2-tests ()
  (run-tests :suite 'libxml2-test))
