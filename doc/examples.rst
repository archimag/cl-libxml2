Code Examples
=============

cl-libxml2 binds C-library, thus resource management is necessary! Use
next functions to simplify your life:

* tree:release
* tree:with-parse-document
* tree:with-object
* `garbage-pools`_

Tree API
--------

**parse file**

.. code-block:: common-lisp

  (tree:release (tree:parse #P"example.xml"))

**parse string**

.. code-block:: common-lisp

  (tree:with-object (doc (tree:parse "<root />"))
      ...)

**with-parse-document**

.. code-block:: common-lisp
 
  (tree:with-parse-document (doc "<root />")
      ...)

**parse and register in garbage-pool**

.. code-block:: common-lisp

  (gp:with-garbage-pool ()
      (gp:register-object (tree:parse #u"http://www.example.org/test.xml")))

**serialize to file**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root />")
      (tree:serialize doc #P"out.xml"))

**serialize to string**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root />")
      (tree:serialize doc :to-string))
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <root/>
  "

**serialize to stream**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root />")
      (tree:serialize doc *standard-output*))

**make element (no namespace)**

.. code-block:: common-lisp

  (tree:release (make-element "root"))

**make element (with namespace, no prefix)**

.. code-block:: common-lisp

  (tree:release (make-element "root" "http://www.sample.org"))

**make element (with namespace, with prefix)**

.. code-block:: common-lisp

  (tree:release (make-element "root" "http://www.sample.org" "my"))

**attributes (no namespace)**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root />")
      (setf (tree:attribute-value (tree:root doc) "attr") "Hello")
      (tree:attribute-value (tree:root doc) "attr"))
  "Hello"

**attributes (with namesapce)**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root />")
      (setf (tree:attribute-value (tree:root doc) "attr" "www.sample.org") "Buy!")
      (tree:attribute-value (tree:root doc) "attr" "www.sample.org"))
  "Buy!"

**text nodes**

.. code-block:: common-lisp

  (tree:release (tree:make-text "Hello world!"))

**comment nodes**

.. code-block:: common-lisp

  (tree:release (tree:make-comment "It is a comment!"))

**process instructions**

.. code-block:: common-lisp

  (tree:release (tree:make-process-instruction "my-pi" "pi content"))

**iterate by child nodes**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root><a /><b /><c /></root>")
      (iter (for node in-child-nodes (tree:root doc) with (:type :xml-element-node))
            (collect (tree:local-name node))))
  ("a" "b" "c")

XPath API
---------

**find string**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root attr=\"Hello world!\" />")
      (xpath:find-string doc "/root/@attr"))
  "Hello world!"

**find number**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root><a /><b /><c /></root>")
      (xpath:find-number doc "count(/root/node())"))
  3.0d0

**iterate nodes in xpath query result**

.. code-block:: common-lisp

  (tree:with-parse-document (doc "<root><a /><b /><c /></root>")
      (iter (for node in-xpath-result "/root/node()" on doc)
            (collect (tree:local-name node))))
  ("a" "b" "c")

XSLT API
--------

**simple transformation**

.. code-block:: common-lisp

  (xslt:with-stylesheet (style "<?xml version=\"1.0\"?>
  <xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">
      <xsl:template match=\"/\">
          <result>
              <xsl:value-of select=\".\" />
          </result>
      </xsl:template>
  </xsl:stylesheet>")
    (tree:with-parse-document (doc "<root>Hello world</root>")
      (xslt:with-transform-result (res (style doc))
        (tree:serialize res :to-string))))
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <result>Hello world</result>
  "

**transformation with args**

.. code-block:: common-lisp

  (xslt:with-stylesheet (style "<?xml version=\"1.0\"?>
  <xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">
      <xsl:param name=\"arg1\" />
      <xsl:template match=\"/\">
          <result>
              <xsl:value-of select=\"concat($arg1, ' ', $arg2)\" />
          </result>
      </xsl:template>
  </xsl:stylesheet>")
    (xslt:stylesheet-set-param style "arg1" "Hello")
    (xslt:stylesheet-set-param style "arg2" "world")
    (tree:with-parse-document (doc "<root />")
      (xslt:with-transform-result (res (style doc))
        (tree:serialize res :to-string))))
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <result>Hello world</result>
  "

Extending cl-libxml2
--------------------

**Custom URL resolving and process-xinclude**

.. code-block:: common-lisp

  (tree:with-custom-resolvers ((lambda (url id ctxt)
                                 (declare (ignore id))
                                 (if (eql (puri:uri-scheme url) :my1)
                                     (tree:resolve-string "<node1 />" ctxt)))
                               (lambda (url id ctxt)
                                 (declare (ignore id))
                                 (if (eql (puri:uri-scheme url) :my2)
                                     (tree:resolve-string "<node2 />" ctxt))))
    (tree:with-parse-document (doc "<root xmlns:xi=\"http://www.w3.org/2001/XInclude\">
      <xi:include href=\"my1:doc\" />
      <xi:include href=\"my2:doc\" />
      <xi:include href=\"my3:doc\" />
  </root>")
      (tree:process-xinclude doc)
      (tree:serialize doc :to-string)))
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <root xmlns:xi=\"http://www.w3.org/2001/XInclude\">
      <node1/>
      <node2/>
      <xi:include href=\"my3:doc\"/>
  </root>
  "

**Custom URL resolving and XSLT-transformation**

.. code-block:: common-lisp

  (tree:with-custom-resolvers ((lambda (url id ctxt)
                                 (declare (ignore url id))
                                 (tree:resolve-string "<node>Hello world</node>" ctxt)))
  (xslt:with-stylesheet (style "<?xml version=\"1.0\"?>
  <xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">
      <xsl:template match=\"/\">
          <result>
              <xsl:copy-of select=\"document('data')\" />
          </result>
      </xsl:template>
  </xsl:stylesheet>")
    (tree:with-parse-document (doc "<root/>")
      (xslt:with-transform-result (res (style doc))
        (tree:serialize res :to-string)))))
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <result>
      <node>Hello world</node>
  </result>
  "

**XPath extension functions**

.. code-block:: common-lisp

  (xpath:define-xpath-function hello-world () "Hello world!")
  HELLO-WORLD

  (xpath:define-xpath-function echo (msg) msg)
  ECHO

  (xpath:define-xpath-function join (delimiter &rest strs)
    (iter (for str in strs)
          (reducing str
             by (lambda (s x) (concatenate 'string s delimiter x)))))
  JOIN

  (xpath:with-xpath-functions ((hello-world "hello-world")
                               (echo "echo")
                               (join "join"))
    (tree:with-parse-document (doc "<root />")
      (xpath:find-string doc "join('//', hello-world(), '---', echo('Buy!'))")))
  "Hello world!//---//Buy!"


**XSLT extension elements**

.. code-block:: common-lisp
  
  (xslt:define-xslt-element my-copy-of (self input output)
    (iter (for node in-xpath-result (tree:attribute-value self "select")  on input)
          (tree:append-child output (tree:copy node))))
  MY-COPY-OF

  (xslt:with-xslt-elements ((my-copy-of "copy-of" "www.sample.org"))
    (xslt:with-stylesheet (style "<?xml version=\"1.0\"?>
    <xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:my=\"www.sample.org\"  extension-element-prefixes=\"my\" version=\"1.0\">
      <xsl:template match=\"/root\">
          <result><my:copy-of select=\"node()[@attr]\" /></result>
      </xsl:template>
  </xsl:stylesheet>")
      (tree:with-parse-document (doc "<root><a attr=\"1\"/><b /><c attr=\"2\"/><d /></root>")
        (xslt:with-transform-result (res (style doc))
          (tree:serialize res :to-string)))))
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <result>
      <a attr=\"1\"/>
      <c attr=\"2\"/>
  </result>
  "

X-Factory system
----------------

.. code-block:: common-lisp

  (xtree:with-object (doc (xfactory:with-document-factory ((XUL "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul" "xul")
                                                           (SVG "http://www.w3.org/2000/svg" "svg"))
                            (xfactory:process-instruction "xml-stylesheet" "href=\"style.css\" type=\"text/css\"")
                            (XUL "window"
                                 (xfactory:namespace "http://www.w3.org/1999/xlink" "xlink")
                                 (XUL "vbox"
                                      (xfactory:attributes :flex 1
                                                           :width 500)
                                      (loop for i from 1 to 3
                                         do (XUL "label" 
                                                 (xfactory:text "text ~A" i)))
                                      (SVG "svg"
                                           (xfactory:attributes :width "500px"
                                                                :height "500px")
                                           (SVG "a"
                                                (xfactory:attributes "xlink:href" "http://www.w3.org")
                                                (SVG "rect"
                                                     (xfactory:attributes :x 100
                                                                          :y 100
                                                                          :width 300
                                                                          :height 200
                                                                          :fill "red"))))))))
    (xtree:serialize doc *standard-output* :pretty-print t))
  ==>
  <?xml version="1.0" encoding="utf-8"?>
  <?xml-stylesheet href="style.css" type="text/css"?>
  <xul:window xmlns:xul="http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul" xmlns:xlink="http://www.w3.org/1999/xlink">
    <xul:vbox flex="1" width="500">
      <xul:label>text 1</xul:label>
      <xul:label>text 2</xul:label>
      <xul:label>text 3</xul:label>
      <svg:svg xmlns:svg="http://www.w3.org/2000/svg" width="500px" height="500px">
        <svg:a xlink:href="http://www.w3.org">
          <svg:rect x="100" y="100" width="300" height="200" fill="red"/>
        </svg:a>
      </svg:svg>
    </xul:vbox>
  </xul:window>


.. _garbage-pools: http://code.google.com/p/garbage-pools/
