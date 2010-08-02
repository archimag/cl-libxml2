Quick Start
===========

Parsing
-------

Parsing string to tree:

.. code-block:: common-lisp

  (xtree:parse "<root attr=\"Hello world!\"><a /><b /><c /></root>")
  #<LIBXML2.TREE:DOCUMENT {BE2C8F1}>

Save result for later:

.. code-block:: common-lisp

  (defparameter *doc* *)
  *DOC*

Inspect tree
------------

Root element (documentElement):

.. code-block:: common-lisp

  (xtree:root *doc*)
  #<LIBXML2.TREE:NODE {C093499}>

Local name of the node (tagName):

.. code-block:: common-lisp

  (xtree:local-name (xtree:root *doc*))
  "root"

Attribute value:

.. code-block:: common-lisp

  (xtree:attribute-value (xtree:root *doc*) "attr")
  "Hello world!"

Enumerate child nodes:

.. code-block:: common-lisp

  (use-package :iter)
  T
  (iter (for node in-child-nodes (xtree:root *doc*) with (:type :xml-element-node))
        (collect (xtree:local-name node)))
  ("a" "b" "c")

Modify tree
-----------

Change attribute value:

.. code-block:: common-lisp

  (setf (xtree:attribute-value (xtree:root *doc*) "attr") "Buy!")
  "Buy!"

Add new attribute:

.. code-block:: common-lisp

  (setf (xtree:attribute-value (xtree:root *doc*) "test") "new attribute")
  "new attribute"

Append child nodes:

.. code-block:: common-lisp

  (iter (for x from 1 to 3)
        (setf (xtree:text-content (xtree:append-child (xtree:root *doc*)
                                                      (xtree:make-element "item")))
                                  (format nil "Text ~A" x)))        

Serialization
-------------

Serialize to string:

.. code-block:: common-lisp

  (xtree:serialize *doc* :to-string)
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <root attr=\"Buy!\" test=\"new attribute\">
      <a/>
      <b/>
      <c/>
      <item>Text 1</item>
      <item>Text 2</item>
      <item>Text 3</item>
  </root>
  "

Serialize to file:

.. code-block:: common-lisp

  (xtree:serialize *doc* #P"/tmp/out.xml")

Free memory
-----------

.. code-block:: common-lisp

  (xtree:release *doc*)
  NIL

To simlify memory management, use

* xtree:with-parse-document
* xtree:with-libxml2-object
* `garbage-pools`_

.. code-block:: common-lisp

  (gp:with-garbage-pool ()
    (let ((doc (gp:object-register (xtree:parse ...))))
      (using-document doc)))

HTML
----

.. code-block:: common-lisp

  (html:with-parse-html (html "<a href=\"http://www.xmlsoft.org/\">libxml2</a> is a <b>great</b> library")
     (html:serialize-html html *standard-output*))
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">
  <html><body>
  <a href=\"http://www.xmlsoft.org/\">libxml2</a> is a <b>great</b> library</body></html>
  "


.. _garbage-pools: http://code.google.com/p/garbage-pools/
