;;; libxslt.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage :libxml2.xslt.test
  (:use :cl :iter :libxml2.tree :libxml2.xpath :libxml2.xslt :lift)
  (:export #:run-libxslt-tests))

(in-package :libxml2.xslt.test)

(register-exslt-extensions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; libxslt-test-suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite libxslt-test (libxml2.test:libxml2-test) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addtest (libxslt-test)
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
                   (with-transform-result (res (style (root doc)))
                     (list (local-name (root res))
                           (find-string (root res) "text()")))))))


(addtest (libxslt-test)
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
                   (with-transform-result (res (style doc))
                     (list (local-name (root res))
                           (find-string (root res) "text()")))))))

(addtest (libxslt-test)
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
                   (with-transform-result (res (style doc))
                     (list (local-name (root res))
                           (find-string (root res) "text()")))))))


(addtest (libxslt-test)
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
                   (with-transform-result (res (style doc))
                     (list (local-name (root res))
                           (find-string (root res) "text()")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom resolve test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addtest (libxslt-test)
  with-custom-resolvers-xsl-1
  (ensure-same '("result" "hello world")
               (with-custom-resolvers ((lambda (url id ctxt)
                                         (declare (ignore url id))
                                         (resolve-string "<node>hello world</node>" ctxt)))
                 (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">
    <xsl:template match=\"/\">
        <result>
            <xsl:copy-of select=\"document('data')\" />
        </result>
    </xsl:template>
</xsl:stylesheet>")
                   (with-parse-document (doc "<root/>")
                     (with-transform-result (res (style doc))
                       (list (local-name (root res))
                             (find-string (root res) "/result/node/text()"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use custom xpath function in xslt 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-xpath-function test-echo (msg)
  msg)

(addtest (libxslt-test)
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
                     (with-transform-result (res (style doc))
                       (iter (for node in-child-nodes (root res) with (:type :xml-element-node))
                             (collect (find-string node "text()")))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use custom xslt elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(deftestsuite custom-xslt-elements-test (libxml2-test) ())

;;; custom-xslt-element-1

(define-xslt-element msg-element (self input output)
  (let ((str (find-string input (attribute-value self "select"))))
    (when str
      (append-child output (make-text str)))))

(addtest (libxslt-test)
  custom-xslt-element-1
  (ensure-same "Hello world!"
               (with-xslt-elements ((msg-element "msg" "www.sample.org"))
                 (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:my=\"www.sample.org\"  extension-element-prefixes=\"my\" version=\"1.0\">
    <xsl:template match=\"/\">
        <xsl:variable name=\"var\">Hello world!</xsl:variable>
        <result><my:msg select=\"$var\" /></result>
    </xsl:template>
</xsl:stylesheet>")
                   (with-parse-document (doc "<root/>")
                     (with-transform-result (res (style doc))
                       (find-string res "/result/text()")))))))

(define-xslt-element my-copy-of (self input output)
   (iter (for node in-xpath-result (attribute-value self "select")  on input)
         (append-child output (copy node))))

;;; custom-xslt-element-2

(addtest (libxslt-test)
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
                     (with-transform-result (res (style doc))
                       (iter (for node in-child-nodes (root res) with (:type :xml-element-node))
                             (collect (local-name node)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-libxslt-tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-libxslt-tests ()
  (libxml2.test:run-libxml2-tests))
