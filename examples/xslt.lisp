;;; xslt.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.xslt)

;;; custom resolving

(defun my-resolve (url id ctxt)
  (declare (ignore url id))
  (resolve-string "<node />" ctxt))


(with-custom-resolvers (#'my-resolve)
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
        (serialize res :to-string)))))


;;; custom xpath functions

(libxml2.xpath:define-xpath-function echo (msg)
  msg)

(libxml2.xpath:with-xpath-functions ((echo "echo" "www.sample.org"))
  (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:my=\"www.sample.org\"  version=\"1.0\">
    <xsl:template match=\"/\">
        <result>
            <node><xsl:value-of select=\"my:echo('Hello world')\" /></node>
        </result>
    </xsl:template>
</xsl:stylesheet>")
    (with-parse-document (doc "<root/>")
      (with-transform-result (res (style doc))
        (serialize res :to-string)))))


;;; custom elements

(define-xslt-element copy-of (self input output)
   (iter (for node in-xpath-result (attribute-value self "select")  on input)
         (append-child output (copy node))))

(with-xslt-elements ((copy-of "copy-of" "www.sample.org"))
  (with-stylesheet (style "<?xml version=\"1.0\"?>
<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:my=\"www.sample.org\"  extension-element-prefixes=\"my\" version=\"1.0\">
    <xsl:template match=\"/root\">
        <result><my:copy-of select=\"node()[@attr]\" /></result>
    </xsl:template>
</xsl:stylesheet>")
    (with-parse-document (doc "<root><a attr=\"1\"/><b /><c attr=\"2\"/><d /></root>")
      (with-transform-result (res (style doc))
        (serialize res :to-string)))))
