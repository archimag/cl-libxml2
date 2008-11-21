;; resolving.lisp

(in-package #:libxml2.tree)

(defun my-resolve-1 (url id ctxt)
  (declare (ignore url id))
  (resolve-string "<node />" ctxt))

(defun my-resolve-2 (url id ctxt)
  (declare (ignore id))
  (if (eql (puri:uri-scheme url) :my)
      (resolve-string (format nil "<node>~A</node>" url) ctxt)))

(with-custom-resolvers (#'my-resolve-2 #'my-resolve-1)
  (with-parse-document (doc "<root xmlns:xi=\"http://www.w3.org/2001/XInclude\">
    <xi:include href=\"my:doc\" />
    <xi:include href=\"my2:doc\" />
</root>")
    (process-xinclude doc)
    (serialize doc :to-string)))