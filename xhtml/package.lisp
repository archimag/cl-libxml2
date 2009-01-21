;; xhtml/package.lisp

(defpackage :libxml2.xhtml
  (:use :cl :cffi :libxml2.private :libxml2.tree)
  (:nicknames :xhtml)
  (:export :html-p
           :meta-encoding
           :serialize-html))