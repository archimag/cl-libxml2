;; html/package.lisp

(defpackage :libxml2.html
  (:use :cl :cffi :libxml2.private :libxml2.tree)
  (:nicknames :html)
  (:export :html-p
           :parse-html
           :with-parse-html
           :meta-encoding
           :serialize-html))