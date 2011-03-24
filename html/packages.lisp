;;; html/package.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:libxml2.html
  (:use #:cl #:cffi #:libxml2.private #:libxml2.tree #:iter)
  (:nicknames #:html)
  (:export #:html-p
           #:parse-html
           #:with-parse-html
           #:parse-html-fragment
           #:with-parse-html-fragment
           #:meta-encoding
           #:serialize-html))