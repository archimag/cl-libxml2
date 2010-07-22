;;; html/package.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:libxml2.html
  (:use #:cl #:cffi #:libxml2.private #:libxml2.tree)
  (:nicknames #:html)
  (:export #:html-p
           #:parse-html
           #:with-parse-html
           #:meta-encoding
           #:serialize-html))