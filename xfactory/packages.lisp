;;; packages.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:libxml2.xfactory
  (:nicknames #:xfactory)
  (:use #:cl #:iter)
  (:export #:with-element-factory
           #:with-document-factory
           #:namespace
           #:attributes
           #:text
           #:process-instruction
           #:*node*))