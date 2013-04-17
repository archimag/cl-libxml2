;;; packages.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:libxml2.xpath
  (:use #:cl #:cffi #:iter #:libxml2.private #:libxml2.tree #+sbcl #:sb-ext #:metabang.bind #:garbage-pools)
  (:nicknames #:xpath)
  (:export #:compiled-expression
           #:compile-expression
           #:with-compiled-expression
           #:node-set
           #:node-set-length
           #:node-set-at
   
           #:xpath-object
           #:xpath-object-type
           #:xpath-object-value
           #:with-xpath-object
           #:xpath-object-cast

           #:eval-expression

           #:find-string
           #:find-number
           #:find-boolean
           #:find-single-node
           #:find-list

           #:*default-ns-map*
           #:getpath

           #:xpath-parser-context
           #:with-xpath-functions
           #:define-xpath-function

           #:*lisp-xpath-functions*))
