;;; entities.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.tree)

(define-libxml2-function ("xmlEncodeEntitiesReentrant" %xmlEncodeEntitiesReentrant) %xmlCharPtr
  (doc %xmlDocPtr)
  (input %xmlCharPtr))

(defun encode-entitites (doc str)
  (with-foreign-string (%str str)
    (let ((%result (%xmlEncodeEntitiesReentrant (pointer doc)
                                                %str)))
      (unwind-protect
           (foreign-string-to-lisp %result)
        (%xmlFree %result)))))

(define-libxml2-function ("xmlEncodeSpecialChars" %xmlEncodeSpecialChars) %xmlCharPtr
  (doc %xmlDocPtr)
  (input %xmlCharPtr))

(defun encode-special-chars (doc str)
  (with-foreign-string (%str str)
    (let ((%result (%xmlEncodeSpecialChars (pointer doc)
                                           %str)))
      (unwind-protect
           (foreign-string-to-lisp %result)
        (%xmlFree %result)))))

