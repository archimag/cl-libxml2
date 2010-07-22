;;;; package.lisp

(defpackage #:libxml2.xoverlay
  (:use #:cl #:iter)
  (:nicknames #:xoverlay)
  (:export #:apply-overlay))