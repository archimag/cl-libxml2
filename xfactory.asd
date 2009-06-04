;;; cl-libxml2-xfactory.asd

(defpackage :cl-libxml2-xfactory-system
  (:use :cl :asdf))

(in-package :cl-libxml2-xfactory-system)

(defsystem :cl-libxml2-xfactory
    :depends-on (#:cl-libxml2)
    :components ((:module :xfactory
                          :components ((:file "packages")
                                       (:file "xfactory" :depends-on ("packages"))))))