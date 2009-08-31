;;; xoverlay.asd

(defpackage :xoverlay-system
  (:use :cl :asdf))

(in-package :xoverlay-system)

(defsystem :xoverlay
    :depends-on (#:cl-libxml2)
    :components ((:module :xoverlay
                          :components ((:file "packages")
                                       (:file "xoverlay" :depends-on ("packages"))))))

