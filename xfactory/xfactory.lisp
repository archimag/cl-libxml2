;;; xfactory.lisp

(in-package :libxml2.xfactory)

(defvar *node*)

(defgeneric call-marker (marker &rest args))

(build-elemnt ((html "www.sample.org" "html")
               (xlink "www.sample.org" "xlink"))
              (:html.html :xlink.href "hello"
                          (:html.body 

                    (loop 

(defmacro build-element ((&rest nsmap) tree)
  (declare (ignore nsmap))
  `(xtree:make-element (string-downcase (symbol-name ,(car tree)))))

    