;; attribute.lisp

(in-package #:libxml2.tree)

(defun attribute-value (element name &optional uri)
  (foreign-string-to-lisp 
   (with-foreign-string (%name name)
     (if uri
         (with-foreign-string (%uri uri)
           (%xmlGetNsProp (pointer element) %name %uri))
         (%xmlGetProp (pointer element) %name)))))


(defun set-attribute-value (element name &optional uri value)
  (with-foreign-strings ((%name name) (%value (or value uri)))
    (if value
        (let ((ns (or (search-ns-by-href element uri)
                      (make-ns element uri))))
          (%xmlSetNsProp (pointer element) (pointer ns) %name %value))
        (%xmlSetProp (pointer element) %name %value)))
  (or value uri))

(defsetf attribute-value set-attribute-value)

(defun remove-attribute (element name &optional uri)
  (let ((%attr (with-foreign-string (%name name)
                  (if uri
                      (with-foreign-string (%uri uri)
                        (%xmlHasNsProp (pointer element) %name %uri))
                      (%xmlHasProp (pointer element) %name)))))
    (unless (null-pointer-p %attr)
      (= 0 (%xmlRemoveProp %attr)))))
