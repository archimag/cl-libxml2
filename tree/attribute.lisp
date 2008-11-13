;; attribute.lisp

(in-package #:libxml2.tree)

;;; attribute-value

(defun attribute-value (element name &optional uri)
  (foreign-string-to-lisp 
   (with-foreign-string (%name name)
     (if uri
         (with-foreign-string (%uri uri)
           (%xmlGetNsProp (pointer element) %name %uri))
         (%xmlGetNsProp (pointer element) %name (null-pointer))))))


(defun set-attribute-value (element name &optional uri value)
  (with-foreign-strings ((%name name) (%value (or value uri)))
    (if value
        (let ((ns (or (search-ns-by-href element uri)
                      (make-ns element uri))))
          (%xmlSetNsProp (pointer element) (pointer ns) %name %value))
        (%xmlSetNsProp (pointer element) (null-pointer) %name %value)))
  (or value uri))

(defsetf attribute-value set-attribute-value)

;;; attribute-node-value

(defun attribute-node-value (attr)
  (text-content (first-child attr)))

;;; remove-attribute

(defun remove-attribute (element name &optional uri)
  (let ((%attr (with-foreign-string (%name name)
                  (if uri
                      (with-foreign-string (%uri uri)
                        (%xmlHasNsProp (pointer element) %name %uri))
                      (%xmlHasNsProp (pointer element) %name (null-pointer))))))
    (unless (null-pointer-p %attr)
      (= 0 (%xmlRemoveProp %attr)))))

;;; with-attributes

(defmacro with-attributes ((&rest entries) element &body body)
  (alexandria:once-only (element)
    `(symbol-macrolet
	 ,(mapcar (lambda (entry)
		    (destructuring-bind (var name &optional uri)
			entry
		      `(,var (attribute-value ,element ,name ,uri))))
		  entries)
       ,@body)))

;;; iter (FOR (value name href)  IN-NODE-ATTRIBUTES node )

(defmacro-driver (for attr in-attributes node)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd %attr first (foreign-slot-value (pointer ,node) '%xmlNode '%properties) then (foreign-slot-value %attr '%xmlAttr '%next))
       (while (not (null-pointer-p %attr)))
       (for ,attr = (list (foreign-string-to-lisp 
                           (foreign-slot-value (foreign-slot-value %attr 
                                                                   '%xmlAttr 
                                                                   '%children) 
                                               '%xmlNode 
                                               '%content))
                          (foreign-string-to-lisp (foreign-slot-value %attr '%xmlAttr '%name))
                          (let ((%ns (foreign-slot-value %attr '%xmlAttr '%ns)))
                            (unless (null-pointer-p %ns)
                              (foreign-string-to-lisp (foreign-slot-value %ns
                                                                          '%xmlNs
                                                                          '%href)))))))))

;;; add-extra-namespace (element prefix uri)

(defun add-extra-namespace (element href prefix)
  (with-foreign-strings ((%href href) (%prefix prefix))
                         (%xmlNewNs (pointer element)
                                    %href
                                    %prefix)))

  