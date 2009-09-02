;;;; xoverlay.lisp

(in-package :libxml2.xoverlay)

(defgeneric apply-overlay (origin overlay &key html))

(defun apply-overlay/impl (master overlay)
  (if master
      (iter (for child in-child-nodes overlay)
            (cond 
              ((string= (xtree:attribute-value child "asfirst") "true") 
               (xtree:insert-child-before (xtree:copy child)
                                          (xtree:first-child master)))
               (t (xtree:append-child master (xtree:copy child)))))))

(defmethod apply-overlay ((origin xtree:document) (overlay xtree:document) &key html)
  (xtree:process-xinclude origin (xtree:parse-options :xml-parse-noxincnode :xml-parse-nobasefix))
  (xtree:process-xinclude overlay (xtree:parse-options :xml-parse-noxincnode :xml-parse-nobasefix))
  (iter (for child in-child-nodes (xtree:root overlay) with (:type :xml-element-node))
        (apply-overlay/impl (or (if (and html
                                         (string= "head" (xtree:local-name child)))
                                    (xtree:find-node (xtree:first-child (xtree:root origin))
                                                     (xtree:node-filter :local-name "head")))
                                (let ((id (xtree:attribute-value child "id")))
                                  (if id
                                      (xpath:find-single-node origin
                                                              (format nil "//*[@id='~A']" id)))))
                            child))
  origin)


(defmethod apply-overlay ((origin xtree:document) overlay &key html)
  (xtree:with-parse-document (doc overlay)
    (apply-overlay origin
                   overlay
                   :html html)))

(defmethod apply-overlay (origin (overlay xtree:document) &key html)
  (apply-overlay (xtree:parse origin)
                 overlay
                 :html html))

(defmethod apply-overlay (origin overlay &key html)
  (xtree:with-parse-document (overlay-doc overlay)
    (apply-overlay (xtree:parse origin)
                   overlay-doc
                   :html html)))


