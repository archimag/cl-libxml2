;;; node-set.lisp

(in-package #:libxml2.xpath)

;;; node-set

(defwrapper node-set %xmlNodeSet)

;;; node-set-length

(defun node-set-length (nodeset)
  (foreign-slot-value (pointer nodeset)
                      '%xmlNodeSet
                      '%nodeNr))

;;; node-set-at

(defun node-set-at (nodeset index)
  (make-instance 'node
                 :pointer (mem-aref (wrapper-slot-value nodeset '%nodeTab)
                                    :pointer index)))

;;; ITER (FOR node IN-NODESET nodeset

(defmacro-driver (for var in-nodeset nodeset)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd index from 0 to (if ,nodeset
                                 (1- (node-set-length ,nodeset))
                                 -1))
       (for ,var = (if ,nodeset (node-set-at ,nodeset index))))))
