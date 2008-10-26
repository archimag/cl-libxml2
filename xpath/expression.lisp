;; expression.lisp

(in-package #:libxml2.xpath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiled-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass compiled-expression (libxml2.tree::libxml2-cffi-object-wrapper) ())

(defmethod release ((expr compiled-expression))
  (%xmlXPathFreeCompExpr (pointer expr)))

(defun compile-expression (str)
  (let ((%expr (with-foreign-string (%str str)
                            (%xmlXPathCompile %str))))
    (unless (null-pointer-p %expr)
      (make-instance 'compiled-expression
                     :pointer %expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       (,kwd index from 0 to (1- (node-set-length ,nodeset)))
       (for ,var = (node-set-at ,nodeset index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xpath-result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwrapper xpath-result %xmlXPathObject)

(defmethod libxml2.tree::release/impl ((result xpath-result))
  (%xmlXPathFreeObject (pointer result)))

(defun xpath-result-type (res)
  (wrapper-slot-value res '%type))

(defun xpath-result-value (res)
  (case (xpath-result-type res)
    (:xpath-undefined nil)
    (:xpath-boolean (> (wrapper-slot-value res '%boolval) 0))
    (:xpath-number  (wrapper-slot-value res '%floatval))
    (:xpath-string (foreign-string-to-lisp (wrapper-slot-value res '%stringval)))
    (:xpath-nodeset (libxml2.tree::wrapper-slot-wrapper res '%nodesetval 'node-set))
    (otherwise nil)))

(defun xpath-result-node-count (res)
  (if (eql (xpath-result-type res) :xpath-nodeset)
      (foreign-slot-value (wrapper-slot-value res '%nodesetval)
                          '%xmlNodeSet
                          '%nodeNr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-xpath-epression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric eval-xpath-expression (node expr))

(defmethod eval-xpath-expression ((doc document) expr)
  (eval-xpath-expression (root doc) expr))

(defmethod eval-xpath-expression ((node node) (expr string))
  (let ((%ctxt (%xmlXPathNewContext (pointer (document node)))))
    (unwind-protect
         (progn 
           (setf (foreign-slot-value %ctxt
                                     '%xmlXPathContext
                                     '%node)
                 (pointer node))
           (let ((%object (with-foreign-string (%expr expr)
                            (%xmlXPathEvalExpression %expr %ctxt))))
             (unless (null-pointer-p %object)
               (make-instance 'xpath-result
                                     :pointer %object))))
      (%xmlXPathFreeContext %ctxt))))

(defmethod eval-xpath-expression ((node node) (expr compiled-expression))
  (let ((%ctxt (%xmlXPathNewContext (pointer (document node)))))
    (unwind-protect
         (setf (foreign-slot-value (pointer node)
                                   '%xmlXPathContext
                                   '%node)
               (pointer node))
         (let ((%object (%xmlXPathCompiledEval (pointer expr)
                                               %ctxt)))
           (unless (null-pointer-p %object)
             (make-instance 'xpath-resul
                            :pointer %object)))
      (%xmlXPathFreeContext %ctxt))))

(defmacro with-xpath-result ((res (obj expr)) &rest body)
  `(with-libxml2-object (,res (eval-xpath-expression ,obj ,expr)) ,@body))


(defmacro-driver (for var in-xpath-result expr on node)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (with res = (eval-xpath-expression ,node ,expr))
       (,kwd ,var in-nodeset (xpath-result-value res))
       (finally-protected (release res)))))

