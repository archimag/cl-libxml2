;; expression.lisp

(in-package #:libxml2.xpath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiled-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass compiled-expression (libxml2.tree::libxml2-cffi-object-wrapper) ())

(defmethod libxml2.tree::release/impl ((expr compiled-expression))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-xpath-epression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-%context ((var node) &rest body)
  `(let ((,var (%xmlXPathNewContext (pointer (document ,node)))))
     (unwind-protect
          (progn
            (setf (foreign-slot-value %ctxt
                                     '%xmlXPathContext
                                     '%node)
                  (pointer node))
            ,@body)
       (%xmlXPathFreeContext %ctxt))))

(defgeneric eval-xpath-expression (node expr))

(defmethod eval-xpath-expression ((doc document) expr)
  (eval-xpath-expression (root doc) expr))

(defmethod eval-xpath-expression ((node node) (expr string))
  (with-%context (%ctxt node)
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (with-foreign-string (%expr expr)
                                                           (%xmlXPathEvalExpression %expr %ctxt))
                                                         'xpath-result)))

(defmethod eval-xpath-expression ((node node) (expr compiled-expression))
  (with-%context (%ctxt node)
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (%xmlXPathCompiledEval (pointer expr)
                                                                                %ctxt)
                                                         'xpath-result)))

(defmacro with-xpath-result ((res (obj expr)) &rest body)
  `(with-libxml2-object (,res (eval-xpath-expression ,obj ,expr)) ,@body))


(defmacro-driver (for var in-xpath-result expr on node)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (with res = (eval-xpath-expression ,node ,expr))
       (,kwd ,var in-nodeset (xpath-result-value res))
       (finally-protected (release res)))))

