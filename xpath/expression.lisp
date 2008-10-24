;; expression.lisp

(in-package #:libxml2.xpath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiled-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass compiled-expression (libxml2.tree::libxml2-cffi-object-wrapper) ())

(defmethod release ((expr compiled-expression))
  (%xmlXPathFreeCompExpr (pointer expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xpath-result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defwrapper xpath-result %xmlXPathObject)

(defclass xpath-result ()
  ((context :initarg :context :reader xpath-context)
   (xpath-object :initarg :xpath-object :reader xpath-object)))

(defmethod release ((result xpath-result))
  (%xmlXPathFreeContext (xpath-context result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric eval-xpath-epression (node expr))

;; (defmethod eval-xpath-epression ((node node) (expr string))
;;   (let ((%context (%xmlXPathNewContext (document node))))