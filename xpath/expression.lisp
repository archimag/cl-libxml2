;;; expression.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.xpath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiled-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass compiled-expression (libxml2.tree::libxml2-cffi-object-wrapper) ())

(defctype %xmlXPathCompExprPtr :pointer)

(define-libxml2-function ("xmlXPathFreeCompExpr" %xmlXPathFreeCompExpr) :void
  (comp %xmlXPathCompExprPtr))

(defmethod libxml2.tree::release/impl ((expr compiled-expression))
  (%xmlXPathFreeCompExpr (pointer expr)))

;;; compile-expression

(define-libxml2-function ("xmlXPathCompile" %xmlXPathCompile) %xmlXPathCompExprPtr
  (str %xmlCharPtr))

(defun compile-expression (str)
  (let ((%expr (with-foreign-string (%str str)
                            (%xmlXPathCompile %str))))
    (unless (null-pointer-p %expr)
      (make-instance 'compiled-expression
                     :pointer %expr))))

;;; with-compiled-expression

(defmacro with-compiled-expression ((var expr) &body body)
  `(with-libxml2-object (,var (compile-expression ,expr)) ,@body))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *default-ns-map*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-ns-map*
  (list '("html" "http://www.w3.org/1999/xhtml")
        '("xlink" "http://www.w3.org/1999/xlink")
        '("xsl" "http://www.w3.org/1999/XSL/Transform")
        '("svg" "http://www.w3.org/2000/svg")
        '("xul" "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eval-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric eval-expression (node expr &key ns-map))


;;; eval-expression ((doc document) expr &key ns-map

(define-libxml2-function ("xmlXPathEvalExpression" %xmlXPathEvalExpression) %xmlXPathObjectPtr
  (str %xmlCharPtr)
  (ctxt %xmlXPathContextPtr))

(defmethod eval-expression ((doc document) expr &key (ns-map *default-ns-map*))
  (with-%context (%ctxt doc nil ns-map)    
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (with-foreign-string (%expr expr)
                                                           (%xmlXPathEvalExpression %expr %ctxt))
                                                         'xpath-object)))

;;; eval-expression ((node node) (expr string) &key ns-map

(defmethod eval-expression ((node node) (expr string) &key (ns-map *default-ns-map*))
  (with-%context (%ctxt (document node) node ns-map)
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (with-foreign-string (%expr expr)
                                                           (%xmlXPathEvalExpression %expr %ctxt))
                                                         'xpath-object)))

;;; eval-expression ((node node) (expr compiled-expression) &key ns-map

(define-libxml2-function ("xmlXPathCompiledEval" %xmlXPathCompiledEval) %xmlXPathObjectPtr
  (comp %xmlXPathCompExprPtr)
  (ctxt %xmlXPathContextPtr))

(defmethod eval-expression ((node node) (expr compiled-expression) &key (ns-map *default-ns-map*))
  (with-%context (%ctxt (document node) node ns-map)
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (%xmlXPathCompiledEval (pointer expr)
                                                                                %ctxt)
                                                         'xpath-object)))

;;; iter (for node in-xpath-result expr on node)

(defmacro-driver (for var in-xpath-result expr on node &optional with-ns-map (ns-map '*default-ns-map*))
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (with res = (eval-expression ,node ,expr :ns-map ,ns-map))
       (,kwd ,var in-nodeset (if res (xpath-object-value res)))
       (finally-protected (if res (release res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-string (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-object (res (obj expr ns-map))
    (if res (xpath-object-cast res 'string))))

(defun find-number (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-object (res (obj expr ns-map))
    (xpath-object-cast res 'number)))

(defun find-boolean (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-object (res (obj expr ns-map))
    (if res (xpath-object-cast res 'boolean))))

(defun find-single-node (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-object (res (obj expr ns-map))
    (if (and res
             (eql (xpath-object-type res) :xpath-nodeset)
             (xpath-object-value res)
             (> (node-set-length (xpath-object-value res)) 0))
        (node-set-at (xpath-object-value res) 0))))

(defun find-list (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-object (res (obj expr ns-map))
    (if (and res
             (eql (xpath-object-type res) :xpath-nodeset)
             (xpath-object-value res))
        (iter (for node in-nodeset (xpath-object-value res))
              (collect node)))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getpath
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlGetNodePath" %xmlGetNodePath) :pointer
  (node %xmlNodePtr))

(defun getpath (node)
  (with-garbage-pool ()
    (cffi:foreign-string-to-lisp (cleanup-register (%xmlGetNodePath (pointer node)) 'libxml2.tree::%xmlFree))))
