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

(defmacro with-compiled-expression ((var expr) &rest body)
  `(with-libxml2-object (,var (compile-expression ,expr)) ,@body))
     

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
       (,kwd index from 0 to (if ,nodeset
                                 (1- (node-set-length ,nodeset))
                                 -1))
       (for ,var = (if ,nodeset (node-set-at ,nodeset index))))))

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
;; *default-ns-map*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-ns-map*
  (list '("html" "http://www.w3.org/1999/xhtml")
        '("xlink" "http://www.w3.org/1999/xlink")
        '("xsl" "http://www.w3.org/1999/XSL/Transform")
        '("svg" "http://www.w3.org/2000/svg")
        '("xul" "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-%context ((var doc node ns-map) &rest body)  
  `(let ((,var (%xmlXPathNewContext (pointer ,doc))))
     #+sbcl(declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     (unwind-protect
          (progn
            (if ,node
                (setf (foreign-slot-value %ctxt
                                          '%xmlXPathContext
                                          '%node)
                      (pointer ,node)))
            (if ,ns-map
                (iter (for (prefix uri) in ,ns-map)
                      (with-foreign-strings ((%prefix prefix) (%uri uri))
                        (%xmlXPathRegisterNs ,var
                                             %prefix
                                             %uri))))
            ,@body)
       (%xmlXPathFreeContext %ctxt))))

(defgeneric eval-expression (node expr &key ns-map))

(defmethod eval-expression ((doc document) expr &key (ns-map *default-ns-map*))
  (with-%context (%ctxt doc nil ns-map)    
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (with-foreign-string (%expr expr)
                                                           (%xmlXPathEvalExpression %expr %ctxt))
                                                         'xpath-result)))

(defmethod eval-expression ((node node) (expr string) &key (ns-map *default-ns-map*))
  (with-%context (%ctxt (document node) node ns-map)
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (with-foreign-string (%expr expr)
                                                           (%xmlXPathEvalExpression %expr %ctxt))
                                                         'xpath-result)))

(defmethod eval-expression ((node node) (expr compiled-expression) &key (ns-map *default-ns-map*))
  (with-%context (%ctxt (document node) node ns-map)
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (%xmlXPathCompiledEval (pointer expr)
                                                                                %ctxt)
                                                         'xpath-result)))

;;; with-xpath-result
(defmacro with-xpath-result ((res (obj expr &optional (ns-map '*default-ns-map*))) &rest body)
  `(let ((,res (eval-expression ,obj ,expr :ns-map ,ns-map)))
     (unwind-protect
          (progn ,@body)
       (if ,res (release ,res)))))


(defmacro-driver (for var in-xpath-result expr on node &optional with-ns-map (ns-map '*default-ns-map*))
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (with res = (eval-expression ,node ,expr :ns-map ,ns-map))
       (,kwd ,var in-nodeset (if res (xpath-result-value res)))
       (finally-protected (if res (release res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-expression-as-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defun eval-expression-as-string (obj expr &key (ns-map *default-ns-map*))
(defun find-string (obj expr &key (ns-map *default-ns-map*))
  (flet ((nil-is-empty (str)
           (unless (string= str "") str)))
    (let ((%str (with-xpath-result (res (obj expr ns-map))
                  (if res (%xmlXPathCastToString (pointer res))))))
      (if %str
          (unwind-protect
               (nil-is-empty (foreign-string-to-lisp %str))
            (libxml2.tree::%xmlFree %str))))))

(defun find-number (obj expr &key (ns-map *default-ns-map*))
  (let ((val (with-xpath-result (res (obj expr ns-map))
               (if res (%xmlXPathCastToNumber (pointer res))))))
    #+:IEEE-FLOATING-POINT(if (and val (not (float-nan-p val))) val)
     #-:IEEE-FLOATING-POINT val))
        

(defun find-boolean (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-result (res (obj expr ns-map))
    (if res (not (= 0 (%xmlXPathCastToBoolean (pointer res)))))))

(defun find-single-node (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-result (res (obj expr ns-map))
    (if (and (eql (xpath-result-type res) :xpath-nodeset)
             (> (node-set-length (xpath-result-value res)) 0))
        (node-set-at (xpath-result-value res) 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getpath
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getpath (node)
  (gp:with-garbage-pool ()
    (cffi:foreign-string-to-lisp (gp:cleanup-register (%xmlGetNodePath (pointer node)) 'libxml2.tree::%xmlFree))))