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

(defvar *lisp-xpath-functions*)

(defmacro with-%context ((var doc node ns-map) &rest body)  
  `(let ((,var (%xmlXPathNewContext (pointer ,doc))))
     #+sbcl(declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     (unwind-protect
          (progn
            (if (boundp 'libxml2.xpath::*lisp-xpath-functions*)
                (gp:with-garbage-pool ()
                  (iter (for (func name ns) in *lisp-xpath-functions*)
                        (%xmlXPathRegisterFuncNS ,var
                                                 (gp:cleanup-register (foreign-string-alloc (eval name))
                                                                      #'foreign-string-free)
                                                 (if ns
                                                     (gp:cleanup-register (foreign-string-alloc (eval ns))
                                                                          #'foreign-string-free)
                                                     (null-pointer))
                                                 (get-callback func)))))
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
                                                         'xpath-object)))

(defmethod eval-expression ((node node) (expr string) &key (ns-map *default-ns-map*))
  (with-%context (%ctxt (document node) node ns-map)
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (with-foreign-string (%expr expr)
                                                           (%xmlXPathEvalExpression %expr %ctxt))
                                                         'xpath-object)))

(defmethod eval-expression ((node node) (expr compiled-expression) &key (ns-map *default-ns-map*))
  (with-%context (%ctxt (document node) node ns-map)
    (libxml2.tree::make-libxml2-cffi-object-wrapper/impl (%xmlXPathCompiledEval (pointer expr)
                                                                                %ctxt)
                                                         'xpath-object)))

(defmacro-driver (for var in-xpath-object expr on node &optional with-ns-map (ns-map '*default-ns-map*))
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (with res = (eval-expression ,node ,expr :ns-map ,ns-map))
       (,kwd ,var in-nodeset (if res (xpath-object-value res)))
       (finally-protected (if res (release res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-string (obj expr &key (ns-map *default-ns-map*))
  (flet ((nil-is-empty (str)
           (unless (string= str "") str)))
    (let ((%str (with-xpath-object (res (obj expr ns-map))
                  (if res (%xmlXPathCastToString (pointer res))))))
      (if %str
          (unwind-protect
               (nil-is-empty (foreign-string-to-lisp %str))
            (libxml2.tree::%xmlFree %str))))))

(defun find-number (obj expr &key (ns-map *default-ns-map*))
  (let ((val (with-xpath-object (res (obj expr ns-map))
               (if res (%xmlXPathCastToNumber (pointer res))))))
    #+:IEEE-FLOATING-POINT(if (and val (not (float-nan-p val))) val)
     #-:IEEE-FLOATING-POINT val))
        

(defun find-boolean (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-object (res (obj expr ns-map))
    (if res (not (= 0 (%xmlXPathCastToBoolean (pointer res)))))))

(defun find-single-node (obj expr &key (ns-map *default-ns-map*))
  (with-xpath-object (res (obj expr ns-map))
    (if (and (eql (xpath-object-type res) :xpath-nodeset)
             (> (node-set-length (xpath-object-value res)) 0))
        (node-set-at (xpath-object-value res) 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getpath
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getpath (node)
  (gp:with-garbage-pool ()
    (cffi:foreign-string-to-lisp (gp:cleanup-register (%xmlGetNodePath (pointer node)) 'libxml2.tree::%xmlFree))))