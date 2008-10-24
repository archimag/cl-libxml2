;; xpath-low-level.lisp

(in-package #:libxml2.xpath)

(defctype %xmlXPathContextPtr :pointer)

(defctype %xmlXPathAxisPtr :pointer)
(defctype %xmlXPathTypePtr :pointer)

(defctype %xmlXPathVariableLookupFunc :pointer)
(defctype %xmlStructuredErrorFunc :pointer)
(defctype %xmlXPathVariableLookupFunc :pointer)
(defctype %xmlXPathFuncLookupFunc :pointer)
(defctype %xmlStructuredErrorFunc :pointer)

(defctype %xmlXPathObjectPtr :pointer)
(defctype %xmlXPathCompExprPtr :pointer)


(defcenum %xmlErrorLevel
  (:xml-err-none  0)
  ;; A simple warning
  (:xml-err-warning 1) 
  ;; A recoverable error
  (:xml-err-error 2) 
  ;; A fatal error
  (:xml-err-fatal 3))


;;struct xmlError
(defcstruct %xmlError
  ;; int	domain	: What part of the library raised this er
  (%domain :int)
  ;; int	code	: The error code, e.g. an xmlParserError
  (%code :int)
  ;; char *	message	: human-readable informative error messag
  (%message %charPtr)
  ;; xmlErrorLevel	level	: how consequent is the error
  (%level %xmlErrorLevel)
  ;; char *	file	: the filename
  (%file %charPtr)
  ;; int	line	: the line number if available
  (%line :int)
  ;; char *	str1	: extra string information
  (%str1 %charPtr)
  ;; char *	str2	: extra string information
  (%str2 %charPtr)
  ;; char *	str3	: extra string information
  (%str3 %charPtr)
  ;; int	int1	: extra number information
  (%int1 :int)
  ;; int	int2	: column number of the error or 0 if N/A
  (%int2 :int)
  ;; void *	ctxt	: the parser context if available
  (%ctxt :pointer)
  ;; void *	node	: the node in the tree
  (%node :pointer))


;;struct xmlXPathContext
(defcstruct %xmlXPathContext 
  ;; xmlDocPtr	doc	: The current document
  (%doc %xmlDocPtr)
  ;; xmlNodePtr	node	: The current node
  (%node	%xmlNodePtr)
  ;; int	nb_variables_unused	: unused (hash table)
  (%nb_variables_unused :int)
  ;; int	max_variables_unused	: unused (hash table)
  (%max_variables_unused :int)
  ;; xmlHashTablePtr	varHash	: Hash table of defined variables
  (%varHash	%xmlHashTablePtr)
  ;; int	nb_types	: number of defined types
  (%nb_types :int)
  ;; int	max_types	: max number of types
  (%max_types :int)
  ;; xmlXPathTypePtr	types	: Array of defined types
  (%types	%xmlXPathTypePtr)
  ;; int	nb_funcs_unused	: unused (hash table)
  (%nb_funcs_unused :int)
  ;; int	max_funcs_unused	: unused (hash table)
  (%max_funcs_unused :int)
  ;; xmlHashTablePtr	funcHash	: Hash table of defined funcs
  (%funcHash	%xmlHashTablePtr)
  ;; int	nb_axis	: number of defined axis
  (%nb_axis :int)
  ;; int	max_axis	: max number of axis
  (%max_axis :int)
  ;; xmlXPathAxisPtr	axis	: Array of defined axis the namespace nod
  (%axis	%xmlXPathAxisPtr)
  ;; xmlNsPtr *	namespaces	: Array of namespaces
  (%namespaces %xmlNsPtr)
  ;; int	nsNr	: number of namespace in scope
  (%nsNr :int)
  ;; void *	user	: function to free extra variables
  (%user :pointer)
  ;; int	contextSize	: the context size
  (%contextSize :int)
  ;; int	proximityPosition	: the proximity position extra stuff for
  (%proximityPosition :int)
  ;; int	xptr	: is this an XPointer context?
  (%xptr :int)
  ;; xmlNodePtr	here	: for here()
  (%here	%xmlNodePtr)
  ;; xmlNodePtr	origin	: for origin() the set of namespace decla
  (%origin	%xmlNodePtr)
  ;; xmlHashTablePtr	nsHash	: The namespaces hash table
  (%nsHash	%xmlHashTablePtr)
  ;; xmlXPathVariableLookupFunc	varLookupFunc	: variable lookup func
  (%varLookupFunc	%xmlXPathVariableLookupFunc)
  ;; void *	varLookupData	: variable lookup data Possibility to lin
  (%varLookupData :pointer)
  ;; void *	extra	: needed for XSLT The function name and U
  (%extra :pointer)
  ;; const xmlChar *	function
  (%function %xmlCharPtr)
  ;; const xmlChar *	functionURI	: function lookup function and data
  (%functionURI %xmlCharPtr)
  ;; xmlXPathFuncLookupFunc	funcLookupFunc	: function lookup func
  (%funcLookupFunc	%xmlXPathFuncLookupFunc)
  ;; void *	funcLookupData	: function lookup data temporary namespac
  (%funcLookupData :pointer)
  ;; xmlNsPtr *	tmpNsList	: Array of namespaces
  (%tmpNsList %xmlNsPtr)
  ;; int	tmpNsNr	: number of namespaces in scope error rep
  (%tmpNsNr :int)
  ;; void *	userData	: user specific data block
  (%userData :pointer)
  ;; xmlStructuredErrorFunc	error	: the callback in case of errors
  (%error	%xmlStructuredErrorFunc)
  ;; xmlError	lastError	: the last error
  (%lastError	%xmlError)
  ;; xmlNodePtr	debugNode	: the source node XSLT dictionary
  (%debugNode	%xmlNodePtr)
  ;; xmlDictPtr	dict	: dictionary if any
  (%dict	%xmlDictPtr)
  ;; int	flags	: flags to control compilation Cache for
  (%flags :int)
  ;; void *	cache
  (%cache :pointer))

;; enum xmlXPathObjectType
(defcenum %xmlXPathObjectType
  (:xpath-undefined  0)
  (:xpath-nodeset  1)
  (:xpath-boolean  2)
  (:xpath-number  3)
  (:xpath-string  4)
  (:xpath-point  5)
  (:xpath-range  6)
  (:xpath-locationset  7)
  (:xpath-users  8)
  (:xpath-xslt-tree 9))

;;struct xmlNodeSet
(defcstruct %xmlNodeSet
  ;; int	nodeNr	: number of nodes in the set
  (%nodeNr :int)
  ;; int	nodeMax	: size of the array as allocated
  (%nodeMax :int)
  ;; xmlNodePtr *	nodeTab	: array of nodes in no particular order @
  (%nodeTab %xmlNodePtr))

;;struct xmlXPathObject
(defcstruct %xmlXPathObject 
  (%type %xmlXPathObjectType)
  (%nodesetval %xmlNodeSetPtr)
  (%boolval :int)
  (%floatval :double)
  (%stringval %xmlCharPtr)
  (%user :pointer)
  (%index :int)
  (%user2 :pointer)
  (%index2 :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("xmlXPathNewContext" %xmlXPathNewContext) %xmlXPathContextPtr
  (doc %xmlDocPtr))

(defcfun ("xmlXPathEvalExpression" %xmlXPathEvalExpression) %xmlXPathObjectPtr
  (str %xmlCharPtr)
  (ctxt %xmlXPathContextPtr))

(defcfun ("xmlXPathCompile" %xmlXPathCompile) %xmlXPathCompExprPtr
  (str %xmlCharPtr))

(defcfun ("xmlXPathCompiledEval" %xmlXPathCompiledEval) %xmlXPathObjectPtr
  (comp %xmlXPathCompExprPtr)
  (ctxt %xmlXPathContextPtr))

(defcfun ("xmlXPathFreeCompExpr" %xmlXPathFreeCompExpr) :void
  (comp %xmlXPathCompExprPtr))


(defcfun ("xmlXPathFreeContext" %xmlXPathFreeContext) :void
  (ctxt %xmlXPathContextPtr))
