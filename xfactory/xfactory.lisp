;;; xfactory.lisp

(in-package :libxml2.xfactory)

(defvar *node*)

(defvar *makers*)

(defun tagname (tag)
  (if (keywordp tag)
      (string-downcase (symbol-name tag))
      tag))

;;; xmaker

(defclass xmaker () ())

(defgeneric tree-to-commands/impl (maker-symbol maker-class args))

(defun tree-to-commands (tree)
  (let ((maker-info (if (and (consp tree)
                             (symbolp (car tree)))
                        (assoc (car tree)
                               *makers*))))
    (cond (maker-info (tree-to-commands/impl (car maker-info)
                                             (cdr maker-info)
                                             (cdr tree)))
          ((atom tree) tree)
          (t (iter (for child in tree)
                   (collect (tree-to-commands child))))))))

;;; element-maker

(defclass element-maker ()
  ((namespace :initarg :namespace :initform nil :reader maker-namespace)
   (prefix :initarg :prefix :initform nil :reader maker-prefix)))


(defmethod tree-to-commands/impl (maker-symbol (maker-class (eql 'element-maker)) args)
  (let ((tag (car args))
        (childs (if (cdr args)
                    (tree-to-commands (cdr args)))))
    `(let ((el (xtree:make-element (tagname ,tag)
                                   (maker-namespace ,maker-symbol)
                                   (maker-prefix ,maker-symbol))))
       (let ((*node* el))
          ,@childs)
       (if (boundp '*node*)
           (xtree:append-child *node*
                               el)
           el))))

;;; attribute-maker

(defclass attribute


;;; xfactory
    
(defmacro xfactory ((&rest makers) &body body)
  (let ((*makers* (iter (for maker in makers)
                        (collect (cons (first maker)
                                       (second maker))))))    
    (let ((vars (iter (for maker in makers)
                      (collect `(,(car maker) (make-instance
                                               (quote ,(second maker))
                                               ,@(cddr maker))))))
          (commands (tree-to-commands body)))
    `(let (,@vars)
       ,@commands))))
         
  
;;(xml ((node element-maker :namespace "http://www.sample.org"))
;;     (node :root))