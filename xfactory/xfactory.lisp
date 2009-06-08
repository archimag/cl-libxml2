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
                   (collect (tree-to-commands child)))))))

;;; element-maker

(defclass element-maker ()
  ((namespace :initarg :namespace :initform nil :reader maker-namespace)
   (prefix :initarg :prefix :initform nil :reader maker-prefix)))


(defmethod tree-to-commands/impl (maker-symbol (maker-class (eql 'element-maker)) args)
  (let ((tag (car args))
        (childs (if (cdr args)
                    (tree-to-commands (cdr args)))))
    `(let ((*node* (if (boundp '*node*)
                       (xtree:make-child-element *node*
                                                 (tagname ,tag)
                                                 (maker-namespace ,maker-symbol)
                                                 (maker-prefix ,maker-symbol))
                       (xtree:make-element (tagname ,tag)
                                           (maker-namespace ,maker-symbol)
                                           (maker-prefix ,maker-symbol)))))
       ,@childs
       *node*)))

;;; attribute-maker

(defclass attribute-maker ()
  ((namespace :initarg :namespace :initform nil :reader maker-namespace)
   (prefix :initarg :prefix :initform nil :reader maker-prefix)))

(defmethod tree-to-commands/impl (maker-symbol (maker-class (eql 'attribute-maker)) args)
  `(setf (xtree:attribute-value *node*
                                (tagname ,(car args))
                                (maker-namespace ,maker-symbol))
         ,(second args)))

;;; xfactory
    
(defmacro xfactory ((&rest makers) &body args)
  (let ((*makers* (iter (for maker in makers)
                        (collect (cons (first maker)
                                       (second maker))))))    
    (let ((vars (iter (for maker in makers)
                      (collect `(,(car maker) (make-instance
                                               (quote ,(second maker))
                                               ,@(cddr maker))))))
          (commands (tree-to-commands args)))
    `(let (,@vars)
       ,@commands))))

    

;;(xml ((node element-maker :namespace "http://www.sample.org"))
;;     (node :root))

;; (xfactory ((E element-maker)
;;            (A attribute-maker))
;;           (E :root
;;              (A :href "www.kuban.ru")))

     