;;; xfactory.lisp

(in-package :libxml2.xfactory)

(defvar *node*)

(defvar *makers*)

(defun tagname (tag)
  (if (keywordp tag)
      (string-downcase (symbol-name tag))
      tag))


(defun tree-to-commands (tree)
  (let ((maker-info (if (and (consp tree)
                             (symbolp (car tree)))
                        (assoc (car tree)
                               *makers*))))
    (cond (maker-info `(let ((*node* (if (boundp '*node*)
                                         (xtree:make-child-element *node*
                                                                   (tagname ,(second tree))
                                                                   ,(second maker-info)
                                                                   ,(third maker-info))
                                         (xtree:make-element (tagname ,(second tree))
                                                             ,(second maker-info)
                                                             ,(third maker-info)))))
                         ,@(tree-to-commands (cddr tree))
                         *node*))
          ((atom tree) tree)
          (t (iter (for child in tree)
                   (collect (tree-to-commands child)))))))


;;; namespace

(defun namespace (href prefix)
  (xtree:make-ns *node* href prefix))

;;; attribute

(defun attribute (name value)
  (let* ((pos (position #\: name))
         (ns (if pos
                 (xtree::search-ns-by-prefix *node* (subseq name 0 pos))))
         (attr (if pos
                   (subseq name (1+ pos))
                   name)))
  (setf (xtree:attribute-value *node*
                               attr
                               (if ns (xtree:namespace-uri ns)))
        value)))
  

;;; xfactory

(defmacro with-xfactory ((&rest makers) &body args)
  (let ((*makers* makers))    
    `(progn ,@(tree-to-commands args))))

;; (with-xfactory ((E))
;;   (E :root
;;      (E :a)
;;      (E :b)))



