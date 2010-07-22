;;; xfactory.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.xfactory)

(defvar *node*)

(defun tagname (tag)
  (if (keywordp tag)
      (string-downcase (symbol-name tag))
      tag))

(defun tree-to-commands (tree makers &optional (string-as-text-node nil))
  (let ((maker-info (if (and (consp tree)
                             (symbolp (car tree)))
                        (assoc (car tree)
                               makers))))
    (cond (maker-info `(let ((*node* (if (boundp '*node*)
                                         (xtree:make-child-element *node*
                                                                   (tagname ,(second tree))
                                                                   ,(second maker-info)
                                                                   ,(third maker-info))
                                         (xtree:make-element (tagname ,(second tree))
                                                             ,(second maker-info)
                                                             ,(third maker-info)))))
                         ,@(iter (for child in (cddr tree))
                                 (collect (tree-to-commands child makers t)))
                         *node*))
          ((and string-as-text-node (stringp tree)) `(xtree:make-child-text *node* ,tree))
          ((atom tree) tree)
          (t (iter (for child in tree)
                   (collect (tree-to-commands child makers)))))))

;;; namespace

(defun namespace (href prefix)
  (xtree:make-ns *node* href prefix))

;;; attribute

(defun attributes (&rest args)
  (when (oddp (length args))
    (error "odd number of args to SETF"))
  (iter (for l first args then (cddr l))
        (while l)
        (let* ((name (tagname (first l)))
               (value (second l))
               (pos (position #\: name))
               (ns (if pos
                       (xtree::search-ns-by-prefix *node* (subseq name 0 pos))))
               (attr (if pos
                         (subseq name (1+ pos))
                         name)))
          (setf (xtree:attribute-value *node*
                                       attr
                                       (if ns (xtree:namespace-uri ns)))
                (if (stringp value)
                    value
                    (write-to-string value))))))
        
;;; text

(defun text (control-string &rest format-arguments)
  (xtree:make-child-text *node*
                         (if format-arguments
                             (apply  #'format nil (cons control-string format-arguments))
                             control-string)))

;;; process-instruction

(defun process-instruction (name content)
  (xtree:append-child *node*
                      (xtree:make-process-instruction name
                                                      content)))
  
;;; xfactory

(defmacro with-element-factory ((&rest makers) &body args)
  (let ((commands (tree-to-commands args makers)))
    (when (cdr commands)
      (error "invalid number of root commands: ~A" (length commands)))
    `,(car commands)))

(defmacro with-document-factory ((&rest makers) &body args)
  `(let ((*node* (xtree:make-document)))
     ,@(tree-to-commands args makers)
     *node*))
