;; add-remove.lisp

(in-package #:libxml2.tree)


(defun pointer-to-node (ptr)
  (unless (null-pointer-p ptr)
    (make-instance 'node
                   :pointer ptr)))

;;; insert-child-before

(defun insert-child-before (new-child ref-child)
  (pointer-to-node (%xmlAddPrevSibling (pointer ref-child)
                                       (pointer new-child))))

;;; insert-child-after

(defun insert-child-after (new-child ref-child)
  (pointer-to-node (%xmlAddNextSibling (pointer ref-child)
                                       (pointer new-child))))

;;; append-child

(defun append-child (parent node)
  (pointer-to-node (%xmlAddChild (pointer parent)
                                 (pointer node))))

;;; prepend-child

(defun prepend-child (parent node)
  (let ((first (first-child parent)))
    (if first
        (insert-child-before node first)
        (append-child parent node))))

;;; detach

(defun detach (node)
  (%xmlUnlinkNode (pointer node)))

;;; remove-child

(defun remove-child (child)
  (detach child)
  (release child))

;;; replace-child

(defun replace-child (old-child new-child &key (delete t))
  (let ((%old (%xmlReplaceNode (pointer old-child)
                               (pointer new-child))))
    (if delete
        (%xmlFreeNode %old)
        (pointer-to-node %old))))