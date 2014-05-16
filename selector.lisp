#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

(defun make-selector (&rest groups)
  `(:selector ,@groups))

(defun make-group (&rest matches-and-ops)
  `(:group ,@matches-and-ops))

(defun make-clss-matcher (&rest constraints)
  `(:matcher ,@constraints))

(defun make-any-constraint ()
  `(:c-any))

(defun make-tag-constraint (tag)
  `(:c-tag ,tag))

(defun make-id-constraint (id)
  `(:c-id ,id))

(defun make-class-constraint (class)
  `(:c-class ,class))

(defun make-attribute-constraint (attribute &optional value (comparator :=))
  (if (and value comparator)
      `(:c-attr-equals ,comparator ,attribute ,value)
      `(:c-attr-exists ,attribute)))

(defun make-pseudo-constraint (function &rest args)
  `(:c-pseudo ,function ,@args))
