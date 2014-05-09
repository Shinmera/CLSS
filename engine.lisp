#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

(defvar *pseudo-selectors* (make-hash-table :test 'equalp))

(defun match-constraint (constraint item)
  (ecase (car constraint)
    (:c-any
     T)
    (:c-tag
     (string-equal (tag-name item) (second constraint)))
    (:c-id
     (string-equal (attribute item "id") (second constraint)))
    (:c-class
     (cl-ppcre:scan (format NIL "\\b~a\\b" (second constraint)) (or (attribute item "class") "")))
    (:c-attr-exists
     (attribute item (second constraint)))
    (:c-attr-equals
     (destructuring-bind (comparator attribute value) (cdr constraint)
       (let ((attr (attribute item attribute)))
         (when attr
           (ecase (aref comparator 0)
             (#\= (string-equal attr value))
             (#\~ (cl-ppcre:scan (format NIL "\\b~a\\b" value) attr))
             (#\^ (and (<= (length value) (length attr))
                       (string= value attr :end2 (length value))))
             (#\$ (and (<= (length value) (length attr))
                       (string= value attr :start2 (- (length attr) (length value)))))
             (#\* (search value attr))
             (#\| (cl-ppcre:scan (format NIL "(^|-)~a(-|$)" value) attr)))))))
    (:c-pseudo
     (destructuring-bind (name args) (cdr constraint)
       (let ((pseudo (gethash name *pseudo-selectors*)))
         (assert (not (null pseudo)))
         (apply pseudo item args))))))

(defun match-matcher (matcher item)
  (assert (eq (pop matcher) :matcher))
  (loop for constraint in matcher
        always (match-constraint constraint item)))

(defun match-pair (comb matcher set)
  (let ((resultset (make-array (length set) :adjustable T :fill-pointer 0)))
    (labels ((match-items (items)
               (loop for item across items
                     when (and (element-p item)
                               (match-matcher matcher item))
                       do (vector-push-extend item resultset)))
             (match-recursive (items)
               (loop for item across items
                     when (and (element-p item)
                               (match-matcher matcher item))
                       do (vector-push-extend item resultset)
                     when (nesting-node-p item)
                       do (match-recursive (children item)))))
      (case (aref comb 0)
        (#\Space
         (match-recursive set))
        (#\>
         (loop for item across set
               do (match-items (children item))))
        (#\+)
        (#\~)))
    resultset))

(defun match-selector (selector root-node)
  (assert (eq (pop selector) :selector))
  (loop with set = (etypecase root-node
                     (node (make-array 1 :initial-element root-node))
                     (vector root-node)
                     (list (coerce root-node 'vector)))
        for combinator = (pop selector)
        for matcher = (pop selector)
        while matcher
        do (setf set (match-pair combinator matcher set))
        finally (return set)))

(defun select (selector root-node)
  (match-selector (etypecase selector
                    (list selector)
                    (string (parse-selector selector))) root-node))

(defmacro define-pseudo-selector (name (nodename &rest args-lambda) &body body)
  `(setf (gethash ,(string name) *pseudo-selectors*)
         #'(lambda (,nodename ,@args-lambda)
             ,@body)))
