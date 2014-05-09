#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

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
    (:c-pseudo)))

(defun match-matcher (matcher item)
  (assert (eq (pop matcher) :matcher))
  (loop for constraint = (pop matcher)
        always (match-constraint constraint item)))

(defun match-pair (comb matcher set)
  (let ((list ()))
    (labels ((match-items (items)
               (loop for child in items
                     when (match-matcher matcher child)
                       do (push child list)))
             (match-recursive (items)
               (match-items items)
               (dolist (item items)
                 (match-recursive (children item)))))
      (case (aref comb 0)
        (#\Space
         (match-recursive set))
        (#\>
         (dolist (item set)
           (match-items (children item))))
        (#\+)
        (#\~)))
    list))

(defun match-selector (selector root-node)
  (assert (eq (pop selector) :selector))
  (loop with set = (list root-node)
        for combinator = (pop selector)
        for matcher = (pop selector)
        while matcher
        do (setf set (match-pair combinator matcher set))
        finally (return set)))
