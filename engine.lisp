#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

(defvar *pseudo-selectors* (make-hash-table :test 'equalp)
  "Hash table for pseudo selector functions.
Links string names to functions of one or more arguments.")

(defmacro define-pseudo-selector (name (nodename &rest args-lambda) &body body)
  "Define a new pseudo-selector of NAME.

NAME        --- A symbol or string naming the selector (case insensitive always).
NODENAME    --- A variable symbol the matched node is bound to.
ARGS-LAMBDA --- A lambda-list of the expected arguments for the pseudo-selector.
                Note that keyword arguments make no sense in this context.
BODY        ::= form*"
  `(setf (gethash ,(string name) *pseudo-selectors*)
         #'(lambda (,nodename ,@args-lambda)
             (declare (ignorable ,nodename))
             ,@body)))

(define-condition pseudo-selector-not-available (error)
  ((%name :initarg :name :initform (error "NAME required.") :accessor name))
  (:report (lambda (c s) (format s "The ~a pseudo selector doesn't make sense in a node matching engine." (name c))))
  (:documentation "Condition signalled when a pseudo selector is defined according to spec, 
but makes no sense in the context of CLSS and has thus been left
unimplemented."))

(define-condition undefined-pseudo-selector (error)
  ((%name :initarg :name :initform (error "NAME required.") :accessor name))
  (:report (lambda (c s) (format s "The ~a pseudo selector is not defined!" (name c))))
  (:documentation "Condition signalled when trying to use a pseudo selector that has not
been defined. This is signalled at match-time, rather than at
selector-compile-time."))

(define-condition selector-malformed (error)
  ((%selector :initarg :selector :initform (error "Selector malformed.") :accessor selector))
  (:report (lambda (c s) (format s "Selector is malformed: ~a" (selector c))))
  (:documentation "Signalled when a selector or matcher has been found to be malformed.
This really shouldn't happen unless you're passing raw lists
for the selector to the matcher."))

(define-condition complete-match-pair (condition)
  ((%value :initarg :value :initform NIL :accessor value))
  (:documentation "Condition signalled to immediately return from MATCH-PAIR."))

(defun match-constraint (constraint node)
  "Attempts to match the CONSTRAINT form against the node.
Returns NIL if it fails to do so, unspecified otherwise."
  (ecase (car constraint)
    (:c-any
     T)
    (:c-tag
     (string-equal (tag-name node) (second constraint)))
    (:c-id
     (string-equal (attribute node "id") (second constraint)))
    (:c-class
     (cl-ppcre:scan (format NIL "(^| )~a( |$)" (second constraint)) (or (attribute node "class") "")))
    (:c-attr-exists
     (attribute node (second constraint)))
    (:c-attr-equals
     (destructuring-bind (comparator attribute value) (cdr constraint)
       (let ((attr (attribute node attribute)))
         (when attr
           (ecase (aref comparator 0)
             (#\=
              (string-equal attr value))
             (#\~
              (cl-ppcre:scan (format NIL "(^| )~a( |$)" value) attr))
             (#\^
              (and (<= (length value) (length attr))
                       (string= value attr :end2 (length value))))
             (#\$
              (and (<= (length value) (length attr))
                       (string= value attr :start2 (- (length attr) (length value)))))
             (#\*
              (search value attr))
             (#\|
              (cl-ppcre:scan (format NIL "(^|-)~a(-|$)" value) attr)))))))
    (:c-pseudo
     (destructuring-bind (name &rest args) (cdr constraint)
       (let ((pseudo (gethash name *pseudo-selectors*)))
         (assert (not (null pseudo)) () 'undefined-pseudo-selector :name name)
         (apply pseudo node args))))))

(defun match-matcher (matcher node)
  "Attempts to match a matcher against a node.
Returns T if all constraints match, NIL otherwise."
  (assert (eq (pop matcher) :matcher) () 'selector-malformed matcher)
  (loop for constraint in matcher
        always (match-constraint constraint node)))

(defun match-pair (combinator matcher nodes)
  "Match a combinator and matcher pair against a list of nodes.
Returns a vector of matching nodes."
  (handler-case
      (let ((resultset (make-array (length nodes) :adjustable T :fill-pointer 0)))
        (case (aref combinator 0)
          (#\Space
           (labels ((match-recursive (nodes)
                      (loop for node across nodes
                            when (and (element-p node)
                                      (match-matcher matcher node))
                              do (vector-push-extend node resultset)
                            when (nesting-node-p node)
                              do (match-recursive (children node)))))
             (loop for node across nodes
                   do (match-recursive (children node)))))
          (#\>
           (loop for parent across nodes
                 do (loop for node across (children parent)
                          when (and (element-p node)
                                    (match-matcher matcher node))
                            do (vector-push-extend node resultset))))
          (#\+
           (loop for node across nodes
                 for sibling = (next-element node)
                 when (and sibling (match-matcher matcher sibling))
                   do (vector-push-extend sibling resultset)))
          (#\~
           (loop for node across nodes
                 for position = (child-position node)
                 do (loop for i from position below (length (family node))
                          for sibling = (elt (family node) i)
                          when (and (element-p sibling)
                                    (match-matcher matcher sibling))
                            do (vector-push-extend sibling resultset)))))
        resultset)
    (complete-match-pair (o)
      (return-from match-pair (value o)))))

(defun match-selector (selector root-node)
  "Match a selector against the root-node and possibly all its children.
Returns an array of matched nodes."
  (assert (eq (pop selector) :selector) () 'selector-malformed)
  (loop with nodes = (etypecase root-node
                       (node (make-array 1 :initial-element root-node))
                       (vector root-node)
                       (list (coerce root-node 'vector)))
        for combinator = (pop selector)
        for matcher = (pop selector)
        while matcher
        do (setf nodes (match-pair combinator matcher nodes))
        finally (return nodes)))

(defun %select (selector root-node)
  (match-selector (etypecase selector
                    (list selector)
                    (string (parse-selector selector))) root-node))

(defun select (selector root-node)
  "Match the given selector against the root-node and possibly all its children.
Returns an array of matched nodes.

SELECTOR  --- A CSS-selector string or a compiled selector list.
ROOT-NODE --- The root note to start matching from."
  (%select selector root-node))

(define-compiler-macro select (selector root-node)
  (typecase selector
    (list `(%select ',selector ,root-node))
    (string `(%select ',(parse-selector selector) ,root-node))
    (T `(%select ,selector ,root-node))))
