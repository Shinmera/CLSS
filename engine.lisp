#|
 This file is a part of CLSS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.clss)

(defvar *pseudo-selectors* (make-hash-table :test 'equalp)
  "Hash table for pseudo selector functions.
Links string names to functions of one or more arguments.")

(defun pseudo-selector (name)
  "Returns the pseudo-selector function associated with NAME, if any."
  (gethash (string name) *pseudo-selectors*))

(defun (setf pseudo-selector) (function name)
  "Sets FUNCTION as the pseudo-selector for NAME."
  (setf (gethash (string name) *pseudo-selectors*)
        function))

(defun remove-pseudo-selector (name)
  "Removes the pseudo-selector associated with NAME."
  (remhash (string name) *pseudo-selectors*))

(defmacro define-pseudo-selector (name (nodename &rest args-lambda) &body body)
  "Define a new pseudo-selector of NAME.

NAME        --- A symbol or string naming the selector (case insensitive always).
NODENAME    --- A variable symbol the matched node is bound to.
ARGS-LAMBDA --- A lambda-list of the expected arguments for the pseudo-selector.
                Note that keyword arguments make no sense in this context.
BODY        ::= form*"
  `(setf (pseudo-selector ,(string name))
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

(defvar *whitespace-regex* (cl-ppcre:create-scanner "\\s+"))
(defvar *hyphen-regex* (cl-ppcre:create-scanner "-"))

(declaim (ftype (function (list plump-dom:node)
                          (values boolean))
                match-constraint))
(defun match-constraint (constraint node)
  "Attempts to match the CONSTRAINT form against the node.
Returns NIL if it fails to do so, unspecified otherwise."
  (declare (optimize (speed 3)))
  (ecase (car constraint)
    (:c-any
     (and (not (text-node-p node))
          (not (comment-p node))))
    (:c-tag
     (and (element-p node)
          (string-equal (tag-name node) (second constraint))))
    (:c-type
     (typep node (second constraint)))
    (:c-id
     (and (element-p node)
          (string-equal (attribute node "id") (second constraint))))
    (:c-class
     (and (element-p node)
          (not (null (member (second constraint) (cl-ppcre:split *whitespace-regex* (or (attribute node "class") "")) :test #'string-equal)))))
    (:c-attr-exists
     (and (element-p node)
          (not (null (attribute node (second constraint))))))
    (:c-attr-equals
     (and (element-p node)
          (destructuring-bind (comparator attribute value) (cdr constraint)
            (declare ((and simple-string) comparator attribute value))
            (let ((attr (attribute node attribute)))
              (declare ((or null (and string)) attr))
              (when attr
                (ecase (aref comparator 0)
                  (#\=
                   (string-equal attr value))
                  (#\~
                   (not (null (member value (cl-ppcre:split *whitespace-regex* attr) :test #'string-equal))))
                  (#\^
                   (and (<= (length value) (length attr))
                        (string= value attr :end2 (length value))))
                  (#\$
                   (and (<= (length value) (length attr))
                        (string= value attr :start2 (- (length attr) (length value)))))
                  (#\*
                   (not (null (search value attr))))
                  (#\|
                   (not (null (member value (cl-ppcre:split *hyphen-regex* attr) :test #'string-equal))))))))))
    (:c-pseudo
     (and (element-p node)
          (destructuring-bind (name &rest args) (cdr constraint)
            (let ((pseudo (pseudo-selector name)))
              (declare (function pseudo))
              (assert (not (null pseudo)) () 'undefined-pseudo-selector :name name)
              (not (null (apply pseudo node args)))))))))

(declaim (ftype (function (list plump:node)
                          (values boolean))
                match-matcher))
(defun match-matcher (matcher node)
  "Attempts to match a matcher against a node.
Returns T if all constraints match, NIL otherwise."
  (declare (optimize (speed 3)))
  (assert (eq (car matcher) :matcher) () 'selector-malformed matcher)
  (loop for constraint in (cdr matcher)
        always (match-constraint constraint node)))

(declaim (ftype (function (character list (and (vector plump:node) (not simple-array)))
                          (values (and (vector plump:node) (not simple-array)) &optional)) match-pair))
(defun match-pair (combinator matcher nodes)
  "Match a combinator and matcher pair against a list of nodes.
Returns a vector of matching nodes."
  (declare (optimize (speed 3)))
  (handler-case
      (let ((resultset (make-array (length nodes) :adjustable T :fill-pointer 0)))
        (case combinator
          (#\Space
           (labels ((match-recursive (nodes)
                      (declare ((and (vector plump:node) (not simple-array)) nodes))
                      (loop for node across nodes
                            when (match-matcher matcher node)
                            do (vector-push-extend node resultset)
                            when (nesting-node-p node)
                            do (match-recursive (children node)))))
             (loop for node across nodes
                   do (match-recursive (children node)))))
          (#\>
           (loop for parent across nodes
                 do (loop for node across (the (and (vector plump:node) (not simple-array))
                                               (children parent))
                          when (match-matcher matcher node)
                          do (vector-push-extend node resultset))))
          (#\+
           (loop for node across nodes
                 for position of-type fixnum = (child-position node)
                 for family = (family node)
                 do (loop for i of-type fixnum from position below (1- (fill-pointer family))
                          for sibling = (aref family (1+ i))
                          ;; This is gross. In order to properly support
                          ;; edge cases like a foo+^bar we cannot exclude
                          ;; anything other than these two...
                          do (when (and (not (text-node-p node))
                                        (not (comment-p node)))
                               (when (match-matcher matcher sibling)
                                 (vector-push-extend sibling resultset))
                               (return)))))
          (#\~
           (loop for node across nodes
                 for position of-type fixnum = (child-position node)
                 for family = (family node)
                 do (loop for i of-type fixnum from position below (fill-pointer family)
                          for sibling = (aref family i)
                          do (when (match-matcher matcher sibling)
                               (vector-push-extend sibling resultset)
                               (return))))))
        resultset)
    (complete-match-pair (o)
      (return-from match-pair (value o)))))

(declaim (ftype (function (list (or plump:node vector list))
                          (values (and (vector plump:node) (not simple-array))))
                match-selector))
(defun match-group (group root-node)
  "Match a matcher group against the root-node and possibly all its children.
Returns an array of mached nodes."
  (declare (optimize (speed 3)))
  (assert (eq (car group) :group) () 'selector-malformed)
  (let ((group (cdr group)))
    (loop with nodes = (etypecase root-node
                         (plump:node (make-array 1 :initial-element root-node :adjustable T :fill-pointer T))
                         (vector root-node)
                         (list (coerce root-node 'vector)))
          for combinator = (pop group)
          for matcher = (pop group)
          while matcher
          do (setf nodes (match-pair combinator matcher nodes))
          finally (return nodes))))

(declaim (ftype (function (list (or plump:node vector list))
                          (values (and (vector plump:node) (not simple-array))))
                match-selector))
(defun match-selector (selector root-node)
  "Match a selector against the root-node and possibly all its children.
Returns an array of matched nodes."
  (declare (optimize (speed 3)))
  (assert (eq (car selector) :selector) () 'selector-malformed)
  (let ((selector (cdr selector)))
    (loop with result = (match-group (pop selector) root-node)
          for group in selector
          do (array-utils:vector-append result (match-group group root-node))
          finally (return result))))

(declaim (ftype (function ((or string list) (or plump:node vector list))
                          (values (and (vector plump:node) (not simple-array)) &optional))
                select))
(defun select (selector root-node)
  "Match the given selector against the root-node and possibly all its children.
Returns an array of matched nodes.

SELECTOR  --- A CSS-selector string or a compiled selector list.
ROOT-NODE --- A single node, list or vector of nodes to start matching from."
  (match-selector (ensure-selector selector) root-node))

(define-compiler-macro select (&whole whole &environment env selector root-node)
  (if (constantp selector env)
      `(match-selector (load-time-value (ensure-selector ,selector)) ,root-node)
      whole))

(declaim (ftype (function (list plump:node) boolean) match-group-backwards))
(defun match-group-backwards (group node)
  (declare (optimize (speed 3)))
  (assert (eql (car group) :group) () 'selector-malformed)
  (let ((group (reverse (cdr group))))
    (when (match-matcher (pop group) node)
      (loop for combinator = (pop group)
            for matcher = (pop group)
            while matcher
            do (case combinator
                 (#\Space
                  (loop do (setf node (parent node))
                           (when (or (not node) (root-p node))
                             (return-from match-group-backwards NIL))
                        until (match-matcher matcher node)))
                 (#\>
                  (setf node (parent node))
                  (unless (and node (not (root-p node)) (match-matcher matcher node))
                    (return-from match-group-backwards NIL)))
                 (#\+
                  (setf node (previous-element node))
                  (unless (and node (match-matcher matcher node))
                    (return-from match-group-backwards NIL)))
                 (#\~
                  (loop for i of-type fixnum downfrom (child-position node) above 0
                        for sibling = (aref (family node) i)
                        do (when (match-matcher matcher sibling)
                             (setf node sibling)
                             (return))
                        finally (return-from match-group-backwards NIL))))
            finally (return T)))))

(declaim (ftype (function (T plump:node) boolean) node-matches-p))
(defun node-matches-p (selector node)
  "Tests whether the node matches the selector.

SELECTOR --- A CSS-selector string or a compiled selector list.
NODE     --- The node to test."
  (declare (optimize (speed 3)))
  (let ((selector (ensure-selector selector)))
    (assert (eql (car selector) :selector) () 'selector-malformed)
    (loop for group in (cdr selector)
          thereis (match-group-backwards group node))))

(define-compiler-macro node-matches-p (&whole whole &environment env selector root-node)
  (if (constantp selector env)
      `(node-matches-p (load-time-value (ensure-selector ,selector)) ,root-node)
      whole))
