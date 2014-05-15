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

(defvar *whitespace-regex* (cl-ppcre:create-scanner "\\s+"))
(defvar *hyphen-regex* (cl-ppcre:create-scanner "-"))

(declaim (ftype (function (list plump-dom:node) (values boolean)) match-constraint))
(defun match-constraint (constraint node)
  "Attempts to match the CONSTRAINT form against the node.
Returns NIL if it fails to do so, unspecified otherwise."
  (declare (optimize (speed 3)))
  (ecase (car constraint)
    (:c-any
     T)
    (:c-tag
     (string-equal (tag-name node) (second constraint)))
    (:c-id
     (string-equal (attribute node "id") (second constraint)))
    (:c-class
     (not (null (member (second constraint) (cl-ppcre:split *whitespace-regex* (or (attribute node "class") "")) :test #'string-equal))))
    (:c-attr-exists
     (not (null (attribute node (second constraint)))))
    (:c-attr-equals
     (destructuring-bind (comparator attribute value) (cdr constraint)
       (declare (simple-string comparator attribute value))
       (let ((attr (attribute node attribute)))
         (declare (simple-string attr))
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
              (not (null (member value (cl-ppcre:split *hyphen-regex* attr) :test #'string-equal)))))))))
    (:c-pseudo
     (destructuring-bind (name &rest args) (cdr constraint)
       (let ((pseudo (gethash name *pseudo-selectors*)))
         (declare (function pseudo))
         (assert (not (null pseudo)) () 'undefined-pseudo-selector :name name)
         (not (null (apply pseudo node args))))))))

(declaim (ftype (function (list plump:node) (values boolean)) match-matcher))
(defun match-matcher (matcher node)
  "Attempts to match a matcher against a node.
Returns T if all constraints match, NIL otherwise."
  (declare (optimize (speed 3)))
  (assert (eq (car matcher) :matcher) () 'selector-malformed matcher)
  (loop for constraint in (cdr matcher)
        always (match-constraint constraint node)))

(defvar *selectors*)
(declaim (ftype (function (list plump:node) (values (and (vector plump:node) (not simple-array)))) match-selector))
(defun match-selector (selector root-node)
  "Match a selector against the root-node and possibly all its children.
Returns an array of matched nodes."
  (declare (optimize (speed 3)))
  (assert (eq (car selector) :selector) () 'selector-malformed)
  (let ((*selectors* (list (cdr selector)))
        (results (make-array 0 :adjustable T :fill-pointer 0 :element-type 'plump:node)))
    (labels ((r (node)
               (loop for child across (children node)
                     when (plump:element-p child)
                       do (let ((*selectors* *selectors*))
                            (loop for selector in *selectors*
                                  do (when (match-matcher (second selector) child)
                                       (if (cddr selector)
                                           (setf *selectors* (cons (cddr selector) *selectors*))
                                           (vector-push-extend child results))))
                            (r child)))))
      (r root-node))
    results))

(declaim (ftype (function (T plump:node) (values (and (vector plump:node) (not simple-array)) &optional)) %select select))
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
