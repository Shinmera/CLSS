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

(defun split-member (item split string)
  (declare (optimize speed)
           (type string item string)
           (type character split))
  (macrolet ((with-stringcase (var body)
               `(typecase ,var
                  (simple-string ,body)
                  (string ,body))))
    (with-stringcase item
      (with-stringcase string
        (loop  with res = nil
               and i = 0
               and length = (length string)
               and item-length = (length item)

               while (and (not res) (< i length))
               do (loop  for j  from 0
                         while (and (< j item-length) (< i length))
                         for char = (aref string i)
                         for item-char = (aref item j)
                         do (cond ((char= char split)
                                   (loop  while (and (< i length)
                                                     (char= (aref string i)
                                                            split))
                                          do (incf i))
                                   (return nil))
                                  ((char= char item-char)
                                   (when (and (= (1+ j) item-length)
                                              (or (= (1+ i) length)
                                                  (char= (aref string (1+ i))
                                                         split)))
                                     (return-from split-member t))
                                   (incf i))
                                  (t
                                   (incf i)
                                   (return nil)))))))))

(declaim (ftype (function (list plump-dom:node)
                          (values boolean))
                match-constraint))
(defun match-constraint (constraint node)
  "Attempts to match the CONSTRAINT form against the node.
Returns NIL if it fails to do so, unspecified otherwise."
  (declare (optimize speed))
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
          (split-member (second constraint) #\Space (or (attribute node "class") ""))))
    (:c-attr-exists
     (and (element-p node)


          (not (null (attribute node (second constraint))))))
    (:c-attr-equals
     (and (element-p node)
          (destructuring-bind (comparator attribute value) (cdr constraint)
            (declare (type simple-string comparator attribute value))
            (let ((attr (attribute node attribute)))
              (declare (type (or null string) attr))
              (when attr
                (ecase (aref comparator 0)
                  (#\=
                   (string-equal attr value))
                  (#\~
                   (split-member value #\Space attr))
                  (#\^
                   (and (<= (length value) (length attr))
                        (string= value attr :end2 (length value))))
                  (#\$
                   (and (<= (length value) (length attr))
                        (string= value attr :start2 (- (length attr) (length value)))))
                  (#\*
                   (not (null (search value attr))))
                  (#\|
                   (split-member value #\- attr))))))))
    (:c-pseudo
     (and (element-p node)
          (destructuring-bind (name &rest args) (cdr constraint)
            (let ((pseudo (pseudo-selector name)))
              (declare (type function pseudo))
              (assert (not (null pseudo)) () 'undefined-pseudo-selector :name name)
              (not (null (apply pseudo node args)))))))))

(declaim (ftype (function (list plump:node)
                          (values boolean))
                match-matcher))
(defun match-matcher (matcher node)
  "Attempts to match a matcher against a node.
Returns T if all constraints match, NIL otherwise."
  (declare (optimize speed))
  (assert (eq (car matcher) :matcher) () 'selector-malformed matcher)
  (loop for constraint in (cdr matcher)
        always (match-constraint constraint node)))

(declaim (ftype (function (character list plump:node (function (plump:node) T))
                          (values &optional null))
                match-pair-depth))
(defun match-pair-depth (combinator matcher parent matching-nodes-processor)
  "Match a combinator and matcher pair against a list of nodes. For every match
the function specified in \"MATCHING-NODES-PROCESSOR\" is called with the found
match as the only argument."
  (declare (optimize speed))
  (handler-case
      (prog1 nil
          (case combinator
            (#\Space
             (labels ((match-recursive (nodes)
                        (declare ((and (vector plump:node) (not simple-array)) nodes))
                        (loop for node across nodes
                              when (match-matcher matcher node)
                                do (funcall matching-nodes-processor node)
                              when (plump:nesting-node-p node)
                                do (match-recursive (children node)))))
               (match-recursive (children parent))))
            (#\>
             (loop for node across (the (and (vector plump:node) (not simple-array))
                                        (children parent))
                   when (match-matcher matcher node)
                     do (funcall matching-nodes-processor node)))
            (#\+
             (let ((position (child-position parent))
                   (family (family parent)))
               (declare (type fixnum position)
                        (type (and (vector plump-dom:child-node)
                                   (not simple-array))
                              family))
               (loop for i of-type fixnum from position below (1- (fill-pointer family))
                     for sibling = (aref family (1+ i))
                     ;; This is gross. In order to properly support
                     ;; edge cases like a foo+^bar we cannot exclude
                     ;; anything other than these two...
                     do (when (and (not (text-node-p parent))
                                   (not (comment-p parent)))
                          (when (match-matcher matcher sibling)
                            (funcall matching-nodes-processor sibling))
                          (return)))))
            (#\~
             (let ((position (child-position parent))
                   (family (family parent)))
               (declare (type fixnum position)
                        (type (and (vector plump-dom:child-node)
                                   (not simple-array))
                              family))
               (loop for i of-type fixnum from position below (fill-pointer family)
                     for sibling = (aref family i)
                     do (when (match-matcher matcher sibling)
                          (funcall matching-nodes-processor sibling)
                          (return)))))))
    (complete-match-pair (o)
      (declare (ignore o))
      (return-from match-pair-depth nil))))

(declaim (ftype (function (character list (and (vector plump:node) (not simple-array)))
                          (values (and (vector plump:node) (not simple-array)) &optional))
                match-pair-breadth))
(defun match-pair-breadth (combinator matcher nodes)
  "Match a combinator and matcher pair against a list of nodes.
Returns a vector of matching nodes."
  (declare (optimize speed))
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
      (return-from match-pair-breadth (value o)))))

(declaim (ftype (function (list (or plump:node vector list) &optional keyword)
                          (values (and (vector plump:node) (not simple-array))))
                match-selector))
(defun match-group (group root-node &optional (search-type :depth-first))
  "Match a matcher group against the root-node and possibly all its children.
Returns an array of mached nodes."
  (declare (optimize debug))
  (assert (eq (car group) :group) () 'selector-malformed)
  (let ((group (cdr group)))
    (ecase search-type
        (:depth-first
         (let* ((result (make-array 10 :adjustable T :fill-pointer 0)))
           (labels ((add-to-result (node)
                      (vector-push-extend node result))
                    (search-node (node group)
                      (let ((combinator (car group))
                            (matcher (cadr group))
                            (group (cddr group)))
                        (if group
                            (match-pair-depth combinator
                                              matcher
                                              node
                                              (lambda (node)
                                                (search-node node group)))
                            (match-pair-depth combinator
                                              matcher
                                              node
                                              #'add-to-result)))))
             (etypecase root-node
               (plump:node (search-node root-node group))
               (sequence (map nil
                              (lambda (node) (search-node node group))
                              root-node)))
             result)))
      (:breadth-first
       (loop with nodes = (etypecase root-node
                            (plump:node (make-array 1 :initial-element root-node :adjustable T :fill-pointer T))
                            (vector root-node)
                            (list (coerce root-node 'vector)))
             for combinator = (pop group)
             for matcher = (pop group)
             while matcher
             do (setf nodes (match-pair-breadth combinator matcher nodes))
             finally (return nodes))))))

(declaim (ftype (function (list (or plump:node vector list) keyword)
                          (values (and (vector plump:node) (not simple-array))))
                match-selector))
(defun match-selector (selector root-node search-type)
  "Match a selector against the root-node and possibly all its children.
Returns an array of matched nodes."
  (declare (optimize speed))
  (assert (eq (car selector) :selector) () 'selector-malformed)
  (let ((selector (cdr selector)))
    (loop with result = (match-group (pop selector) root-node search-type)
          for group in selector
          do (array-utils:vector-append result (match-group group root-node search-type))
          finally (return result))))

(declaim (ftype (function ((or string list) (or plump:node vector list) &optional keyword)
                          (values (and (vector plump:node) (not simple-array)) &optional))
                select))
(defun select (selector root-node &optional (search-type :depth-first))
  "Match the given selector against the root-node and possibly all its children.
Returns an array of matched nodes.

SELECTOR    --- A CSS-selector string or a compiled selector list.
ROOT-NODE   --- A single node, list or vector of nodes to start matching from.
SEARCH-TYPE --- Select the search algorithm, options are \":depth-first\" and \":breadth-first\"."
  (match-selector (ensure-selector selector) root-node search-type))

(define-compiler-macro select (&whole whole &environment env selector root-node &optional (search-type :depth-first))
  (if (constantp selector env)
      `(match-selector (load-time-value (ensure-selector ,selector)) ,root-node ,search-type)
      whole))

(declaim (ftype (function (list plump:node) boolean) match-group-backwards))
(defun match-group-backwards (group node)
  (declare (optimize speed))
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
  (declare (optimize speed))
  (let ((selector (ensure-selector selector)))
    (assert (eql (car selector) :selector) () 'selector-malformed)
    (loop for group in (cdr selector)
          thereis (match-group-backwards group node))))

(define-compiler-macro node-matches-p (&whole whole &environment env selector root-node)
  (if (constantp selector env)
      `(node-matches-p (load-time-value (ensure-selector ,selector)) ,root-node)
      whole))
