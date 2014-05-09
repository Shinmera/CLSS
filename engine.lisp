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
             (#\=
              (string-equal attr value))
             (#\~
              (cl-ppcre:scan (format NIL "\\b~a\\b" value) attr))
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
    (case (aref comb 0)
      (#\Space
       (labels ((match-recursive (items)
                  (loop for item across items
                        when (and (element-p item)
                                  (match-matcher matcher item))
                          do (vector-push-extend item resultset)
                        when (nesting-node-p item)
                          do (match-recursive (children item)))))
         (match-recursive set)))
      (#\>
       (loop for item across set
             when (and (element-p item)
                       (match-matcher matcher item))
               do (vector-push-extend item resultset)))
      (#\+
       (loop for item across set
             for sibling = (next-element item)
             when (and sibling (match-matcher matcher sibling))
               do (vector-push-extend sibling resultset)))
      (#\~
       (loop for item across set
             for position = (child-position item)
             do (loop for i from position below (length (family item))
                      for sibling = (elt (family item) i)
                      when (and (element-p sibling)
                                (match-matcher matcher sibling))
                        do (vector-push-extend sibling resultset)))))
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
             (declare (ignorable ,nodename))
             ,@body)))

(define-condition pseudo-selector-not-available (error)
  ((%name :initarg :name :initform (error "NAME required.") :accessor name))
  (:report (lambda (c s) (format s "The ~a pseudo selector doesn't make sense in a node matching engine." (name c)))))

(define-pseudo-selector root (node)
  (root-p node))

(defun match-nth (i n)
  (cond ((string-equal n "odd") (oddp i))
        ((string-equal n "even") (evenp i))
        ((find #\n n)
         (let ((ppos (position #\+ n)))
           (if ppos
               (= (+ (mod i (parse-integer n :junk-allowed T)) (parse-integer (subseq n (1+ ppos)))) 0)
               (= (mod i (parse-integer n :junk-allowed T)) 0))))
        (T (= (parse-integer n) i))))

(define-pseudo-selector nth-child (node n)
  (match-nth (1+ (element-position node)) n))

(define-pseudo-selector nth-last-child (node n)
  (match-nth (- (length (sibling-elements node))
                (element-position node)) n))

(define-pseudo-selector nth-of-type (node n)
  (match-nth (loop with count = 0
                   for sibling across (family node)
                   when (and (element-p sibling)
                             (string-equal (tag-name sibling) (tag-name node)))
                     do (incf count)
                   until (eq sibling node)
                   finally (return count)) n))

(define-pseudo-selector nth-last-of-type (node n)
  (match-nth (loop with count = 0
                   for i downfrom (1- (length (family node))) to 0
                   for sibling = (aref (family node) i)
                   when (and (element-p sibling)
                             (string-equal (tag-name sibling) (tag-name node)))
                     do (incf count)
                   until (eq sibling node)
                   finally (return count)) n))

(define-pseudo-selector first-child (node)
  (= (element-position node) 0))

(define-pseudo-selector last-child (node)
  (loop for i downfrom (length (family node)) to 0
        when (element-p node)
          do (return (eq (elt (family node) i) node))))

(define-pseudo-selector first-of-type (node)
  (loop for sibling across (family node)
        when (and (element-p sibling)
                  (string-equal (tag-name sibling) (tag-name node)))
          do (return (eq sibling node))))

(define-pseudo-selector last-of-type (node)
  (loop for i downfrom (1- (length (family node))) to 0
        for sibling = (aref (family node) i)
        when (and (element-p sibling)
                  (string-equal (tag-name sibling) (tag-name node)))
          do (return (eq sibling node))))

(define-pseudo-selector only-child (node)
  (loop for sibling across (family node)
        always (or (eq sibling node)
                   (not (element-p sibling)))))

(define-pseudo-selector only-of-type (node)
  (loop for sibling across (family node)
        always (or (eq sibling node)
                   (not (element-p sibling))
                   (not (string-equal (tag-name sibling) (tag-name node))))))

(define-pseudo-selector empty (node)
  (= (length (children node)) 0))

(define-pseudo-selector link (node)
  (error 'pseudo-selector-not-available :name "LINK"))

(define-pseudo-selector visited (node)
  (error 'pseudo-selector-not-available :name "VISITED"))

(define-pseudo-selector active (node)
  (error 'pseudo-selector-not-available :name "ACTIVE"))

(define-pseudo-selector hover (node)
  (error 'pseudo-selector-not-available :name "HOVER"))

(define-pseudo-selector focus (node)
  (error 'pseudo-selector-not-available :name "FOCUS"))

(define-pseudo-selector target (node)
  (error 'pseudo-selector-not-available :name "TARGET"))

(define-pseudo-selector lang (node language)
  (or (cl-ppcre:scan (format NIL "(^|-)~a(-|$)" language) (attribute node "lang"))
      (cl-ppcre:scan (format NIL "(^|-)~a(-|$)" language) (attribute node "xml:lang"))))

(define-pseudo-selector enabled (node)
  (has-attribute node "enabled"))

(define-pseudo-selector disabled (node)
  (has-attribute node "disabled"))

(define-pseudo-selector checked (node)
  (has-attribute node "checked"))

(define-pseudo-selector first-line (node)
  (error 'pseudo-selector-not-available :name "FIRST-LINE"))

(define-pseudo-selector first-letter (node)
  (error 'pseudo-selector-not-available :name "FIRST-LETTER"))

(define-pseudo-selector before (node)
  (error 'pseudo-selector-not-available :name "BEFORE"))

(define-pseudo-selector after (node)
  (error 'pseudo-selector-not-available :name "AFTER"))

(defvar *warning-regex* (cl-ppcre:create-scanner "\\bwarning\\b"))
(define-pseudo-selector warning (node)
  (let ((classes (attribute node "class")))
    (when classes
      (cl-ppcre:scan *warning-regex* classes))))

(define-pseudo-selector not (node selector)
  )
