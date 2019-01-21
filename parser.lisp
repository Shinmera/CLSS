#|
 This file is a part of CLSS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.clss)

;;; Selector grammar
;; SELECTOR      ::= GROUP (, GROUP)*
;; GROUP         ::= MATCHER (OPERATOR MATCHER)*
;; OPERATOR      ::= #\> | #\+ | #\~ | #\Space
;; MATCHER       ::= (#\* | TAG | TYPE | ID | CLASS | ATTRIBUTE) ID? CLASS* ATTRIBUTE* PSEUDO*
;; ID            ::= #\# NAME
;; TAG           ::= NAME
;; CLASS         ::= #\. NAME
;; ATTRIBUTE     ::= #\[ NAME ATTR-VALUE? #\]
;; ATTR-VALUE    ::= ATTR-OPERATOR (NAME | STRING)
;; ATTR-OPERATOR ::= (#\~ | #\^ | #\$ | #\* | #\|)? #\=
;; PSEUDO        ::= #\: NAME ARGUMENTS?
;; ARGUMENTS     ::= #\( VALUE (#\, VALUE)* #\)

(define-matcher clss-name (or (in #\/ #\9) (in #\? #\Z) (in #\a #\z) (any #\- #\\ #\_ #\!)))
(define-matcher clss-tag-name (or :clss-name (and (is #\:) (next (is #\:))) (and (is #\:) (prev (is #\:)))))
(define-matcher combinator (any #\Space #\Newline #\> #\+ #\~))
(define-matcher grouper (is #\,))
(define-matcher attr-comparator (or (is #\=) (and (any #\~ #\^ #\$ #\* #\|) (next (is #\=)))))
(defvar *valid-combinators* " >+~")

(defun read-name ()
  "Reads a CSS selector name-like string."
  (unless (funcall (make-matcher :clss-name))
    (error "~s at position ~d is not a valid name char." (peek) *index*))
  (consume-until (make-matcher (not :clss-name))))

(defun read-any-constraint ()
  "Reads an any constraint and returns it."
  (make-any-constraint))

(defun read-type-constraint ()
  "Reads a DOM type constraint and returns it."
  (make-type-constraint (read-name)))

(defun read-tag-constraint ()
  "Reads a tag constraint and returns it."
  (let ((out (make-string-output-stream)))
    (loop with matcher = (make-matcher :clss-tag-name)
          for prev = #\  then char
          for char = (peek)
          while (funcall matcher)
          do (unless (and (eql char #\:) (eql prev #\:))
               (write-char char out))
             (advance))
    (let ((name (get-output-stream-string out)))
      (when (string= "" name)
        (error "The CSS selector ~s contains invalid characters around position ~d."
               plump-lexer:*string* plump-lexer:*index*))
      (make-tag-constraint name))))

(defun read-id-constraint ()
  "Reads an ID attribute constraint and returns it."
  (make-id-constraint (read-name)))

(defun read-class-constraint ()
  "Reads a class constraint and returns it."
  (make-class-constraint (read-name)))

(defun read-attribute-comparator ()
  "Reads an attribute comparator string and returns it if found."
  (let ((op (consume-until (make-matcher (not :attr-comparator)))))
    (when (< 0 (length op))
      op)))

(defun read-attribute-value ()
  "Reads an attribute value and returns it."
  (case (peek)
    ((#\" #\')
     (prog2
         (consume)
         (consume-until (make-matcher (or (is #\") (is #\'))))
       (consume)))
    (T (consume-until (make-matcher (is #\]))))))

(defun read-attribute-constraint ()
  "Reads a complete attribute constraint and returns it."
  (let ((name (read-name))
        (oper (read-attribute-comparator))
        (val (read-attribute-value)))
    (prog1
        (make-attribute-constraint name val oper)
      (consume))))

(defun read-pseudo-args ()
  "Reads an arguments list of a pseudo selector."
  (when (char= (or (peek) #\Space) #\()
    (consume)
    (loop with index = *index*
          with args = ()
          for char = (consume)
          until (or (not char) (char= char #\)))
          do (when (char= char #\,)
               (push (string-trim " " (subseq *string* index (1- *index*))) args)
               (setf index *index*))
          finally (progn
                    (let ((arg (string-trim " " (subseq *string* index (1- *index*)))))
                      (unless (string= arg "")
                        (push arg args)))
                    (return (nreverse args))))))

(defun read-pseudo-constraint ()
  "Reads a complete pseudo constraint and returns it."
  (apply #'make-pseudo-constraint (read-name) (read-pseudo-args)))

(defun read-constraint ()
  "Read any constraint. Dispatches depending on the next character consumed."
  (case (consume)
    (#\* (read-any-constraint))
    (#\# (read-id-constraint))
    (#\. (read-class-constraint))
    (#\[ (read-attribute-constraint))
    (#\: (read-pseudo-constraint))
    (#\^ (read-type-constraint))
    (T (unread) (read-tag-constraint))))

(defun read-matcher ()
  "Read a matcher (a sequence of constraints) and return it."
  (loop for peek = (peek)
        for loop-cond = (and peek (funcall (make-matcher (not (or :combinator :grouper)))))
        for constraint = (and loop-cond (read-constraint))
        while loop-cond
        when constraint
          collect constraint into constraints
        finally (return (apply #'make-clss-matcher constraints))))

(defun read-combinator ()
  "Reads the combinator between matchers and returns it."
  (let ((op (consume-until (make-matcher (not :combinator))))
        (next (peek)))
    (unless (or (not next) (char= next #\,))
      (let ((op (string-trim '(#\Space #\Newline) op)))
        (if (string= op "")
            #\Space
            (aref op 0))))))

(defun read-group ()
  "Reads a selector group and returns it."
  (loop with list = ()
        for combinator = (read-combinator)
        for matcher = (read-matcher)
        while combinator
        do (unless (find combinator *valid-combinators* :test #'char=)
             (error "Invalid combinator ~a." combinator))
           (push combinator list)
           (push matcher list)
        finally (return (apply #'make-group (nreverse list)))))

(defun read-selector ()
  "Reads a complete selector and returns it."
  (loop for next = (peek)
        while next
        do (when (char= next #\,)
             (consume))
        collect (read-group) into groups
        finally (return (apply #'make-selector groups))))

(defun parse-selector (string)
  "Parse a selector string into its \"compiled\" list form."
  (setf string (concatenate 'string " " string))
  (with-lexer-environment (string)
    (read-selector)))

(defun ensure-selector (thing)
  (etypecase thing
    (list thing)
    (string (parse-selector thing))))
