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

;; https://www.w3.org/TR/CSS21/syndata.html#value-def-identifier
(define-matcher clss-name (or (in #\/ #\9) (in #\? #\Z) (in #\a #\z) (any #\- #\\ #\_ #\! #.(code-char #xB7))
                              (not (in #.(code-char #x0) #.(code-char #xA0)))
                              (and (in #.(code-char #x1F) #.(code-char #xFF))
                                   (prev (is #\\)))))
(define-matcher clss-tag-name (or :clss-name (and (is #\:) (next (is #\:))) (and (is #\:) (prev (is #\:)))))
(define-matcher combinator (any #\Space #\Newline #\> #\+ #\~))
(define-matcher grouper (is #\,))
(define-matcher attr-comparator (or (is #\=) (and (any #\~ #\^ #\$ #\* #\|) (next (is #\=)))))
(defvar *valid-combinators* " >+~")

(defun escapable (char)
  "A helper function to decide whether a part of identifier needs escaping."
  (let ((code (char-code char)))
    (or (<= code #x1f)
        (= code #x7f)
        (not (or (>= code #x80)
                 (char= char #\-)
                 (char= char #\_)
                 (digit-char-p char)
                 (char<= #\A char #\Z)
                 (char<= #\a char #\z))))))

;; https://drafts.csswg.org/cssom/#serialize-an-identifier
(defun css-escape (string)
  "Escape all the invalid CSS characters to their safe counterparts."
  (declare (optimize speed)
           (type string string))
  (let ((buffer (make-array 0 :adjustable t :fill-pointer 0)))
    (labels ((extend-with-string (string)
               (loop for char across string
                     do (vector-push-extend char buffer)))
             (extend-with-escaped (char)
               (extend-with-string (format nil "\\~X " (char-code char))))
             (escape-regular-string (string)
               (loop for char across string
                     if (char= #\Nul char)
                       do (extend-with-string "\\fffd ")
                     else if (or (<= #x1 (char-code char) #x1f)
                                 (= (char-code char) #x7f))
                            do (extend-with-escaped char)
                     else if (escapable char)
                            do (vector-push-extend #\\ buffer)
                            and do (vector-push-extend char buffer)
                     else
                       do (vector-push-extend char buffer))))
      (when (and (= (length string) 1)
                 (char= #\- (elt string 0)))
        (extend-with-escaped (elt string 0)))
      ;; Process first char.
      (when (>= (length string) 1)
        (cond
          ((digit-char-p (elt string 0))
           (extend-with-escaped (elt string 0)))
          (t
           (escape-regular-string (subseq string 0 1)))))
      ;; Second char.
      (when (>= (length string) 2)
        (cond
          ((and (char= #\- (elt string 0))
                (digit-char-p (elt string 1)))
           (extend-with-escaped (elt string 1)))
          (t
           (escape-regular-string (subseq string 1 2)))))
      ;; All the rest.
      (when (> (length string) 2)
        (escape-regular-string (subseq string 2)))
      (coerce buffer 'string))))

(defun css-unescape (string)
  "Get the original contents of the escaped STRING."
  (declare (optimize speed)
           (type string string))
  (if (search "\\" string)
      (loop with buffer = (make-array 0 :adjustable t :fill-pointer 0)
            for index below (length string)
            for char = (elt string index)
            until (= index (length string))
            if (search "\\fffd " string
                       :start2 index :end2 (min (length string)
                                                (+ index 6)))
              do (vector-push-extend #\Nul buffer)
              and do (setf index (position #\Space string :start (1+ index)))
            else if (and (eql #\\ char)
                         (digit-char-p (elt string (1+ index)) 16))
                   do (vector-push-extend
                       (code-char (parse-integer string :radix 16 :start (1+ index)))
                       buffer)
                   and do (setf index (position #\Space string :start (1+ index)))
            else if (eql #\\ char)
                   do (vector-push-extend (elt string (1+ index)) buffer)
                   and do (incf index)
            else
              do (vector-push-extend char buffer)
            finally (return (coerce buffer 'string)))
      string))

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
  (make-id-constraint (css-unescape (read-name))))

(defun read-class-constraint ()
  "Reads a class constraint and returns it."
  (make-class-constraint (css-unescape (read-name))))

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
         (consume-until (make-matcher (and (not (prev (is #\\)))
                                           (or (is #\") (is #\')))))
       (consume)))
    (T (consume-until (make-matcher (is #\]))))))

(defun read-attribute-constraint ()
  "Reads a complete attribute constraint and returns it."
  (let ((name (css-unescape (read-name)))
        (oper (read-attribute-comparator))
        (val (css-unescape (read-attribute-value))))
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
