#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

;;; Selector grammar
;; SELECTOR      ::= MATCHER (OPERATOR MATCHER)*
;; OPERATOR      ::= #\> | #\+ | #\~ | #\Space
;; MATCHER       ::= (#\* | TAG | ID | CLASS | ATTRIBUTE) ID? CLASS* ATTRIBUTE* PSEUDO*
;; ID            ::= #\# NAME
;; TAG           ::= NAME
;; CLASS         ::= #\. NAME
;; ATTRIBUTE     ::= #\[ NAME ATTR-VALUE? #\]
;; ATTR-VALUE    ::= ATTR-OPERATOR (NAME | STRING)
;; ATTR-OPERATOR ::= (#\~ | #\^ | #\$ | #\* | #\|)? #\=
;; PSEUDO        ::= #\: NAME ARGUMENTS?
;; ARGUMENTS     ::= #\( VALUE (#\, VALUE)* #\)

(define-matcher name (or (is #\-) (in #\/ #\9) (in #\? #\Z) (in #\a #\z) (is #\\) (is #\_) (is #\!)))
(define-matcher operator (any #\Space #\> #\+ #\~))
(define-matcher attr-operator (or (is #\=) (and (any #\~ #\^ #\$ #\* #\|) (next (is #\=)))))

(defun read-name ()
  (consume-until (make-matcher (not :name))))

(defun read-any ()
  (make-any-constraint))

(defun read-tag ()
  (make-tag-constraint (read-name)))

(defun read-id ()
  (make-id-constraint (read-name)))

(defun read-class ()
  (make-class-constraint (read-name)))

(defun read-attribute-operator ()
  (let ((op (consume-until (make-matcher (not :attr-operator)))))
    (when (< 0 (length op))
      op)))

(defun read-attribute-value ()
  (case (peek)
    (#\"
     (prog2
         (consume)
         (consume-until (make-matcher (is #\")))
       (consume)))
    (T (consume-until (make-matcher (is #\]))))))

(defun read-attribute ()
  (let ((name (read-name))
        (oper (read-attribute-operator))
        (val (read-attribute-value)))
    (prog1
        (make-attribute-constraint name val oper)
      (consume))))

(defun read-args ()
  (when (char= (or (peek) #\Space) #\()
    (consume)
    (loop with index = plump::*index*
          with args = ()
          for char = (consume)
          until (or (not char) (char= char #\)))
          do (when (char= char #\,)
               (push (string-trim " " (subseq plump::*string* index (1- plump::*index*))) args)
               (setf index plump::*index*))
          finally (progn
                    (let ((arg (string-trim " " (subseq plump::*string* index (1- plump::*index*)))))
                      (unless (string= arg "")
                        (push arg args)))
                    (return (nreverse args))))))

(defun read-pseudo ()
  (apply #'make-pseudo-constraint (read-name) (read-args)))

;; Make generic
(defun read-constraint ()
  (case (consume)
    (#\* (read-any))
    (#\# (read-id))
    (#\. (read-class))
    (#\[ (read-attribute))
    (#\: (read-pseudo))
    (T (unread) (read-tag))))

(defun read-matcher ()
  (loop for peek = (peek)
        while (and peek (funcall (make-matcher (not :operator))))
        for constraint = (read-constraint)
        when constraint
          collect constraint into constraints
        finally (return (apply #'make-clss-matcher constraints))))

(defun read-operator ()
  (let ((op (string-trim " " (consume-until (make-matcher (not :operator))))))
    (when (peek) (if (string= op "") " " op))))

(defun read-selector ()
  (loop with list = ()
        for matcher = (read-matcher)
        for operator = (read-operator)
        do (push matcher list)
           (when operator
             (push operator list))
        while operator
        finally (return (apply #'make-selector (nreverse list)))))

(defun parse-selector (string)
  (with-lexer-environment (string)
    (read-selector)))
