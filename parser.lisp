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

(define-matcher name (or (in #\a #\z) (in #\? #\Z) (in #\- #\9) (is #\\) (is #\_) (is #\!) (is #\#)))
(define-matcher operator (any #\Space #\> #\+ #\~))
(define-matcher attr-operator (or #\= (and (any #\~ #\^ #\$ #\* #\|) (next (is #\=)))))

(defun read-name ()
  (consume-until (make-matcher (not name))))

(defun read-any ()
  (make-instance 'any-constraint))

(defun read-tag ()
  (make-instance 'tag-constraint :name (read-name)))

(defun read-id ()
  (make-instance 'id-constraint :name (read-name)))

(defun read-class ()
  (make-instance 'class-constraint :name (read-name)))

(defun read-attribute-operator ()
  (let ((char (consume)))
    (if (char= char #\=)
        "="
        (progn (consume) (format NIL "~a=" char)))))

(defun read-attribute-value ()
  (case (peek)
    (#\"
     (prog2
         (consume)
         (consume-until (make-matcher (is #\")))
       (consume)))
    (T (consume-until (make-matcher (is #\]))))))

(defun read-attribute ()
  (prog1
      (make-instance 'attribute-constraint
                     :name (read-name)
                     :operator (read-attribute-operator)
                     :value (read-attribute-value))
    (consume)))

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
  (make-instance 'pseudo-constraint :name (read-name) :args (read-args)))

(defun read-constraint ()
  (case (consume)
    (#\* (read-any))
    (#\# (read-id))
    (#\. (read-class))
    (#\[ (read-attribute))
    (#\: (read-pseudo))
    (T (unread) (read-tag))))

(defun read-matcher ()
  (loop with matcher = (make-instance 'matcher)
        for peek = (peek)
        while (and peek (funcall (make-matcher (not operator))))
        for constraint = (read-constraint)
        when constraint
          do (add-constraint matcher constraint)
        finally (return matcher)))

(defun read-operator ()
  (let ((op (string-trim " " (consume-until (make-matcher (not operator))))))
    (when (peek) (if (string= op "") " " op))))

(defun read-selector ()
  (loop with selector = (make-instance 'selector)
        for matcher = (read-matcher)
        for operator = (read-operator)
        do (add-matcher selector matcher)
           (when operator
             (add-operator selector operator))
        while operator
        finally (return selector)))

(defun parse-selector (string)
  (with-lexer-environment (string)
    (read-selector)))
