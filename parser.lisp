#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

;;; Selector grammar
;; SELECTOR      ::= MATCHER (OPERATOR MATCHER)*
;; OPERATOR      ::= #\> | #\+ | #\~ | #\Space
;; MATCHER       ::= (#\* | ID | TAG | CLASS | ATTRIBUTE) CLASS* ATTRIBUTE* PSEUDO*
;; ID            ::= #\# NAME
;; TAG           ::= NAME
;; CLASS         ::= #\. NAME
;; ATTRIBUTE     ::= #\[ NAME ATTR-VALUE? #\]
;; ATTR-VALUE    ::= ATTR-OPERATOR (NAME | STRING)
;; ATTR-OPERATOR ::= (#\~ | #\^ | #\$ | #\* | #\|)? #\=
;; PSEUDO        ::= #\: NAME ARGUMENTS?
;; ARGUMENTS     ::= #\( VALUE (#\, VALUE)* #\)

