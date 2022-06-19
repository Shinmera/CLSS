#|
 This file is a part of CLSS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl)
(defpackage #:CLSS
  (:nicknames #:org.shirakumo.clss)
  (:use #:cl #:plump)
  (:shadow #:read-name #:read-attribute #:read-attribute-value)
  ;; engine.lisp
  (:export
   #:pseudo-selector
   #:remove-pseudo-selector
   #:define-pseudo-selector
   
   #:pseudo-selector-not-available
   #:name
   
   #:undefined-pseudo-selector
   #:name
   
   #:selector-malformed
   #:selector
   
   #:match-constraint
   #:match-matcher
   #:match-pair
   #:match-group
   #:match-selector
   
   #:select
   #:ordered-select

   #:match-group-backwards
   #:node-matches-p)
  ;; parser.lisp
  (:export
   #:parse-selector)
  ;; selector.lisp
  (:export
   #:make-selector
   #:make-clss-matcher
   #:make-any-constraint
   #:make-tag-constraint
   #:make-type-constraint
   #:make-id-constraint
   #:make-class-constraint
   #:make-attribute-constraint
   #:make-pseudo-constraint))
