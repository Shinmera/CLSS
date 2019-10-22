#|
 This file is a part of CLSS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.clss)

(define-pseudo-selector root (node)
  (root-p node))

(defun match-nth (i n)
  (cond ((string-equal n "odd") (oddp i))
        ((string-equal n "even") (evenp i))
        ((find #\n n)
         (let ((a (let ((r (parse-integer n :junk-allowed t)))
                    (cond (r r)
                          ((eq #\- (elt n 0)) -1)
                          (T 1))))
               (b (let ((ppos (position #\+ n)))
                    (if ppos
                        (parse-integer (subseq n (1+ ppos)))
                        0))))
           (multiple-value-bind (quot rem)
             (floor (- i b) a)
             (and (zerop rem) (>= quot 0)))))
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
  (loop for i downfrom (1- (length (family node))) to 0
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
  (let ((languages (or (attribute node "lang")
                       (attribute node "xml:lang"))))
    (when (and languages (find-substring language languages #\-))
      language)))

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

(define-pseudo-selector warning (node)
  (let ((classes (attribute node "class")))
    (when (and classes (find-substring "warning" classes #\Space))
      "warning")))

(define-pseudo-selector not (node selector)
  (not (match-matcher (third (second (parse-selector selector))) node)))

;;; Extra extensions specific to CLSS

(define-pseudo-selector first-only (node)
  (signal 'complete-match-pair :value (make-array 1 :initial-element node :adjustable T :fill-pointer T)))
