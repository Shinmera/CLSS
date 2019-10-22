(in-package :cl-user)
(defpackage pseudo-selector-test
  (:use
    :cl
    :prove
    :clss)
  (:import-from
    :clss match-nth))
(in-package :pseudo-selector-test)

(defvar test-input '(1 2 3 4 5))

(defmacro match-nth-is (n selected)
  `(is (mapcar #'(lambda (i) (match-nth i ,n)) ',test-input) ,selected ,n))

(plan nil)

(match-nth-is "1" '(t nil nil nil nil))
(match-nth-is "3" '(nil nil t nil nil))
(match-nth-is "odd" '(t nil t nil t))
(match-nth-is "even" '(nil t nil t nil))

(match-nth-is "n" '(t t t t t))
(match-nth-is "n+1" '(t t t t t))
(match-nth-is "n+2" '(nil t t t t))
(match-nth-is "n+3" '(nil nil t t t))

(match-nth-is "-n+1" '(t nil nil nil nil))
(match-nth-is "-n+2" '(t t nil nil nil))
(match-nth-is "-n+3" '(t t t nil nil))

(match-nth-is "2n" '(nil t nil t nil))
(match-nth-is "2n+1" '(t nil t nil t))

(match-nth-is "-2n+3" '(t nil t nil nil))

(finalize)
