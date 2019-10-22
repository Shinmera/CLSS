#|
 This file is a part of CLSS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#
(in-package :cl-user)
(defpackage clss-test-asd
  (:use :cl :asdf))
(in-package :clss-test-asd)

(defsystem clss-test
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :depends-on (:clss
	       :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
	        :components
		((:test-file "pseudo-selectors"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
