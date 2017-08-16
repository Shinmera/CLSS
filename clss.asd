#|
 This file is a part of CLSS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem clss
  :name "CSS Like Simple Selectors"
  :version "0.3.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A DOM tree searching engine based on CSS selectors."
  :serial T
  :components ((:file "package")
               (:file "selector")
               (:file "parser")
               (:file "engine")
               (:file "pseudo-selectors"))
  :depends-on (:array-utils
               :plump))
