#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.clss.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.clss.asdf)

(defsystem clss
  :name "CSS Like Simple Selectors"
  :version "0.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A DOM tree searching engine based on CSS selectors."
  :serial T
  :components ((:file "package")
               (:file "selector")
               (:file "parser")
               (:file "compiler"))
  :depends-on (:plump))
