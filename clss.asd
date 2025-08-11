(asdf:defsystem clss
  :name "CSS Like Simple Selectors"
  :version "0.3.1"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A DOM tree searching engine based on CSS selectors."
  :homepage "https://shinmera.com/docs/CLSS/"
  :bug-tracker "https://shinmera.com/project/CLSS/issues"
  :source-control (:git "https://shinmera.com/project/CLSS.git")
  :serial T
  :components ((:file "package")
               (:file "selector")
               (:file "parser")
               (:file "engine")
               (:file "pseudo-selectors"))
  :depends-on (:array-utils
               :plump))
