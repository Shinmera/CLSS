#|
 This file is a part of CLSS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage :org.tymoonnext.clss.doc
  (:use :cl :lquery :lquery-doc)
  (:nicknames :clss-doc)
  (:export :build-documentation))

(in-package :org.tymoonnext.clss.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object))))
  ($ template "a.funcname" (attr :href (format NIL "#~a" (symbol-name (nth 0 object))))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :clss))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :clss :exclude '(:internal :method))))
      ($ "#clss-docs" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :clss)))))
