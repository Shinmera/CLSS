#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

(defclass selector ()
  ((%operations :initarg :operations :initform (make-array 1 :adjustable T :fill-pointer 0) :accessor operations))
  (:documentation ""))

(defclass matcher ()
  ((%constraints :initarg :constraints :initform (make-array 1 :adjustable T :fill-pointer 0) :accessor constraints))
  (:documentation ""))

(defmethod print-object ((selector selector) stream)
  (print-unreadable-object (selector stream :type T)
    (loop for op across (operations selector)
          do (format stream "~a " op)))
  selector)

(defmethod print-object ((matcher matcher) stream)
  (loop for con across (constraints matcher)
        do (princ con stream))
  matcher)

(defgeneric add-constraint (matcher constraint)
  (:documentation "")
  (:method ((matcher matcher) constraint)
    (vector-push-extend constraint (constraints matcher))))

(defgeneric add-matcher (selector matcher)
  (:documentation "")
  (:method ((selector selector) matcher)
    (vector-push-extend matcher (operations selector))))

(defgeneric add-operator (selector operator)
  (:documentation "")
  (:method ((selector selector) operator)
    (vector-push-extend operator (operations selector))))

(defclass any-constraint () ())

(defmethod print-object ((con any-constraint) stream)
  (write-char #\* stream))

(defclass named-constraint ()
  ((%name :initarg :name :initform (error "Name required.") :accessor name)))

(defmethod print-object ((con named-constraint) stream)
  (princ (name con) stream))

(defclass tag-constraint (named-constraint) ())

(defclass class-constraint (named-constraint) ())

(defclass id-constraint (named-constraint) ())

(defclass attribute-constraint (named-constraint)
  ((%value :initarg :value :initform NIL :accessor value)
   (%operator :initarg :operator :initform #\= :accessor operator)))

(defclass pseudo-constraint (named-constraint)
  ((%args :initarg :args :initform () :accessor args))
  (:documentation ""))

(defmethod print-object ((pseudo-constraint pseudo-constraint) stream)
  (format stream ":~a(~{~a~^, ~})" (name pseudo-constraint) (args pseudo-constraint)))
