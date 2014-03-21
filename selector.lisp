#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

(defclass compilable () ())

(defclass selector (compilable)
  ((%operations :initarg :operations :initform (make-array 1 :adjustable T :fill-pointer 0) :accessor operations))
  (:documentation ""))

(defclass matcher (compilable)
  ((%constraints :initarg :constraints :initform (make-array 1 :adjustable T :fill-pointer 0) :accessor constraints))
  (:documentation ""))

(defclass constraint (compilable)
  ()
  (:documentation ""))

(defmethod print-object ((selector selector) stream)
  (print-unreadable-object (selector stream :type T)
    (write-char #\{ stream)
    (loop with length = (length (operations selector))
          for i from 0 below length
          do (princ (aref (operations selector) i) stream)
             (unless (= i (1- length))
               (write-char #\Space stream)))
    (write-char #\} stream))
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

(defclass any-constraint (constraint) ())

(defmethod print-object ((con any-constraint) stream)
  (write-char #\* stream)
  con)

(defclass named-constraint (constraint)
  ((%name :initarg :name :initform (error "Name required.") :accessor name)))

(defmethod print-object ((con named-constraint) stream)
  (princ (name con) stream)
  con)

(defclass tag-constraint (named-constraint) ())

(defclass class-constraint (named-constraint) ())

(defmethod print-object ((class class-constraint) stream)
  (format stream ".~a" (name class))
  class)

(defclass id-constraint (named-constraint) ())

(defmethod print-object ((id id-constraint) stream)
  (format stream "#~a" (name id))
  id)

(defclass attribute-constraint (named-constraint)
  ((%value :initarg :value :initform NIL :accessor value)
   (%operator :initarg :operator :initform #\= :accessor operator)))

(defmethod print-object ((attribute attribute-constraint) stream)
  (format stream "[~a~@[~a\"~a\"~]]" (name attribute) (operator attribute) (value attribute))
  attribute)

(defclass pseudo-constraint (named-constraint)
  ((%args :initarg :args :initform () :accessor args))
  (:documentation ""))

(defmethod print-object ((constraint pseudo-constraint) stream)
  (format stream ":~a(~{~a~^, ~})" (name constraint) (args constraint))
  constraint)
