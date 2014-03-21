#|
 This file is a part of CLSS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clss)

(defgeneric generate (compilable)
  (:documentation "Compiles the given compilable (a selector, matcher or constraint, usually) to a tree searching engine."))

(defmethod generate ((selector selector))
  )

(defmethod generate ((matcher matcher))
  )

(defmethod generate ((any any-constraint))
  )

(defmethod generate ((tag tag-constraint))
  )

(defmethod generate ((id id-constraint))
  )

(defmethod generate ((class class-constraint))
  )

(defmethod generate ((attr attribute-constraint))
  )

(defmethod generate ((pseudo pseudo-constraint))
  )
