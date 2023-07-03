(in-package #:org.shirakumo.clss)

(defun make-selector (&rest groups)
  `(:selector ,@groups))

(defun make-group (&rest matches-and-ops)
  `(:group ,@matches-and-ops))

(defun make-clss-matcher (&rest constraints)
  `(:matcher ,@constraints))

(defun make-any-constraint ()
  `(:c-any))

(defun make-tag-constraint (tag)
  `(:c-tag ,tag))

(defun make-type-constraint (name)
  (let ((type (or (find-symbol (string-upcase name) "PLUMP-DOM")
                  (find-symbol (string-upcase name))
                  (error "No such PLUMP-DOM class: ~s" name))))
    (or (subtypep type 'plump-dom:node)
        (error "~s is not a PLUMP-DOM:NODE subclass." name))
    `(:c-type ,type)))

(defun make-id-constraint (id)
  `(:c-id ,id))

(defun make-class-constraint (class)
  `(:c-class ,class))

(defun make-attribute-constraint (attribute &optional value (comparator :=))
  (if (and value comparator)
      `(:c-attr-equals ,comparator ,attribute ,value)
      `(:c-attr-exists ,attribute)))

(defun make-pseudo-constraint (function &rest args)
  `(:c-pseudo ,function ,@args))
