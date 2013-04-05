(in-package "CHAINS")

(defgeneric output-name (object)
  (:documentation "A name which uniquely describes OBJECT."))

(defgeneric operation-equal (object-a object-b)
  (:documentation "A predicate that tests for equality of two OPERATION objects."))

(defmethod operation-equal ((object-a t) (object-b t))
  t)

(defun make-chain (&rest args)
  args)

(defvar *database-pathname*)
