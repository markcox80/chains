(in-package "CHAINS")

(defgeneric output-name (object)
  (:documentation "A name which uniquely describes OBJECT."))

(defun make-chain (&rest args)
  args)

(defvar *database-pathname*)
