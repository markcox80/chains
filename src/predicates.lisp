(in-package "CHAINS")

(defgeneric test=-function (object))
(defgeneric test<-function (object))
(defgeneric test>-function (object))

(defun %predicates (name)
  (get name 'predicates))

(defun (setf %predicates) (value name)
  (setf (get name 'predicates) value))

(defun find-predicates (name)
  (%predicates name))

(defclass predicates ()
  ((test=-function
    :initarg :test=-function
    :reader test=-function)
   (test<-function
    :initarg :test<-function
    :reader test<-function)
   (test>-function
    :initarg :test>-function
    :reader test>-function)))

(defun ensure-predicates (name equal less-than greater-than)
  (setf (%predicates name) (make-instance 'predicates
					  :test=-function equal
					  :test<-function less-than
					  :test>-function greater-than)))

(defmacro define-predicates (name equal less-than greater-than)
  `(ensure-predicates ',name ,equal ,less-than ,greater-than))

(defmethod test=-function ((object symbol))
  (when object
    (test=-function (%predicates object))))

(defmethod test<-function ((object symbol))
  (when object
    (test<-function (%predicates object))))

(defmethod test>-function ((object symbol))
  (when object
    (test>-function (%predicates object))))
