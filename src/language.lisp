(in-package "CHAINS")

(defun find-object-with-class (class list)
  (declare (type list list))
  (labels ((test (a)
	     (typep a class)))
    (find-if #'test list)))

(defun body-values (body keyword)
  (rest (find keyword body :key #'first)))

(defun body-value (body keyword)
  (let ((v (body-values body keyword)))
    (when v
      (first v))))

(defmacro define-step (name &body body)
  (let ((documentation (let ((v (body-value body :documentation)))
			 (when v
			   (list (list :documentation v))))))
    `(progn
       (defclass ,name ()
	 ()
	 ,@documentation)

       (defun ,name (chain)
	 (find-object-with-class ',name chain)))))

;;; DEFINE OPERATION
;;; - Utilities
(defun operation/interesting-slots (class-name)
  (closer-mop:compute-slots (find-class class-name)))

;;; - Print object
(defun print-object/helper (class-name object)
  (let ((class (find-class class-name)))
    (loop
       :for slot :in (operation/interesting-slots class-name)
       :when (closer-mop:slot-definition-initargs slot)
       :append
       (list (first (closer-mop:slot-definition-initargs slot))
	     (closer-mop:slot-value-using-class class object slot)))))

(defun print-object/operation (class-name object stream)
  (let ((arguments (print-object/helper class-name object)))
    (cond
      (*print-readably*
       (write-string "#." stream)
       (write (append (list 'make-instance `',class-name)
		      arguments)
	      :stream stream))
      (t
       (print-unreadable-object (object stream :type t :identity t)
	 (format stream "~{~A~^ ~}" arguments))))))

(defun define-operation/defmethod/print-object (class-name)
  `(defmethod print-object ((object ,class-name) stream)
     (print-object/operation ',class-name object stream)))

;;; - Output name
(defun output-name/helper (class-name object)
  (let ((slots (operation/interesting-slots class-name)))	   
    (if slots
	(format nil "~A-~{~A~^-~}"
		(string-downcase (symbol-name class-name))
		(mapcar #'(lambda (slot)
			    (slot-value object (closer-mop:slot-definition-name slot)))
			slots))
	(string-downcase (symbol-name class-name)))))

(defun define-operation/defmethod/output-name (class-name)
  `(defmethod output-name ((object ,class-name))
     (output-name/helper ',class-name object)))

;;; - MACRO
(defun operation-slots-to-class-slots (link-slots)
  link-slots)

(defmacro define-operation (name super-classes slots &body body)
  `(progn
     (defclass ,name ,super-classes
       ,(operation-slots-to-class-slots slots)
       ,@body)

     ,(define-operation/defmethod/print-object name)
     ,(define-operation/defmethod/output-name name)))
