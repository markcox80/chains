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
		(substitute #\- #\/ (string-downcase (symbol-name class-name)))
		(mapcar #'(lambda (slot)
			    (slot-value object (closer-mop:slot-definition-name slot)))
			slots))
	(string-downcase (symbol-name class-name)))))

(defun define-operation/defmethod/output-name (class-name)
  `(defmethod output-name ((object ,class-name))
     (output-name/helper ',class-name object)))

;;; - Operation equal
(defun operation-equal/helper (class-name object-a object-b test-functions slot-names)
  (and (typep object-a class-name)
       (typep object-b class-name)
       (every #'(lambda (test-function slot-name)
		  (funcall test-function
			   (slot-value object-a slot-name)
			   (slot-value object-b slot-name)))
	      test-functions slot-names)))

(defun define-operation/defmethod/operation-equal (class-name slot-definitions)
  (labels ((slot-definition-test-function (slot-definition)
	     (destructuring-bind (slot-name &rest args) slot-definition
	       (declare (ignore slot-name))
	       (or (getf args :test)
		   '(function equal))))
	   (slot-test-function (slot-name)
	     (let ((v (find slot-name slot-definitions :key #'first)))
	       (slot-definition-test-function v))))
    (let* ((slot-names (mapcar #'first slot-definitions))
	   (test-functions (mapcar #'slot-test-function slot-names)))
      `(let ((test-functions (list ,@test-functions)))
	 (defmethod operation-equal ((object-a ,class-name) (object-b ,class-name))
	   (and (call-next-method)
		(operation-equal/helper ',class-name object-a object-b
					test-functions ',slot-names)))))))

;;; - MACRO
(defun operation-slots-to-class-slots (link-slots)
  (labels ((to-class-slot (slot)
	     (destructuring-bind (slot-name &rest args) slot
	       (cons slot-name (alexandria:remove-from-plist args :test)))))
    (mapcar #'to-class-slot link-slots)))

(defmacro define-operation (name super-classes slots &body body)
  `(progn
     (defclass ,name ,super-classes
       ,(operation-slots-to-class-slots slots)
       ,@body)
     
     ,(define-operation/defmethod/print-object name)
     ,(define-operation/defmethod/output-name name)     
     ,(define-operation/defmethod/operation-equal name slots)))

(defmacro define-step (name super-classes slots &body body)
  (let ((class-body (remove-if #'(lambda (x)
				   (member x '(:value)))
			       body :key #'first))
	(value-function-name (body-value body :value)))
    `(progn
       (defclass ,name ,super-classes
	 ,(operation-slots-to-class-slots slots)
	 ,@class-body)

       (defun ,name (list)
	 (find-object-with-class ',name list))

       ,(when value-function-name
	 `(defun ,value-function-name (chain)
	    (chain-result chain ',name)))

       ,(define-operation/defmethod/operation-equal name slots))))
