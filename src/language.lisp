(in-package "CHAINS")

(defun find-link-if/class (class chain)
  (labels ((test (a)
	     (typep a class)))
    (find-link-if #'test chain)))

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
       (defclass ,name (link)
	 ()
	 ,@documentation)

       (defgeneric ,name (object))

       (defmethod ,name ((object chain))
	 (find-link-if/class ',name object)))))

(defun link-slots/to-class-slots (link-slots)
  link-slots)

(defun link-slots/initarg (link-slots slot-name)
  (let ((v (rest (find slot-name link-slots :key #'first))))
    (when v
      (getf v :initarg))))

(defun link-slots/slot-names (link-slots)
  (mapcar #'first link-slots))

(defun link/interesting-slots (class-name)
  (assert (subtypep class-name 'link))
  (let* ((class      (find-class class-name))
	 (link-class (find-class 'link)))
    (loop
       :for class :in (closer-mop:compute-class-precedence-list class)
       :until (equal class link-class)
       :append
       (closer-mop:class-direct-slots class))))

(defun print-object/helper (class-name object)
  (loop
     :for slot :in (link/interesting-slots class-name)
     :when (closer-mop:slot-definition-initargs slot)
     :append
     (list (first (closer-mop:slot-definition-initargs slot))
	   (slot-value object (closer-mop:slot-definition-name slot)))))

(defun define-link/defmethod/print-object (class-name)
  `(defmethod print-object ((object ,class-name) stream)
     (let ((arguments (print-object/helper ',class-name object)))
       (cond
	 (*print-readably*
	  (write-string "#." stream)
	  (write (append (list 'make-instance '',class-name)
			 arguments)
		 :stream stream))
	 (t
	  (print-unreadable-object (object stream :type t :identity t)
	    (format stream "~{~A~^ ~}" arguments)))))))

(defun output-name/helper (class-name object)
  (let ((slots (link/interesting-slots class-name)))	   
    (if slots
	(format nil "~A-~{~A~^-~}"
		(string-downcase (symbol-name class-name))
		(mapcar #'(lambda (slot)
			    (slot-value object (closer-mop:slot-definition-name slot)))
			slots))
	(string-downcase (symbol-name class-name)))))

(defun define-link/defmethod/output-name (class-name)
  `(defmethod output-name ((object ,class-name))
     (output-name/helper ',class-name object)))

(defun copy-link/helper (class-name object)
  (print-object/helper class-name object))

(defun define-link/defmethod/copy-link (class-name)
  `(defmethod copy-link ((object ,class-name) &optional parent)
     (apply #'make-instance ',class-name
	    :parent-link parent
	    (copy-link/helper ',class-name object))))

(defmacro define-link (name super-classes slots &body body)
  (declare (ignore body))
  `(progn
     (defclass ,name ,super-classes
       ,(link-slots/to-class-slots slots))

     ,(define-link/defmethod/print-object name)
     ,(define-link/defmethod/output-name name)
     ,(define-link/defmethod/copy-link name)))
