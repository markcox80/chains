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

(defun link-slots/print-object (class-name slots)
  `(defmethod print-object ((object ,class-name) stream)
     (cond
       (*print-readably*
	(write-string "#." stream)
	(write (list 'make-instance '',class-name
		     ,@(reduce #'append
			       (mapcar #'(lambda (slot-name)
					   (list (link-slots/initarg slots slot-name)
						 `(slot-value object ',slot-name)))
				       (link-slots/slot-names slots))))
	       :stream stream))
       (t
	(print-unreadable-object (object stream :type t :identity t)
	  ,@(mapcar #'(lambda (slot-name)
			`(progn
			   (write ,(link-slots/initarg slots slot-name)
				  :stream stream)
			   (write-string " " stream)
			   (write (slot-value object ',slot-name)
				  :stream stream)))
		    (link-slots/slot-names slots)))))))

(defun link-slots/output-name (class-name slots)
  `(defmethod output-name ((object ,class-name))
     ,(if slots
	  `(format nil "~A-~{~A~^-~}"
		   ,(string-downcase (symbol-name class-name))
		   (list ,@(mapcar #'(lambda (slot-name)
				       `(slot-value object ',slot-name))
				   (link-slots/slot-names slots))))
	  (string-downcase (symbol-name class-name)))))

(defmacro define-link (name super-classes slots &body body)
  (declare (ignore body))
  `(progn
     (defclass ,name ,super-classes
       ,(link-slots/to-class-slots slots))

     ,(link-slots/print-object name slots)
     ,(link-slots/output-name name slots)))
