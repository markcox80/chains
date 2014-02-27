(in-package "CHAINS")

;; Task string
(defgeneric task-string (task))

(defmethod task-string ((task task))
  (let ((slots (mapcar #'(lambda (slot)
			   (slot-value task (closer-mop:slot-definition-name slot)))
		       ;; Not sure if this is needed. The text is hazy. 
		       #-abcl (sort (closer-mop:compute-slots (class-of task)) #'<
				    :key #'closer-mop:slot-definition-location)
		       ;; The above does not work on ABCL as
		       ;; SLOT-DEFINITION-LOCATION returns NIL. (2013/07/03)
		       #+abcl (closer-mop:compute-slots (class-of task)))))
    (let ((*print-case* :upcase))
      (substitute #\. #\/
		  (if slots
		      (format nil "~A-~{~A~^-~}" (string-downcase (type-of task)) slots)
		      (string-downcase (type-of task)))))))

;; Serialisation of objects and tasks
(defgeneric object-sexp (object))

(defmethod object-sexp ((object t))
  (cond
    ((null object)
     nil)
    ((symbolp object)
     `',object)
    ((alexandria:proper-list-p object)
     `(list ,@(mapcar #'object-sexp object)))
    ((consp object)
     `(cons ,(object-sexp (car object)) ,(object-sexp (cdr object))))
    (t
     object)))

(defun serialise-object (stream object)
  (pprint-logical-block (stream nil)
    (write-string "#." stream)
    (write (object-sexp object) :stream stream :pretty t :readably t)))

(defun serialise-task (stream task)
  (declare (type task task))
  (serialise-object stream task))

(defmethod object-sexp ((object task))
  (labels ((compute-slot-initialisation ()
	     (let ((slots (closer-mop:compute-slots (class-of object))))
	       (loop
		  :for slot :in slots
		  :for initarg := (first (closer-mop:slot-definition-initargs slot))
		  :for value := (slot-value object (closer-mop:slot-definition-name slot))
		  :append
		  (list initarg (object-sexp value))))))
    `(make-instance ',(type-of object)
		    ,@(compute-slot-initialisation))))
