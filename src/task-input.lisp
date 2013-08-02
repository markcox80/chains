(in-package "CHAINS")

;; Conditions
(define-condition no-applicable-task-input-function (error)
  ((task-input
    :initarg :task-input)
   (target-class
    :initarg :target-class)
   (performed-classes
    :initarg :performed-classes))
  (:report (lambda (condition stream)
	     (with-slots (task-input target-class performed-classes) condition
	       (format stream "No applicable TASK-INPUT-FUNCTION exists for task input ~S, target class ~S and performed classes ~A."
		       task-input target-class performed-classes)))))

;; Class TASK-INPUT
;;
;; An instance of the class TASK-INPUT maintains all information
;; associated with an input function created with DEFINE-TASK-INPUT or
;; ENSURE-TASK-INPUT.
;;
;; A TASK-INPUT instance is used to contain all task input function
;; definitions created with DEFINE-TASK-INPUT or ENSURE-TASK-INPUT.
;;
;; When a task OPERATION is about to be performed, the appropriate
;; task input function is obtained from the instance of
;; TASK-INPUT. Determining which function is used depends on the task
;; being performed and the performed tasks needed to compute the
;; required input value. If there exists no applicable task input
;; function for the given operation, then a NO-APPLICABLE-TASK-INPUT
;; error is signalled.
;;
;; All functions maintained by an instance of TASK-INPUT are instances
;; of the TASK-INPUT-FUNCTION class, which inherits from
;; FUNCALLABLE-STANDARD-OBJECT.

(defclass task-input ()
  ((name
    :initarg :name
    :reader task-input-name)
   (functions
    :initarg :functions
    :accessor task-input-functions)
   (documentation
    :initarg :documentation
    :accessor task-input-documentation))
  (:default-initargs
   :functions nil))

(defmethod print-object ((object task-input) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (task-input-name object) :stream stream)))

(defun find-task-input (name)
  (or (get name 'task-input)
      (error "Unable to find TASK-INPUT with name ~A" name)))

(defun ensure-task-input (name &key documentation)
  (setf (get name 'task-input) (make-instance 'task-input
					      :name name
					      :documentation (or documentation ""))))

(defun task-input-equal (input-a input-b)
  (assert (typep input-a 'task-input))
  (assert (typep input-b 'task-input))
  (equal input-a input-b))

(defmacro define-task-input (name &rest options)
  (labels ((canonicalise-option (option)
	     (ecase (first option)
	       (:documentation
		(list :documentation (second option))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (ensure-task-input ',name ,@(reduce #'append options :key #'canonicalise-option)))))

(defmethod make-load-form ((object task-input) &optional environment)
  (declare (ignore environment))
  `(find-task-input ',(task-input-name object)))

;; Class TASK-INPUT-FUNCTION
;;
;; A simple class that stores all information about a task input
;; function. Specifically,
;; - The class object of the task that is about to be performed.
;; - The class objects of the tasks that needed to have been performed. 
;; - The instance of TASK-INPUT for which the function belongs to.
;;
;; TASK-INPUT-FUNCTION instances are created using the
;; ENSURE-TASK-INPUT-FUNCTION function. The macro
;; DEFINE-TASK-INPUT-FUNCTION provides a more convenient method of
;; invoking ENSURE-TASK-INPUT-FUNCTION.

(defclass task-input-function ()
  ((performed-classes
    :initarg :performed-classes
    :reader task-input-function-performed-classes)
   (task-input
    :initarg :task-input
    :reader task-input-function-task-input)
   (target-class
    :initarg :target-class
    :reader task-input-function-target-class)
   (documentation
    :initarg :documentation
    :reader task-input-function-documentation))
  (:metaclass closer-mop:funcallable-standard-class))

(defun task-class-equal (class-a class-b)
  (assert (typep class-a 'task-class))
  (assert (typep class-b 'task-class))
  (equal class-a class-b))

(defun performed-classes-equal (classes-a classes-b)
  (and (= (length classes-a) (length classes-b))
       (null (set-difference classes-a classes-b :test #'task-class-equal))
       (null (set-difference classes-b classes-a :test #'task-class-equal))))

(defun target-class-equal (class-a class-b)
  (task-class-equal class-a class-b))

(defun task-input-function-equal (function-a function-b)
  (and (task-input-equal (task-input-function-task-input function-a)
			 (task-input-function-task-input function-b))
       (target-class-equal (task-input-function-target-class function-a)
			   (task-input-function-target-class function-b))
       (performed-classes-equal (task-input-function-performed-classes function-a)
				(task-input-function-performed-classes function-b))))

(defun ensure-task-input-function (task-input target-class performed-classes function &key documentation)
  (setf documentation (or documentation ""))
  (assert (typep task-input 'task-input))
  (assert (and (closer-mop:classp target-class)
	       (closer-mop:subclassp target-class (find-class 'task))))

  ;; Make sure PERFORMED-CLASSES contains valid values.
  (assert (every #'(lambda (x)
		     (and (closer-mop:classp x)
			  (typep x 'task-class)))
		 performed-classes))
  (assert (functionp function))
  (assert (= (length performed-classes)
	     (length (remove-duplicates performed-classes))))

  (let ((tif (make-instance 'task-input-function
			    :task-input task-input
			    :target-class target-class
			    :performed-classes performed-classes
			    :documentation documentation)))
    (closer-mop:set-funcallable-instance-function tif function)

    ;; Remove any existing function.
    (setf (task-input-functions task-input) (remove tif (task-input-functions task-input)
						    :test #'task-input-function-equal))
    
    ;; Add the new function.
    (push tif (task-input-functions task-input))
    tif))

(defmacro define-task-input-function (input target performed-tasks &body body)
  (let ((arguments (mapcar #'first performed-tasks))
	(performed-classes (mapcar #'(lambda (x)
				       `(find-class ',(second x)))
				   performed-tasks))
	(target    (if (eql target t)
		       'task
		       target)))
    (multiple-value-bind (remaining-forms declarations doc-string) (alexandria:parse-body body :documentation t)
      (declare (ignore remaining-forms declarations))
      `(ensure-task-input-function (find-task-input ',input)
				   (find-class ',target)
				   (list ,@performed-classes)
				   #'(lambda ,arguments
				       ,@body)
				   ,@(when doc-string
				       (list :documentation doc-string))))))

;; Finding the right task input function
;;
;; The function COMPUTE-TASK-INPUT-FUNCTIONS is used by an operation
;; to invoke the function needed to compute an input value.
;;
;; The function chosen is based on the target task and the tasks
;; already performed. The sorting predicates (in order of execution)
;; are:
;; - the most specific target class (See predicates SAF/TARGET-CLASS)
;; - the most specific performed tasks (See predicates SAF/PERFORMED-TASKS)
;;
;; More predicates are needed to guarantee uniqueness, but it is not
;; clear what is required until a better understanding of the usage
;; patterns are obtained. If more than one function matches then an
;; error is signalled.
;;
;; Another to aspect to consider is CALL-NEXT-TASK-INPUT-FUNCTION.

(defun compute-task-input-function (task-input target-task-class performed-task-classes)
  (first (compute-task-input-functions task-input target-task-class performed-task-classes)))

(defun compute-task-input-functions (task-input target-task-class performed-task-classes)
  (let ((functions (task-input-functions task-input)))
    (setf functions (remove-if-not #'(lambda (function)
				       (applicable-task-input-function-p function task-input target-task-class performed-task-classes))
				   functions))
    (sort-applicable-functions functions performed-task-classes)))

(defun applicable-task-input-function-p (function task-input target-task performed-task-classes)
  (declare (type task-input-function function)
	   (type task-input task-input)
	   (type task-class target-task)
	   (type list performed-task-classes))
  (and (task-input-equal task-input (task-input-function-task-input function))
       (closer-mop:subclassp target-task (task-input-function-target-class function))
       (every #'(lambda (p)
		  (declare (type task-class p))
		  (some #'(lambda (o)
			    (declare (type task-class o))
			    (closer-mop:subclassp o p))
			performed-task-classes))
	      (task-input-function-performed-classes function))))

(defun sort-applicable-functions (functions performed-tasks)
  (declare (ignore performed-tasks))
  (let ((groups (group-by (test=-function 'sort-applicable-functions) functions)))
    (dolist (group groups)
      (when (> (length group) 1)
	(error "
Unable to compute task input function as more than
one TASK-INPUT-FUNCTION fall in to the same bucket.

~A

You have not encountered a bug. This situation was intentionally
omitted as it was not clear what should be done at the time of
writing. I am very interested in your use case and your expectation on
what should occur. Please email me." group)))
    (mapcar #'first (sort groups (test>-function 'sort-applicable-functions) :key #'first))))

;; SAF == Sort Applicable Functions
;;; - target class
(defun saf/target-class-equal (a b)
  (declare (type task-class a b))
  (equal a b))

(defun saf/target-class-more-specific (a b)
  (declare (type task-class a b))
  (and (not (saf/target-class-equal a b))
       (closer-mop:subclassp a b)))

(defun saf/target-class-less-specific (a b)
  (declare (type task-class a b))
  (and (not (saf/target-class-equal a b))
       (closer-mop:subclassp b a)))

(define-predicates saf/target-class
  #'saf/target-class-equal
  #'saf/target-class-less-specific
  #'saf/target-class-more-specific)

;;; - performed classes
(defun saf/performed-classes-equal (classes-a classes-b)
  (and (= (length classes-a) (length classes-b))
       (null (set-difference classes-a classes-b :test #'saf/target-class-equal))
       (null (set-difference classes-b classes-a :test #'saf/target-class-equal))))

(defun saf/performed-classes-more-specific (classes-a classes-b)
  (labels ((count-subtypes (seq-a seq-b)
	     (count-if #'(lambda (class)
			   (some #'(lambda (other-class)
				     (closer-mop:subclassp class other-class))
				 seq-b))
		       seq-a)))
    (let ((len-a (length classes-a))
	  (len-b (length classes-b)))
      (cond	
	((> len-a len-b)
	 t)
	((= len-a len-b)
	 (let ((a-count (count-subtypes classes-a classes-b))
	       (b-count (count-subtypes classes-b classes-a)))
	   (and (plusp a-count)
		(zerop b-count))))
	(t
	 nil)))))

(defun saf/performed-classes-less-specific (classes-a classes-b)
  (and (not (saf/performed-classes-equal classes-a classes-b))
       (not (saf/performed-classes-more-specific classes-a classes-b))))

(define-predicates saf/performed-classes
  #'saf/performed-classes-equal
  #'saf/performed-classes-less-specific
  #'saf/performed-classes-more-specific)

;; - Combining target class and performed classes.
(defun saf/equal (a b)
  (declare (type task-input-function a b))
  (and (saf/target-class-equal (task-input-function-target-class a)
			       (task-input-function-target-class b))
       (saf/performed-classes-equal (task-input-function-performed-classes a)
				    (task-input-function-performed-classes b))))

(defun saf/more-specific (a b)
  (declare (type task-input-function a b))
  (let ((target-class-a (task-input-function-target-class a))
	(target-class-b (task-input-function-target-class b)))
    (or (saf/target-class-more-specific target-class-a target-class-b)
	(and (saf/target-class-equal target-class-a target-class-b)
	     (saf/performed-classes-more-specific (task-input-function-performed-classes a)
						  (task-input-function-performed-classes b))))))

(defun saf/less-specific (a b)
  (and (not (saf/equal a b))
       (not (saf/more-specific a b))))

(define-predicates sort-applicable-functions
  #'saf/equal
  #'saf/less-specific
  #'saf/more-specific)

;; Evaluating the task input function
;;
;; Given a chain containing the performed tasks, compute the task
;; input values.
(defun evaluate-task-input-function (task-input-function chain)  
  (let ((performed-tasks (mapcar #'(lambda (performed-class)
				     (declare (type task-class performed-class))
				     (let ((v (find-if #'(lambda (task)
							   (closer-mop:subclassp (class-of task) performed-class))
						       chain)))
				       (unless v
					 (error "Task input function ~A is not suitable for chain ~A"
						task-input-function chain))
				       v))
				 (task-input-function-performed-classes task-input-function))))
    (apply task-input-function performed-tasks)))
