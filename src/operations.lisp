(in-package "CHAINS")

;; Conditions
(define-condition no-applicable-task-input (error)
  ((name
    :initarg :name)
   (task
    :initarg :task)
   (chain
    :initarg :chain))
  (:report (lambda (stream condition)
	     (with-slots (name task chain) condition
	       (format stream "No applicable TASK-INPUT exists for name ~S, task ~S and chain ~A."
		       name (class-of task) chain)))))

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

(defun find-task-input (name)
  (or (get name 'task-input)
      (error "Unable to no TASK-INPUT with name ~A" name)))

(defun ensure-task-input (name &key documentation)
  (setf (get name 'task-input) (make-instance 'task-input
					      :name name
					      :documentation (or documentation ""))))

(defun task-input-equal (input-a input-b)
  (declare (type task-input input-a input-b))
  (equal input-a input-b))

(defmacro define-task-input (name &rest options)
  (labels ((canonicalise-option (option)
	     (ecase (first option)
	       (:documentation
		(list :documentation (second option))))))
    `(ensure-task-input ',name ,@(reduce #'append options :key #'canonicalise-option))))

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
  (declare (type task-class class-a class-b))
  (equal class-a class-b))

(defun performed-classes-equal (classes-a classes-b)
  (and (= (length classes-a) (length classes-b))
       (null (set-difference classes-a classes-b :test #'task-class-equal))))

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
  (declare (type task-input task-input)
	   (type task-class target-class))
  (setf documentation (or documentation ""))

  ;; Make sure PERFORMED-CLASSES contains valid values.
  (assert (every #'(lambda (x)
		     (typep x 'task-class))
		 performed-classes))
  (assert (functionp function))

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
