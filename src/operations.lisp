(in-package "CHAINS")

(defgeneric perform-operation (task chain))

(defun obtain-task-inputs (chain target-class task-inputs)
  "Compute the task inputs for the given target class using the chain."
  (loop
     :with performed-classes := (mapcar #'class-of chain)
     :for task-input :in task-inputs
     :collect
     (let ((fn (compute-task-input-function task-input target-class performed-classes)))
       (unless fn
	 (error 'no-applicable-task-input-function
		:task-input task-input
		:target-class target-class
		:performed-classes performed-classes))
       (evaluate-task-input-function fn chain))))

(defun make-operation-method-lambda (task-var task-input-vars task-inputs chain-var body)
  "Create a method lambda that will be used for a method added to the
  PERFORM-OPERATION generic function. The resulting method added akin to
  the method lambda created when evaluating the following DEFMETHOD

  `(let ((#:task-inputs ',task-input-classes))
     (defmethod perform-operation ((,task-var ,task-class) #:chain)
       (destructuring-bind ,task-input-vars
           (obtain-task-inputs #:chain (class-of ,task-var) #:task-input-classes)
         ,@body))))

  Note that the TASK-CLASS above is not used in
  MAKE-OPERATION-METHOD-LAMBDA. It is however used in MAKE-OPERATION-METHOD.
"
  (let ((gf (ensure-generic-function 'perform-operation)))
    (closer-mop:make-method-lambda
     (ensure-generic-function 'perform-operation)
     (closer-mop:class-prototype (closer-mop:generic-function-method-class gf))
     `(lambda (,task-var ,chain-var)
	(destructuring-bind ,task-input-vars
	    (obtain-task-inputs ,chain-var
				(class-of ,task-var)
				',task-inputs)
	  ,@body))
     nil)))

(defun make-operation-method (task-var task-class task-input-vars task-inputs chain-var body)
  (let ((gf (ensure-generic-function 'perform-operation)))
    (multiple-value-bind (method-lambda initialisation-arguments)
	(make-operation-method-lambda task-var task-input-vars
				      task-inputs chain-var body)
      (apply #'make-instance (closer-mop:generic-function-method-class gf)
	     :specializers (list task-class (find-class t))
	     :lambda-list `(,task-var ,chain-var)
	     :function (compile nil method-lambda)
	     initialisation-arguments))))

(defun ensure-operation (task-var task-class task-input-vars task-inputs chain-var body)
  "Add a method to the PERFORM-OPERATION generic function which is similar
  to the method created with the following DEFMETHOD

  `(let ((#:task-inputs ',task-input-classes))
     (defmethod perform-operation ((,task-var ,task-class) #:chain)
       (destructuring-bind ,task-input-vars
           (obtain-task-inputs #:chain (class-of ,task-var) #:task-input-classes)
         ,@body))))

NOTE!! The input to ENSURE-OPERATION is slightly different to the above.

TASK-VAR is a symbol used to denote the task whose operation is being performed.
TASK-CLASS is a class metaobject.
TASK-INPUT-VARS is a list of symbols to assign the compute input values.
TASK-INPUTS is a list of TASK-INPUT instances.
CHAIN-VAR is a symbol used to denote the performed tasks.
BODY is the body of the method.
"
  (let ((gf (ensure-generic-function 'perform-operation)))
    (add-method gf (make-operation-method task-var task-class
					  task-input-vars task-inputs
					  chain-var body))))

(defmacro define-operation ((task-var task-class) (&rest task-inputs) &body body)
  (let ((task-input-vars (mapcar #'first task-inputs))
	(task-inputs (mapcar #'(lambda (x)
				 `(find-task-input ',(second x)))
			     task-inputs)))
    `(ensure-operation ',task-var
		       (find-class ',task-class)
		       ',task-input-vars
		       (list ,@task-inputs)
		       (gensym "CHAIN")
		       ',body)))
