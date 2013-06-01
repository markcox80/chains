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

(defun make-operation-method-lambda (task-var task-input-vars task-inputs chain-var body environment)
  "Create a method lambda that will be used for a method added to the
PERFORM-OPERATION generic function. The resulting method lambda is
similar to the method lambda created when evaluating the following
DEFMETHOD

  `(let ((#:task-inputs ',task-input-classes))
     (defmethod perform-operation ((,task-var ,task-class) #:chain)
       (destructuring-bind ,task-input-vars
           (obtain-task-inputs #:chain (class-of ,task-var) #:task-input-classes)
         ,@body))))

Note that the TASK-CLASS above is not used in
MAKE-OPERATION-METHOD-LAMBDA. It is however used in
MAKE-OPERATION-METHOD.

Like CLOSER-MOP:MAKE-METHOD-LAMBDA, this function should be invoked
within a macro and the result used in the returned expression.

See DEFINE-OPERATION on how to use this function correctly.
"
  (let ((gf (ensure-generic-function 'perform-operation)))
    (closer-mop:make-method-lambda
     (ensure-generic-function 'perform-operation)
     (closer-mop:class-prototype (closer-mop:generic-function-method-class gf))
     `(lambda (,task-var ,chain-var)
	(destructuring-bind ,task-input-vars
	    (obtain-task-inputs ,chain-var
				(class-of ,task-var)
				(list ,@task-inputs))
	  ,@body))
     environment)))

(defun make-operation-method (task-var task-class chain-var method-lambda initialisation-arguments)
  "Create a method for the PERFORM-OPERATION function. 

TASK-VAR is the name of the argument that will be bound to the
instance of TASK-CLASS.

CHAIN-VAR is the name of the argument that will be bound the chain.

TASK-CLASS is the class of the task the method is for.

METHOD-LAMBDA is the :FUNCTION initialisation argument for the method.

INITIALISATION-ARGUMENTS are any other initialisation arguments for
the new method object.

See the macro DEFINE-OPERATION on how to use this function correctly."
  (declare (type task-class task-class))
  (assert (closer-mop:subclassp task-class (find-class 'task)))
  (let ((gf (ensure-generic-function 'perform-operation)))
    (apply #'make-instance (closer-mop:generic-function-method-class gf)
	   :specializers (list task-class (find-class t))
	   :lambda-list `(,task-var ,chain-var)
	   :function method-lambda
	   initialisation-arguments)))

(defun ensure-operation (method)
  "Add METHOD to the PERFORM-OPERATION generic function."
  (let ((gf (ensure-generic-function 'perform-operation)))
    (add-method gf method)))

(defmacro define-operation ((task-var task-class) (&rest task-inputs) &body body &environment env)
  "Define the operation to be performed for the TASK-CLASS. All task
inputs are defined with TASK-INPUTS. 

Example of DEFINE-OPERATION:
  (define-operation (task my-task) ((input task-input))
    (compute-solution (rho task) input)
"
  (let ((task-input-vars (mapcar #'first task-inputs))
	(task-inputs (mapcar #'(lambda (x)
				 `(find-task-input ',(second x)))
			     task-inputs))
	(chain-var (gensym)))
    (multiple-value-bind (method-lambda initialisation-arguments)
	(make-operation-method-lambda task-var task-input-vars task-inputs chain-var body env)
      `(let ((m (make-operation-method ',task-var
				       (find-class ',task-class)
				       ',chain-var
				       (function ,method-lambda)
				       ',initialisation-arguments)))
	 (ensure-operation m)))))
