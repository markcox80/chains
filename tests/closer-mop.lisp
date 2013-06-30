(in-package "CHAINS.CLOSER-MOP-TESTS")
;; Remember that the CHAINS.CLOSER-MOP-TESTS package is isolated from
;; the CHAINS package.
;;
;; The tests seem strange given it is almost an exact copy of the code
;; in src/tasks.lisp and src/operations.lisp. The reason I have done
;; this is this code only depends on CLOSER-MOP and not other
;; components of CHAINS.

;; Programmatic creation of methods

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closer-mop:defgeneric perform-operation (task chain))

  (defun make-perform-operation-method-lambda (task-var chain-var body environment)
    (let ((gf #'perform-operation))
      (closer-mop:make-method-lambda gf
				     (closer-mop:class-prototype
				      (closer-mop:generic-function-method-class gf))
				     `(lambda (,task-var ,chain-var)
					,@body)
				     environment)))

  (defun make-perform-operation-method (task-var chain-var task-class chain-class method-lambda initialisation-arguments)
    (assert (listp initialisation-arguments))
    (assert (closer-mop:classp task-class))
    (assert (closer-mop:classp chain-class))
    (let ((gf #'perform-operation))
      (apply #'make-instance (closer-mop:generic-function-method-class gf)
	     :specializers (list task-class chain-class)
	     :lambda-list (list task-var chain-var)
	     :function method-lambda
	     initialisation-arguments)))

  (defmacro define-operation ((task-var task-class) (chain-var chain-class) &body body &environment environment)
    (multiple-value-bind (method-lambda initialisation-arguments)
	(make-perform-operation-method-lambda task-var chain-var body environment)
      `(let ((m (make-perform-operation-method ',task-var ',chain-var
					       (find-class ',task-class) (find-class ',chain-class)
					       (function ,method-lambda)
					       ',initialisation-arguments)))
	 (add-method #'perform-operation m)))))

(defclass my-task ()
  ())

(let ((object 2))
  (define-operation (task my-task) (chain t)
    (list task chain object)))

(lisp-unit:define-test perform-operation
  (let ((task (make-instance 'my-task)))
    (lisp-unit:assert-equal (list task 1 2) (perform-operation task 1))))

;; Programmatic creation of classes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (closer-mop:defclass my-word-class (closer-mop:standard-class)
    ()))

(defmethod closer-mop:validate-superclass ((class my-word-class)
					   (superclass (eql (find-class 'closer-mop:standard-object))))
  t)

(closer-mop:defclass my-word (closer-mop:standard-object)
  ()
  (:metaclass my-word-class))

(lisp-unit:define-test programmatic-my-word
  (let ((class (closer-mop:ensure-class 'my-subword 
					:direct-superclasses (list (find-class 'my-word))
					:metaclass (find-class 'my-word-class))))
    (lisp-unit:assert-true (typep class 'my-word-class))
    (lisp-unit:assert-true (closer-mop:subclassp class (find-class 'my-word)))
    (lisp-unit:assert-true (find-class 'my-subword))))
