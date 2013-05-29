(in-package "CHAINS.TESTS")

(define-task-input example-task-input
  (:documentation "A test task input definition."))

(define-task-input-function example-task-input t ((example example-task))
  "Example task input function documentation."
  (format nil "Computing EXAMPLE-TASK-INPUT value using task: ~A" example))

(define-test example-task-input
  (let ((v (find-task-input 'example-task-input)))
    (assert-true (typep v 'task-input))
    (assert-equal 'example-task-input (task-input-name v))
    (assert-equal "A test task input definition." (task-input-documentation v))
    (assert-equal 1 (length (task-input-functions v)))

    (let ((f (first (task-input-functions v))))
      (assert-equal v (task-input-function-task-input f))
      (assert-equal (find-class 'task) (task-input-function-target-class f))
      (assert-equal "Example task input function documentation." (task-input-function-documentation f))
      (assert-equal (list (find-class 'example-task)) (task-input-function-performed-classes f)))))

(define-test programmatic-task-input
  (assert-true (find-task-input 'example-task-input))
  (assert-error 'error (find-task-input 'no-such-task-input))

  (let ((v (ensure-task-input 'programmatic-task-input :documentation "Hello there matey.")))
    (assert-equal v (find-task-input 'programmatic-task-input))

    (let ((v2 (ensure-task-input 'programmatic-task-input :documentation "Hello there matey.")))
      (assert-false (equal v v2))
      (assert-equal v2 (find-task-input 'programmatic-task-input))

      (assert-equal "Hello there matey." (task-input-documentation v2))
      (assert-equal 0 (length (task-input-functions v2))))))

(define-task target-task ()
  ()
  (:documentation "A task which is used as a target in the task input
  function tests."))

(define-test programmatic-task-input-function
  (ensure-task-input 'programmatic-task-input)
  
  ;; Invalid TASK-INPUT value. Must be an instance of type TASK-INPUT.
  (assert-error 'error (ensure-task-input-function 'programmatic-task-input
						   (find-class 'example-task)
						   nil #'identity))

  (assert-error 'error (ensure-task-input-function (find-class 'task-input)
						   (find-class 'example-task)
						   nil #'identity))

  ;; Invalid TASK value. Must be a class which inherits from TASK-CLASS.
  (assert-error 'error (ensure-task-input-function (find-task-input 'example-task-input)
						   'example-task
						   nil #'identity))

  (assert-error 'error (ensure-task-input-function (find-task-input 'example-task-input)
						   (find-class 'standard-object)
						   nil #'identity))

  ;; Invalid processed tasks value. Must be instances of TASK-CLASS.
  (assert-error 'error (ensure-task-input-function (find-task-input 'example-task-input)
						   (find-class 'target-task)
						   (list 'example-task)
						   #'identity))

  ;; Duplicate performed class.
  (assert-error 'error (ensure-task-input-function (find-task-input 'example-task-input)
						   (find-class 'target-task)
						   (list (find-class 'example-task)
							 (find-class 'example-task))
						   #'identity))

  ;; Programmtic creation
  (let* ((ti (find-task-input 'programmatic-task-input))
	 (tif (ensure-task-input-function ti
					  (find-class 'target-task)
					  (list (find-class 'example-task))
					  #'identity)))
    (assert-equal (list tif) (task-input-functions ti))
    (assert-equal (find-class 'target-task) (task-input-function-target-class tif))
    (assert-equal "" (task-input-function-documentation tif))
    (assert-equal ti (task-input-function-task-input tif))
    (assert-equal (list (find-class 'example-task)) (task-input-function-performed-classes tif))

    ;; Reinsert the same task input function.
    (let ((tif2 (ensure-task-input-function ti
					    (find-class 'target-task)
					    (list (find-class 'example-task))
					    #'identity)))

      (assert-false (equal (list tif) (task-input-functions ti)))
      (assert-equal (list tif2) (task-input-functions ti)))

    ;; Inser another function with different performed-classes
    (ensure-task-input-function ti (find-class 'target-task) nil #'identity)
    (assert-equal 2 (length (task-input-functions ti)))

    ;; Insert another function with different target class.
    (ensure-task-input-function ti (find-class 'example-task) nil #'identity)
    (assert-equal 3 (length (task-input-functions ti)))))

;; Compute task input functions tests
(define-task input-data-1 ()
  ())

(define-task input-data-1-1 (input-data-1)
  ((sigma
    :initarg :sigma
    :reader sigma))
  (:default-initargs
   :sigma 1))

(define-task input-data-2 ()
  ())

(define-task-input algorithm-input-data
  (:documentation "Retrieve the input data to the algorithm."))

(define-task algorithm ()
  ())

(define-task algorithm-1 (algorithm)
  ())

(define-task-input-function algorithm-input-data algorithm ((data input-data-1))
  (declare (ignore data))
  'input-data-1)

(define-task-input-function algorithm-input-data algorithm ((data input-data-1-1))
  (values 'input-data-1-1 (1+ (sigma data))))

(define-task-input-function algorithm-input-data algorithm ((data input-data-2))
  (declare (ignore data))
  'input-data-2)

(define-test compute-task-input-functions
  (let ((ti (find-task-input 'algorithm-input-data)))
    (assert-equal 3 (length (task-input-functions ti)))

    (let ((fns (compute-task-input-functions ti (find-class 'algorithm-1) (list (find-class 'input-data-2)))))
      (assert-equal 1 (length fns))
      (assert-equal 'input-data-2 (funcall (first fns) :does-not-matter)))

    (let ((fns (compute-task-input-functions ti (find-class 'algorithm-1) (list (find-class 'input-data-1)))))
      (assert-equal 1 (length fns))
      (assert-equal 'input-data-1 (funcall (first fns) :does-not-matter)))

    (let ((fns (compute-task-input-functions ti (find-class 'algorithm-1) (list (find-class 'input-data-1-1)))))
      (assert-equal 2 (length fns))
      (assert-equal 'input-data-1-1 (funcall (first fns) (make-instance 'input-data-1-1))))))

(define-test evaluate-task-input-function
  (let* ((chain (list (make-instance 'input-data-1-1 :sigma 2)))
	 (ti (find-task-input 'algorithm-input-data))
	 (tif   (compute-task-input-function ti (find-class 'algorithm-1) (list (find-class 'input-data-1-1)))))
    (multiple-value-bind (arg1 arg2) (evaluate-task-input-function tif chain)
      (assert-equal 'input-data-1-1 arg1)
      (assert-equal 3 arg2))))
