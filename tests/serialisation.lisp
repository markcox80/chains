(in-package "CHAINS.TESTS")

(define-task base-serialisation-task ()
  ((sigma
    :initarg :sigma
    :reader sigma)))

(define-task serialisation-task (base-serialisation-task)
  ((rho
    :initarg :rho
    :reader rho)))

(define-test serialisation
  (let* ((obj (make-instance 'serialisation-task :sigma 0.5 :rho 2))
	 (str (with-output-to-string (out)
		(serialise-task out obj)))
	 (v   (with-input-from-string (in str)
		(eval (read in)))))
    (assert-equal (sigma obj) (sigma v))
    (assert-equal (rho obj) (rho v))))

(define-test task-string
  (let ((obj (make-instance 'serialisation-task :sigma 0.5 :rho 2)))
    (assert-equal "serialisation-task-0.5-2" (task-string obj)))

  (let ((obj (make-instance 'base-serialisation-task :sigma 0.1)))
    (assert-equal "base-serialisation-task-0.1" (task-string obj))))

(define-task task-with-no-slots ()
  ())

(define-test task-string/no-slots
  (let ((object (make-instance 'task-with-no-slots)))
    (assert-equal "task-with-no-slots" (task-string object))))
