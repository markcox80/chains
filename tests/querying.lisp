(in-package "CHAINS.TESTS")

(define-task query-algorithm ()
  ((sigma
    :initarg :sigma
    :reader sigma
    :predicates number)))

(define-task query-algorithm-1 (query-algorithm)
  ((rho
    :initarg :rho
    :reader rho
    :predicates number)))

(define-task query-algorithm-2 (query-algorithm)
  ((rho
    :initarg :rho
    :reader rho
    :predicates number)))

(define-task query-algorithm-3 (query-algorithm)
  ((gamma
    :initarg :gamma
    :reader gamma
    :predicates number)))

(define-test prepare-group-chains-test
  (let ((chains-1   (list (make-instance 'query-algorithm-1 :rho 1 :sigma 2)))
	(chains-1-other (list (make-instance 'query-algorithm-1 :rho 1 :sigma 2)))
	(chains-1-2 (list (make-instance 'query-algorithm-1 :rho 1 :sigma 3)))
	(chains-2 (list (make-instance 'query-algorithm-2 :rho 1 :sigma 2)))
	(chains-3 (list (make-instance 'query-algorithm-3 :gamma 3 :sigma 3)))
	(chains-other (list (make-instance 'example-task))))

    ;; TASK-CLASS
    ;; symbol
    (dolist (fn (list (prepare-group-chains-test 'query-algorithm)
		      (prepare-group-chains-test (find-class 'query-algorithm))))
      (assert-true (funcall fn chains-1 chains-1))
      (assert-true (funcall fn chains-1 chains-1-2))
      (assert-false (funcall fn chains-1 chains-2))
      (assert-false (funcall fn chains-1 chains-3))
      (assert-false (funcall fn chains-1 chains-other)))

    ;; (= TASK-CLASS)
    ;; (= symbol)
    (dolist (fn (list (prepare-group-chains-test '(= query-algorithm))
		      (prepare-group-chains-test `(= ,(find-class 'query-algorithm)))))
      (assert-true (funcall fn chains-1 chains-1))
      (assert-true (funcall fn chains-1 chains-1-other))
      (assert-false (funcall fn chains-1 chains-1-2))
      (assert-true (funcall fn chains-1 chains-2))
      (assert-false (funcall fn chains-1 chains-3))
      (assert-false (funcall fn chains-1 chains-other)))

    ;; (= TASK-CLASS TASK-DIRECT-SLOT-DEFINITION)
    ;; (= symbol slot-name)
    (dolist (fn (list (prepare-group-chains-test '(= query-algorithm sigma))
		      (prepare-group-chains-test `(= ,(find-class 'query-algorithm)
						     ,(find 'sigma (closer-mop:class-direct-slots (find-class 'query-algorithm))
							    :key #'closer-mop:slot-definition-name)))))
      (assert-true (funcall fn chains-1 chains-1))
      (assert-false (funcall fn chains-1 chains-1-2))
      (assert-true (funcall fn chains-1 chains-2))
      (assert-false (funcall fn chains-1 chains-3))
      (assert-false (funcall fn chains-1 chains-other)))))
