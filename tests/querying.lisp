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

(define-task query-data ()
  ((fold
    :initarg :fold
    :reader fold
    :predicates number)))

(define-task query-preparation ()
  ((operation
    :initarg :operation
    :reader operation)))

(define-design compound-query-design
  ((:documentation "The set of chains used in PREPARE-GROUP-CHAINS-TEST/COMPOUND."))
  ((query-data (:fold 0 1 2 3 4)))
  ((query-preparation (:operation :paper :scissors :rock)))
  ((query-algorithm-1 (:sigma 1 2)
		      (:rho 1 2))))

(define-test prepare-group-chains-test/compound
  (let* ((tree (generate 'compound-query-design))
	 (chains (compute-chains tree))
	 (groups (group-chains chains '(and (= query-preparation)
					    (= query-algorithm-1)))))
    (assert-equal 12 (length groups))
    (labels ((chain-fold (chain)
	       (fold (first chain)))
	     (chain-op-sigma-rho (chain)
	       (destructuring-bind (data preparation algorithm) chain
		 (declare (ignore data))
		 (list (operation preparation) (sigma algorithm) (rho algorithm)))))
      (let ((op-sigma-rhos nil))
	(dolist (group groups)
	  (assert-equal 5 (length group))
	  (let ((op-sigma-rho (chain-op-sigma-rho (first group))))	  
	    (assert-false (find op-sigma-rho op-sigma-rhos :test #'equal))
	    (push op-sigma-rho op-sigma-rhos)
	    (dolist (chain group)
	      (assert-equal op-sigma-rho (chain-op-sigma-rho chain))))
	  (dotimes (fold-number 5)
	    (assert-equal 1 (count fold-number group :test #'= :key (alexandria:compose #'fold #'first)))))))))

(define-test prepare-group-chains-sort-test
  ;; (< TASK-CLASS SLOT)
  (dolist (exp (list `(< query-algorithm sigma)
		     `(< ,(find-class 'query-algorithm)
			 ,(find 'sigma (closer-mop:class-direct-slots (find-class 'query-algorithm))
				:key #'closer-mop:slot-definition-name))))
    (let ((fn (prepare-group-chains-sort-test exp)))
      (assert-true (funcall fn
			    (list (make-instance 'query-algorithm-1 :sigma 1))
			    (list (make-instance 'query-algorithm-2 :sigma 2))))
      (assert-false (funcall fn
			     (list (make-instance 'query-algorithm-1 :sigma 2))
			     (list (make-instance 'query-algorithm-2 :sigma 2))))
      (assert-false (funcall fn
			     (list (make-instance 'query-algorithm-1 :sigma 2))
			     (list (make-instance 'query-algorithm-2 :sigma 1))))))
  ;; (> TASK-CLASS SLOT)
  (dolist (exp (list `(> query-algorithm sigma)
		     `(> ,(find-class 'query-algorithm)
			 ,(find 'sigma (closer-mop:class-direct-slots (find-class 'query-algorithm))
				:key #'closer-mop:slot-definition-name))))
    (let ((fn (prepare-group-chains-sort-test exp)))
      (assert-false (funcall fn
			    (list (make-instance 'query-algorithm-1 :sigma 1))
			    (list (make-instance 'query-algorithm-2 :sigma 2))))
      (assert-false (funcall fn
			     (list (make-instance 'query-algorithm-1 :sigma 2))
			     (list (make-instance 'query-algorithm-2 :sigma 2))))
      (assert-true (funcall fn
			     (list (make-instance 'query-algorithm-1 :sigma 2))
			     (list (make-instance 'query-algorithm-2 :sigma 1))))))

  ;; (:classes &rest classes)
  (dolist (exp (list `(:classes query-algorithm-2 query-algorithm-1 query-algorithm-3)
		     `(:classes ,(find-class 'query-algorithm-2)
				,(find-class 'query-algorithm-1)
				,(find-class 'query-algorithm-3))))
    (let ((fn (prepare-group-chains-sort-test exp)))
      (assert-false (funcall fn
			    (list (make-instance 'query-algorithm-1 :sigma 1))
			    (list (make-instance 'query-algorithm-2 :sigma 2))))
      (assert-false (funcall fn
			    (list (make-instance 'query-algorithm-3 :sigma 1))
			    (list (make-instance 'query-algorithm-1 :sigma 2))))
      (assert-true (funcall fn
			     (list (make-instance 'query-algorithm-2 :sigma 2))
			     (list (make-instance 'query-algorithm-1 :sigma 2))))
      (assert-true (funcall fn
			     (list (make-instance 'query-algorithm-1 :sigma 2))
			     (list (make-instance 'query-algorithm-3 :sigma 1))))))
  (let ((fn (prepare-group-chains-sort-test `(:classes query-algorithm-2 query-algorithm-1))))
    (assert-error 'error (funcall fn
				  (list (make-instance 'query-algorithm-1 :sigma 1))
				  (list (make-instance 'query-algorithm-3 :sigma 2))))))

(define-test group-chains
  (let* ((chain-a (list (make-instance 'input-data-1) (make-instance 'query-algorithm-1 :sigma 1)))
	 (chain-b (list (make-instance 'input-data-1) (make-instance 'query-algorithm-1 :sigma 2)))
	 (chain-c (list (make-instance 'input-data-2) (make-instance 'query-algorithm-2 :sigma 1)))
	 (chains (list chain-a chain-b chain-c)))
    (assert-equal (list (list chain-a chain-b)
			(list chain-c))
		  (group-chains chains 'query-algorithm :sort '(:classes input-data-1 input-data-2)))
    (assert-equal (list (list chain-b chain-a)
			(list chain-c))
		  (group-chains chains 'query-algorithm
				:sort '(:classes input-data-1 input-data-2)
				:sort-inner '(> query-algorithm sigma)))
    (assert-equal (list (list chain-c)
			(list chain-b chain-a))
		  (group-chains chains 'query-algorithm
				:sort '(:classes input-data-2 input-data-1)
				:sort-inner '(> query-algorithm sigma)))
    (assert-equal (list (list chain-b)
			(list chain-a chain-c))
		  (group-chains chains '(= query-algorithm sigma)
				:sort '(> query-algorithm sigma)))
    (assert-equal (list (list chain-c chain-a)
			(list chain-b))
		  (group-chains chains '(= query-algorithm sigma)
				:sort '(< query-algorithm sigma)
				:sort-inner '(:classes input-data-2 input-data-1)))))

(define-test find-chains-with-task
  (let* ((chain-a (list (make-instance 'query-algorithm-1 :rho 1 :sigma 1)))
	 (chain-b (list (make-instance 'query-algorithm-2 :rho 2 :sigma 1)))
	 (chains (list chain-a chain-b)))
    (assert-equal chains (find-chains-with-task chains 'query-algorithm))    
    (assert-equal (list chain-a) (find-chains-with-task chains 'query-algorithm-1))
    (assert-equal (list chain-a) (find-chains-with-task chains (find-class 'query-algorithm-1)))
    (assert-equal (list chain-b) (find-chains-with-task chains 'query-algorithm-2))
    (assert-equal (list chain-b) (find-chains-with-task chains (find-class 'query-algorithm-2)))))
