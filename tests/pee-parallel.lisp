(in-package "CHAINS.PEE.TESTS")

(chains:define-task parallel-task ()
  ((sleep-time
    :initarg :sleep-time
    :reader sleep-time)
   (depth
    :initarg :depth
    :reader depth)))

(chains:define-task parallel-error-task ()
  ())

(defmethod chains:task-string ((task parallel-task))
  (format nil "parallel-task-~d-~d-~d"
	  (depth task)
	  (numerator (sleep-time task))
	  (denominator (sleep-time task))))

(chains:define-operation (task parallel-task) ()
  (format t ";;; Starting ~d: (~{~A~^, ~})~%" (depth task) (mapcar #'sleep-time chains:*chain*))
  (sleep (sleep-time task))
  (format t ";;; Finished ~d: (~{~A~^, ~})~%" (depth task) (mapcar #'sleep-time chains:*chain*)))

(chains:define-operation (task parallel-error-task) ()
  (error "Error signalled."))

(chains:define-design parallel-design
    ((:documentation "Tasks for the PARALLEL-PERFORM test."))
  ((parallel-task (:depth 1)
		  (:sleep-time (:splice (loop :for x :from 1/4 :to 1 :by 1/4 :collect x)))))
  ((parallel-task (:depth 2)
		  (:sleep-time (:splice (loop :for x :from 1/3 :to 1 :by 1/3 :collect x))))))

(chains:define-design parallel-error-design
    ((:documentation "Tasks for the PARALLEL-PERFORM/ERROR test."))
  ((parallel-task (:depth 1)
		  (:sleep-time 1 5)))
  ((parallel-task (:depth 2)
		  (:sleep-time (:splice (loop :for x :from 1/4 :to 1 :by 1/4 :collect x)))))
  ((parallel-error-task)))

(define-test parallel-perform
  (format t ";; PARALLEL-PERFORM test.")
  (dolist (*number-of-processes* (list 1 2 3 4))
    (format t "~%;; - ~d Processes~%" *number-of-processes*)
    (with-temporary-directory (dir)
      (let* ((tree (generate 'parallel-design))
	     (area (chains:prepare-directory dir))
	     (chains (compute-chains tree)))
	(labels ((completedp (chain)
		   (chains:chain-completed-p area chain)))
	  (assert-false (every #'completedp chains))
	  (let ((threads-before (bordeaux-threads:all-threads)))
	    (parallel-perform area tree)
	    (assert-equal (length threads-before)
			  (length (bordeaux-threads:all-threads))))
	  (assert-true (every #'completedp chains)))))))

(define-test parallel-perform/error
  (format t ";; PARALLEL-PERFORM/ERROR test.")
  (with-temporary-directory (dir)
    (let* ((tree (generate 'parallel-error-design))
	   (area (chains:prepare-directory dir))
	   (chains (compute-chains tree)))
      (labels ((completedp (chain)
		 (chains:chain-completed-p area chain)))
	(assert-false (every #'completedp chains))
	(let ((threads-before (bordeaux-threads:all-threads)))
	  (assert-error 'error (parallel-perform area tree))
	  (assert-equal (length threads-before)
			(length (bordeaux-threads:all-threads))))
	(assert-false (every #'completedp chains))))))
