(in-package "CHAINS.PEE")

(defclass parallel-perform-queue/worker-data ()
  ((area
    :initarg :area
    :reader area)
   (perform-args
    :initarg :perform-args
    :reader perform-args)
   (number-of-processes
    :initarg :number-of-processes
    :reader number-of-processes)
   (fifo-mutex
    :initarg :fifo-mutex
    :reader fifo-mutex)
   (fifo-condition
    :initarg :fifo-condition
    :reader fifo-condition)
   (job-fifo
    :initarg :job-fifo
    :accessor job-fifo)
   (job-error
    :initarg :job-error
    :accessor job-error)
   (processes-waiting
    :initarg :processes-waiting
    :accessor processes-waiting))
  (:default-initargs
   :job-error nil
   :job-fifo nil
   :fifo-mutex (bordeaux-threads:make-lock "PARALLEL-PERFORM-MUTEX")
   :fifo-condition (bordeaux-threads:make-condition-variable :name "PARALLEL-PERFORM-CONDITION")
   :processes-waiting nil))

;; Access to the fields JOB-FIFO and JOB-ERROR should be serialised
;; using the mutex FIFO-MUTEX. The functions WAIT-FOR-JOB,
;; ENQUEUE-JOB, INFORM-SUPERVISOR-OF-ERROR and
;; WAIT-UNTIL-DONE-OR-ERROR should be the only functions that use
;; FIFO-MUTEX and FIFO-CONDITION.

(defun wait-for-job/attempt (worker-data)
  (declare (type parallel-perform-queue/worker-data worker-data))
  (let ((current-thread (bordeaux-threads:current-thread)))
    (with-accessors ((job-fifo job-fifo)
		     (fifo-mutex fifo-mutex)
		     (fifo-condition fifo-condition)
	     (number-of-processes number-of-processes)
		     (processes-waiting processes-waiting))
	worker-data
      (bordeaux-threads:with-lock-held (fifo-mutex)

	(unless job-fifo
	  (push current-thread processes-waiting)
	  (cond
	    ((= number-of-processes (length processes-waiting))
	     (push :quit job-fifo))
	    (t	     
	     (bordeaux-threads:condition-wait fifo-condition fifo-mutex)))
	  (setf processes-waiting (remove current-thread processes-waiting)))

	(let ((v (if (quit-job-p (first job-fifo))
		     (first job-fifo)
		     (pop job-fifo))))	 
	  (bordeaux-threads:condition-notify fifo-condition)
	  v)))))

(defun wait-for-job (worker-data)
  (loop
     :for v := (wait-for-job/attempt worker-data)
     :until v
     :finally (return v)))

(defun enqueue-job (worker-data new-performed-tasks child)
  (declare (type parallel-perform-queue/worker-data worker-data))
  (with-accessors ((job-fifo job-fifo)
		   (fifo-mutex fifo-mutex)
		   (fifo-condition fifo-condition))
      worker-data
    (bordeaux-threads:with-lock-held (fifo-mutex)
      (setf job-fifo (append job-fifo
			     (list (list new-performed-tasks child))))
      (bordeaux-threads:condition-notify fifo-condition))))

(defun inform-supervisor-of-error (worker-data chain error)
  (declare (type parallel-perform-queue/worker-data worker-data))
  (with-accessors ((job-fifo job-fifo)
		   (job-error job-error)
		   (fifo-mutex fifo-mutex)
		   (fifo-condition fifo-condition))
      worker-data
    (bordeaux-threads:with-lock-held (fifo-mutex)
      (unless job-error
	(setf job-error (list chain error))
	(push :quit job-fifo))
      (bordeaux-threads:condition-notify fifo-condition))))

(defun quit-job-p (object)
  (eql object :quit))

(defun job-details (object)
  (assert (and (listp object)
	       (= 2 (length object))))
  (values-list object))

(defclass parallel-perform-queue ()
  ((processes
    :initarg :processes
    :reader processes)
   (worker-data
    :initarg :worker-data
    :reader worker-data)))

(defun wait-until-done-or-error (queue)
  (declare (type parallel-perform-queue queue))

  (map nil #'bordeaux-threads:join-thread (processes queue))

  (with-accessors ((job-error job-error)
		   (job-fifo job-fifo))
      (worker-data queue)
    (cond
      (job-error
       (assert job-fifo)
       (destructuring-bind (chain error) job-error
	 (error "Whilst processing chain ~A the following error was signaled.~%~A~%" 
		chain error)))
      (t
       (assert (eql :quit (first job-fifo)))
       nil))))

(defun wait-until-ready-for-work (queue)
  (loop
     :for number-waiting := (bordeaux-threads:with-lock-held ((fifo-mutex (worker-data queue)))
			      (length (processes-waiting (worker-data queue))))
     :while (< number-waiting (number-of-processes (worker-data queue))))  
  (setf (processes-waiting (worker-data queue)) nil))

(defun make-parallel-perform-worker (worker-data)
  (declare (type parallel-perform-queue/worker-data worker-data))
  (let ((area (area worker-data)))
    (lambda ()
      ;; Wait for first notification before beginning
      (bordeaux-threads:with-lock-held ((fifo-mutex worker-data))
	(push (bordeaux-threads:current-thread) (processes-waiting worker-data))
	(bordeaux-threads:condition-wait (fifo-condition worker-data)
					 (fifo-mutex worker-data)))

      (loop
	 :for job := (wait-for-job worker-data)	 
	 :until (quit-job-p job)
	 :do
	 (multiple-value-bind (performed-tasks tree) (job-details job)
	   (let ((new-performed-tasks (append performed-tasks (list (value tree)))))
	     (handler-case (progn
			     (apply #'perform-leaf area new-performed-tasks (perform-args worker-data))
			     (dolist (child (children tree))
			       (enqueue-job worker-data new-performed-tasks child)))
	       (error (c)
		 (inform-supervisor-of-error worker-data new-performed-tasks c)))))))))

(defun make-parallel-perform-queue (number-of-processes area perform-args)
  (let ((worker-data (make-instance 'parallel-perform-queue/worker-data
				    :area area
				    :perform-args perform-args
				    :number-of-processes number-of-processes)))
    (make-instance 'parallel-perform-queue
		   :worker-data worker-data
		   :processes (loop
				 :for x :from 0 :below number-of-processes
				 :collect
				 (bordeaux-threads:make-thread (make-parallel-perform-worker worker-data)
							       :name "PARALLEL-WORKER-THREAD")))))

(defvar *number-of-processes* 2)

(defun parallel-perform (area tree &rest args &key force &allow-other-keys)
  "Execute all tasks in TREE employing parallelism if possible. All
ARGS are passed to the function PERFORM-LEAF.

The number of processes used to execute the tree is governed by the
dynamic variable *NUMBER-OF-PROCESSES*. If an error occurs in any
task, all currently executing tasks are finished before resignalling
the error."
  (declare (ignore force))
  (let ((queue (make-parallel-perform-queue *number-of-processes* area args)))
    (wait-until-ready-for-work queue)

    ;; Push stuff on to the queue.
    (if (value tree)
	(enqueue-job (worker-data queue) nil tree)
	(dolist (item (children tree))
	  (enqueue-job (worker-data queue) nil item)))
    (wait-until-done-or-error queue)))
