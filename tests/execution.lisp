(in-package "CHAINS.TESTS")

(define-task execution-task-1 ()
  ((sigma
    :initarg :sigma
    :reader sigma)))

(define-operation (task execution-task-1) ()
  (with-open-file (out "data.sexp" :direction :output)
    (write "hello-world" :stream out))
  :task-1)

(define-task execution-task-2 ()
  ((rho
    :initarg :rho
    :reader rho)))

(define-task-input input-2)
(define-task-input-function input-2 execution-task-2 ((task execution-task-1))
  (with-open-file (in (merge-pathnames "data.sexp" (task-data-directory task)))
    (read in)))

(define-operation (task execution-task-2) ((input-task input-2))
  input-task)

(define-task execution-task-3 ()
  ((gamma
    :initarg :gamma
    :reader gamma)))

(define-operation (task execution-task-3) ()
  (format nil "laughing-~A" (getf (operation-plist) :text)))

(defmethod task-completed-p ((task execution-task-3))
  (and (call-next-method)
       (not (string-equal (getf (operation-plist) :text) "dog"))))

(define-design execution-design
    ((:documentation "Designs for execution"))
  ((execution-task-1 (:sigma 1 2 3 4 5)))
  ((execution-task-2 (:rho -1 -2 -3 -4 -5)))
  ((execution-task-3 (:gamma 11 12 13 14 15))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-temporary-directory ()
    (labels ((random-directory-name ()
	       (merge-pathnames (make-pathname :directory (list :relative
								(concatenate 'string
									     "chains-"
									     (cl-fad::generate-random-string))))
				(translate-logical-pathname (logical-pathname "TEMPORARY-FILES:"))))
	     (trial ()
	       (multiple-value-bind (pathspec created?)
		   (ensure-directories-exist (random-directory-name))
		 (when created?
		   pathspec))))
      (loop
	 :for directory := (trial)
	 :for attempt :from 0 :below 100
	 :until directory
	 :finally (progn
		    (unless directory
		      (error "Unable to create temporary directory."))
		    (return (truename directory))))))

  (defun do-with-temporary-directory (function)
    (let ((tmp-directory (ensure-temporary-directory)))
      (unwind-protect
	   (funcall function tmp-directory)
	(cl-fad:delete-directory-and-files tmp-directory))))

  (defmacro with-temporary-directory ((var) &body body)
    `(do-with-temporary-directory #'(lambda (,var)
				      ,@body))))

(define-test compute-chains
  (let ((chains (compute-chains (generate 'execution-design))))
    (assert-true (= 125 (length chains)))
    (assert-true (every #'(lambda (x)
			    (= 3 (length x)))
			chains))
    (assert-true (every #'(lambda (x)
			    (typep (first x) 'execution-task-1))
			chains))
    (assert-true (every #'(lambda (x)
			    (typep (second x) 'execution-task-2))
			chains))
    (assert-true (every #'(lambda (x)
			    (typep (third x) 'execution-task-3))
			chains))
    
    (let ((group-1 (group-by #'= chains :key #'(lambda (x)
						 (sigma (first x))))))
      (assert-equal 5 (length group-1))
      (assert-true (every #'(lambda (x)
			      (let ((group-2 (group-by #'= x :key #'(lambda (x)
								      (rho (second x))))))
				(assert-equal 5 (length group-2))
				(assert-true (every #'(lambda (x)
							(assert-equal 5 (length x)))
						    group-2))))
			  group-1)))))

(define-test prepare-directory
  (let ((tmpdir nil))
    (with-temporary-directory (dir)
      (setf tmpdir dir)
      (let ((area (prepare-directory dir)))
	(assert-equal dir (area-directory area))
	(assert-true (cl-fad:directory-exists-p (area-directory area)))
	(assert-true (typep area 'prepared-directory))
	(assert-true (probe-file (merge-pathnames "chains-root.sexp" dir)))))
    (assert-false (cl-fad:directory-exists-p tmpdir))))

(define-test task-data-directory*
  (with-temporary-directory (dir)
    (let* ((area (prepare-directory dir))
	   (chain (list (make-instance 'execution-task-1 :sigma 1)
			(make-instance 'execution-task-2 :rho 2)
			(make-instance 'execution-task-3 :gamma 10))))
      (loop
	 :for len :from 1 :to 3
	 :for permutations := (let ((rv nil))
				(alexandria:map-permutations #'(lambda (x)
								 (push x rv))
							     chain
							     :length len)
				rv)
	 :do
	 (dotimes (i (length permutations))
	   (dotimes (j (length permutations))
	     (if (= i j)
		 (assert-true  (pathname-match-p (task-data-directory* area (elt permutations i))
						 (task-data-directory* area (elt permutations j))))
		 (assert-false (pathname-match-p (task-data-directory* area (elt permutations i))
						 (task-data-directory* area (elt permutations j))))))))

      (assert-false (pathname-match-p (task-data-directory* area (list (make-instance 'execution-task-1 :sigma 5)))
				      (task-data-directory* area (list (make-instance 'execution-task-1 :sigma 10))))))))

(define-test perform-leaf
  (with-temporary-directory (dir)
    (let* ((area (prepare-directory dir))
	   (task-1 (make-instance 'execution-task-1 :sigma 1))
	   (task-2 (make-instance 'execution-task-2 :rho 2))
	   (task-3 (make-instance 'execution-task-3 :gamma 10)))

      ;; Task-1
      (assert-false (chain-completed-p area (list task-1)))
      (assert-false (cl-fad:directory-exists-p (task-data-directory* area (list task-1))))
      (perform-leaf area (list task-1))
      (assert-true (chain-completed-p area (list task-1)))
      (assert-true (cl-fad:directory-exists-p (task-data-directory* area (list task-1))))
      (assert-true (cl-fad:file-exists-p (merge-pathnames "data.sexp" (task-data-directory* area (list task-1)))))

      ;; Task-2
      (assert-false (chain-completed-p area (list task-1 task-2)))
      (assert-false (cl-fad:directory-exists-p (task-data-directory* area (list task-1 task-2))))
      (perform-leaf area (list task-1 task-2))
      (assert-true (chain-completed-p area (list task-1 task-2)))
      (assert-true (cl-fad:directory-exists-p (task-data-directory* area (list task-1 task-2))))
      (assert-equal "hello-world" (task-value* area (list task-1 task-2)))

      ;; Task-3
      (assert-false (chain-completed-p area (list task-1 task-2 task-3)))
      (assert-false (cl-fad:directory-exists-p (task-data-directory* area (list task-1 task-2 task-3))))
      (perform-leaf area (list task-1 task-2 task-3) :text "man")
      (assert-true (chain-completed-p area (list task-1 task-2 task-3)))
      (assert-true (cl-fad:directory-exists-p (task-data-directory* area (list task-1 task-2 task-3))))
      (assert-equal "laughing-man" (task-value* area (list task-1 task-2 task-3)))

      (assert-false (chain-completed-p area (list task-1 task-2 task-3) :text "dog")))))

(define-test perform

  (with-temporary-directory (dir)
    (let ((*area* (prepare-directory dir))
	  (*chain* nil)
	  (chains (compute-chains (generate 'execution-design))))
      (dolist (chain chains)
	(perform *area* chain))
      
      (assert-true (null *chain*))
      (dolist (chain chains)
	(assert-true (chain-completed-p *area* chain))
	(assert-equal :task-1 (task-value 'execution-task-1 chain))
	(assert-equal "hello-world" (task-value 'execution-task-2 chain))
	(assert-equal "laughing-NIL" (task-value 'execution-task-3 chain)))

      (assert-true (null *chain*))
      (dolist (*chain* chains)
	(assert-equal :task-1 (task-value 'execution-task-1))
	(assert-equal "hello-world" (task-value 'execution-task-2))
	(assert-equal "laughing-NIL" (task-value 'execution-task-3))))))


(define-task task-complete-p-t1 ()
  ())

(define-operation (task task-complete-p-t1) ()
  (let ((iterations (or (getf (operation-plist) :iterations)
			0)))
    (loop
       :for iteration :from 0 :to iterations
       :do       
       (with-open-file (out (format nil "~d.sexp" iteration) :direction :output :if-exists nil)
	 (when out
	   (write iteration :stream out)
	   (terpri out))))
    iterations))

(defmethod task-completed-p ((task task-complete-p-t1))
  (let ((iterations (or (getf (operation-plist) :iterations)
			0)))
    (loop
       :for iteration :from 0 :to iterations
       :while (probe-file (format nil "~d.sexp" iteration))
       :finally (return (and (if (> iteration iterations)
				 t
				 nil)
			     (call-next-method))))))

(define-task task-complete-p-t2 ()
  ())

(define-operation (task task-complete-p-t2) ()
  (task-value (contains-task-p *chain* 'task-complete-p-t1)))

(define-test task-completed-p/iterations
  (let ((chain (list (make-instance 'task-complete-p-t1)
		     (make-instance 'task-complete-p-t2))))
    (with-temporary-directory (dir)
      (let ((area (prepare-directory dir)))
	(perform area chain)
	(assert-equal 0 (task-value 'task-complete-p-t2 chain area))

	(perform area chain :iterations 1)
	(assert-equal 1 (task-value 'task-complete-p-t2 chain area))))))
