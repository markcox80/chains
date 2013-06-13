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
       (not (eql (getf (operation-plist) :text) "dog"))))

(define-design execution-design
    ((:documentation "Designs for execution"))
  ((execution-task-1 (:sigma 1 2 3 4 5)))
  ((execution-task-2 (:rho -1 -2 -3 -4 -5)))
  ((execution-task-3 (:gamma 11 12 13 14 15))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-temporary-directory (function)
    (let ((tmp-directory "/tmp/chains-execution-test/"))
      (multiple-value-bind (pathspec created?) (ensure-directories-exist tmp-directory)
	(unless created?
	  (error "Directory ~S already exists." tmp-directory))
	(unwind-protect
	     (funcall function pathspec)
	  (cl-fad:delete-directory-and-files tmp-directory)))))

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
