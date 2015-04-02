(in-package "CHAINS")

;; Area protocol
(defgeneric task-data-directory* (area chain))
(defgeneric task-completed-p* (area chain))
(defgeneric task-value* (area chain))
(defgeneric (setf task-value*) (value area chain))

(defvar *area* nil)
(defvar *chain* nil)
(defvar *operation-plist*)

;; The follow functions can only be called if CHAIN-COMPLETED-P and/or
;; PERFORM-LEAF is on the call stack.
;; - TASK-COMPLETED-P
;; - TASK-DATA-DIRECTORY
;; - TASK-VALUE
;;
;; These functions also rely entirely on the *AREA*, *PEROFMRED-TASKS*
;; and *OPERATION-PLIST* dynamic variables which are set up by
;; CHAIN-COMPLETED-P and PERFORM-LEAF.
(defgeneric task-completed-p (task))

(defun position-of-task-in-chain (task chain)
  (labels ((fn/task-instance (object)
	     (eql task object))
	   (fn/task-class-instance (object)
	     (typep object task)))
    (position-if (cond
		   ((typep task 'task)
		    #'fn/task-instance)
		   ((typep task 'task-class)
		    #'fn/task-class-instance)
		   ((symbolp task)
		    #'fn/task-class-instance)
		   ((functionp task)
		    task)
		   (t
		    (error "Invalid task object ~A" task)))
		 chain
		 :from-end t)))

(defun task-data-directory (task &optional chain area)
  (setf chain (or chain *chain*)
	area (or area *area*))
  (assert (and chain area))
  (let ((pos (position-of-task-in-chain task chain)))
    (unless pos
      (error "Unable to find task ~A in chain ~A" task chain))
    (task-data-directory* area (subseq chain 0 (1+ pos)))))

(defun task-value (task &optional chain area)
  (setf chain (or chain *chain*)
	area (or area *area*))
  (assert (and chain area))
  (let ((pos (position-of-task-in-chain task chain)))
    (unless pos
      (error "Unable to find task ~A in chain ~A" task chain))
    (task-value* area (subseq chain 0 (1+ pos)))))

(defun operation-plist ()
  *operation-plist*)

(defun forcedp ()
  (getf (operation-plist) :force))

(defun chain-completed-p (area chain &rest args &key &allow-other-keys)
  (let ((*operation-plist* args)
	(*area* area)
	(all-steps (loop
		      :for x :from 1 :to (length chain)
		      :collect
		      (subseq chain 0 x))))
    (every #'(lambda (x)
	       (let ((*chain* x))
		 (task-completed-p (car (last x)))))
	   all-steps)))

(defun perform-leaf (area chain &rest args &key force &allow-other-keys)
  (let ((*area* area)
	(*operation-plist* args)
	(*chain* chain)
	(performed-tasks (butlast chain))
	(task (car (last chain))))    

    (unless (apply #'chain-completed-p area performed-tasks :force nil *operation-plist*)
      (error "Not all tasks in chain ~A have been completed." chain))

    (cond
      ((or force (not (task-completed-p task)))
       (let* ((*default-pathname-defaults* (ensure-task-data-directory* *area* chain))
	      (v (perform-operation task performed-tasks)))
	 (setf (task-value* *area* chain) v)
	 (values v t)))
      (t
       (values (task-value task) nil)))))

(defmethod task-completed-p :around ((task task))
  (let ((pos (position task *chain*)))
    (unless pos
      (error "Unable to find task ~A in chain ~A" task *chain*))
    (let ((*default-pathname-defaults* (or (probe-file (task-data-directory* *area* (subseq *chain* 0 (1+ pos))))
					   *default-pathname-defaults*)))
      (call-next-method))))

(defmethod task-completed-p ((task task))
  (let ((pos (position task *chain*)))
    (unless pos
      (error "Unable to find task ~A in chain ~A" task *chain*))
    (task-completed-p* *area* (subseq *chain* 0 (1+ pos)))))

(defun ensure-task-data-directory* (area chain)
  (ensure-directories-exist (task-data-directory* area chain)))

(defun perform (area chain &rest args &key force &allow-other-keys)
  (alexandria:remove-from-plistf args :force)
  (let* ((chains (loop
		    :for x :from 1 :to (length chain)
		    :collect
		    (subseq chain 0 x)))
	 (results (mapcar #'(lambda (chain)
			      (multiple-value-bind (task-value performed?) (apply #'perform-leaf area chain :force force args)
				(when performed?
				  (setf force t))
				task-value))
			  chains)))
    (values (car (last results))
	    results)))

;; Prepared Area implementation
(defgeneric area-directory (area))

(defclass prepared-directory ()
  ((directory
    :initarg :directory
    :reader area-directory)))

(defmethod object-sexp ((object prepared-directory))
  `(prepare-directory ,(area-directory object) :if-exists :skip))

(defun prepare-directory (directory &key (if-exists :skip))
  (declare (type (member :error :skip) if-exists))
  (multiple-value-bind (pathspec created?) (ensure-directories-exist directory)
    (declare (ignore pathspec))
    (cond
      ((and (eql if-exists :error) (not created?))
       (error "Cannot prepare directory ~S as it already exists." directory))
      (t
       (with-open-file (out (merge-pathnames "chains-root.sexp" directory)
			    :direction :output
			    :if-exists (if (eql if-exists :skip)
					   nil
					   if-exists))))))
  (make-instance 'prepared-directory :directory directory))

(defmethod task-data-directory* ((area prepared-directory) chain)
  (let ((dir (mapcar #'task-string chain)))
    (merge-pathnames (make-pathname :directory (cons :relative dir))
		     (area-directory area))))

(defun prepared-directory-task-value-pathname (area chain)
  (make-pathname :name "task-value"
		 :type "sexp"
		 :defaults (task-data-directory* area chain)))

(defun prepared-directory-task-object-pathname (area chain)
  (make-pathname :name "task-object"
		 :type "sexp"
		 :defaults (task-data-directory* area chain)))

(defmethod task-value* ((area prepared-directory) chain)
  (let ((*package* (find-package "COMMON-LISP")))
    (with-open-file (in (prepared-directory-task-value-pathname area chain))
      (read in))))

(defmethod (setf task-value*) (value (area prepared-directory) chain)
  (ensure-task-data-directory* area chain)

  (let ((*package* (find-package "COMMON-LISP")))
    ;; Record the task used to compute the value.
    #- (and)
    (with-open-file (out (prepared-directory-task-object-pathname area chain)
			 :if-exists :supersede
			 :direction :output)
      (serialise-object out (car (last chain))))

    (with-open-file (out (prepared-directory-task-value-pathname area chain)
			 :if-exists :supersede
			 :direction :output)
      (serialise-object out value))))

(defmethod task-completed-p* ((area prepared-directory) chain)
  (probe-file (prepared-directory-task-value-pathname area chain)))
