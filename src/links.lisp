(in-package "CHAINS")

(defgeneric output-name (object)
  (:documentation "A name which uniquely describes OBJECT."))

(defgeneric operation-equal (object-a object-b)
  (:documentation "A predicate that tests for equality of two OPERATION objects."))

(defgeneric perform (object &key if-performed chain directory &allow-other-keys)
  (:documentation "Tell OBJECT to get on with it."))

(defgeneric performedp (object chain)
  (:documentation "Has OBJECT got on with it?"))

(defmethod operation-equal ((object-a t) (object-b t))
  t)

(defun make-chain (&rest args)
  args)

(defvar *database-pathname*)

;; Performing a CHAIN
(defun write-data (pathname object &key (if-exists :error))
  (let ((*package* (find-package "COMMON-LISP")))
    (with-open-file (out pathname :if-exists if-exists :direction :output)
      (write object :stream out :readably t)
      (terpri out))))

(defun read-data (pathname)
  (let ((*package* (find-package "COMMON-LISP")))
    (with-open-file (in pathname)
      (read in))))

(defun compute-link-data-pathname (link stack)
  (merge-pathnames (make-pathname :name "link"
				  :type "sexp")
		   (compute-link-data-directory (cons link stack))))

(defun compute-link-result-pathname (link stack)
  (merge-pathnames (make-pathname :name "result"
				  :type "sexp")
		   (compute-link-data-directory (cons link stack))))

(defun compute-link-data-directory (stack)
  (merge-pathnames (reduce #'(lambda (current next)
			       (merge-pathnames current
						(make-pathname :directory (list :relative (output-name next)))))
			   stack
			   :initial-value (make-pathname :directory (list :relative)))
		   *database-pathname*))

(defmethod performedp ((link t) stack)
  (probe-file (compute-link-result-pathname link stack)))

(define-condition already-performed-error (error)
  ((object
    :initarg :object
    :reader already-performed-error-object))
  (:report (lambda (condition stream)
	     (format stream "PERFORM has already been executed on object ~A" (already-performed-error-object condition)))))

(defmethod perform ((object list) &rest args &key (if-performed :skip))
  (labels ((do-action (stack link)
	     (cond
	       ((performedp link stack)
		(ecase if-performed
		  (:skip
		   (cons link stack))
		  (:supersede
		   (delete-file (compute-link-result-pathname link stack))
		   (do-action stack link))
		  (:error
		   (error 'already-performed-error :object link))))
	       (t
		(let* ((pathname (compute-link-result-pathname link stack)))		  
		  (ensure-directories-exist (directory-namestring pathname))
		  (let ((object (apply #'perform link :chain object :directory pathname args)))
		    (write-data pathname object)))
		(cons link stack)))))
    (reduce #'do-action object :initial-value nil)))

(defun write-chain (chain &key (if-exists :skip))
  (labels ((do-action (stack link)
	     (let ((pathname (compute-link-data-pathname link stack)))
	       (ensure-directories-exist (directory-namestring pathname))
	       (cond
		 ((probe-file pathname)
		  (ecase if-exists
		    (:skip
		     (cons link stack))
		    (:error
		     (error "File exists at path ~A" pathname))
		    (:supersede
		     (delete-file pathname)
		     (do-action stack link))))
		 (t
		  (write-data pathname link
			      :if-exists (if (eql :skip if-exists)
					     :error
					     if-exists)))))
	     (cons link stack)))

    ;; Mark that this is the top of the chain.
    (with-open-file (out (merge-pathnames "root.sexp" *database-pathname*) :if-exists nil :direction :output)
      ;; Do nothing
      )

    (reduce #'do-action chain :initial-value nil)))

(defun chain-link-pathname (chain class-name)
  (let ((pos (position-if #'(lambda (x)
			      (typep x class-name))
			  chain)))
    (compute-link-data-pathname (elt chain pos) (reverse (subseq chain 0 pos)))))

(defun chain-result-pathname (chain class-name)
  (let ((pos (position-if #'(lambda (x)
			      (typep x class-name))
			  chain)))
    (unless pos
      (error "Unable to find object with class ~A in chain ~A" class-name chain))
    (compute-link-result-pathname (elt chain pos) (reverse (subseq chain 0 pos)))))

(defun chain-result (chain class-name)
  (read-data (chain-result-pathname chain class-name)))

;; Loading a chain
(define-condition link-not-found-error (error)
  ((pathname
    :initarg :pathname
    :reader link-not-found-error-pathname))
  (:report (lambda (condition stream)
	     (format stream "Unable to find link file in pathname ~S" (link-not-found-error-pathname condition)))))

(defun read-chain (pathname)
  (cond
    ((eql :relative (first (pathname-directory pathname)))
     (read-chain (truename pathname)))
    ((cl-fad:directory-pathname-p pathname)
     (read-chain (or (probe-file (merge-pathnames "link.sexp" pathname))
		     (probe-file (merge-pathnames "root.sexp" pathname))
		     (error 'link-not-found-error :pathname pathname))))
    ((pathname-match-p pathname "/**/root.sexp")
     (values nil (directory-namestring pathname)))
    ((pathname-match-p pathname "/**/link.sexp")
     (multiple-value-bind (parent-chain root-directory) (read-chain (merge-pathnames (make-pathname :directory '(:relative :up))
										     (directory-namestring pathname)))
       (values (append parent-chain
		       (list (read-data pathname)))
	       root-directory)))
    (t
     (error "Do not know what to do with pathname ~S" pathname))))

(defun discover-chains (pathname)
  (let ((sublinks (directory (merge-pathnames (make-pathname :name "link"
							     :type "sexp"
							     :directory '(:relative :wild))
					      pathname))))
    (cond
      (sublinks
       (reduce #'append sublinks :key #'discover-chains))
      (t
       (handler-case (list (read-chain pathname))
	 (link-not-found-error ()
	   nil))))))

(defun parallel-link-data-pathnames (chains)
  (let ((max-depth (reduce #'max chains :key #'length)))
    (loop
       :for depth :from 1 :to max-depth
       :collect
       (remove-duplicates (remove nil (map 'list #'(lambda (chain)
						     (when (>= (length chain) depth)
						       (let ((truncated-chain (reverse (subseq chain 0 depth))))
							 (compute-link-data-pathname (car truncated-chain) (rest truncated-chain)))))
					   chains))
			  :test #'pathname-match-p))))

(defun write-parallel-link-data-pathnames (directory chains &key (format-control "depth-~2,'0d.list") (if-exists :error))
  (let ((all-pathnames (parallel-link-data-pathnames chains)))
    (loop
       :for pathnames :in all-pathnames
       :for depth :from 1
       :for output-pathname := (merge-pathnames (format nil format-control depth) directory)
       :do
       (with-open-file (out output-pathname :direction :output :if-exists if-exists)
	 (dolist (item pathnames)
	   (format out "~A~%" (namestring item)))))))
