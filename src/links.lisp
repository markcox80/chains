(in-package "CHAINS")

(defgeneric output-name (object)
  (:documentation "A name which uniquely describes OBJECT."))

(defgeneric operation-equal (object-a object-b)
  (:documentation "A predicate that tests for equality of two OPERATION objects."))

(defgeneric perform (object &key if-performed &allow-other-keys)
  (:documentation "Tell OBJECT to get on with it."))

(defmethod operation-equal ((object-a t) (object-b t))
  t)

(defun make-chain (&rest args)
  args)

(defvar *database-pathname*)

;; Performing a CHAIN
(defun write-data (pathname object &key (if-exists :error))
  (with-open-file (out pathname :if-exists if-exists :direction :output)
    (write object :stream out :readably t)
    (terpri out)))

(defun read-data (pathname)
  (with-open-file (in pathname)
    (read in)))

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

(defun performedp (link stack)
  (probe-file (compute-link-result-pathname link stack)))

(define-condition already-performed-error (error)
  ((object
    :initarg :object
    :reader already-performed-error-object))
  (:report (lambda (condition stream)
	     (format stream "PERFORM has already been executed on object ~A" (already-performed-error-object condition)))))

(defmethod perform ((object list) &key (if-performed :skip))
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
		(let ((object (perform link))
		      (pathname (compute-link-result-pathname link stack)))		  
		  (ensure-directories-exist (directory-namestring pathname))
		  (write-data pathname object))
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
    (compute-link-result-pathname (elt chain pos) (reverse (subseq chain 0 pos)))))
