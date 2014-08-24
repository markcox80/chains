(in-package "CHAINS.PEE")

(defvar *xargs-arguments-string* "")
(defvar *xargs-program-arguments-string* "")

(defun xargs-sequence (count)
  #+darwin
  (format nil "/usr/bin/jot ~d 0" count)
  #+linux
  (format nil "/usr/bin/seq 0 ~d" (1- count))
  #- (or darwin linux)
  (error "XARGS-SEQUENCE is not supported on your operating system."))

(defun prepare-xargs-script (directory area tree program &key (if-exists :error if-exists-p))
  (declare (type (member :error :supersede :supersede-all) if-exists))
  (let ((program-path (if (listp program)
			  (apply #'lisp-executable-pathname program)
			  program)))
    (unless (probe-file program-path)
      (error "Unable to find program ~A" program))

    (multiple-value-bind (pathspec created?) (ensure-directories-exist directory)
      (when (and (not created?) (eql if-exists :error))
	(error "Directory ~S already exists!" directory))

      (let ((*default-pathname-defaults* (parse-namestring pathspec)))
	(let ((if-exists (cond			   
			   ((eql if-exists :supersede-all)
			    :supersede)
			   (if-exists-p
			    if-exists)
			   (t
			    :error))))
	  (write-program-data "xargs-data.sexp" area tree :if-exists if-exists)

	  (with-open-file (out "xargs-program.sh" :if-exists if-exists :direction :output)
	    (format out "#!/bin/sh~%")
	    (format out "~S `cat ~S` $1 $2 $3~%"
		    (namestring program-path)
		    (namestring (merge-pathnames "xargs-program-arguments"))))

	  (with-open-file (out "xargs.sh":if-exists if-exists :direction :output)
	    (format out "#!/bin/sh~%")
	    (format out "set -e~%")
	    (let ((counts (number-of-tasks-at-depths tree)))
	      (loop
		 :for count :in counts
		 :for depth :from 0
		 :when (plusp count)
		 :do
		 (format out "~A | xargs -L 1 `cat ~S` /bin/sh ~S ~S ~d~%"
			 (xargs-sequence count)
			 (namestring (merge-pathnames "xargs-arguments"))
			 (namestring (merge-pathnames "xargs-program.sh"))
			 (namestring (merge-pathnames "xargs-data.sexp"))
			 depth)))))
	
	(let ((if-exists (case if-exists
			   (:supersede-all
			    :supersede)
			   (:supersede
			    nil)
			   (t
			    (if if-exists-p
				if-exists
				:error)))))
	  (with-open-file (out "xargs-arguments" :if-exists if-exists :direction :output)
	    (when out
	      (write-string *xargs-arguments-string* out)))

	  (with-open-file (out "xargs-program-arguments" :if-exists if-exists :direction :output)
	    (when out
	      (write-string *xargs-arguments-string* out))))))
    nil))
