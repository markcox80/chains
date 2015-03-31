(in-package "CHAINS.PEE")

(defvar *slurm-arguments-string* "")
(defvar *slurm-program-arguments-string* "")

(defun determine-slurm-output-arguments (directory output error)
  (assert (stringp output))
  (with-output-to-string (out)
    (format out "--output ~S" (namestring (merge-pathnames (make-pathname :name "%A-%a"
									  :type "out"
									  :defaults output)
							   directory)))
    (write-char #\Space out)
    (unless (eql error :output)
      (format nil "--error ~S" (namestring (merge-pathnames (make-pathname :name "%A-%a"
									   :type "out"
									   :defaults error)
									  directory))))))

(defun prepare-slurm-script (directory area tree program &key (if-exists :error if-exists-p) (output "stdout/") (error :output))
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
	(ensure-directories-exist (merge-pathnames output))
	(when (stringp error)
	  (ensure-directories-exist (merge-pathnames error)))

	(let ((if-exists (cond
			   ((eql if-exists :supersede-all)
			    :supersede)
			   (if-exists-p
			    if-exists)
			   (t
			    :error))))
	  (write-program-data "slurm-data.sexp" area tree :if-exists if-exists)

	  (with-open-file (out "slurm-program.sh" :if-exists if-exists :direction :output)
	    (format out "#!/bin/sh~%")
	    (format out "~S `cat ~S` $1 $2 $(( ${SLURM_ARRAY_TASK_ID} - 1 ))~%"
		    (namestring program-path)
		    (namestring (merge-pathnames "slurm-program-arguments"))))

	  (with-open-file (out "slurm.sh" :if-exists if-exists :direction :output)
	    (format out "#!/bin/sh~%")
	    (format out "set -e~%")

	    (let ((counts (number-of-tasks-at-depths tree)))
	      (loop
		 :for count :in counts
		 :for depth :from 0
		 :when (plusp count)
		 :do		 
		 (format out "
LEVEL~d=`sbatch --array 1-~d \\
     ~A \\
     -J ~S \\
     $(cat ~S) \\
    ~A \\
    --parsable \\
    ~S ~S ~d`

echo ${LEVEL~d}
"
			 depth
			 count
			 (determine-slurm-output-arguments directory output error)
			 (pathname-name program-path)
			 (namestring (merge-pathnames "slurm-arguments"))
			 (if (> depth 1)
			     (format nil "-d \"afterok:${LEVEL~d}\"" (1- depth))
			     "")
			 (namestring (merge-pathnames "slurm-program.sh"))
			 (namestring (merge-pathnames "slurm-data.sexp"))
			 depth
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
	  (with-open-file (out "slurm-arguments" :if-exists if-exists :direction :output)
	    (when out
	      (write-string *slurm-arguments-string* out)))

	  (with-open-file (out "slurm-program-arguments" :if-exists if-exists :direction :output)
	    (when out
	      (write-string *slurm-program-arguments-string* out))))))
    nil))
