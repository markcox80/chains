(in-package "CHAINS.PEE")

(defvar *pbs-arguments-string* "")
(defvar *pbs-program-arguments-string* "")

(defun prepare-pbs-script (directory area tree program &key (if-exists :error if-exists-p) (output "stdout/"))
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

	(let ((if-exists (cond
			   ((eql if-exists :supersede-all)
			    :supersede)
			   (if-exists-p
			    if-exists)
			   (t
			    :error))))
	  (write-program-data "pbs-data.sexp" area tree :if-exists if-exists)

	  (with-open-file (out "pbs-program.sh" :if-exists if-exists :direction :output)
	    (format out "#!/bin/sh~%")
	    (format out "~S `cat ~S` $1 $2 $(( ${PBS_ARRAYID} - 1 ))~%"
		    (namestring program-path)
		    (namestring (merge-pathnames "pbs-program-arguments"))))

	  (with-open-file (out "pbs.sh" :if-exists if-exists :direction :output)
	    (format out "#!/bin/sh~%")
	    (format out "set -e~%")

	    (let ((counts (number-of-tasks-at-depths tree)))
	      (loop
		 :for count :in counts
		 :for depth :from 0
		 :when (plusp count)
		 :do		 
		 (format out "
LEVEL~d=`qsub -t 1-~d \\
     -j oe -o ~S \\
     -N ~S \\
     $(cat ~S) \\
    ~A \\
    ~S`

echo ${LEVEL~d}
"
			 depth
			 count
			 (namestring (merge-pathnames output))
			 (pathname-name program-path)
			 (namestring (merge-pathnames "pbs-arguments"))
			 (if (> depth 1)
			     (format nil "-W \"depend=afterokarray:${LEVEL~d}\"" (1- depth))
			     "")
			 (namestring (merge-pathnames (format nil "pbs-script-~d.sh" depth)))
			 depth))))

	  (loop
	     :for depth :from 1 :below (length (number-of-tasks-at-depths tree))
	     :do
	     (with-open-file (out (format nil "pbs-script-~d.sh" depth) :direction :output :if-exists if-exists)
	       (format out "#!/bin/sh~%")
	       (format out "set -e~%")
	       (format out "
sh ~S ~S ~d~%"
		       (namestring (merge-pathnames "pbs-program.sh"))
		       (namestring (merge-pathnames "pbs-data.sexp"))
		       depth))))
	
	(let ((if-exists (case if-exists
			   (:supersede-all
			    :supersede)
			   (:supersede
			    nil)
			   (t
			    (if if-exists-p
				if-exists
				:error)))))
	  (with-open-file (out "pbs-arguments" :if-exists if-exists :direction :output)
	    (when out
	      (write-string *pbs-arguments-string* out)))

	  (with-open-file (out "pbs-program-arguments" :if-exists if-exists :direction :output)
	    (when out
	      (write-string *pbs-program-arguments-string* out))))))
    nil))
