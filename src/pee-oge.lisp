(in-package "CHAINS.PEE")

(defun lisp-executable-pathname (asdf-system &rest asdf-tree-path)
  (let ((system (asdf:find-system asdf-system)))
    (asdf:component-pathname (reduce #'asdf:find-component asdf-tree-path
				     :initial-value system))))

(defvar *oge-arguments-string* "")
(defvar *oge-program-arguments-string* "")

(defun maximum-tree-depth (tree)
  (labels ((recurse (tree depth)
	     (if (leafp tree)
		 depth
		 (reduce #'max (children tree) :key #'(lambda (child)
							(recurse child (1+ depth)))))))
    (recurse tree 0)))

(defun leaf-counts-at-depths (tree)
  (loop
     :for depth :from 0 :below (maximum-tree-depth tree)
     :collect
     (let ((truncated-tree (truncate-tree-to-depth tree depth)))
       (count-leaves truncated-tree))))

(defgeneric determine-qsub-args (output error directory))

(defmethod determine-qsub-args ((output string) (error string) directory)
  (format nil "-o ~S -e ~S"
	  (namestring (merge-pathnames output directory))
	  (namestring (merge-pathnames error directory))))

(defmethod determine-qsub-args ((output string) (error (eql :output)) directory)
  (format nil "-o ~S -j y"
	  (namestring (merge-pathnames output directory))))

(defun prepare-oge-script (directory area tree program &key (if-exists :error) (output "stdout/") (error :output))
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
	(when (stringp output)
	  (ensure-directories-exist (merge-pathnames output)))
	
	(when (stringp error)
	  (ensure-directories-exist (merge-pathnames error)))

	(let ((if-exists (if (eql if-exists :supersede-all) :supersede if-exists)))
	  (with-open-file (out "oge-data.sexp" :if-exists if-exists :direction :output)
	    (serialise-object out (list area tree))
	    (terpri out))

	  (with-open-file (out "oge-program.sh" :if-exists if-exists :direction :output)
	    (format out "#!/bin/sh~%")
	    (format out "~S `cat ~S` $1 $2 $(( ${SGE_TASK_ID} - 1 ))~%"
		    (namestring program-path)
		    (namestring (merge-pathnames "oge-program-arguments"))))

	  (with-open-file (out "oge.sh":if-exists if-exists :direction :output)
	    (format out "#!/bin/sh~%")
	    (format out "set -e~%")
	    (format out "QSUB_ARGS=~S~%" (determine-qsub-args output error directory))
	    (let ((counts (leaf-counts-at-depths tree)))
	      (loop
		 :for count :in counts
		 :for depth :from 0
		 :do
		 (format out "qsub -t 1-~d ${QSUB_ARGS} `cat ~S` -sync y ~S ~S ~d~%"
			 count
			 (namestring (merge-pathnames "oge-arguments"))
			 (namestring (merge-pathnames "oge-program.sh"))
			 (namestring (merge-pathnames "oge-data.sexp"))
			 depth)))))
	
	(let ((if-exists (if (eql if-exists :supersede) nil if-exists)))
	  (with-open-file (out "oge-arguments" :if-exists if-exists :direction :output)
	    (when out
	      (write-string *oge-arguments-string* out)))

	  (with-open-file (out "oge-program-arguments" :if-exists if-exists :direction :output)
	    (when out
	      (write-string *oge-arguments-string* out))))))
    nil))
