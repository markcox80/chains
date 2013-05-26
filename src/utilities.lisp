(in-package "CHAINS")

(defun separate-according-to (predicate sequence &key key)
  "Separate the SEQUENCE in to two sets according to PREDICATE. The
first of the two VALUES returned contain all items of SEQUENCE for
which PREDICATE is non-NIL. The second set contains the rest. The
values returned are lists."
  (setf key (or key #'identity))

  (let (set-a set-b)
    (labels ((separate (item)
	       (if (funcall predicate (funcall key item))
		   (push item set-a)
		   (push item set-b))))
      (map nil #'separate sequence))
    (values (nreverse set-a)
	    (nreverse set-b))))

(defun group-by/helper (test item sequence &key key)
  (separate-according-to (let ((v (funcall key item)))
			   #'(lambda (other)
			       (funcall test v other)))
			 sequence
			 :key key))

(defun group-by (test sequence &key key)
  (setf key (or key #'identity))

  (labels ((recursive (rv sequence)
	     (cond
	       ((zerop (length sequence))
		rv)
	       (t
		(let ((item   (elt sequence 0))
		      (others (subseq sequence 1)))
		  (unless (funcall test (funcall key item) (funcall key item))
		    (error "Invalid TEST and KEY combination as an item does not group with itself."))
		  
		  (multiple-value-bind (set-a set-b) (group-by/helper test item others :key key)
		    (push (cons item set-a) rv)
		    (recursive rv set-b)))))))
    (nreverse (recursive nil sequence))))
