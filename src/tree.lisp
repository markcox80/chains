(in-package "CHAINS")

(defclass tree ()
  ((value
    :initarg :value
    :reader value)
   (children
    :initarg :children
    :reader children)))

(defun treep (object)
  (typep object (find-class 'tree)))

(defun make-tree (&optional value children)
  (declare (type list children))
  (assert (every #'treep children))
  (make-instance 'tree :value value :children children))

(defun leafp (tree)
  (declare (type tree tree))
  (null (children tree)))

(defun replace-leaves (function tree)
  (cond
    ((leafp tree)
     (let ((rv (funcall function tree)))
       (declare (type tree rv))
       rv))
    (t
     (let ((children (mapcar #'(lambda (child)
				 (replace-leaves function child))
			     (children tree))))       
       (assert (every #'treep children))
       (make-instance 'tree
		      :value (value tree)
		      :children children)))))

(defun count-leaves (tree)
  (labels ((process (tree)
	     (if (leafp tree)
		 1
		 (reduce #'+ (children tree) :key #'process))))
    (process tree)))

(defun compute-chains (tree)
  (let ((child-chains (reduce #'append (children tree) :key #'compute-chains)))
    (cond
      ((leafp tree)
       (assert (value tree))
       (list (list (value tree))))
      ((value tree)
       (mapcar #'(lambda (chain)
		   (cons (value tree) chain))
	       child-chains))
      (t
       ;; Root node
       child-chains))))
