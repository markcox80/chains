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

(defun remove-duplicates-in-tree (test-function tree)
  (cond
    ((leafp tree)
     tree)
    (t
     (let* ((children (children tree))
            (groups (group-by test-function children :key #'value)))
       (make-tree (value tree)
                  (loop
                     for group in groups
                     for group-children = (reduce #'append group :key #'children)
                     for exemplar-value = (value (first group))
                     collect
                       (remove-duplicates-in-tree test-function (make-tree exemplar-value group-children))))))))

(defun count-leaves (tree)
  (count-leaves-if (constantly t) tree))

(defun count-leaves-if (predicate tree &key (key #'identity))
  (labels ((process (tree)
	     (if (and (leafp tree) (funcall predicate (funcall key tree)))
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

(defmethod object-sexp ((object tree))
  `(make-tree ,(object-sexp (value object)) 
	      ,(object-sexp (children object))))
