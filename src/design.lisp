(in-package "CHAINS")

;; DESIGN
(defclass design ()
  ((name
    :initarg :name
    :reader design-name)
   (levels
    :initarg :levels
    :reader design-levels)
   (documentation
    :initarg :documentation
    :reader design-documentation)))

(defun designp (object)
  (typep object (find-class 'design)))

(defun find-design (name)
  (get name 'design))

(defun ensure-design (name &key levels documentation)
  (let ((design (make-instance 'design
			       :name name
			       :levels levels
			       :documentation documentation)))
    (setf (get name 'design) design)))

;; GENERATE
(defun generate (design)
  (cond
    ((null design)
     (error "Unable to perform GENERATE on object ~A" design))
    ((symbolp design)
     (generate (find-design design)))
    (t
     (reduce #'(lambda (tree level)
		 (let ((children (generate-children-from-level level)))
		   (replace-leaves #'(lambda (leaf)
				       (make-tree (value leaf) children))
				   tree)))
	     (design-levels design)
	     :initial-value (make-tree)))))

(defun generate-children-from-level (level)
  "Return a list of trees using the data in LEVEL."
  (declare (type list level))
  (labels ((process-generator (fn)
	     (declare (type function fn))
	     (mapcar #'make-tree (funcall fn)))
	   (process-design (design)
	     (let ((design-tree (generate design)))
	       (assert (null (value design-tree)))
	       (children design-tree)))
	   (process (object)
	     (cond
	       ((functionp object)
		(process-generator object))
	       ((designp object)
		(process-design object))
	       (t
		(error "Unable to compute children for object ~A" object)))))
    (reduce #'append level :key #'process)))

;; MAKE-GENERATOR
(defun make-generator (class &rest initarg-generators)
  "Return a function that generates instances of CLASS using all
permutations of initargs generated using INITARG-GENERATORS.

CLASS must be a class metaobject. INITARG-GENERATORS is a list of CONS
objects where the CAR is a slot initarg and the CDR is a function. The
function which returns a list of objects. Each object will be used as
a value for the slot."
  (if (null initarg-generators)
      (lambda ()
	(make-instance class))
      (lambda ()
	(let ((initarg-data (mapcar #'(lambda (x)
					(cons (car x) (funcall (cdr x))))
				    initarg-generators)))
	  (mapcar #'(lambda (x)
		      (apply #'make-instance class x))
		  (make-generator/permutations initarg-data))))))

(defun make-generator/permutations (initarg-data)
  "Compute all permutations of initargs from INITARG-DATA.

INITARG-DATA is a list of lists. The first element of each list is a
slot initarg and the rest are the slot values. 
"
  (let ((rv nil))
    (labels ((compute (initargs next)
	       (let ((current (first next)))
		 (cond
		   ((null current)
		    (push initargs rv)
		    (values))
		   (t
		    (dolist (item (cdr current))
		      (compute (append (list (car current) item)
				       initargs)
			       (rest next))))))))
      (compute nil initarg-data)
      (nreverse rv))))

;; DEFINE-DESIGN
(defmacro define-design (name design-options &body level-definitions)
  `(ensure-design ',name
		  :levels ,(canonicalise-level-definitions level-definitions)
		  ,@(canonicalise-design-options design-options)))

(defun canonicalise-generate-expression (generate-expression)
  (cond
    ((symbolp generate-expression)
     `(make-generator (find-class ',generate-expression)))
    ((listp generate-expression)
     (alexandria:destructuring-case generate-expression
       ((:design name)
	`(find-design ',name))
       ((t &rest task-initarg-definitions)
	`(make-generator (find-class ',(first generate-expression))
			 ,@(canonicalise-task-initarg-definitions task-initarg-definitions)))))))

(defun canonicalise-design-option (design-option)
  (alexandria:destructuring-ecase design-option
    ((:documentation doc-string)
     (list :documentation doc-string))))

(defun canonicalise-task-initarg-definition (initarg-definition)
  (destructuring-bind (initarg &rest initarg-expressions) initarg-definition
    `(cons ',initarg
	   ,(capture-initarg-expression initarg-expressions))))

(defun capture-initarg-expression (initarg-expressions)
  "Return a function that computes a list of objects which will be
  used to initialise a slot.

e.g.
 (list 0 1 2 3 4 5 6) 
 => '(lambda ()
       (list 0 1 2 3 4 5 6))

 '(0 1 2 3 (:splice (loop :for x :from 0 :below 10 :collect x)))
 => '(lambda ()
       (append (list 0 1 2 3)
               (loop :for x :from 0 :below 10 :collect x)))                  
"
  (let ((expressions nil)
	(current-list nil))
    (labels ((push-list-expression ()
	       (push `(list ,@(nreverse current-list)) expressions)
	       (setf current-list nil)))
      (dolist (item initarg-expressions)
	(cond
	  ((and (listp item) (eql (first item) :splice))
	   (push-list-expression)
	   (push `(progn
		    ,@(rest item))
		 expressions))
	  (t
	   (push item current-list))))
      (push-list-expression)
      `(function (lambda ()
	 (append ,@(nreverse expressions)))))))

;;; - DEFINE-DESIGN Helpers
(defun canonicalise-design-options (design-options)
  (reduce #'append design-options :key #'canonicalise-design-option))

(defun canonicalise-level-definitions (levels)  
  `(list ,@(mapcar #'canonicalise-generate-expressions levels)))

(defun canonicalise-generate-expressions (generate-expressions)
  `(list ,@(mapcar #'canonicalise-generate-expression generate-expressions)))

(defun canonicalise-task-initarg-definitions (initargs-definitions)
  (mapcar #'canonicalise-task-initarg-definition initargs-definitions))
