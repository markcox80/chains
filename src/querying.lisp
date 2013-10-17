(in-package "CHAINS")

(defun contains-task-p (chain task-class &key (from-end t))
  "Search the list CHAIN for an instances whose class is a subclass of
TASK-CLASS."
  (check-type chain list)
  (check-type task-class (or symbol task-class))
  (let ((task-class (if (symbolp task-class)
			(find-class task-class)
			task-class)))
    (find-if #'(lambda (class)
		 (closer-mop:subclassp class task-class))
	     chain
	     :key #'class-of
	     :from-end from-end)))

(defun find-chains-with-task (chains task-class)
  (check-type task-class (or symbol task-class))
  (if (symbolp task-class)
      (find-chains-with-task chains (find-class task-class))
      (remove-if-not #'(lambda (chain)
			 (contains-task-p chain task-class))
		     chains)))

(defun chain-task-slot-predicate (task-class slot task-slot-definition-function)
  "This is a helper function for PREPARE-GROUP-CHAINS-TEST and
PREPARE-GROUP-CHAINS-SORT-TEST. This function returns an arity two
predicate which accepts to arguments, both of which are chains. The
predicate first finds a task in chain which is of type TASK-CLASS. The
values of the SLOT in each class are compared with a function that is
obtained by invoking TASK-SLOT-DEFINITION-FUNCTION on the
TASK-DIRECT-SLOT-DEFINITION object representing the SLOT.

TASK-CLASS may be a class metaobject which inherits from TASK-CLASS or
a name of such a class.

SLOT may be a TASK-DIRECT-SLOT-DEFINITION or a symbol name. If it is a
symbol name, then that symbol must be the name of a slot withing
TASK-CLASS.

TASK-SLOT-DEFINITION-FUNCTION is a function to be called on the
TASK-DIRECT-SLOT-DEFINITION for SLOT. The value of this function
should only be one of the functions #'TEST=-FUNCTION, #'TEST<-FUNCTION
or #'TEST>-FUNCTION.
"
  (let* ((task-class (if (symbolp task-class)
			 (find-class task-class)
			 task-class))
	 (slot-definition (if (symbolp slot)
			      (find slot (closer-mop:class-direct-slots task-class)
				    :key #'closer-mop:slot-definition-name)
			      slot)))
    (declare (type task-class task-class)
	     (type task-direct-slot-definition slot-definition))
    (let ((slot-name (closer-mop:slot-definition-name slot-definition))
	  (fn (funcall task-slot-definition-function slot-definition)))
      (lambda (chain-a chain-b)
	(let ((a-task (contains-task-p chain-a task-class))
	      (b-task (contains-task-p chain-b task-class)))
	  (when (and a-task b-task)
	    (funcall fn
		     (slot-value a-task slot-name)
		     (slot-value b-task slot-name))))))))

(defun prepare-group-chains-test (expression)
  "Convert EXPRESSION to a two arity predicate. Both arguments for the
  returned predicate must be chains (a list of task instances).

EXPRESSION can be one of:

 symbol

   The symbol denotes a name of a task class. See TASK-CLASS

 TASK-CLASS

   Find two tasks, one from each chain, whose classes are EQUAL and a
   CLOSER-MOP:SUBCLASSP of TASK-CLASS. If found return non-NIL,
   otherwise NIL.

 (= symbol)

   The symbol denotes a name of a task class. See (= TASK-CLASS)  

 (= TASK-CLASS :static)

   Find two tasks, one from each chain, whose classes are a
   CLOSER-MOP:SUBCLASSP of TASK-CLASS and are equal according to the
   TEST=-FUNCTION for TASK-CLASS.

 (= TASK-CLASS :dynamic)

   Find two tasks, one from each chain, whose classes are EQUAL and a
   CLOSER-MOP:SUBCLASSP of TASK-CLASS and are equal according to the
   TEST=-FUNCTION for the found class.

 (= TASK-CLASS)
  
   Like (= TASK-CLASS :static) except it is an error to encounter a
   subclass of TASK-CLASS.

 (= symbol name)
  
  The symbol denotes a name of a task class and name deontes the name
  of a slot. See (= TASK-CLASS TASK-DIRECT-SLOT-DEFINITION).

 (= TASK-CLASS TASK-DIRECT-SLOT-DEFINITION)

  Find two tasks, one from each chain, which are a subclass of
  TASK-CLASS and the value of the slot represented by
  TASK-DIRECT-SLOT-DEFINITION is equal according to its corresponding
  TEST=-FUNCTION.
"
  (labels ((equal-exp-with-length-p (len)
	     (and (listp expression) (eql '= (first expression)) (= len (length (rest expression))))))
    (cond
      ;; TASK-CLASS
      ;; symbol
      ((atom expression)
       (let ((task-class (if (symbolp expression)
			     (find-class expression)
			     expression)))
	 (declare (type task-class task-class))
	 (lambda (chain-a chain-b)
	   (let ((a-task (contains-task-p chain-a task-class))
		 (b-task (contains-task-p chain-b task-class)))
	     (and a-task b-task
		  (task-class-equal (class-of a-task) (class-of b-task)))))))

      ;; (= TASK-CLASS)
      ;; (= symbol)
      ((equal-exp-with-length-p 1)
       (let* ((task-class (if (symbolp (second expression))
			      (find-class (second expression))
			      (second expression)))
	      (fn (test=-function task-class)))
	 (declare (type task-class task-class))
	 (lambda (chain-a chain-b)
	   (let ((a-task (contains-task-p chain-a task-class))
		 (b-task (contains-task-p chain-b task-class)))
	     (unless (and (equal (class-of a-task) task-class)
			  (equal (class-of b-task) task-class))
	       (error "Encountered a subclass of ~A. Please specify :static or :dynamic in CHAINS:GROUP-CHAINS expression." task-class))
	     (and a-task b-task (funcall fn a-task b-task))))))

      ;; (= TASK-CLASS :static)
      ;; (= symbol :static)
      ((and (equal-exp-with-length-p 2)
	    (eql :static (third expression)))
       (let* ((task-class (if (symbolp (second expression))
			      (find-class (second expression))
			      (second expression)))
	      (fn (test=-function task-class)))
	 (declare (type task-class task-class))
	 (lambda (chain-a chain-b)
	   (let ((a-task (contains-task-p chain-a task-class))
		 (b-task (contains-task-p chain-b task-class)))
	     (when (and a-task b-task)
	       (funcall fn a-task b-task))))))

      ;; (= TASK-CLASS :dynamic)
      ;; (= symbol :dynamic)
      ((and (equal-exp-with-length-p 2)
	    (eql :dynamic (third expression)))
       (let* ((task-class (if (symbolp (second expression))
			      (find-class (second expression))
			      (second expression))))
	 (declare (type task-class task-class))
	 (lambda (chain-a chain-b)
	   (let ((a-task (contains-task-p chain-a task-class))
		 (b-task (contains-task-p chain-b task-class)))
	     (and a-task b-task
		  (let ((a-task-class (class-of a-task))
			(b-task-class (class-of b-task)))
		    (and (closer-mop:subclassp a-task-class task-class)
			 (closer-mop:subclassp b-task-class task-class)
			 (equal a-task-class b-task-class)
			 (funcall (test=-function a-task-class) a-task b-task))))))))
      
      ;; (= TASK-CLASS TASK-DIRECT-SLOT-DEFINITION)
      ;; (= symbol slot-name)
      ((equal-exp-with-length-p 2)
       (chain-task-slot-predicate (second expression) (third expression) #'test=-function))
      
      ;; (AND &REST EXPRESSIONS)
      ((eql 'and (first expression))
       (if (rest expression)
	   (apply #'alexandria:conjoin (mapcar #'prepare-group-chains-test (rest expression)))
	   (constantly t)))

      (t
       (error "Unsupported expression ~A for PREPARE-GROUP-CHAINS-TEST." EXPRESSION)))))

(defun position-of-one-of-classes (chain task-classes)
  "Find the first task in CHAIN (starting from the end) whose class is
equal to one of the classes in TASK-CLASSES."
  (labels ((testp (task)
	     (declare (type task task))
	     (position (class-of task) task-classes :test #'task-class-equal)))
    (some #'testp (reverse chain))))

(defun prepare-group-chains-sort-test (expression)
  "Return an arity two predicate that can be used to sort a collection
  of chains. The predicate returned is governed by the value of
  EXPRESSION.
  
  The descriptions below use CHAIN-A to refer to the first argument of
  the returned predicate and CHAIN-B for the second argument of this
  predicate. Additionally, the values TASK-A and TASK-B represent the
  tasks found in CHAINS-A and CHAINS-B respectively.

 (> symbol name)

   The symbol denotes a name of a TASK class, and NAME is the name of
   a slot of that class. See (> TASK-CLASS TASK-DIRECT-SLOT-DEFINITION)
   for the value that is returned.

 (> TASK-CLASS TASK-DIRECT-SLOT-DEFINITION)

   The function returned returns non-NIL if both chains contain a task
   which is of type TASK-CLASS and the value of the slot represented
   by TASK-DIRECT-SLOT-DEFINITION in TASK-A is /greater than/ the
   corresponding value in TASK-B. The comparison function used is
   obtained by invoking the TEST>-FUNCTION generic function on
   TASK-DIRECT-SLOT-DEFINITION.

 (< symbol name)
 (< TASK-CLASS TASK-DIRECT-SLOT-DEFINITION)

   The predicate returned is identical to (> TASK-CLASS
   TASK-DIRECT-SLOT-DEFINITION) except that the slot comparison
   function used is obtained by invoking the TEST<-FUNCTION generic
   function on TASK-DIRECT-SLOT-DEFINITION.

 (:classes &rest classes)
 (:classes &rest TASK-CLASSes)

   The predicate returned returns non-NIL if CHAINS-A contains a task
   whose class has a lower position in the sequence TASK-CLASSes when
   compared with the task obtained from CHAIN-B. An error is signalled
   if no task inheriting from one of the TASK-CLASSes can be found in
   either CHAINS-A or CHAINS-B.
"
  (assert (listp expression))
  (cond
    ((and (eql '> (first expression)) (= 3 (length expression)))
     (chain-task-slot-predicate (second expression) (third expression) #'test>-function))
    ((and (eql '< (first expression)) (= 3 (length expression)))
     (chain-task-slot-predicate (second expression) (third expression) #'test<-function))
    ((and (eql :classes (first expression)))
     (let ((task-classes (mapcar #'(lambda (x)
				     (if (symbolp x)
					 (find-class x)
					 x))
				 (rest expression))))
       (lambda (chain-a chain-b)
	 (let ((pos-a (position-of-one-of-classes chain-a task-classes))
	       (pos-b (position-of-one-of-classes chain-b task-classes)))
	   (unless pos-a
	     (error "The chain ~A does not contain a task whose class is one of ~A." chain-a task-classes))
	   (unless pos-b
	     (error "The chain ~A does not contain a task whose class is one of ~A." chain-b task-classes))
	   (< pos-a pos-b)))))
    (t
     (error "Unable to process expression ~A" expression))))

(defun prepare-group-chains (expression &key sort sort-inner)
  (let ((group-test (prepare-group-chains-test expression))
	(sort-test (when sort (prepare-group-chains-sort-test sort)))
	(sort-inner-test (when sort-inner (prepare-group-chains-sort-test sort-inner))))
    (lambda (chains)
      (let ((groups (group-by group-test chains)))
	(when sort-test
	  (setf groups (sort groups sort-test :key #'first)))
	(if sort-inner-test
	    (mapcar #'(lambda (group)
			(sort group sort-inner-test))
		    groups)
	    groups)))))

(defun group-chains (chains expression &key sort sort-inner)
  (let ((fn (prepare-group-chains expression :sort sort :sort-inner sort-inner)))
    (funcall fn chains)))
