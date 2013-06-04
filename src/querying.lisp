(in-package "CHAINS")

(defun find-task-in-chain (task-class chain)
  "Search the list CHAIN for an instances whose class is a subclass of
TASK-CLASS."
  (declare (type list chain))
  (find-if #'(lambda (class)
	       (closer-mop:subclassp class task-class))
	   chain
	   :key #'class-of))

(defun prepare-group-chains-test (expression)
  "Convert EXPRESSION to a two arity predicate. Both arguments for the
  returned predicate must be chains (a list of task instances).

EXPRESSION can be one of:

 symbol

   The symbol denotes a name of a task class. See TASK-CLASS

 TASK-CLASS

   Find two tasks, one from each chain, whose class is EQUAL and is a
   CLOSER-MOP:SUBCLASSP of TASK-CLASS. If found return non-NIL,
   otherwise NIL.

 (= symbol)

   The symbol denotes a name of a task class. See (= TASK-CLASS)  

 (= TASK-CLASS)
   Find two tasks, one from each chain, which are EQUAL according to
   the TEST=-FUNCTION for TASK-CLASS.

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
	   (let ((a-task (find-task-in-chain task-class chain-a))
		 (b-task (find-task-in-chain task-class chain-b)))
	     (when (and (closer-mop:subclassp (class-of a-task) task-class)
			(closer-mop:subclassp (class-of b-task) task-class))
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
	   (let ((a-task (find-task-in-chain task-class chain-a))
		 (b-task (find-task-in-chain task-class chain-b)))
	     (when (and a-task b-task)
	       (funcall fn a-task b-task))))))
      
      ;; (= TASK-CLASS TASK-DIRECT-SLOT-DEFINITION)
      ;; (= symbol slot-name)
      ((equal-exp-with-length-p 2)
       (let* ((task-class (if (symbolp (second expression))
			      (find-class (second expression))
			      (second expression)))
	      (slot-definition (if (symbolp (third expression))
				   (find (third expression) (closer-mop:class-direct-slots task-class)
					 :key #'closer-mop:slot-definition-name)
				   (third expression)))
	      (slot-name (closer-mop:slot-definition-name slot-definition)))
	 (declare (type task-class task-class)
		  (type task-direct-slot-definition slot-definition))
	 (let ((fn (test=-function slot-definition)))
	   (lambda (chain-a chain-b)
	     (let ((a-task (find-task-in-chain task-class chain-a))
		   (b-task (find-task-in-chain task-class chain-b)))
	       (when (and a-task b-task)
		 (funcall fn
			  (slot-value a-task slot-name)
			  (slot-value b-task slot-name)))))))))))

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
")
