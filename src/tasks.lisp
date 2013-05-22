(in-package "CHAINS")

(define-predicates default-predicates
  #'eql
  #'(lambda (a b)
      (error "No /less-than/ predicate specified for values ~A and ~B. Use DEFINE-PREDICATES." a b))
  #'(lambda (a b)
      (error "No /greater-than/ predicate specified for values ~A and ~B. Use DEFINE-PREDICATES." a b)))

(define-predicates number #'= #'< #'>)
(define-predicates string/case-sensitive #'string= #'string< #'string>)
(define-predicates string/case-insensitive #'string-equal #'string-lessp #'string-greaterp)

(defclass task-class (standard-class)
  ()
  (:documentation "The metaclass for all objects that represent task classes."))

(defclass task-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((predicates
    :initarg :predicates
    :reader task-direct-slot-definition-predicates))
  (:default-initargs
   :predicates 'default-predicates))

(defmethod test=-function ((object task-direct-slot-definition))
  (test=-function (task-direct-slot-definition-predicates object)))

(defmethod test<-function ((object task-direct-slot-definition))
  (test<-function (task-direct-slot-definition-predicates object)))

(defmethod test>-function ((object task-direct-slot-definition))
  (test>-function (task-direct-slot-definition-predicates object)))

;; Not sure why this is needed
(defmethod closer-mop:validate-superclass ((class task-class) (superclass (eql (find-class 'standard-object))))
  t)

;; Specify the direct slot definition class for new task classes.
(defmethod closer-mop:direct-slot-definition-class ((class task-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'task-direct-slot-definition))

(defclass task ()
  ()
  (:metaclass task-class)
  (:documentation "The base class for all tasks."))

(defun ensure-task (name &key
			   direct-superclasses 
			   direct-slots
			   direct-default-initargs
			   documentation)
  (closer-mop:ensure-class name
			   :direct-superclasses (or direct-superclasses (list (find-class 'task)))
			   :direct-slots direct-slots
			   :direct-default-initargs direct-default-initargs
			   :metaclass (find-class 'task-class)
			   :documentation documentation))

(defmacro define-task (name superclasses direct-slots &rest options)
  `(ensure-task ',name
		:direct-superclasses ,(canonicalise-direct-superclasses superclasses)
		:direct-slots ,(canonicalise-direct-slots direct-slots)
		,@(reduce #'append options :key #'canonicalise-define-task-option)))

;; Canonicalise functions
;; Most of these are taken from the book
;; The Art of the Metaobject Protocol
;; by Gregor Kiczales, Jim des Rivieres and Daniel G. Bobrow

(defun canonicalise-direct-superclasses (superclasses)
  (mapcar #'canonicalise-direct-superclass superclasses))

(defun canonicalise-direct-superclass (superclass)
  `(find-class ',superclass))

;; Page 286
(defun canonicalise-direct-slots (direct-slots)
  `(list ,@(mapcar #'canonicalise-direct-slot direct-slots)))

;; Page 286
(defun canonicalise-direct-slot (spec)
  "Create initialisation arguments for a new instance of
TASK-DIRECT-SLOT-DEFINITION."
  (if (symbolp spec)
      `(list :name ',spec)
      (let ((name (first spec))
	    initargs
	    readers
	    writers
	    type
	    initform initfunction
	    predicates)
	(assert (zerop (mod (length (rest spec)) 2)))
	(do* ((slot-options (rest spec) (cddr slot-options))
	      (key (first slot-options) (first slot-options))
	      (value (second slot-options) (second slot-options)))
	     ((null slot-options))
	  (ecase key
	    (:initform
	     (setf initfunction `(function (lambda () ,value))
		   initform     `',value))
	    (:initarg
	     (push value initargs))
	    (:reader
	     (push value readers))
	    (:writer
	     (push `(setf ,value) writers))
	    (:accessor
	     (push value readers)
	     (push `(setf ,value) writers))
	    (:type
	     (setf type value))
	    (:predicates
	     (setf predicates value))))
	`(list :name ',name
	       ,@(when initfunction `(:initform ,initform :initfunction ,initfunction))
	       ,@(when initargs `(:initargs ',(reverse initargs)))
	       ,@(when readers  `(:readers ',(reverse readers)))
	       ,@(when writers  `(:writers ',(reverse writers)))
	       ,@(when type `(:type ',type))
	       ,@(when predicates `(:predicates '(find-predicates ',predicates)))))))

(defun canonicalise-define-task-option (option)
  (alexandria:destructuring-case option
    ((:documentation string)
     (list :documentation string))
    ((:default-initargs &rest args)
     (assert (evenp (length args)))
     (list :direct-default-initargs	   
	   `(list ,@(loop
		       :while args
		       :for key := (first args)
		       :for value := (second args)
		       :collect
		       (prog1 `(list ',key ',value #'(lambda ()
						       ,value))
			 (setf args (cddr args)))))))))
