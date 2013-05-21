(in-package "CHAINS")

(defclass task-class (standard-class)
  ()
  (:documentation "The metaclass for all objects that represent task classes."))

(defclass task-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((test=-form
    :initarg :test=-form
    :reader test=-form)
   (test=-function
    :initarg :test=-function
    :reader test=-function)
   (test<-form
    :initarg :test<-form
    :reader test<-form)
   (test<-function
    :initarg :test<-function
    :reader test<-function)
   (test>-form
    :initarg :test>-form
    :reader test>-form)
   (test>-function
    :initarg :test>-function
    :reader test>-function)))

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
	    initform initfunction
	    test=-form test=-function
	    test<-form test<-function
	    test>-form test>-function)
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
	    (:writers
	     (push value writers))
	    (:accessor
	     (push value readers)
	     (push `(setf ,value) writers))
	    (:test=
	     (setf test=-function value
		   test=-form `',value))
	    (:test<
	     (setf test<-function value
		   test<-form `',value))
	    (:test>
	     (setf test>-function value
		   test>-form `',value))))
	`(list :name ',name
	       ,@(when initfunction `(:initform ,initform :initfunction ,initfunction))
	       ,@(when initargs `(:initargs ',(reverse initargs)))
	       ,@(when readers  `(:readers ',(reverse readers)))
	       ,@(when writers  `(:writers ',(reverse writers)))
	       ,@(when test=-function `(:test=-form ,test=-form :test=-function ,test=-function))
	       ,@(when test<-function `(:test<-form ,test<-form :test<-function ,test<-function))
	       ,@(when test>-function `(:test>-form ,test>-form :test>-function ,test>-function))))))

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
