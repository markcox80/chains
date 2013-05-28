(defpackage "CHAINS"
  (:use "COMMON-LISP")

  ;; Utilities
  (:export #:separate-according-to
	   #:group-by)

  ;; Predicates
  (:export #:ensure-predicates
	   #:define-predicates
	   #:find-predicates
	   
	   #:test=-function
	   #:test<-function
	   #:test>-function

	   #:default-predicates
	   #:number
	   #:string/case-sensitive
	   #:string/case-insensitive)

  ;; Tasks Metaobjects and programmatic creation
  (:export #:task-class
	   #:task-direct-slot-definition
	   #:ensure-task)

  ;; Tasks
  (:export #:task
	   #:define-task)

  ;; Serialisation
  (:export #:object-sexp
	   #:serialise-object
	   #:serialise-task

	   #:task-string)

  ;; Task input objects and programmatic creation.
  (:export #:task-input
	   #:find-task-input
	   #:ensure-task-input
	   #:define-task-input
	   #:task-input-name
	   #:task-input-functions
	   #:task-input-documentation
	   
	   #:no-applicable-task-input)

  ;; Task input function objects and programmatic creation.
  (:export #:task-input-function
	   #:ensure-task-input-function
	   #:define-task-input-function
	   #:task-input-function-performed-classes
	   #:task-input-function-task-input
	   #:task-input-function-target-class
	   #:task-input-function-documentation)
  
  ;; Determining task input functions
  (:export #:compute-task-input-function
	   #:compute-task-input-functions))
