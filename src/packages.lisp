(defpackage "CHAINS"
  (:use "COMMON-LISP")

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

  ;; Tasks Metaobjects and programatic creation
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

	   #:task-string))
