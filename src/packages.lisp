(defpackage "CHAINS"
  (:use "COMMON-LISP")

  ;; Tasks Metaobjects and programatic creation
  (:export #:task-class
	   #:task-direct-slot-definition
	   #:ensure-task

	   #:test=-form
	   #:test<-form
	   #:test>-form

	   #:test=-function
	   #:test<-function
	   #:test>-function)

  ;; Tasks
  (:export #:task
	   #:define-task))
