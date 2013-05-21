(defpackage "CHAINS"
  (:use "COMMON-LISP")

  ;; Tasks Metaobjects and programatic creation
  (:export #:task-class
	   #:task-direct-slot-definition
	   #:ensure-task)

  ;; Tasks
  (:export #:task
	   #:define-task))
