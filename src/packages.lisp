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
	   
	   #:no-applicable-task-input-function)

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
	   #:compute-task-input-functions)

  ;; Evaluate a task input function
  (:export #:evaluate-task-input-function)

  ;; Operations
  (:export #:ensure-operation
	   #:define-operation
	   #:perform-operation)

  ;; Trees
  (:export #:tree
	   #:value
	   #:children
	   #:treep
	   #:make-tree
	   #:leafp
	   
	   #:count-leaves
	   #:count-leaves-if
	   #:replace-leaves

	   #:compute-chains)

  ;; Designs and programmatic creation
  (:export #:design
	   #:design-name
	   #:design-documentation
	   #:design-levels

	   #:designp
	   #:find-design
	   #:ensure-design
	   
	   #:make-generator
	   
	   #:define-design
	   
	   #:generate)

  ;; Querying
  (:export #:prepare-group-chains-test
	   #:prepare-group-chains-sort-test
	   #:prepare-group-chains
	   #:group-chains)

  ;; Execution
  (:export ;; Area protocol
	   #:task-data-directory*
	   #:task-completed-p*
	   #:task-value*
	   
	   ;; Prepared directory area
	   #:prepared-directory
	   #:prepare-directory
	   #:area-directory

	   ;; Execution
	   #:task-data-directory
	   #:task-value
	   #:task-completed-p	   
	   #:operation-plist
	   #:forcedp
	   
	   #:chain-completed-p
	   #:perform-leaf
	   #:perform

	   ;; Operation Results
	   #:*area*
	   #:*chain*
	   #:task-data-directory
	   #:task-value))

  ;; Parallel Execution Environments
(defpackage "CHAINS.PEE"
  (:use "COMMON-LISP")
  (:import-from "CHAINS"
		#:perform-leaf
		#:compute-chains

		#:make-tree
		#:leafp
		#:children
		#:value
		#:count-leaves-if		

		#:serialise-object)

  ;; Parallel execution
  (:export #:*number-of-processes*
	   #:parallel-perform)

  ;; Command line programs
  (:export #:write-program-data
	   #:read-program-data
	   #:define-program
	   #:truncate-tree-to-depth)

  ;; Oracle Grid Engine
  (:export #:prepare-oge-script)

  ;; XARGS
  (:export #:prepare-xargs-script))
