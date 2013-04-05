(defpackage "CHAINS"
  (:use "COMMON-LISP")
  ;; Links
  (:export #:output-name
	   #:operation-equal)

  ;; Chains
  (:export #:chain
	   #:make-chain

	   #:*database-pathname*
	   #:write-chain

	   #:chain-link-pathname
	   #:chain-result-pathname)

  ;; Tasks
  (:export #:perform
	   #:performedp
	   #:already-performed-error)
  
  ;; Macros
  (:export #:define-step
	   #:define-operation))
