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
	   #:read-chain
	   #:discover-chains

	   #:chain-link-pathname
	   #:chain-result-pathname
	   #:chain-result)

  ;; Tasks
  (:export #:perform
	   #:performedp
	   #:already-performed-error)
  
  ;; Macros
  (:export #:define-step
	   #:define-operation))
