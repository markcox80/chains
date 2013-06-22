(defpackage "CHAINS.TESTS"
  (:use "COMMON-LISP"
	"LISP-UNIT"
	"CHAINS")
  (:export #:with-temporary-directory))

(defpackage "CHAINS.PEE.TESTS"
  (:use "COMMON-LISP"
	"LISP-UNIT"
	"CHAINS.PEE")
  (:import-from "CHAINS"
		#:prepare-directory
		#:generate
		#:compute-chains
		#:serialise-object
		#:chain-completed-p
		#:task-value)
  (:import-from "CHAINS.TESTS"
		#:with-temporary-directory))
