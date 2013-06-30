(in-package "CHAINS.TESTS")

(defmethod asdf:perform ((op asdf:test-op) (component (eql (asdf:find-system "chains-tests"))))
  (dolist (pkg (list "CHAINS.TESTS"
		     "CHAINS.PEE.TESTS"
		     "CHAINS.CLOSER-MOP-TESTS"))
    (fresh-line)
    (format t "~&;; Running tests in package ~S" pkg)
    (let ((results (lisp-unit:run-tests :all pkg)))
      (print-failures results)
      (print-errors results))))
