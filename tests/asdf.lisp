(in-package "CHAINS.TESTS")

(defmethod asdf:perform ((op asdf:test-op) (component (eql (asdf:find-system "chains-tests"))))
  (lisp-unit:run-tests :all "CHAINS.TESTS"))
