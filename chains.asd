(in-package "ASDF")

(defsystem "chains"
  :author "Mark Cox"
  :description "A simple set of interfaces for defining experiments."
  :depends-on ("alexandria" "cl-fad" "closer-mop" "lisp-executable" "split-sequence")
  :serial t
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "utilities")
				     (:file "predicates")
				     (:file "tasks")
				     (:file "task-input")
				     (:file "operations")
				     (:file "serialisation")
				     (:file "tree")
				     (:file "design")
				     (:file "querying")
				     (:file "execution")
				     (:file "pee-programs")
				     (:file "pee-oge"))))
  :in-order-to ((test-op (test-op "chains-tests"))))
