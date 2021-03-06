(in-package "ASDF")

(defsystem "chains"
  :author "Mark Cox"
  :description "A simple set of interfaces for defining experiments."
  :depends-on ("alexandria" "cl-fad" "closer-mop" "lisp-executable" "split-sequence" "bordeaux-threads")
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
				     (:file "pee-parallel")
				     (:file "pee-programs")
				     (:file "pee-oge")
				     (:file "pee-xargs")
				     (:file "pee-pbs")
				     (:file "pee-slurm"))))
  :in-order-to ((test-op (test-op "chains-tests"))))
