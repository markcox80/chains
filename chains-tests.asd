(in-package "ASDF")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "lisp-executable"))

(defsystem "chains-tests"
  :author "Mark Cox"
  :description "Tests for the CHAINS system."
  :depends-on ("chains" "lisp-unit")
  :serial t
  :components ((:module "tests"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     (:file "closer-mop")
				     (:file "utilities")
				     (:file "tasks")
				     (:file "task-input")
				     (:file "operations")
				     (:file "serialisation")
				     (:file "design")
				     (:file "querying")
				     (:file "execution")
				     (:file "pee-parallel")
				     (:file "pee-programs")
				     (:file "pee-oge")))))
