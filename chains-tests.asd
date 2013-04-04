(defsystem "chains-tests"
  :author "Mark Cox"
  :description "Tests for the CHAINS system."
  :depends-on ("chains" "lisp-unit")
  :serial t
  :components ((:module "tests"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     (:file "chains")))))
