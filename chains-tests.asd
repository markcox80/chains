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
				     (:file "chains")))
	       (:module "bin"
			:serial t
			:components ((lisp-executable:executable "chains-test" :program ("CHAINS" "PROGRAM/PERFORM"))))))
