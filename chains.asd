(defsystem "chains"
  :author "Mark Cox"
  :description "A simple set of interfaces for defining experiments."
  :depends-on ("alexandria" "cl-fad" "closer-mop")
  :serial t
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "links")
				     (:file "language"))))
  :in-order-to ((test-op (test-op "chains-tests"))))
