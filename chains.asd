(defsystem "chains"
  :author "Mark Cox"
  :description "A simple set of interfaces for defining experiments."
  :depends-on ("alexandria" "cl-fad")
  :serial t
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "chains")
				     (:file "language"))))
  :in-order-to ((test-op (test-op "chains-tests"))))
