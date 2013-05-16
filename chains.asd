(in-package "ASDF")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "lisp-executable"))

(defsystem "chains"
  :author "Mark Cox"
  :description "A simple set of interfaces for defining experiments."
  :depends-on ("alexandria" "cl-fad" "closer-mop" "lisp-executable")
  :serial t
  :components ((:module "src"
			:serial t
			:components ((:file "packages"))))
  :in-order-to ((test-op (test-op "chains-tests"))))
