(in-package "CHAINS.TESTS")

(define-operation (task algorithm-1) ((obj algorithm-input-data))
  (list obj :performed))

(define-test define-operation
  (let ((chain (list (make-instance 'input-data-1)))
	(task  (make-instance 'algorithm-1)))
    (assert-equal (list 'input-data-1 :performed) (perform-operation task chain))))
