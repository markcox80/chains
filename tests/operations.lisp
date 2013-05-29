(in-package "CHAINS.TESTS")

(define-task no-algorithm-input-data-task ()
  ())

(define-operation (task algorithm-1) ((obj algorithm-input-data))
  (list obj :performed))

(define-test define-operation
  (let ((chain (list (make-instance 'input-data-1)))
	(task  (make-instance 'algorithm-1)))
    (assert-equal (list 'input-data-1 :performed) (perform-operation task chain)))

  (let ((chain (list (make-instance 'no-algorithm-input-data-task)))
	(task  (make-instance 'algorithm-1)))
    (assert-error 'no-applicable-task-input-function (perform-operation task chain))))
