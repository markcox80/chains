(in-package "CHAINS.TESTS")

(define-test separate-according-to
  (labels ((test (exp-set-a exp-set-b fn sequence &optional (key #'identity))
	     (multiple-value-bind (set-a set-b) (separate-according-to fn sequence :key key)
	       (assert-equal exp-set-a set-a)
	       (assert-equal exp-set-b set-b))))
    (test '(1 3 5) '(2 4 6) #'oddp (list 1 2 3 4 5 6))
    (test '(2 4 6) '(1 3 5) #'oddp #(1 2 3 4 5 6) #'1+)))

(define-test group-by
  ;; Element not equal with itself.
  (assert-error 'error (group-by #'/= (list 0) :key #'(lambda (x)
						       (mod x 3))))

  (assert-equal '((0 3 6) (1 4 7) (2 5 8)) (group-by #'= (list 0 1 2 3 4 5 6 7 8) :key #'(lambda (x)
											   (mod x 3)))))
