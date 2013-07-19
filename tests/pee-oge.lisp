(in-package "CHAINS.PEE.TESTS")

(define-test number-of-tasks-at-depths
  (let ((tree (make-tree nil (list (make-tree 2 nil)
				 (make-tree 3 (list (make-tree 4 (list (make-tree 5 nil)))
						    (make-tree nil nil)))
				 (make-tree 7 nil)))))
    (assert-equal '(0 3 1 1) (chains.pee::number-of-tasks-at-depths tree))))
