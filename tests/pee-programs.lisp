(in-package "CHAINS.PEE.TESTS")

(define-test truncate-tree-to-depth
  (let ((tree (make-tree 1 (list (make-tree 2 nil)
				 (make-tree 3 (list (make-tree 4 (list (make-tree 5 nil)))
						    (make-tree 6 nil)))
				 (make-tree 7 nil)))))
    (assert-true (= 1 (count-leaves (truncate-tree-to-depth tree 0))))
    (assert-true (= 3 (count-leaves (truncate-tree-to-depth tree 1))))
    (assert-true (= 2 (count-leaves (truncate-tree-to-depth tree 2))))
    (assert-true (= 1 (count-leaves (truncate-tree-to-depth tree 3))))
    (assert-equal nil (truncate-tree-to-depth tree 4))))

(define-test count-leaves-if
  (let ((tree (make-tree 1 (list (make-tree 2 nil)
				 (make-tree 3 (list (make-tree 4 (list (make-tree 5 nil)))
						    (make-tree 6 nil)))
				 (make-tree 7 nil)))))
    (assert-true (= 2 (count-leaves-if #'oddp (truncate-tree-to-depth tree 1) :key #'chains:value)))
    (assert-true (= 1 (count-leaves-if #'evenp (truncate-tree-to-depth tree 1) :key #'chains:value)))

    (assert-true (= 0 (count-leaves-if #'oddp (truncate-tree-to-depth tree 2) :key #'chains:value)))
    (assert-true (= 2 (count-leaves-if #'evenp (truncate-tree-to-depth tree 2) :key #'chains:value)))

    (assert-true (= 1 (count-leaves-if #'oddp (truncate-tree-to-depth tree 3) :key #'chains:value)))
    (assert-true (= 0 (count-leaves-if #'evenp (truncate-tree-to-depth tree 3) :key #'chains:value)))))

(define-program test-program
  ((text
    :documentation "Text for EXECUTION-TASK-3."
    :argument ("text" string))
   (verbose
    :documentation "Output more information.")))

(define-test test-program/help
  (let ((expected-help "Usage: [options] [custom options] <data> <depth> <leaf number>

Options:
  --help                        This helpful message.
  --force                       Overwrite any existing output.
  --chains-verbose              Output information about which leaf is being
                                executed.
  --chains-succeed-on-error     Return success if an error occurs.
  --chains-group-size <size>    Execute <size> leaves starting at <leaf>*<size>.
  
Custom Options:
  --text <text>    Text for EXECUTION-TASK-3.
  --verbose        Output more information.

<data> Information needed to execute a tree of tasks.
<depth> The depth of the tasks that are to be executed.
<leaf number> The index of the leaf at <depth> that is to be executed.

"))
    (assert-equal expected-help (with-output-to-string (*standard-output*)
				  (lisp-executable:program-funcall 'test-program "--help")))))

(define-test test-program
  (with-temporary-directory (dir)
    (let* ((*default-pathname-defaults* dir)
	   (area (prepare-directory dir))
	   (tree (generate 'chains.tests::execution-design))
	   (chains (compute-chains tree)))
      (write-program-data "tmp-data.sexp" area tree)
      
      (assert-error 'error (lisp-executable:program-funcall 'test-program "tmp-data.sexp" "0" "0"))

      ;; First Chain
      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "1" "0")
      (assert-false (chain-completed-p area (elt chains 0)))

      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "2" "0")
      (assert-false (chain-completed-p area (elt chains 0)))

      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "3" "0")
      (assert-true (chain-completed-p area (elt chains 0)))

      ;; Second Chain
      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "1" "1")
      (assert-false (chain-completed-p area (elt chains 1)))

      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "2" "1")
      (assert-false (chain-completed-p area (elt chains 1)))

      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "3" "1")
      (assert-true (chain-completed-p area (elt chains 1)))

      ;; Arguments
      (assert-equal "laughing-at-nothing" (task-value 'chains.tests::execution-task-3 (first chains) area))
      (lisp-executable:program-funcall 'test-program "tmp-data.sexp" "3" "0" "--force" "--text" "man")
      (assert-equal "laughing-man" (task-value 'chains.tests::execution-task-3 (first chains) area)))))

(define-program test-program-with-no-options
  ())

(define-test test-program-with-no-options
  (let ((expected-help "Usage: [options] <data> <depth> <leaf number>

Options:
  --help                        This helpful message.
  --force                       Overwrite any existing output.
  --chains-verbose              Output information about which leaf is being
                                executed.
  --chains-succeed-on-error     Return success if an error occurs.
  --chains-group-size <size>    Execute <size> leaves starting at <leaf>*<size>.
  
<data> Information needed to execute a tree of tasks.
<depth> The depth of the tasks that are to be executed.
<leaf number> The index of the leaf at <depth> that is to be executed.

"))
    (assert-equal expected-help (with-output-to-string (*standard-output*)
				  (lisp-executable:program-funcall 'test-program-with-no-options "--help")))))
