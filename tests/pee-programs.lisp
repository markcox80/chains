(in-package "CHAINS.TESTS")

(chains.pee:define-program test-program
  ((text
    :documentation "Text for EXECUTION-TASK-3"
    :argument ("text" string))))

(define-test test-program/help
  (let ((expected-help "Usage: [options] [custom options] <data> <depth> <leaf number>

Options:
  --help     This helpful message.
  --force    Overwrite any existing output.
  
Custom Options:
  --text <text>    Text for EXECUTION-TASK-3

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
	   (tree (generate 'execution-design))
	   (chains (compute-chains tree)))
      (with-open-file (out "tmp-data.sexp" :direction :output)
	(serialise-object out (list area tree)))
      
      ;; First Chain
      (assert-false (chain-completed-p area (elt chains 0)))
      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "0" "0")
      (assert-false (chain-completed-p area (elt chains 0)))

      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "1" "0")
      (assert-false (chain-completed-p area (elt chains 0)))

      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "2" "0")
      (assert-true (chain-completed-p area (elt chains 0)))

      ;; Second Chain
      (assert-false (chain-completed-p area (elt chains 1)))
      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "0" "1")
      (assert-false (chain-completed-p area (elt chains 1)))

      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "1" "1")
      (assert-false (chain-completed-p area (elt chains 1)))

      (lisp-executable:program-funcall 'test-program
				       "tmp-data.sexp" "2" "1")
      (assert-true (chain-completed-p area (elt chains 1)))

      ;; Arguments
      (assert-equal "laughing-NIL" (task-value 'execution-task-3 (first chains) area))
      (lisp-executable:program-funcall 'test-program "tmp-data.sexp" "2" "0" "--force" "--text" "man")
      (assert-equal "laughing-man" (task-value 'execution-task-3 (first chains) area)))))
