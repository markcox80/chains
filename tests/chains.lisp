(in-package "CHAINS.TESTS")

(defvar *chain-temporary-directory* #P "/tmp/chains/")
(defun chain-temporary-directory ()
  (when (cl-fad:directory-exists-p *chain-temporary-directory*)
    (cl-fad:delete-directory-and-files *chain-temporary-directory*))
  (ensure-directories-exist *chain-temporary-directory*))

(define-step noise-model ()
    ()
  (:value noise-model-value))

(define-operation gaussian (noise-model)
  ((sigma
    :initarg :sigma
    :reader sigma
    :test #'=))
  (:default-initargs
   :sigma 1))

(defmethod perform ((object gaussian) &key)
  :gaussian)

(define-operation salt-and-pepper (noise-model)
    ())

(defmethod perform ((object salt-and-pepper) &key)
  :salt-and-pepper)

(define-step algorithm ()
    ())

(define-operation platypus (algorithm)
    ())

(defmethod perform ((object platypus) &key chain)
  (assert (typep (noise-model chain) 'noise-model))
  (assert (keywordp (noise-model-value chain)))
  :platypus)

(define-operation kangaroo (algorithm)
    ())

(defmethod perform ((object kangaroo) &key)
  :kangaroo)

(define-test output-name
  (let ((obj (make-instance 'gaussian :sigma 0.1)))
    (assert-equal "gaussian-0.1" (output-name obj)))

  (let ((obj (make-instance 'platypus)))
    (assert-equal "platypus" (output-name obj))))

(define-test query
  (let* ((noise-model (make-instance 'gaussian :sigma 0.1))
	 (algorithm   (make-instance 'platypus))
	 (chain (make-chain noise-model algorithm)))
    (assert-true (typep (noise-model chain) 'gaussian))
    (assert-true (typep (algorithm chain) 'platypus))))

(define-test operation-equal
  (let ((a (make-instance 'gaussian :sigma 0.1))
	(b (make-instance 'gaussian :sigma 0.1))
	(c (make-instance 'gaussian :sigma 0.2)))
    (assert-true (operation-equal a b))
    (assert-false (operation-equal a c))))

(define-test pathnames
  (let ((*database-pathname* (chain-temporary-directory))
	(chain-1 (make-chain (make-instance 'gaussian :sigma 0.1)
			     (make-instance 'kangaroo))))
    (assert-equal (merge-pathnames "gaussian-0.1/link.sexp" *database-pathname*)
		  (chain-link-pathname chain-1 'noise-model))
    (assert-equal (merge-pathnames "gaussian-0.1/kangaroo/link.sexp" *database-pathname*)
		  (chain-link-pathname chain-1 'algorithm))

    (assert-equal (merge-pathnames "gaussian-0.1/result.sexp" *database-pathname*)
		  (chain-result-pathname chain-1 'noise-model))
    (assert-equal (merge-pathnames "gaussian-0.1/kangaroo/result.sexp" *database-pathname*)
		  (chain-result-pathname chain-1 'algorithm))))

(defun pf (pathname)
  (probe-file (merge-pathnames pathname *database-pathname*)))

(define-test serialisation
  (let ((*database-pathname* (chain-temporary-directory))
	(chain-1 (make-chain (make-instance 'gaussian :sigma 0.1)))
	(chain-2 (make-chain (make-instance 'salt-and-pepper)))
	(chain-3 (make-chain (make-instance 'gaussian :sigma 0.2))))
    (write-chain chain-1)
    (write-chain chain-2)
    (write-chain chain-3)

    (assert-true (pf "root.sexp"))
    (assert-true (pf "gaussian-0.1/link.sexp"))
    (assert-true (pf "gaussian-0.2/link.sexp"))
    (assert-true (pf "salt-and-pepper/link.sexp"))

    (perform chain-1)
    (perform chain-2)
    (perform chain-3)
    
    (assert-true (pf "gaussian-0.1/result.sexp"))
    (assert-true (pf "gaussian-0.2/result.sexp"))
    (assert-true (pf "salt-and-pepper/result.sexp"))

    (assert-error 'error (chain-result chain-2 'algorithm))
    (assert-equal :salt-and-pepper (chain-result chain-2 'noise-model))

    (assert-error 'already-performed-error (perform chain-1 :if-performed :error))
    (perform chain-1 :if-performed :supersede)
    (assert-true (pf "gaussian-0.1/result.sexp"))))

(define-test serialisation/multi-chain
  (let ((*database-pathname* (chain-temporary-directory))
	(chain-1 (make-chain (make-instance 'gaussian :sigma 0.1)
			     (make-instance 'kangaroo)))
	(chain-2 (make-chain (make-instance 'salt-and-pepper)
			     (make-instance 'kangaroo)))
	(chain-3 (make-chain (make-instance 'salt-and-pepper)
			     (make-instance 'platypus))))
    (write-chain chain-1)
    (write-chain chain-2)
    (assert-error 'error (write-chain chain-3 :if-exists :error))
    (write-chain chain-3 :if-exists :skip)

    (assert-true (pf "root.sexp"))
    (assert-true (pf "gaussian-0.1/link.sexp"))
    (assert-true (pf "gaussian-0.1/kangaroo/link.sexp"))
    (assert-true (pf "salt-and-pepper/link.sexp"))
    (assert-true (pf "salt-and-pepper/kangaroo/link.sexp"))
    (assert-true (pf "salt-and-pepper/platypus/link.sexp"))

    (perform chain-1)
    (perform chain-2)
    (perform chain-3)
    
    (assert-true (pf "gaussian-0.1/result.sexp"))
    (assert-true (pf "salt-and-pepper/result.sexp"))
    (assert-true (pf "salt-and-pepper/kangaroo/result.sexp"))
    (assert-true (pf "salt-and-pepper/platypus/result.sexp"))

    (assert-error 'error (read-chain (pf "gaussian-0.1/result.sexp")))
    (let ((read-chain (read-chain (pf "gaussian-0.1/link.sexp"))))
      (assert-true (= 1 (length read-chain)))
      (assert-true (operation-equal (first read-chain) (first chain-1))))

    (multiple-value-bind (read-chain root-pathname) (read-chain (pf "salt-and-pepper/kangaroo/link.sexp"))
      (assert-true (pathname-match-p root-pathname (truename *database-pathname*)))
      (assert-true (= 2 (length read-chain)))
      (assert-true (operation-equal (first read-chain) (first chain-2)))
      (assert-true (operation-equal (second read-chain) (second chain-2))))

    (assert-equal 3 (length (discover-chains *database-pathname*)))))

(define-test discover-chains/empty
  (let ((*database-pathname* (chain-temporary-directory)))
    (assert-equal nil (discover-chains *database-pathname*))))
