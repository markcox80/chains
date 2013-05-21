(in-package "CHAINS.TESTS")

(define-task example-task ()
  ((mean
    :initarg :mean
    :reader mean
    :test= #'=
    :test< #'<
    :test> #'>)
   (variance
    :initarg :variance
    :writer variance
    :test= #'=
    :test< #'<
    :test> #'>)
   (name
    :accessor name
    :initform "Example Task"
    :type string
    :test= #'string=
    :test< #'string<
    :test> #'string>))
  (:documentation "Example usage of the DEFINE-TASK macro.")
  (:default-initargs
   :mean 0
   :variance 1))

(defun find-slot-definition-with-name (name definitions)
  (find name definitions :key #'closer-mop:slot-definition-name))

(define-test example-task/class
  (let* ((c (find-class 'example-task))
	 (defs (closer-mop:class-direct-slots c))
	 (mean-slot (find-slot-definition-with-name 'mean defs))
	 (variance-slot (find-slot-definition-with-name 'variance defs))
	 (name-slot (find-slot-definition-with-name 'name defs)))

    (dolist (item (list mean-slot variance-slot))
      (assert-equal #'= (test=-function item))
      (assert-equal '#'= (test=-form item))
      (assert-equal #'< (test<-function item))
      (assert-equal '#'< (test<-form item))
      (assert-equal #'> (test>-function item))
      (assert-equal '#'> (test>-form item)))

    (assert-equal #'string= (test=-function name-slot))
    (assert-equal '#'string= (test=-form name-slot))
    (assert-equal #'string< (test<-function name-slot))
    (assert-equal '#'string< (test<-form name-slot))
    (assert-equal #'string> (test>-function name-slot))
    (assert-equal '#'string> (test>-form name-slot))
    
    (assert-equal "Example usage of the DEFINE-TASK macro."
		  (documentation c t))))

(define-test example-task/object
  (let ((obj (make-instance 'example-task)))
    (assert-equal 0 (mean obj))
    (assert-equal 1 (slot-value obj 'variance))
    (assert-equal "Example Task" (name obj))

    (setf (variance obj) 2)
    (assert-equal 2 (slot-value obj 'variance))

    (setf (name obj) "H")
    (assert-equal "H" (name obj))))
