(in-package "CHAINS.TESTS")

(define-task base-serialisation-task ()
  ((sigma
    :initarg :sigma
    :reader sigma)))

(define-task serialisation-task (base-serialisation-task)
  ((rho
    :initarg :rho
    :reader rho)))

(define-test serialisation
  (let* ((obj (make-instance 'serialisation-task :sigma 0.5 :rho 2))
	 (str (with-output-to-string (out)
		(serialise-task out obj)))
	 (v   (with-input-from-string (in str)
		(eval (read in)))))
    (assert-equal (sigma obj) (sigma v))
    (assert-equal (rho obj) (rho v))))

(define-test task-string
  (let ((obj (make-instance 'serialisation-task :sigma 0.5 :rho 2)))
    (assert-equal "serialisation-task-0.5-2" (task-string obj)))

  (let ((obj (make-instance 'base-serialisation-task :sigma 0.1)))
    (assert-equal "base-serialisation-task-0.1" (task-string obj))))

(define-task task-with-no-slots ()
  ())

(define-test task-string/no-slots
  (let ((object (make-instance 'task-with-no-slots)))
    (assert-equal "task-with-no-slots" (task-string object))))

(defclass readable-object ()
  ((value :initarg :value
          :reader readable-object-value))
  (:default-initargs
   :value nil))

(defmethod print-object ((object readable-object) stream)
  (cond
    (*print-readably*
     (format stream "#.~W" `(make-instance 'readable-object :value ,(readable-object-value object))))
    (t
     (print-unreadable-object (object stream :type t :identity t)
       (write (readable-object-value object) :stream stream)))))

(define-test serialisation-of-readable-objects
  (let* ((original (make-instance 'readable-object :value 1))
         (string (with-output-to-string (out)
                   (serialise-object out original)))
         (object (read-from-string string)))    
    (assert-true (typep object 'readable-object))
    (assert-false (eql object original))
    (assert-equal 1 (readable-object-value object))
    (assert-equal (format nil "#.~W" `(make-instance 'readable-object :value ,(readable-object-value object)))
                  string)))
