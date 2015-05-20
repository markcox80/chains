(in-package "CHAINS.TESTS")

(define-task example-input-data ()
  ((number-of-subjects
    :initarg :number-of-subjects
    :reader number-of-subjects)
   (samples-per-subject
    :initarg :samples-per-subject
    :reader samples-per-subject)))

(define-task example-variation ()
  ((magnitude
    :initarg :magnitude
    :reader magnitude)))

(define-test ensure-design/basic
  (let ((b (ensure-design 'basic-design
			  :levels (list (list (make-generator (find-class 'example-input-data)
							      (cons :number-of-subjects
								    #'(lambda ()
									(list 1 2 5 10)))
							      (cons :samples-per-subject
								    #'(lambda ()
									(list 1 2 5 10)))))
					(list (make-generator (find-class 'example-variation)
							      (cons :magnitude
								    #'(lambda ()
									(list 0.1 0.5 1.0 5.0)))))))))
    (assert-equal b (find-design 'basic-design))
    (let ((tree (generate 'basic-design)))
      (assert-true (treep tree))
      (assert-false (leafp tree))

      (assert-equal 16 (length (children tree)))
      (assert-equal 64 (count-leaves tree)))))

(define-task example-input-data2 ()
  ((subject-id
    :initarg :subject-id
    :reader subject-id)
   (lighting-variation
    :initarg :lighting-variation
    :reader lighting-variation)))

(let ((user-stuff (list 20 30 50)))
  (define-design advanced-design
      ((:documentation "An example of the DEFINE-DESIGN macro."))

    ((example-input-data (:number-of-subjects 1 2 5 10)
			 (:samples-per-subject 1 2 5 10))
     (example-input-data2 (:subject-id 1 5 10)
			  (:lighting-variation 0 1 2 3)))

    ((example-variation (:magnitude (:splice (loop :for x :from 1 :to 10 :collect x))
				    (:splice user-stuff))))))

(define-test ensure-design/advanced
  (let ((tree (generate 'advanced-design)))
    (assert-equal 28  (length (children tree)))
    (assert-equal 364 (count-leaves tree))))

(define-design nested-design/input-data
    ()
  ((example-input-data (:number-of-subjects 1 2 5 10)
		       (:samples-per-subject 1 2 5 10))
   (example-input-data2 (:subject-id 1 5 10)
			(:lighting-variation 0 1 2 3))))

(define-design nested-design
    ()
  ((:design nested-design/input-data))
  ((example-variation (:magnitude (:splice (loop :for x :from 1 :to 10 :collect x))))))

(define-test ensure-design/nested
  (let ((tree (generate 'nested-design)))
    (assert-equal 28  (length (children tree)))
    (assert-equal 280 (count-leaves tree))))


;;;; Duplicate samples in design
(define-design nested-design-with-duplicates/1
    ()
  ((example-input-data (:number-of-subjects 1 2)
                       (:samples-per-subject 1)))
  ((example-input-data2 (:subject-id 1 5)
                        (:lighting-variation 0 1))))

(define-design nested-design-with-duplicates/2
    ()
  ((example-input-data (:number-of-subjects 1)
                       (:samples-per-subject 1)))
  ((example-input-data2 (:subject-id 5)
                        (:lighting-variation 1))))

(define-design nested-design-with-duplicates
    ()
  ((:design nested-design-with-duplicates/1)
   (:design nested-design-with-duplicates/2)))

(define-test nested-design-with-duplicates
  ;; Design 1
  ;; example-input-data -> example-input-data2
  ;;   (1 1)                  (1 0)    
  ;;   (1 1)                  (5 0)    
  ;;   (1 1)                  (1 1)    
  ;;   (1 1)                  (5 1)    
  ;;   (2 1)                  (1 0)
  ;;   (2 1)                  (5 0)
  ;;   (2 1)                  (1 1)
  ;;   (2 1)                  (5 1)
  ;;
  ;; Design 2
  ;; example-input-data -> example-input-data2
  ;;   (1 1)                  (5 1)
  ;;
  ;; Therefore
  ;;   8 chains after removing duplicates.
  (let* ((tree (generate 'nested-design-with-duplicates))
         (chains (compute-chains tree)))
    (assert-equal 8 (length chains))))
