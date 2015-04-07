(in-package "CHAINS.PEE")

(defclass custom-option ()
  ((name
    :initarg :name
    :reader custom-option-name)
   (plist-key
    :initarg :plist-key
    :reader custom-option-plist-key)
   (documentation
    :initarg :documentation
    :reader custom-option-documentation)
   (argument-label
    :initarg :argument-label
    :reader custom-option-argument-label)
   (argument-type
    :initarg :argument-type
    :reader custom-option-argument-type)
   (argument-var
    :initarg :argument-var
    :reader custom-option-argument-var))
  (:default-initargs
   :argument-label nil
   :argument-type nil
   :argument-var (gensym)
   :documentation ""))

(defun option-definition (custom-option)
  (with-accessors ((custom-option-argument-type custom-option-argument-type)
		   (custom-option-argument-var custom-option-argument-var)
		   (custom-option-argument-label custom-option-argument-label)
		   (custom-option-name custom-option-name))
      custom-option
    (cond
      (custom-option-argument-label
       `(,custom-option-name ,custom-option-argument-var))
      (t
       custom-option-name))))

(defun option-declarations (custom-option)
  (with-accessors ((custom-option-argument-type custom-option-argument-type)
		   (custom-option-argument-var custom-option-argument-var)
		   (custom-option-argument-label custom-option-argument-label))
      custom-option
    (when custom-option-argument-label
      (list `(lisp-executable:conversion-function ,(custom-option-argument-type custom-option)
						  ,(custom-option-name custom-option))
	    `(type (or null ,(custom-option-argument-type custom-option))
		   ,(custom-option-argument-var custom-option))))))

(defun option-perform-arguments (custom-option)
  (with-accessors ((custom-option-argument-type custom-option-argument-type)
		   (custom-option-argument-var custom-option-argument-var)
		   (custom-option-argument-label custom-option-argument-label)
		   (custom-option-plist-key custom-option-plist-key)
		   (custom-option-name custom-option-name))
      custom-option
    (cond
      (custom-option-argument-label       
       (list custom-option-plist-key custom-option-argument-var))
      (t
       (list custom-option-plist-key custom-option-name)))))

(defun help-data (custom-option)
  `(list ,(string-downcase (custom-option-name custom-option))
	 ,(custom-option-argument-label custom-option)
	 ,(custom-option-documentation custom-option)))

;; DEFINE-PROGRAM
(defun truncate-tree-to-depth (tree depth)
  (declare (type (integer 0) depth))
  (cond
    ((zerop depth)
     (make-tree (value tree) nil))
    ((leafp tree)
     nil)
    (t
     (let ((children (remove nil (mapcar #'(lambda (child)
					     (truncate-tree-to-depth child (1- depth)))
					 (children tree)))))
       (when children
	 (make-tree (value tree) children))))))

(defun compute-chains-to-depth (tree depth)
  (compute-chains (truncate-tree-to-depth tree depth)))

(defun write-program-data (pathname area tree &key (if-exists :error))
  (let ((*package* (find-package "COMMON-LISP")))
    (with-open-file (out pathname :direction :output :if-exists if-exists)
      (serialise-object out (list area tree))
      (terpri out))))

(defun read-program-data (pathname &key (if-does-not-exist :error))
  (let ((*package* (find-package "COMMON-LISP")))
    (with-open-file (in pathname :if-does-not-exist if-does-not-exist)
      (values-list (read in)))))

(defun perform-program (data-pathname depth leaf &rest args &key chains-group-size chains-verbose &allow-other-keys)
  (alexandria:remove-from-plistf args :chains-group-size)
  (let* ((chains-group-size (or chains-group-size 1))
         (succeeded? t))
    (multiple-value-bind (area tree) (read-program-data data-pathname)
      (let ((chains (compute-chains-to-depth tree depth)))
	(loop
	   with number-of-chains = (length chains)
	   for index from (* leaf chains-group-size) below (min number-of-chains (* (1+ leaf) chains-group-size))
	   do
	     (when chains-verbose
	       (format t "~&;;;; Starting leaf ~d at depth ~d~%" index depth))
	     (handler-case (apply #'perform-leaf area (elt chains index) args)
	       (error (c)
		 (format *error-output* "~&;;;; Encountered error whilst executing leaf ~d for depth ~d:~%~A~%" index depth c)
                 (setf succeeded? nil))))
        succeeded?))))

(defun break-string (string)
  (split-sequence:split-sequence #\Space string))

(defun print-program-usage/option-text (help-data)
  (labels ((arg-string (item)
	     (destructuring-bind (name arg-name help-message) item
	       (declare (ignore help-message))
	       (if arg-name
		   (format nil "--~A <~A>" name arg-name)
		   (format nil "--~A" name)))))
    (let* ((arg-strings (mapcar #'arg-string help-data))
	   (help-messages (mapcar #'third help-data))
	   (maximum-width (+ 4 (reduce #'max arg-strings :key #'length)))
	   (control-string (format nil "  ~~~dA" maximum-width)))
      (with-output-to-string (out)
	(loop
	   :for arg-string :in arg-strings
	   :for help-message :in help-messages
	   :for index :from 0
	   :do
	   (when (plusp index)
	     (fresh-line out))
	   (format out control-string arg-string)
	   (pprint-logical-block (out (break-string help-message))
	     (loop
		(write-string (pprint-pop) out)
		(pprint-exit-if-list-exhausted)
		(write-char #\Space out)
		(pprint-newline :fill out))))))))

(defun print-program-usage/suffix ()
  "<data> Information needed to execute a tree of tasks.
<depth> The depth of the tasks that are to be executed.
<leaf number> The index of the leaf at <depth> that is to be executed.
")

(defun print-program-usage/custom-options (help-data &optional (stream *standard-output*))
  (format stream 
	  "Usage: [options] [custom options] <data> <depth> <leaf number>

Options:
~A
  
Custom Options:
~A

~A
"
	  (print-program-usage/option-text '(("help" nil "This helpful message.")
					     ("force" nil "Overwrite any existing output.")
					     ("chains-verbose" nil "Output information about which leaf is being executed.")
					     ("chains-group-size" "size" "Execute <size> leaves starting at <leaf>*<size>.")))
	  (print-program-usage/option-text help-data)
	  (print-program-usage/suffix)))

(defun print-program-usage/no-custom-options (&optional (stream *standard-output*))
  (format stream 
	  "Usage: [options] <data> <depth> <leaf number>

Options:
~A
  
~A
"
	  (print-program-usage/option-text '(("help" nil "This helpful message.")
					     ("force" nil "Overwrite any existing output.")
					     ("chains-verbose" nil "Output information about which leaf is being executed.")
					     ("chains-group-size" "size" "Execute <size> leaves starting at <leaf>*<size>.")))
	  (print-program-usage/suffix)))

(defun print-program-usage (help-data &optional (stream *standard-output*))
  (if help-data
      (print-program-usage/custom-options help-data stream)
      (print-program-usage/no-custom-options stream)))

(defun do-define-program (name custom-options &key prologue)
  (let ((tree (gensym "TREE"))
	(depth (gensym "DEPTH"))
	(leaf (gensym "LEAF"))
	(help-data (gensym "HELP-DATA")))
    `(let ((,help-data (list ,@(mapcar #'help-data custom-options))))
       (lisp-executable:define-program ,name
	   (lisp-executable:&options help force chains-verbose (chains-group-size chains-group-size-value)
				     ,@(mapcar #'option-definition custom-options)
				     lisp-executable:&arguments ,tree ,depth ,leaf)
	 (declare (lisp-executable:conversion-function (integer 1) ,depth)
		  (lisp-executable:conversion-function (integer 0) ,leaf)
		  (lisp-executable:conversion-function (integer 1) chains-group-size)
		  ,@(reduce #'append custom-options :key #'option-declarations)
		  (ignorable force ,@(mapcar #'custom-option-name custom-options))
		  (ignore chains-group-size))
	 (cond
	   (help
	    (print-program-usage ,help-data)
	    0)
	   ((or (null ,tree) (null ,depth) (null ,leaf))
	    (print-program-usage ,help-data)
	    1)
	   (t
	    ,(when prologue
	      `(progn
		 ,@prologue))	    
	    (if (perform-program ,tree ,depth ,leaf
                                 :force force
                                 :chains-group-size (or chains-group-size-value 1)
                                 :chains-verbose chains-verbose
                                 ,@(reduce #'append custom-options :key #'option-perform-arguments))
                0
                1)))))))

(defmacro define-program (name custom-options &body options)
  (apply #'do-define-program name (mapcar #'custom-option-from-sexp custom-options)
	 (reduce #'append options
		 :key #'(lambda (option)		     
			  (alexandria:destructuring-case option
			    ((:prologue &rest body)
			     (list :prologue body)))))))

(defun custom-option-from-sexp (sexp)
  (destructuring-bind (name &key documentation argument plist-key) sexp
    (destructuring-bind (&optional argument-label argument-type) argument
      (make-instance 'custom-option
		     :name name
		     :plist-key (or plist-key (intern (symbol-name name) "KEYWORD"))
		     :documentation documentation
		     :argument-label argument-label
		     :argument-type argument-type))))
