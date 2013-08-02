(in-package "CHAINS.PEE")

(defclass custom-option ()
  ((name
    :initarg :name
    :reader custom-option-name)
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
   :argument-var (gensym)
   :documentation ""))

(defun option-definition (custom-option)
  `(,(custom-option-name custom-option)
    ,(custom-option-argument-var custom-option)))

(defun option-declarations (custom-option)
  (list `(lisp-executable:conversion-function ,(custom-option-argument-type custom-option)
					      ,(custom-option-argument-var custom-option))
	`(type (or null ,(custom-option-argument-type custom-option))
	       ,(custom-option-argument-var custom-option))))

(defun option-perform-arguments (custom-option)
  (list (intern (symbol-name (custom-option-name custom-option)) "KEYWORD")
	(custom-option-argument-var custom-option)))

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

(defun perform-program (data-pathname depth leaf &rest args &key &allow-other-keys)
  (multiple-value-bind (area tree) (read-program-data data-pathname)
    (let ((chains (compute-chains-to-depth tree depth)))
      (apply #'perform-leaf area (elt chains leaf) args))))

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
					     ("force" nil "Overwrite any existing output.")))
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
					     ("force" nil "Overwrite any existing output.")))
	  (print-program-usage/suffix)))

(defun print-program-usage (help-data &optional (stream *standard-output*))
  (if help-data
      (print-program-usage/custom-options help-data stream)
      (print-program-usage/no-custom-options stream)))

(defun do-define-program (name custom-options)
  (let ((tree (gensym "TREE"))
	(depth (gensym "DEPTH"))
	(leaf (gensym "LEAF"))
	(help-data (gensym "HELP-DATA")))
    `(let ((,help-data (list ,@(mapcar #'help-data custom-options))))
       (lisp-executable:define-program ,name
	   (lisp-executable:&options help force
				     ,@(mapcar #'option-definition custom-options)
				     lisp-executable:&arguments ,tree ,depth ,leaf)
	 (declare (lisp-executable:conversion-function (integer 1) ,depth)
		  (lisp-executable:conversion-function (integer 0) ,leaf)
		  ,@(reduce #'append custom-options :key #'option-declarations)
		  (ignorable force ,@(mapcar #'custom-option-name custom-options)))
	 (cond
	   (help
	    (print-program-usage ,help-data)
	    0)
	   ((or (null ,tree) (null ,depth) (null ,leaf))
	    (print-program-usage ,help-data)
	    1)
	   (t
	    (perform-program ,tree ,depth ,leaf
			     :force force
			     ,@(reduce #'append custom-options :key #'option-perform-arguments))))))))

(defmacro define-program (name custom-options)
  (do-define-program name (mapcar #'custom-option-from-sexp custom-options)))

(defun custom-option-from-sexp (sexp)
  (destructuring-bind (name &key documentation argument) sexp
    (destructuring-bind (argument-label argument-type) argument
      (make-instance 'custom-option
		     :name name
		     :documentation documentation
		     :argument-label argument-label
		     :argument-type argument-type))))
