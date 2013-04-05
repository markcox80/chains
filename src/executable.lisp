(in-package "CHAINS")

(lisp-executable:define-program program/perform (lisp-executable:&options
						 (if-performed if-performed-argument)
						 lisp-executable:&arguments
						 link-file)
  (declare (lisp-executable:conversion-function keyword if-performed)
	   (type (member nil :skip :error :supersede) if-performed-argument)
	   (ignore if-performed))

  (setf if-performed-argument (or if-performed-argument
				  :skip))
  (multiple-value-bind (chain *database-pathname*) (read-chain link-file)
    (perform chain :if-performed if-performed-argument)))
