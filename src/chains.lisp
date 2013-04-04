(in-package "CHAINS")

;; The task protocol
(defgeneric performedp (link)
  (:documentation "Determines if the task has been performed."))

(defgeneric perform (link &key if-performed &allow-other-keys)
  (:documentation "Perform the task specified by the LINK."))

(defgeneric data-pathname (link)
  (:documentation "The relative pathname in which the return value of
  PERFORM saved to."))

;; The link protocol
(defgeneric parent-link (link)
  (:documentation "The parent link in the chain."))

(defgeneric output-name (link)
  (:documentation "A short string that describes LINK uniquely."))

(defgeneric write-link (pathname link &key if-exists)
  (:documentation "A the LINK to PATHNAME."))

(defgeneric copy-link (link &optional parent)
  (:documentation "Create a new instance of LINK that is an exact
  copy. If PARENT is specified, then the new instance will have PARENT
  as its PARENT-LINK."))

(defgeneric link-pathname (link)
  (:documentation "A relative pathname in where the information in
  LINK will be saved to."))

(defclass link ()
  ((parent-link
    :initarg :parent-link
    :reader parent-link))
  (:documentation "A link in an experiment chain.")
  (:default-initargs
   :parent-link nil))

;; The chain protocol

(defgeneric add-link (chain link)
  (:documentation "Return a new CHAIN instance with the LINK added. A
  copy of LINK is inserted in to the new CHAIN instance."))

(defgeneric find-link-if (predicate chain)
  (:documentation "Find a link in CHAIN that satisfies the given PREDICATE."))

(defvar *database-pathname*)

(defgeneric write-chain (chain &key if-link-exists)
  (:documentation "Write CHAIN to disk."))

(defclass chain ()
  ((links
    :initarg :links
    :reader links)))

(defun make-chain (&rest links)
  (make-instance 'chain :links (let ((rv nil))
				 (reduce #'(lambda (parent link)
					     (let ((new-link (copy-link link parent)))
					       (push new-link rv)))
					 links
					 :initial-value nil)
				 (nreverse rv))))

(defmethod find-link-if (predicate (chain chain))
  (find-if predicate (links chain)))
