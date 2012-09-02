(in-package #:cl-git)

(defgeneric git-id (object)
  (:documentation "Return the identifier of OBJECT. 
The identifier is typically the SHA-1 checksum or hash code.

Note that this is an integer, and not the string you typically see reported by git.

To get the string representation use format like this:

    (format nil \"~40,'0X\" (git-id object))

or if you want lowercase hexadecimal digits:

    (format nil \"~(~40,'0X~)\" (git-id object))

If you want to be absolutely sure what libgit2 thinks the hash string is do the following:

    (git-oid-tostr (git-id object))
"))

(defgeneric git-message (object)
  (:documentation "Return the message associated with OBJECT."))

(defgeneric git-author (object)
  (:documentation "Returns an alist containing the author's signature of OBJECT.
A signature is a list with the keys :name :email and :time."))

(defgeneric git-committer (object)
  (:documentation "Returns an alist containing the commiter's signature of OBJECT.
A signature is a list with the keys :name :email and :time."))


(defgeneric git-parentcount (object)
  (:documentation "Returns the number of parents of OBJECT."))

(defgeneric git-parent-oid (object index)
  (:documentation
  "Returns the oid of the parent with index INDEX in the list of
parents of the object OBJECT"))

(defgeneric git-parent-oids (object)
  (:documentation "Returns a list of oids identifying the parent commits of OBJECT"))

(defgeneric git-tree (object)
  (:documentation
  "Returns the tree as a git tree object, for object OBJECT."))

(defgeneric git-lookup (id &key type)) ;; Documentation is copied in.

(defgeneric git-name (object)
  (:documentation "TODO"))

(defgeneric git-tagger (object)
  (:documentation "TODO"))


(defgeneric git-type (object)
  (:documentation "TODO"))

(defgeneric git-target (object)
  (:documentation "Need to rewrite to return an object!!!!"))


(defgeneric git-entry-count (object)
  (:documentation "TODO"))

(defgeneric git-entry-by-index (object index)
  (:documentation "TODO"))

(defgeneric git-entries (object)
  (:documentation "TODO"))

(defgeneric git-values (object)
  (:documentation "TODO"))


;;; how it is now:








(defgeneric walker-next (walker)
  (:documentation "Returns the next object for the walker. 
If no objects are available anymore return nil."))



