(in-package #:cl-git)

(defgeneric commit-id (object)
  (:documentation "Return a string containing the commit hash as a 40 bit hex string,
should be replaced with a generic function for all objects, not just for a commit."))

(defgeneric commit-author (object)
  (:documentation "Returns a string containing the commit author's signature.
A signature is a list with the keys :name :email and :time."))

(defgeneric commit-commiter (object)
  (:documentation "Returns a string containing the commit commiter's signature.
A signature is a list with the keys :name :email and :time."))

(defgeneric commit-message (object)
  (:documentation "Return a string containing the commit message."))

(defgeneric commit-parentcount (object)
  (:documentation "Returns the number of parent commits of the argument."))

(defgeneric commit-parent-oid (object index)
  (:documentation
  "Returns the oid of the parent with index INDEX in the list of
parents of the commit OBJECT"))

(defgeneric commit-parent-oids (object)
  (:documentation "Returns a list of oids identifying the parent commits of OBJECT"))

(defgeneric commit-tree (object)
  (:documentation
  "Returns the tree object of the commit."))

(defgeneric tag-name (object)
  (:documentation "TODO"))

(defgeneric tag-tagger (object)
  (:documentation "TODO"))

(defgeneric tag-type (object)
  (:documentation "TODO"))

(defgeneric tag-message (object)
  (:documentation "TODO"))

(defgeneric tag-target (object)
  (:documentation "Need to rewrite to return an object!!!!"))


(defgeneric walker-next (walker)
  (:documentation "Returns the next object for the walker. 
If no objects are available anymore return nil."))



