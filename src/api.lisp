(in-package #:cl-git)


(defparameter *git-repository* nil
  "A global that stores the current Git repository.")

(defparameter *git-repository-index* nil
  "A global that stores the current Git index")

(defgeneric git-id (object)
  (:documentation "Return the identifier of OBJECT. 
The identifier is typically the SHA-1 checksum or hash code.

Note that this is an integer, and not the string you typically see reported by git.

To get the string representation use format like this:

    (format nil \"~40,'0X\" (git-id object))

or if you want lowercase hexadecimal digits:

    (format nil \"~(~40,'0X~)\" (git-id object))

Supported Objects

- COMMIT
- OBJECT
- TAG
- TREE
"))

(defgeneric git-message (object)
  (:documentation "Return the message associated with OBJECT.

For example for commits this will return the commit message and
for tags the message associated with the tag.

Supported Objects

- COMMIT
- TAG
"))

(defgeneric git-author (object)
  (:documentation "Returns the author's signature of OBJECT.

A signature is a list with the keys :NAME :EMAIL and :TIME.
The :NAME and :EMAIL values are strings, and the :TIME value is LOCAL-TIME timestamp.

Example

    (cl-git:git-author *commit*) 
    ==>
    (:NAME \"Willem Rein Oudshoorn\" 
     :EMAIL \"woudshoo+github@xs4all.nl\" 
     :TIME  @2012-05-06T18:46:35.000000+02:00)

Supported Objects

- COMMIT
"))

(defgeneric git-committer (object)
  (:documentation "Returns the committer's signature of OBJECT.

A signature is a list with the keys :NAME :EMAIL and :TIME.
The :NAME and :EMAIL values are strings, and the :TIME value is LOCAL-TIME timestamp.

Supported Objects

- COMMIT
"))


(defgeneric git-parentcount (object)
  (:documentation "Returns the number of parents of OBJECT.

For Commits this indicate the number of parent commits.  So it is 1 for normal commits, > 1 for merges and 0 for initial commits.

Supported Objects

- COMMIT"))

(defgeneric git-parent-oid (object index)
  (:documentation
  "Returns the oid of the parent with index INDEX in the list of
parents of the object OBJECT.

The index is zero based and has to be less than (GIT-PARENTCOUNT OBJECT).

Supported Objects

- COMMIT"))

(defgeneric git-parent-oids (object)
  (:documentation "Returns a list of oids identifying the parent commits of OBJECT.

This method is a wrapper that collects all oids returned by GIT-PARENT-OID. 
So as such the meaning and applicability of this method is the same a s
GIT-PARENT-OID 

Supported Objects

- COMMIT"))

(defgeneric git-tree (object)
  (:documentation
  "Returns the tree as a git tree object, for object OBJECT."))

(defgeneric git-create (class id/name &key repository &allow-other-keys))

(defgeneric git-lookup (class id/name &key repository &allow-other-keys)) ;; Documentation is copied in.

(defgeneric git-list (class &key repository &allow-other-keys))

(defgeneric git-open (class path/name &key &allow-other-keys))

(defgeneric git-init (class path/name &key &allow-other-keys))

(defgeneric git-add (path &key &allow-other-keys)
  (:documentation
   "Adds the PATH to the current index (*GIT-REPOSITORY-INDEX*) or the explicit keyword argument :INDEX"))

(defgeneric git-clear (object)
  (:documentation
   "Clears the content of OBJECT

Supported Objects

- TREE
"))

(defgeneric git-write (object)
  (:documentation 
   "Writes the OBJECT to its store.  

Supported Objects

- TREE"))

(defgeneric git-name (object)
  (:documentation "Returns the name of OBJECT, as a string.

What exactly the name is depends on the type of the object.

- REFERENCE -- The name of the of the reference, e.g.: \"refs/heads/master\"
- TAG       -- The name of the tag, e.g.: \"v0.17\"

Supported Objects

- REFERENCE
- TAG"))

(defgeneric git-tagger (object)
  (:documentation "Returns the signature of the tagger of OBJECT.

The return value is a signature (a property list with keys :NAME, :EMAIL and :TIME

Supported Objects

- TAG"))


(defgeneric git-type (object)
  (:documentation "Returns the type of OBJECT.

What exactly is returned depends on the class of OBJECT.

- REFERENCE -- returns either :SYMBOLIC or :OID
- TAG -- ???
- OBJECT (and subclasses) -- Type e.g. :COMMIT, :TREE, :REFERENCE

Note that although REFERENCE is a subclass of OBJECT it will not
return :REFERENCE, but the more specific type.

Supported Objects

- OBJECT
- TAG
- REFERENCE"))

(defgeneric git-target (object)
  (:documentation "Returns the target of OBJECT.

- TAG -- only works for :OID tags.

Supported Objects

- TAG"))


(defgeneric git-peel (object)
  (:documentation "Returns the final target of OBJECT.

This is to follow symbolic tag chains to find the object pointed to.

Supported Objects

- TAG"))

(defgeneric git-entry-count (object)
  (:documentation "Returns the number elements in the collection OBJECT.

Supported Objects

- TREE
"))

(defgeneric git-entry-by-index (object index)
  (:documentation "Returns the element at position INDEX from the collection OBJECT.

Supported Objects

- TREE"))

(defgeneric git-entries (object)
  (:documentation "Returns the elements of the collection OBJECT as a list.

Supported Objects

- The same as GIT-ENTRY-BY-INDEX and GIT-ENTRY-COUNT.
"))

(defgeneric git-values (object)
  (:documentation "TODO"))

(defgeneric git-free (object)
  (:documentation "TODO"))


(defgeneric git-config (object))

(defgeneric git-index (object)
  (:documentation "Returns an index object for OBJECT (a repository)"))

(defgeneric git-next (walker)
  (:documentation "Returns the next object for the walker. 
If no objects are available anymore return nil."))



;;; how it is now:











