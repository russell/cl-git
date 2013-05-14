;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
;; Copyright (C) 2012 Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


(in-package #:cl-git)


(defparameter *git-repository* nil
  "A global that stores the current Git repository.

The value is used as a default argument to functions requiring a
repository.  It is bound by (WITH-REPOSITORY ...), but can also be
bound by the user.")

(defparameter *git-repository-index* nil
  "A global that stores the current Git index.

This value is used as the default index for all index related
functions.  It is set bound by (WITH-REPOSITORY-INDEX ..)  But can
also be set by user code.")

(defgeneric git-id (object)
  (:documentation "Return the identifier of OBJECT.
The identifier is typically the SHA-1 checksum or hash code.

Note that this is an integer, and not the string you typically see reported by git.

To get the string representation use format like this:

    (format nil \"~40,'0X\" (git-id object))

or if you want lowercase hexadecimal digits:

    (format nil \"~(~40,'0X~)\" (git-id object))

"))

(defgeneric git-message (object)
  (:documentation "Return the message associated with OBJECT.

For example for commits this will return the commit message and for
tags the message associated with the tag.

"))

(defgeneric git-author (object)
  (:documentation "Returns the author's signature of OBJECT.

A signature is a list with the keys :NAME :EMAIL and :TIME.  The :NAME
and :EMAIL values are strings, and the :TIME value is LOCAL-TIME
timestamp.

Example

    (cl-git:git-author *commit*)
    ==>
    (:NAME \"Willem Rein Oudshoorn\"
     :EMAIL \"woudshoo+github@xs4all.nl\"
     :TIME  @2012-05-06T18:46:35.000000+02:00)

"))

(defgeneric git-committer (object)
  (:documentation "Returns the committer's signature of OBJECT.

A signature is a list with the keys :NAME :EMAIL and :TIME.  The :NAME
and :EMAIL values are strings, and the :TIME value is LOCAL-TIME
timestamp.

"))


(defgeneric git-parentcount (object)
  (:documentation "Returns the number of parents of OBJECT.

For Commits this indicate the number of parent commits.  So it is 1
for normal commits, > 1 for merges and 0 for initial commits.

"))

(defgeneric git-parent-oid (object index)
  (:documentation
  "Returns the oid of the parent with index INDEX in the list of
parents of the object OBJECT.

The index is zero based and has to be less than (GIT-PARENTCOUNT OBJECT).
"))

(defgeneric git-parent-oids (object)
  (:documentation "Returns a list of oids identifying the parent commits of OBJECT.

This method is a wrapper that collects all oids returned by GIT-PARENT-OID.
So as such the meaning and applicability of this method is the same a s
GIT-PARENT-OID
"))

(defgeneric git-tree (object &key path repository)
  (:documentation
  "Returns the tree as a git tree object, for object OBJECT.

The tree from for example a commit is the object that contains a
directory listing of the files in the commit with OIDs pointing to the
content of the files.  So basically the tree of a commit corresponds
to the content of the commit.  If PATH is specified then the tree
returned node will be a child node of the tree.
"))

(defgeneric git-create (class id/name &key repository &allow-other-keys))

(defgeneric git-lookup (class id/name &key repository &allow-other-keys)) ;; Documentation is copied in.

(defgeneric git-list (class &key repository &allow-other-keys))

(defgeneric git-open (class path/name &key &allow-other-keys))

(defgeneric git-init (class path/name &key &allow-other-keys)
  (:documentation
   "Create a new Git repository.  CLASS should be the
keyword :REPOSITORY.  PATH/NAME can be either an instance of a STRING
or a PATHNAME.  A truthful value for the key BARE will init a
repository that does not have a local checkout, it's normally
appropriate for the basename of the path to end in '.git'.  A
REPOSITORY instance is returned."))

(defgeneric git-load (class path/name &key &allow-other-keys))

(defgeneric git-path (object)
  (:documentation "Path to the object."))

(defgeneric git-workdir (object))

(defgeneric git-add (path &key &allow-other-keys)
  (:documentation
   "Adds the PATH to the current index *GIT-REPOSITORY-INDEX* or the
explicit keyword argument :INDEX"))

(defgeneric git-clear (object)
  (:documentation
   "Clears the content of OBJECT
"))

(defgeneric git-read (object)
  (:documentation
   "Reads the OBJECT from disk or object store."))

(defgeneric git-write (object)
  (:documentation
   "Writes the OBJECT to its store.
"))

(defgeneric git-name (object)
  (:documentation "Returns the name of OBJECT, as a string.

What exactly the name is depends on the type of the object.

- REFERENCE -- The name of the of the reference, e.g.: \"refs/heads/master\"
- TAG       -- The name of the tag, e.g.: \"v0.17\"
"))

(defgeneric git-tagger (object)
  (:documentation "Returns the signature of the tagger of OBJECT.

The return value is a signature (a property list with
keys :NAME, :EMAIL and :TIME
"))


(defgeneric git-type (object)
  (:documentation "Returns the type of OBJECT.

What exactly is returned depends on the class of OBJECT.

- REFERENCE -- returns either :SYMBOLIC or :OID
- TAG -- ???
- OBJECT (and subclasses) -- Type e.g. :COMMIT, :TREE, :REFERENCE

Note that although REFERENCE is a subclass of OBJECT it will not
return :REFERENCE, but the more specific type.
"))

(defgeneric git-target (object &key &allow-other-keys)
  (:documentation "Returns the target of OBJECT.

- TAG -- only works for :OID tags.
"))


(defgeneric git-peel (object)
  (:documentation "Returns the final target of OBJECT.

This is to follow symbolic tag chains to find the object pointed to.
"))

(defgeneric git-entry-count (object)
  (:documentation "Returns the number elements in the collection OBJECT.
"))

(defgeneric git-entry-by-index (object index)
  (:documentation "Returns the element at position INDEX from the
collection OBJECT."))

(defgeneric git-entry-by-path (object path)
  (:documentation "Returns the element at location PATH from the
collection OBJECT."))

(defgeneric git-entries (object &key start end)
  (:documentation "Returns the elements of the collection OBJECT as a
list.  The start and end keyword arguments allow to retrieve a subset
of all elements.  All entries with index satisfying

   start <= index < end

are returned.  If end is not specified or nil, no end condition exists.
start defaults to 0."))

(defgeneric git-values (object)
  (:documentation "TODO"))

(defgeneric git-free (object)
  (:documentation "TODO"))


(defgeneric git-config (object &key level)
  (:documentation "Open a git config.  LEVEL can be used to limit the
git config to a specific level.  Possible levels are :HIGHEST-LEVEL
:SYSTEM :XDG :GLOBAL or :LOCAL"))

(defgeneric git-index (object)
  (:documentation "Returns an index object for OBJECT (a repository)"))

(defgeneric git-next (walker)
  (:documentation "Returns the next object for the walker.  If no
objects are available anymore return nil."))

(defgeneric git-connect (object &key &allow-other-keys)
  (:documentation "Connects the object if applicable.

At the moment only supported for
- REMOTE"))

(defgeneric git-disconnect (object)
  (:documentation "Disconnects the object if applicable.

At the moment only supported for
- REMOTE"))

(defgeneric git-connected (object)
  (:documentation "Returns if the object is connected.

At the moment only supported for
- REMOTE"))

(defgeneric git-pushspec (remote))
(defgeneric git-fetchspec (remote))

(defgeneric git-download (remote))

(defgeneric git-ls (remote))


(defgeneric git-head (repository))

(defgeneric git-upstream (branch))
