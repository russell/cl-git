;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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


(defclass pathname-mixin ()
  ((filename :reader filename :initarg :filename :initform nil))
  (:documentation "A mixin that has a filename attribute."))

(defvar *oid-abbrev* 7)

(defvar *diff-options-version* 1)
(defvar *diff-context-lines* 3)
(defvar *diff-interhunk-lines* 0)
(defvar *diff-old-prefix* "a")
(defvar *diff-new-prefix* "b")
(defvar *diff-max-size* (* 512 1024 1024))


(defgeneric oid (object)
  (:documentation "Return the identifier of OBJECT.
The identifier is typically the SHA-1 checksum or hash code.

Note that this is an integer, and not the string you typically see reported by git.

To get the string representation use format like this:

    (format nil \"~40,'0X\" (oid object))

or if you want lowercase hexadecimal digits:

    (format nil \"~(~40,'0X~)\" (oid object))

"))

(defgeneric message (object)
  (:documentation "Return the message associated with OBJECT.

For example for commits this will return the commit message and for
tags the message associated with the tag.

"))

(defgeneric author (object)
  (:documentation "Returns the author's signature of OBJECT.

A signature is a list with the keys :NAME :EMAIL and :TIME.  The :NAME
and :EMAIL values are strings, and the :TIME value is LOCAL-TIME
timestamp.

Example

    GIT> (author *commit*)
    (:NAME \"Willem Rein Oudshoorn\"
     :EMAIL \"woudshoo+github@xs4all.nl\"
     :TIME @2012-05-05T05:05:08.000000+10:00
     :TIMEZONE #<LOCAL-TIME::TIMEZONE +0200>)
"))

(defgeneric committer (object)
  (:documentation "Returns the committer's signature of OBJECT.

A signature is a list with the keys :NAME :EMAIL and :TIME.  The :NAME
and :EMAIL values are strings, and the :TIME value is LOCAL-TIME
timestamp."))

(defgeneric parent-count (object)
  (:documentation "Returns the number of parents of OBJECT.

For Commits this indicate the number of parent commits.  So it is 1
for normal commits, > 1 for merges and 0 for initial commits."))

(defgeneric make-object (class id/name repository &key &allow-other-keys))

(defgeneric get-object (class id/name repository)
  (:documentation
   "Return an object of type CLASS from the object database.  The
lookup will use either an oid or a name to find the object."))

(defgeneric list-objects (class repository &key test test-not))

(defgeneric open-repository (path/name)
  (:documentation
   "Open an existing repository located at PATH/NAME. The repository
object will be garbage collected.  If it's freed explicitly then all
related objects will have undefined behaviour."))

(defgeneric init-repository (path/name &key bare)
  (:documentation
   "Create a new Git repository.  CLASS should be the
keyword :REPOSITORY.  PATH/NAME can be either an instance of a STRING
or a PATHNAME.  A truthful value for the key BARE will init a
repository that does not have a local checkout, it's normally
appropriate for the basename of the path to end in '.git'.  A
REPOSITORY instance is returned."))

(defgeneric repository-path (object)
  (:documentation "Return the path to the repository.  In the case
where the repository isn't bare then it will be the location of the
.git directory."))

(defgeneric repository-workdir (object)
  (:documentation "Return the path to the root of the repository."))

(defgeneric full-name (object)
  (:documentation "Returns the name of OBJECT, as a string.

What exactly the name is depends on the type of the object.

- REFERENCE -- The name of the of the reference, e.g.: \"refs/heads/master\"
- TAG       -- The name of the tag, e.g.: \"refs/tags/v0.17\"
- OBJECT    -- The string representation of the oid, e.g. \"a742eb9f5290476daf54afb5d28429710b81e3f3\"
"))

(defgeneric short-name (object)
  (:documentation "Returns the short name of OBJECT, as a string.

What exactly the name is depends on the type of the object.

- REFERENCE -- The name of the of the reference, e.g.: \"master\"
- TAG       -- The name of the tag, e.g.: \"v0.17\"
- OBJECT    -- The string representation of the oid, e.g. \"a742eb9\"
"))

(defgeneric target (object)
  (:documentation "Returns the target of OBJECT."))

(defgeneric resolve (object &optional stop-at))

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

(defgeneric free (object)
  (:documentation "TODO"))


(defgeneric git-config (object &key level)
  (:documentation "Open a git config.  LEVEL can be used to limit the
git config to a specific level.  Possible levels are :HIGHEST-LEVEL
:SYSTEM :XDG :GLOBAL or :LOCAL"))

(defgeneric open-index (object)
  (:documentation "Returns an index object for OBJECT (a repository)"))

(defgeneric open-odb (path-or-repository)
  (:documentation "Open the ODB at the specified path or
repository."))
