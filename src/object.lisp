;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcenum git-object-type
  (:any -2)       ; Object can be any of the following
  (:bad -1)       ; Object is invalid.
  (:commit 1)     ; A commit object.
  (:tree 2)       ; A tree (directory listing) object.
  (:blob 3)       ; A file revision object.
  (:tag 4)        ; An annotated tag object.
  (:ofs-delta 6)  ; A delta, base is given by an offset.
  (:ref-delta 7)) ; A delta, base is given by object id.

(defcfun ("git_object_id" git-object-id)
    %oid
  "Returns the oid identifying OBJECT"
  (object %object))

(defcfun ("git_object_type" git-object-type)
    git-object-type
  "Returns the type of the git object."
  (object %object))

(defcfun ("git_object_lookup" %git-object-lookup)
    %return-value
  (object %object)
  (repo %repository)
  (oid %oid)
  (type git-object-type))

(defcfun ("git_object_free" git-object-free)
    :void
  "Free the git object."
  (object :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign Type Translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod translate-to-foreign (value (type git-object))
  (if (pointerp value)
      value
      (pointer value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass object (git-pointer)
  ((type :accessor object-type :initarg :object-type :initform 'object
     :documentation "A symbol indicating which libgit2 type this is.
This slot is probably uselss in the sense that we do not necessarily know
on creation time and if we do not know exactly what is the point?
So this is mainly used for printing"))
  (:documentation "Object encapsulating git objects from libgit2"))

(defun make-instance-object (&key pointer facilitator type)
  "Creates an object wrapping OBJECT-PTR.
OBJECT-PTR needs to point to one of the git storage types, such as:
:commit :tag :tree or :blob.  This function is not suitable to
wrap git pointers to repositories, config, index etc."

  (let ((obj-type (case (or (unless (eq type :any) type)
                (git-object-type pointer))
            (:commit 'commit)
            (:tag 'tag)
            (:tree 'tree)
            (:blob 'blob)
            (:config 'config)
            (t 'object))))

    (make-instance obj-type
           :pointer pointer
           :facilitator facilitator
           :object-type obj-type
           :free-function #'git-object-free)))

(defmethod dispose ((object object))
  "Do the normal free and dispose children, but also clear reference to facilitator."
  (call-next-method object)
  (setf (facilitator object) nil))

(defun git-object-lookup (oid type &key (repository *git-repository*))
  "Returns a git object which is identified by the OID.
The type argument specifies which type is expected.  If the found
object is not of the right type, an error will be signaled.  The type
is one of :ANY, :BAD, :COMMIT :TREE :BLOB :TAG :OFS-DELTA :REFS-DELTA.
:ANY and :BAD are special cases.
:ANY means return the object found, regardless of type.  Also :ANY is
not a type of any real object, but only used for querying like in this function.
:BAD should never occur, it indicates an error in the data store."

  (assert (not (null-or-nullpointer repository)))

  (with-foreign-object (obj-ptr :pointer)
    (%git-object-lookup obj-ptr repository oid type)
    (make-instance-object :pointer (mem-ref obj-ptr :pointer)
              :facilitator repository
              :type type)))



;; Copy the documentation to the generic function so
;; we do not have to write it twice.
#|
(setf (documentation #'git-lookup 'function)
      (documentation #'git-object-lookup 'function))

(setf (documentation #'git-type 'function)
      (documentation #'git-object-type 'function))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some generic functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod git-parent-oids (object)
  "Returns a list of oids identifying the parents of OBJECT."
  (loop
    :for index :from 0 :below (git-parentcount object)
    :collect (git-parent-oid object index)))

(defmethod git-id ((object object))
  (git-object-id object))


(defmethod git-lookup ((class (eql :object))
               oid &key (repository *git-repository*))
  (git-object-lookup oid :any :repository repository))

(defmethod git-type ((object object))
  (git-object-type object))

(defmethod git-entries (object &key (start 0) end)
  "Return all entries of OBJECT as a list.

Note that this is basically a wrapper around GIT-ENTRY-BY-INDEX,
so the objects returned are the same as the ones returned by GIT-ENTRY-BY-INDEX.

The START and END keyword have their usual meaning, all entries whose index
satisfies

     START <= INDEX < END

are returned.  If END is not specified or nil, the check on END is omitted.
Also START defauts to 0."
  (loop
        :for index :from start :below (or end (git-entry-count object))
        :collect (git-entry-by-index object index)))
