;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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

(define-foreign-type git-object (git-pointer)
  ((libgit2-oid :initarg :oid
                :initform nil)
   (libgit2-name :initarg :name
                 :initform nil)
   (libgit2-disposed :initform nil))
  (:actual-type :pointer)
  (:simple-parser %object)
  (:documentation "Class wrapping a pointer, handles finalization and
  freeing of the underlying object"))

(defvar object-type-mapping
  (list 'commit :commit
        'tree :tree
        'blob :blob
        'tag :tag))

(defmethod pointer ((object git-object))
  "Try and resolve the pointer if it isn't set."
  (when (and (null-pointer-p (slot-value object 'libgit2-pointer))
             (not (slot-value object 'libgit2-disposed)))
    (cond
      ;; Resolve by oid
      ((slot-value object 'libgit2-oid)
       (setf (slot-value object 'libgit2-pointer)
             (git-object-lookup-ptr (slot-value object 'libgit2-oid)
                                    (getf object-type-mapping (type-of object))
                                    (facilitator object)))
       (enable-garbage-collection object)
       (setf (slot-value object 'libgit2-oid) nil))
      ;; Resolve by name
      ((slot-value object 'libgit2-name)
       (setf (slot-value object 'libgit2-pointer)
             (%git-lookup-by-name (type-of object)
                                  (slot-value object 'libgit2-name)
                                  (facilitator object)))
       (enable-garbage-collection object)
       (setf (slot-value object 'libgit2-name) nil))
      (t (error "Unable to lookup object in git repository."))))
  (slot-value object 'libgit2-pointer))


(defmethod initialize-instance :after ((instance git-object) &rest r)
  "Setup the finalizer to call internal-dispose with the right arguments."
  (when (getf r :pointer)
    (enable-garbage-collection instance)))


(defmethod print-object ((object git-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((not (null-pointer-p (slot-value object 'libgit2-pointer)))
       (format stream "~X" (pointer-address (pointer object))))
      ((or (slot-value object 'libgit2-oid) (slot-value object 'libgit2-name))
       (princ "(weak)" stream))
      ((slot-value object 'libgit2-disposed)
       (princ "(disposed)" stream)))))


(defmethod translate-to-foreign (value (type git-object))
  (if (pointerp value)
      value
      (pointer value)))


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
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-instance-object (&key pointer facilitator type)
  "Creates an object wrapping OBJECT-PTR.
OBJECT-PTR needs to point to one of the git storage types, such as:
:commit :tag :tree or :blob.  This function is not suitable to
wrap git pointers to repositories, config, index etc."
  (let ((obj-type
          (if (eq type :any)
              (case (git-object-type pointer)
                (:commit 'commit)
                (:tag 'tag)
                (:tree 'tree)
                (:blob 'blob)
                (:config 'config)
                (t type))
              type)))

    (make-instance obj-type
                   :pointer pointer
                   :facilitator facilitator
                   :free-function #'git-object-free)))

(defmethod dispose ((object git-object))
  "Dispose of the object and association to the facilitator.  Mark
the pointer as disposed."
  (setf (facilitator object) nil)
  (when (car (finalizer-data object))
    (internal-dispose (finalizer-data object)
                      (pointer object)
                      (free-function object))
    (setf (slot-value object 'libgit2-pointer) nil)
    (setf (slot-value object 'libgit2-disposed) t)))


(defun git-object-lookup-ptr (oid type repository)
  (assert (not (null-or-nullpointer repository)))
  (let ((type
          (case type
            (commit :commit)
            (tag :tag)
            (tree :tree)
            (blob :blob)
            (tree-tree :tree)
            (tree-blob :blob)
            (config :config)
            (t type))))
    (with-foreign-object (obj-ptr :pointer)
      (%git-object-lookup obj-ptr repository oid type)
      (mem-ref obj-ptr :pointer))))

(defun git-object-lookup (oid type repository)
  "Returns a git object which is identified by the OID.
The type argument specifies which type is expected.  If the found
object is not of the right type, an error will be signalled."
  (assert (not (null-or-nullpointer repository)))
  (make-instance-object :pointer (git-object-lookup-ptr oid type repository)
                        :facilitator repository
                        :type type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some generic functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod oid ((object git-object))
  (git-object-id object))

(defmethod full-name ((object git-object))
  (format nil "~x" (oid object)))

(defmethod short-name ((object git-object))
  (subseq (format nil "~x" (oid object)) 0 7))

(defmethod print-object ((object git-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((and (slot-value object 'libgit2-pointer)
            (not (null-pointer-p (slot-value object 'libgit2-pointer))))
       (format stream "~a" (full-name object)))
      ((or (slot-value object 'libgit2-oid) (slot-value object 'libgit2-name))
       (format stream "~a (weak)" (full-name object)))
      ((slot-value object 'libgit2-disposed)
       (format stream "(disposed)")))))

(defmethod get-object ((class (eql 'object)) oid repository)
  (git-object-lookup oid :any repository))

(defmethod object-type ((object git-object))
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
