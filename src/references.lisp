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


(defbitfield git-reference-flags
  (:invalid 0)
  (:oid 1)
  (:symbolic 2)
  (:packed 4)
  (:has-peel 8))

(defcfun ("git_reference_list" %git-reference-list)
    :int
  (strings :pointer)
  (repository :pointer)
  (flags git-reference-flags))

(defcfun ("git_reference_oid" git-reference-oid)
    %oid
  "Return the oid from within the reference."
  (reference :pointer))

(defcfun ("git_reference_lookup" %git-reference-lookup)
    :int
  (reference :pointer)
  (repository :pointer)
  (name :string))

(defcfun ("git_reference_resolve" %git-reference-resolve)
    :int
  (resolved-ref :pointer)
  (reference :pointer))

(defcfun ("git_reference_create_oid" %git-reference-create-oid)
    :int
  (reference :pointer)
  (repository :pointer)
  (name :string)
  (oid %oid)
  (force (:boolean :int)))

(defcfun ("git_reference_free" %git-reference-free)
    :void
  (reference :pointer))

(defcfun ("git_reference_type" %git-reference-type)
    git-reference-flags
  (reference :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun git-reference-lookup (name)
  "Find a reference by its full name e.g.: ref/heads/master"
  (assert (not (null-or-nullpointer *git-repository*)))
  (with-foreign-object (reference :pointer)
    (handle-git-return-code
     (%git-reference-lookup reference *git-repository* name))
    (mem-ref reference :pointer)))

(defun git-reference-resolve (reference)
  "If the reference is symbolic, follow the it until it finds a non
symbolic reference.  The result should be freed independently from the
argument."
  (with-foreign-object (resolved-ref :pointer)
    (handle-git-return-code
     (%git-reference-resolve resolved-ref reference))
    (mem-ref resolved-ref :pointer)))

(defun git-reference-listall (&rest flags)
  "List all the refs, filter by FLAGS.  The flag options
are :INVALID, :OID, :SYMBOLIC, :PACKED or :HAS-PEEL"

  (assert (not (null-or-nullpointer *git-repository*)))

  (let ((git-flags (if flags flags '(:oid))))
    (with-foreign-object (string-array 'git-strings)
      (handle-git-return-code (%git-reference-list
                               string-array *git-repository*
                               git-flags))
      (with-foreign-slots ((strings count) string-array git-strings)
        (let ((refs
                (loop
                  :for i :below count
                  :collect (foreign-string-to-lisp
                            (mem-aref strings :pointer i)))))
          (%git-strarray-free string-array)
          refs)))))

(defun git-reference-create (name &key sha head force)
  "Create new reference in the current repository with NAME linking to
SHA or HEAD.  If FORCE is true then override if it already exists."

  (assert (not (null-or-nullpointer *git-repository*)))

  (let ((oid (lookup-oid :sha sha :head head)))
    (with-foreign-object (reference :pointer)
      (unwind-protect
           (handle-git-return-code
            (%git-reference-create-oid
             reference *git-repository*
             name oid force))
        (progn
          (%git-reference-free (mem-ref reference :pointer))))))
  name)
