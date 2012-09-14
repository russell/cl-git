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
  (:oid 1)
  (:symbolic 2)
  (:packed 4)
  (:has-peel 8))

(defcfun ("git_reference_list" %git-reference-list)
    %return-value
  (strings :pointer)
  (repository %repository)
  (flags git-reference-flags))

(defcfun ("git_reference_oid" git-reference-oid)
    %oid
  "Return the oid from within the reference."
  (reference %reference))

(defcfun ("git_reference_name" %git-reference-name)
    :string
  (reference %reference))

(defcfun ("git_reference_lookup" %git-reference-lookup)
    %return-value
  (reference :pointer)
  (repository %repository)
  (name :string))

(defcfun ("git_reference_resolve" %git-reference-resolve)
    %return-value
  (resolved-ref :pointer)
  (reference %reference))

(defcfun ("git_reference_create_oid" %git-reference-create-oid)
    %return-value
  (reference :pointer)
  (repository %repository)
  (name :string)
  (oid %oid)
  (force (:boolean :int)))

(defcfun ("git_reference_create_symbolic" %git-reference-create-symbolic)
    %return-value
  (reference :pointer)
  (repository %repository)
  (name :string)
  (target :string)
  (force (:boolean :int)))

(defcfun ("git_reference_free" %git-reference-free)
    :void
  (reference %reference))

(defcfun ("git_reference_type" git-reference-type)
    git-reference-flags
  (reference %reference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass reference (git-pointer) ())

(defmethod git-lookup ((class (eql :reference)) name 
		       &key (repository *git-repository*))
  "Find a reference by its full name e.g.: ref/heads/master
Note that this function name clashes with the generic lookup function.
We need to figure this out by using the type argument to do dispatch."
  (assert (not (null-or-nullpointer repository)))
  (with-foreign-object (reference :pointer)
    (%git-reference-lookup reference repository name)
    (make-instance 'reference
		   :pointer (mem-ref reference :pointer)
		   :facilitator repository
		   :free-function #'%git-reference-free)))

(defun git-resolve (reference)
  "If the reference is symbolic, follow the it until it finds a non
symbolic reference.  The result should be freed independently from the
argument."
  (with-foreign-object (resolved-ref :pointer)
    (%git-reference-resolve resolved-ref reference)
    (make-instance 'reference
		   :pointer (mem-ref resolved-ref :pointer)
		   :facilitator (facilitator reference)
		   :free-function #'%git-reference-free)))


(defmethod git-list ((class (eql :reference))
		     &key (repository *git-repository*) (flags '(:oid)))
  "List all the refs, filter by FLAGS.  The flag options
are :INVALID, :OID, :SYMBOLIC, :PACKED or :HAS-PEEL"

  (assert (not (null-or-nullpointer repository)))

  (with-foreign-object (string-array 'git-strings)
    (%git-reference-list string-array repository flags)
    (with-foreign-slots ((strings count) string-array git-strings)
      (let ((refs
	     (loop
		:for i :below count
		:collect (foreign-string-to-lisp
			  (mem-aref strings :pointer i)))))
	(%git-strarray-free string-array)
	refs))))


(defmethod git-create ((class (eql :reference)) name 
		       &key 
			 (repository *git-repository*)
			 (type :oid)
			 force
			 target)
  "Create a reference to TARGET.
The type of reference depends on TYPE.  If TYPE is :OID the value of
TARGET should be an OID and a direct reference is created.  If TYPE
is :SYMBOLIC, a symbolic reference is created and TARGET should be a
string.

If FORCE is t the reference will be created, even if a reference with
the same name already exists.  If FORCE is nil, it will return an
error if that is the case."
  (with-foreign-object (reference :pointer)
    (ecase type
      (:oid 
       (%git-reference-create-oid reference repository name target force))
      (:symbolic 
       (%git-reference-create-symbolic reference repository name target force)))
    (make-instance 'reference
		   :pointer (mem-ref reference :pointer)
		   :facilitator repository
		   :free-function #'%git-reference-free)))



(defun find-oid (name &key (flags :both)
			(repository *git-repository*))
  "Find a head or sha that matches the NAME. Possible flags
are :SHA, :HEAD or :BOTH"
  (flet ((and-both (flag other-flag)
           (find flag (list :both other-flag))))
  (acond
    ((and (and-both flags :head)
          (remove-if-not (lambda (ref) (equal ref name)) 
			 (git-list 'reference :repository repository)))
     (lookup-oid :head (car it) :repository repository))
    ((and (and-both flags :sha)
          (find (length name) '(40 7))
          (not (loop :for char :across name
                     :when (not (find char "1234567890abcdef"))
                       :collect char)))
     (lookup-oid :sha name :repository repository))
    (t (error "Invalid reference named ~A." name)))))

(defun find-oids (name-or-names &key (flags :both) 
				  (repository *git-repository*))
  "Find a head or sha that matches the NAME. Possible flags
are :SHA, :HEAD or :BOTH"
  (if (stringp name-or-names)
      (find-oid name-or-names :flags flags :repository repository)
      (loop :for name :in name-or-names
            :collect (find-oid name :flags flags :repository repository))))

(defmethod git-type ((object reference))
  "TODO"
  (git-reference-type object))

(defmethod git-name ((object reference))
  (%git-reference-name object))
