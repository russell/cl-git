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

(defparameter *branch-values* nil)

(defbitfield git-branch-type-flags
  (:local 1)
  (:remote 2))

(defcfun ("git_branch_foreach" %git-branch-for-each)
    %return-value
  (repository %repository)
  (flags git-branch-type-flags)
  (callback :pointer)
  (payload :pointer))

(defcfun ("git_branch_lookup" %git-branch-lookup)
    %return-value
  (ref :pointer)
  (repository %repository)
  (name :string)
  (type git-branch-type-flags))

(defcallback collect-branch-values :int ((branch-name :string) (type git-branch-type-flags) (payload :pointer))
  (declare (ignore payload))
  (push (cons branch-name type) *branch-values*)
  0)

(defcfun ("git_branch_is_head" git-is-head)
    %bool
  "Returns t is the current HEAD points to this branch.  
This means that this is the branch that is checked out."
  (branch %reference))

(defcfun ("git_branch_upstream" %git-branch-upstream)
    %return-value
  (out :pointer)
  (branch %reference))

(defcfun ("git_branch_upstream_name" %git-branch-upstream-name)
    %return-value
  (out :pointer)
  (size size-t)
  (repository %repository)
  (branch-name :string))


(defcfun ("git_branch_remote_name" %git-branch-remote-name)
    %return-value
  (out :pointer)
  (size size-t)
  (repository %repository)
  (branch-name :string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel interface.
;;;
;;; Branches are just a sort of reference and are returned as instances of references.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod git-list ((class (eql :branch))
             &key (repository *git-repository*)
               (type '(:local :remote)))
  "Returns a list of all branche names in the repository.  Each
branch is represented as cons pair (branch-name . type-flags)

The type flags are :LOCAL and :REMOTE.  

By default all the branches, both remote and local are returned.  
It is possible to limit the list of branches to just the local or remote by
specifying :LOCAL or :REMOTE as argument to the :TYPE parameter.
"
  (let ((*branch-values* (list)))
    (%git-branch-for-each repository
              type
              (callback collect-branch-values)
              (null-pointer))
    *branch-values*))


(defmethod git-lookup ((class (eql :branch))
               (name string)
               &key (repository *git-repository*)
             (type :local))
  "Converts a branch name to a git REFERENCE.  The optional :type argument 
specifies if the branch name names a local branch or a remote branch."
  (with-foreign-object (reference :pointer)
    (%git-branch-lookup reference repository name type)
    (make-instance 'reference
           :pointer (mem-ref reference :pointer)
           :facilitator repository
           :free-function #'%git-reference-free)))

(defmethod git-lookup ((class (eql :branch))
		       (branch-name-and-type cons)
		       &key (repository *git-repository*))
  "Converts a branch/type combination to a git REFERENCE.
The branch-name-and-type should be a cons pair like
\(branch-name branch-type\).

This is the same format as the elements returned by the 

\(git-list :branch ...\)

call.
"
  (git-lookup class (car branch-name-and-type) 
	      :repository repository 
	      :type (cdr branch-name-and-type)))


(defmethod git-upstream ((branch reference))
  "Returns the reference for the remote tracking branch, corresponding to the local
branch BRANCH."
  (with-foreign-object (reference :pointer)
    (%git-branch-upstream reference branch)
    (make-instance 'reference
		   :pointer (mem-ref reference :pointer)
		   :facilitator (facilitator branch)
		   :free-function #'%git-reference-free)))


(defmethod git-upstream-name ((branch-name string) &key (repository *git-repository*))
  (with-foreign-pointer-as-string ((out size) (%git-branch-upstream-name
					       (null-pointer) 0 
					       repository
					       branch-name))
        (%git-branch-upstream-name out size repository branch-name)))


(defmethod git-remote-name ((branch-name string) &key (repository *git-repository*))
  (with-foreign-pointer-as-string ((out size) (%git-branch-remote-name
					       (null-pointer) 0 
					       repository
					       branch-name))
        (%git-branch-remote-name out size repository branch-name)))

