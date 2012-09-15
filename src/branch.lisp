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

(defcfun ("git_branch_tracking" %git-branch-tracking)
    %return-value
  (ref-out :pointer)
  (ref-in %reference))

(defcallback collect-branch-values :int ((branch-name :string) (type git-branch-type-flags) (payload :pointer))
  (declare (ignore payload))
  (push (cons branch-name type) *branch-values*)
  0)

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
  (with-foreign-object (reference :pointer)
    (%git-branch-lookup reference repository name type)
    (make-instance 'reference
		   :pointer (mem-ref reference :pointer)
		   :facilitator repository
		   :free-function #'%git-reference-free)))


(defmethod git-tracking ((in-ref reference))
  (with-foreign-object (out-ref :pointer)
    (%git-branch-tracking out-ref in-ref)
    (make-instance 'reference
		   :pointer (mem-ref out-ref :pointer)
		   :facilitator (facilitator in-ref)
		   :free-function #'%git-reference-free)))
