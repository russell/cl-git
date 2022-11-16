;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2022 Russell Sim <russell.sim@gmail.com>
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

(defcfun %git-graph-reachable-from-any
    %return-value
  "Determine if a commit is reachable from any of a list of commits by
following parent edges."
  (repository %repository)
  (commit %oid)
  (commits :pointer)
  (size size-t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric reachable-from (repository commit commits)
  (:documentation
   "Return T if given commit is an ancestor of any of the given potential
descendant commits."))

#-(or libgit2-0.27 libgit2-0.28 libgit2-1.0 libgit2-1.1)
(defmethod reachable-from ((repository repository) (commit commit) commits)
  (let ((commit-array (foreign-alloc
                       '(:struct git-oid)
                       :count (length commits))))
    (loop :for c :in commits
          :for i :from 0
          :for ptr = (cffi:mem-aptr commit-array '(:struct git-oid) i)
          :do (oid-to-foreign
               (oid c)
               (foreign-slot-pointer ptr '(:struct git-oid) 'id)))
    ;; Returns 1 if the commit is an ancestor
    (eql
     (%git-graph-reachable-from-any repository (oid commit) commit-array
                                    (length commits))
     1)))

#+(or libgit2-0.27 libgit2-0.28 libgit2-1.0 libgit2-1.1)
(defmethod reachable-from ((repository repository) (commit commit) commits)
  (error
   "REACHABLE-FROM is implmented in libgit2 version 1.2 and greater"))
