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

(defparameter *status-values* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbitfield git-status-flags
  (:current 0)
  (:index-new 1)
  (:index-modified 2)
  (:index-deleted 4)
  (:index-renamed 8)
  (:index-typechange 16)
  (:worktree-new 128)
  (:worktree-modified 256)
  (:worktree-deleted 512)
  (:worktree-typechange 1024)
  (:ignored 16384))

(defcfun ("git_status_foreach" %git-status-for-each)
    %return-value
  (repository %repository)
  (callback :pointer)
  (payload :pointer))


(defcallback collect-status-values :int ((path :string) (value git-status-flags) (payload :pointer))
  (declare (ignore payload))
  (push (cons path value) *status-values*)
  0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun git-status (&key (repository *git-repository*))
  "Return the current status values for each of the object in the
repository.  For each element of the list the FIRST is the name of the
file and the CDR is a list of keywords that containing the current
state of the file.  Possible states are: :CURRENT :INDEX-NEW :INDEX-MODIFIED
:INDEX-DELETED :INDEX-RENAMED :INDEX-TYPECHANGE :WORKTREE-NEW :WORKTREE-MODIFIED
:WORKTREE-DELETED :WORKTREE-TYPECHANGE or :IGNORED"
  (let ((*status-values* (list)))
    (%git-status-for-each repository
                          (callback collect-status-values)
                          (null-pointer))
    *status-values*))
