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

(defparameter *status-values* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("git_status_file" %git-status-file)
    git-status-t*
  (repository %repository)
  (path :string))


(defcfun ("git_status_foreach" %git-status-for-each)
    %return-value
  (repository %repository)
  (callback :pointer)
  (payload :pointer))


(defcallback collect-status-values :int ((path :string) (value git-status-t*) (payload :pointer))
  (declare (ignore payload))
  (push (cons path value) *status-values*)
  0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun repository-status (repository)
  "Return the current status values for each of the object in the
repository.  For each element of the list the FIRST is the name of the
file and the CDR is a list of keywords that containing the current
state of the file.  Possible states are: :CURRENT :INDEX-NEW
:INDEX-MODIFIED :INDEX-DELETED :INDEX-RENAMED :INDEX-TYPECHANGE
:WORKTREE-NEW :WORKTREE-MODIFIED :WORKTREE-DELETED
:WORKTREE-TYPECHANGE :WORKTREE-RENAMED :WORKTREE-UNREADABLE :IGNORED
or :CONFLICTED"
  (assert (not (null-or-nullpointer repository)))
  (let ((*status-values* (list)))
    (%git-status-for-each repository
                          (callback collect-status-values)
                          (null-pointer))
    *status-values*))
