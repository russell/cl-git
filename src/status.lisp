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

;;; Git Status
(defbitfield git-status-flags
  (:index-new        1)
  (:index-modified   2)
  (:index-deleted    4)
  (:worktree-new     8)
  (:worktree-modified 16)
  (:worktree-deleted  32)
  (:ignored           64))

(defcfun ("git_status_foreach" %git-status-for-each)
    :int
  (repository :pointer)
  (callback :pointer)
  (payload :pointer))

(defparameter *status-values* nil)

(defcallback collect-status-values :int ((path :string) (value git-status-flags) (payload :pointer))
  (declare (ignore payload))
  (push (cons path value) *status-values*)
  0)

(defun git-status ()
  (assert (not (null-or-nullpointer *git-repository*)))
  (let ((*status-values* (list)))
    (handle-git-return-code
     (%git-status-for-each *git-repository*
			   (callback collect-status-values)
			   (null-pointer)))
    *status-values*))
