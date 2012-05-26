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

(defparameter *git-repository-index* nil
  "A global that stores a pointer to the current Git repository index.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("git_index_add" %git-index-add)
    %return-value
  (index :pointer)
  (path :pointer)
  (stage :int)) ; an int from 0 to 4

(defcfun ("git_index_clear" %git-index-clear)
    :void
  (index :pointer))

(defcfun ("git_index_free" %git-index-free)
    :void
  (index :pointer))

(defcfun ("git_index_write" %git-index-write)
    %return-value
  (index :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-index-add (path)
  "Add a file at PATH to the repository, the PATH should be relative
to the repository."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (let ((path (namestring path)))
    (with-foreign-string (path-str path)
      (%git-index-add *git-repository-index* path-str 0))))

(defun git-index-clear ()
  "Remove all staged data from the index at *GIT-REPOSITORY-INDEX*."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (%git-index-clear *git-repository-index*))

(defun git-index-write ()
  "Write the current index stored in *GIT-REPOSITORY-INDEX* to disk."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (%git-index-write *git-repository-index*))
