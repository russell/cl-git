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
  (index %index)
  (path :string)
  (stage :int)) ; an int from 0 to 4

(defcfun ("git_index_clear" %git-index-clear)
    :void
  (index %index))

(defcfun ("git_index_free" %git-index-free)
    :void
  (index %index))

(defcfun ("git_index_write" %git-index-write)
    %return-value
  (index %index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass index
    (git-pointer)
  ()
  (:documentation "A git index"))


(defmethod git-add ((path string) &key (index *git-repository-index*) (stage 0))
  (%git-index-add index path stage))

(defmethod git-add ((path pathname) &key (index *git-repository-index*) (stage 0))
  (git-add (namestring path) :index index :stage stage))

#+nil (defun git-index-add (path)
  "Add a file at PATH to the repository, the PATH should be relative
to the repository."
  (let ((path (namestring path)))
    (%git-index-add *git-repository-index* path 0)))

(defmethod git-clear ((index index))
  (%git-index-clear index))

#+nil (defun git-index-clear ()
  "Remove all staged data from the index at *GIT-REPOSITORY-INDEX*."
  (%git-index-clear *git-repository-index*))

(defmethod git-write ((index index))
  (%git-index-write index))

#+nil (defun git-index-write ()
  "Write the current index stored in *GIT-REPOSITORY-INDEX* to disk."
  (%git-index-write *git-repository-index*))


(defmacro with-repository-index (&body body)
  "Load a repository index uses the current *GIT-REPOSITORY* as the
current repository and sets *GIT-REPOSITORY-INDEX* as the newly opened
index."
  `(let ((*git-repository-index* (git-index *git-repository*)))
     (unwind-protect
      (progn ,@body)
       (git-free *git-repository-index*))))
