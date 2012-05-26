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
    :int
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
    :int
  (index :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro with-git-repository-index (&body body)
  "Load a repository index uses the current *GIT-REPOSITORY* as the
current repository and sets *GIT-REPOSITORY-INDEX* as the newly opened
index."
  `(let ((*git-repository-index* (null-pointer)))
     (unwind-protect
          (progn
            (assert (not (null-or-nullpointer *git-repository*)))
            (let ((index (foreign-alloc :pointer)))
              (handle-git-return-code (foreign-funcall
                                       "git_repository_index"
                                       :pointer index
                                       git-repository *git-repository*
                                       :int))
              (setf *git-repository-index* (mem-ref index :pointer))
              (foreign-free index))
            ,@body)
       (progn
         (%git-index-free *git-repository-index*)))))

(defun git-index-add (path)
  "Add a file at PATH to the repository, the PATH should be relative
to the repository."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (let ((path (namestring path)))
    (with-foreign-string (path-str path)
      (handle-git-return-code
       (%git-index-add *git-repository-index* path-str 0)))))

(defun git-index-clear ()
  "Remove all staged data from the index at *GIT-REPOSITORY-INDEX*."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (handle-git-return-code
   (%git-index-clear *git-repository-index*)))

(defun git-index-write ()
  "Write the current index stored in *GIT-REPOSITORY-INDEX* to disk."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (handle-git-return-code
   (%git-index-write *git-repository-index*)))

(defun git-oid-from-index ()
  "Write the current index to the disk and return an oid to it."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (with-foreign-object (oid 'git-oid)
    (handle-git-return-code
     (%git-tree-create-fromindex oid *git-repository-index*))
    (convert-from-foreign oid '%oid)))
