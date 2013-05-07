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

#+nil (defbitfield (index-entry-flag :unsigned-short)
  (:update #. (ash 1 0))
  :remove
  :uptodate
  :added
  :hashed
  :unhashed
  :worktree-remove
  :conflicted
  :unpacked
  :new-skip-worktree
  (:intent-to-add #.(ash 1 13))
  :skip-worktree
  :extended-2)


(defcstruct (git-index-time :class index-time-struct-type)
  (seconds %time)
  (nanoseconds :unsigned-int))

(defctype struct-index-time (:struct git-index-time))

(defcstruct git-index-entry
  (ctime (:struct git-index-time))
  (mtime (:struct git-index-time))
  (dev :unsigned-int)
  (ino :unsigned-int)
  (mode :unsigned-int)
  (uid :unsigned-int)
  (gid :unsigned-int)
  (file-size off-t)
  (oid (:struct git-oid))
  (flags :unsigned-short)
  (flags-extended :unsigned-short)
  (path :string))


(defmethod translate-from-foreign (value (type index-entry-type))
  (with-foreign-slots ((ctime mtime file-size oid flags flags-extended path) value (:struct git-index-entry))
    (list :c-time ctime
	  :m-time mtime
	  :file-size file-size
	  :oid oid
	  :flags flags
	  :flags-extended flags-extended
	  :path path)))

(defmethod translate-from-foreign (value (type index-time-struct-type))
  (with-foreign-slots ((seconds nanoseconds) value (:struct git-index-time))
    (local-time:timestamp+ seconds nanoseconds :nsec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("git_index_add_bypath" %git-index-add-by-path)
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

(defcfun ("git_index_read" %git-index-read)
    %return-value
  (index %index))

(defcfun ("git_index_write_tree" %git-index-write-tree)
    %return-value
  (oid :pointer)
  (index %index))

(defcfun ("git_index_entrycount" git-index-entry-count)
    :unsigned-int
  (index %index))

(defcfun ("git_index_get_byindex" git-index-get-by-index)
    %index-entry
  (index %index)
  (position :unsigned-int))

(defcfun ("git_index_get_bypath" git-index-get-by-path)
    %index-entry
  (index %index)
  (path :string)
  (stage :int))


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
  (%git-index-add-by-path index path stage))

(defmethod git-add ((path pathname) &key (index *git-repository-index*) (stage 0))
  (let ((path (if (pathname-relative-p path)
                   path
                   (enough-namestring path (git-workdir (slot-value index 'facilitator))))))
    (git-add (namestring path) :index index :stage stage)))

(defmethod git-clear ((index index))
  (%git-index-clear index))

(defmethod git-read ((index index))
  (%git-index-read index))

(defmethod git-write ((index index))
  (%git-index-write index))

(defmacro with-repository-index (&body body)
  "Load a repository index uses the current *GIT-REPOSITORY* as the
current repository and sets *GIT-REPOSITORY-INDEX* as the newly opened
index."
  `(let ((*git-repository-index* (git-index *git-repository*)))
     (unwind-protect
      (progn ,@body)
       (git-free *git-repository-index*))))


(defmethod git-entry-count ((index index))
  (git-index-entry-count index))

(defmethod git-entry-by-index ((index index) position)
  (git-index-get-by-index index position))

(defmethod git-entry-by-path ((index index) (path string))
  (git-index-get-by-path index path 1))

(defmethod git-write-tree ((index index))
  (with-foreign-object (oid-pointer '(:struct git-oid))
    (%git-index-write-tree oid-pointer index)
    (convert-from-foreign oid-pointer '%oid)))

(defmethod git-entry ((index index))
  (with-foreign-object (oid-pointer '(:struct git-oid))
    (%git-index-write-tree oid-pointer index)
    (convert-from-foreign oid-pointer '%oid)))
