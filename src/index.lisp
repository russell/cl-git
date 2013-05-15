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

(defctype struct-index-time
    (:struct git-index-time))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("git_index_new" %git-index-new)
    %return-value
  (index %index))

(defcfun ("git_index_open" %git-index-open)
    %return-value
  (index %index)
  (path :string))

(defcfun ("git_index_add" %git-index-add)
    %return-value
  (index %index)
  (entry %index-entry))

(defcfun ("git_index_add_bypath" %git-index-add-by-path)
    %return-value
  (index %index)
  (path :string))

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

(defcfun ("git_index_has_conflicts" git-index-has-conflicts)
    :boolean
  (index %index))

(defcfun ("git_index_write_tree" %git-index-write-tree)
    %return-value
  (oid :pointer)
  (index %index))

(defcfun ("git_index_entrycount" git-index-entry-count)
    :unsigned-int
  (index %index))

(defcfun ("git_index_entry_stage" git-index-entry-stage)
    :int
  "Return an int with a value from 0 to 4.  Files in the working
directory return stage 0.  Files with stages 1-3 are in conflict."
  (index-entry :pointer))

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

(defmethod translate-from-foreign (value (type index-entry-type))
  (with-foreign-slots ((ctime mtime file-size oid flags flags-extended path)
                       value
                       (:struct git-index-entry))
    (list :c-time ctime
          :m-time mtime
          :file-size file-size
          :oid oid
          :flags flags
          :flags-extended flags-extended
          :path path
          :stage (git-index-entry-stage value))))

(defmethod translate-to-foreign (value (type index-entry-type))
  (let ((index-entry (foreign-alloc '(:struct git-index-entry))))
    (with-foreign-slots ((file-size flags flags-extended path)
                         index-entry
                         (:struct git-index-entry))
      (with-foreign-slots ((seconds nanoseconds)
                           (foreign-slot-pointer index-entry '(:struct git-index-entry) 'ctime)
                           (:struct git-index-time))
        (let ((time-to-set (getf value :c-time (local-time:now))))
          (setf seconds time-to-set)
          (setf nanoseconds (local-time:nsec-of time-to-set))))

      (with-foreign-slots ((seconds nanoseconds)
                           (foreign-slot-pointer index-entry '(:struct git-index-entry) 'mtime)
                           (:struct git-index-time))
        (let ((time-to-set (getf value :m-time (local-time:now))))
          (setf seconds time-to-set)
          (setf nanoseconds (local-time:nsec-of time-to-set))))

      (setf file-size (getf value :file-size 0))
      (setf flags (getf value :flags 0))
      (oid-to-foreign (getf value :oid 0)
                      (foreign-slot-pointer
                       (foreign-slot-pointer
                        index-entry
                        '(:struct git-index-entry) 'oid)
                       '(:struct git-oid) 'id))
      (setf path (foreign-string-alloc (getf value :path "")))
      index-entry)))

(defmethod translate-from-foreign (value (type index-time-struct-type))
  (with-foreign-slots ((seconds nanoseconds) value (:struct git-index-time))
    (local-time:timestamp+ seconds nanoseconds :nsec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass index
    (git-pointer)
  ()
  (:documentation "A git index"))


(defmethod git-add ((path string) &key (index *git-repository-index*))
  (%git-index-add-by-path index path))

(defmethod git-add ((path pathname) &key (index *git-repository-index*))
  (let ((path (if (pathname-relative-p path)
                   path
                   (enough-namestring path (git-workdir (slot-value index 'facilitator))))))
    (git-add (namestring path) :index index)))

(defmethod git-add ((entry list) &key (index *git-repository-index*))
  (%git-index-add index entry))

(defmethod git-clear ((index index))
  "Clear contents of the index removing all entries.  Changes need to
be written back to disk to take effect."
  (%git-index-clear index))

(defmethod git-read ((index index))
  "Update the index with objects read from disk."
  (%git-index-read index))

(defmethod git-write ((index index))
  "Write the git index back to the file system."
  (%git-index-write index))

(defmacro with-index ((var &optional repository-or-path) &body body)
  "Load a repository index uses the current *GIT-REPOSITORY* as the
current repository and sets *GIT-REPOSITORY-INDEX* as the newly opened
index."
  `(let ((,var ,(if repository-or-path
                    `(git-index ,repository-or-path)
                    `(git-index-new))))
     (unwind-protect
          (progn ,@body)
       (git-free ,var))))

(defmacro with-repository-index (&body body)
  "Load a repository index uses the current *GIT-REPOSITORY* as the
current repository and sets *GIT-REPOSITORY-INDEX* as the newly opened
index."
  `(let ((*git-repository-index* (git-index *git-repository*)))
     (unwind-protect
      (progn ,@body)
       (git-free *git-repository-index*))))

(defun git-index-new ()
  "Create a new in-memory index that can be used to perform in memory
operations that may not be written back to the disk."
  (with-foreign-object (index :pointer)
    (%git-index-new index)
    (make-instance 'index
           :pointer (mem-ref index :pointer)
           :free-function #'%git-index-free)))

(defmethod git-index ((path string))
  "Open a new index in a file."
  (with-foreign-object (index :pointer)
    (%git-index-open index path)
    (make-instance 'index
           :pointer (mem-ref index :pointer)
           :free-function #'%git-index-free)))

(defmethod git-index ((path pathname))
  "Open a new index in a file."
  (git-index (namestring path)))

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
