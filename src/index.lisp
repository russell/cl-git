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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil (defbitfield (index-entry-flag :unsigned-short)
  (:update #.(ash 1 0))
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

(defvar git-index-entry-namemask #x0fff)
(defvar git-index-entry-stagemask #x3000)
(defvar git-index-entry-extended #x4000)
(defvar git-index-entry-valid #x8000)
(defvar git-index-entry-stageshift 12)

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

(defcenum git-index-capabilities
  (:normal 0)
  (:ignore-case 1)
  (:no-filemode 2)
  (:no-symlinks 4)
  ;; (:from-owner ~0u)
  )

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

(defcfun ("git_index_caps" %git-index-caps)
    git-index-capabilities
  (index %index))

(defcfun ("git_index_has_conflicts" %git-index-has-conflicts)
    :boolean
  (index %index))

(defcfun ("git_index_write_tree" %git-index-write-tree)
    %return-value
  (oid :pointer)
  (index %index))

(defcfun ("git_index_entrycount" %git-index-entry-count)
    :unsigned-int
  (index %index))

(defcfun ("git_index_entry_stage" %git-index-entry-stage)
    :int
  "Return an int with a value from 0 to 4.  Files in the working
directory return stage 0.  Files with stages 1 (ancestor), 2 (ours)
and 3 (theirs) are in conflict."
  (index-entry :pointer))

(defcfun ("git_index_get_byindex" %git-index-get-by-index)
    %index-entry
  (index %index)
  (position :unsigned-int))

(defcfun ("git_index_get_bypath" %git-index-get-by-path)
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
          :stage (ash (logand flags git-index-entry-stagemask)
                      (- 0 git-index-entry-stageshift)))))

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

;; TODO (RS) perhaps rename back to index-add, seems unlikely to have
;; naming clash.
(defgeneric index-add-file (path index)
  (:documentation
   "Adds the PATH to the current index *GIT-REPOSITORY-INDEX* or the
explicit keyword argument :INDEX"))

(defmethod index-add-file ((path string) (index index))
  (%git-index-add-by-path index path))

(defmethod index-add-file ((path pathname) (index index))
  (let ((path (if (pathname-relative-p path)
                   path
                   (enough-namestring path (repository-workdir (slot-value index 'facilitator))))))
    (index-add-file (namestring path) index)))

(defmethod index-add-file ((entry list) (index index))
  (%git-index-add index entry))

(defgeneric index-clear (index)
  (:documentation "Clear contents of the index removing all entries.
Changes need to be written back to disk to take effect.")
  (:method ((index index))
    (%git-index-clear index)))

(defgeneric index-reload (index)
  (:documentation "Reload the state of the index with objects read from disk.")
  (:method ((index index))
    (%git-index-read index)))

(defgeneric index-write (index)
  (:documentation "Write the git index back to the file system.")
  (:method ((index index))
    (%git-index-write index)))

(defmacro with-index ((var &optional repository-or-path) &body body)
  "Load an index from a repository, path or if none is specified then
an in-memory index is used.  The newly opened index is bound to the
variable VAR."
  `(let ((,var ,(if repository-or-path
                    `(open-index ,repository-or-path)
                    `(new-index))))
     (unwind-protect
          (progn ,@body)
       (free ,var))))

(defun new-index ()
  "Create a new in-memory index that can be used to perform in memory
operations that may not be written back to the disk."
  (with-foreign-object (index :pointer)
    (%git-index-new index)
    (make-instance 'index
           :pointer (mem-ref index :pointer)
           :free-function #'%git-index-free)))

(defmethod open-index ((path string))
  "Open a new index in a file."
  (with-foreign-object (index :pointer)
    (%git-index-open index path)
    (make-instance 'index
           :pointer (mem-ref index :pointer)
           :free-function #'%git-index-free)))

(defmethod open-index ((path pathname))
  "Open a new index in a file."
  (open-index (namestring path)))

(defmethod index-conflicts-p ((index index))
  (%git-index-has-conflicts index))

(defmethod git-entry-count ((index index))
  (%git-index-entry-count index))

(defmethod git-entry-by-index ((index index) position)
  (%git-index-get-by-index index position))

(defmethod index-entries ((index index))
  (loop
    :for i :from 0 :below (git-entry-count index)
    :for entry = (git-entry-by-index index i)
    :collect entry))

(defmethod git-entry-by-path ((index index) (path string))
  (%git-index-get-by-path index path 1))

(defgeneric index-to-tree (index)
  (:documentation "Write the current index to a new tree object.")
  (:method ((index index))
      (with-foreign-object (oid-pointer '(:struct git-oid))
        (%git-index-write-tree oid-pointer index)
        (get-object 'tree (convert-from-foreign oid-pointer '%oid) (facilitator index)))))

(defmethod git-entry ((index index))
  (with-foreign-object (oid-pointer '(:struct git-oid))
    (%git-index-write-tree oid-pointer index)
    (convert-from-foreign oid-pointer '%oid)))
