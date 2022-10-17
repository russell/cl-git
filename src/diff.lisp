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

(defconstant +git-diff-options-version+ 1)

(define-foreign-type patch (git-pointer)
  nil
  (:actual-type :pointer)
  (:simple-parser %patch))

(define-foreign-type diff-delta-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %diff-delta))

(define-foreign-type diff-options ()
  ((version :reader diff-version
            :initarg :version
            :initform *diff-options-version*)
   (flags :accessor diff-flags
          :initarg :flags
          :initform '(:normal))
   (oid-abbrev :accessor diff-oid-abbrev
               :initarg :oid-abbrev
               :initform *oid-abbrev*)
   (ignore-submodules :accessor diff-ignore-submodules
                     :initarg :submodule-ignore
                     :initform :unspecified)
   (context-lines :accessor diff-context-lines
                  :initarg :context-lines
                  :initform *diff-context-lines*)
   (interhunk-lines :accessor diff-interhunk-lines
                    :initarg :interhunk-lines
                    :initform *diff-interhunk-lines*)
   (old-prefix :accessor diff-old-prefix
               :initarg :old-prefix
               :initform *diff-old-prefix*)
   (new-prefix :accessor diff-new-prefix
               :initarg :new-prefix
               :initform *diff-new-prefix*)
   (max-size :accessor diff-max-size
             :initarg :max-size
             :initform *diff-max-size*)
   (pathspec :accessor diff-pathspec
             :initarg :pathspec
             :initform nil)
   (notify-cb :accessor diff-notify-cb
              :initarg :notify-cb
              :initform (null-pointer))
   (progress-cb :accessor diff-progress-cb
                :initarg :progress-cb
                :initform (null-pointer))
   (payload :accessor diff-payload
            :initarg :payload
            :initform (null-pointer)))
  (:actual-type :pointer)
  (:simple-parser %diff-options))

(define-foreign-type diff-list (git-pointer)
  nil
  (:actual-type :pointer)
  (:simple-parser %diff-list))

(defcfun %git-diff-options-init
    %return-value
  (options :pointer)
  (version :unsigned-int))

(defcfun %git-diff-index-to-workdir
    %return-value
  (diff-list :pointer)
  (repository %repository)
  (index %index)
  (options %diff-options))

(defcfun %git-diff-tree-to-tree
    %return-value
  (diff-list :pointer)
  (repository %repository)
  (old-tree %tree)
  (new-tree %tree)
  (options %diff-options))

(defcfun %git-diff-tree-to-index
    %return-value
  (diff-list :pointer)
  (repository %repository)
  (old_tree %tree)
  (index %index)
  (options %diff-options))

(defcfun %git-diff-free
    :void
  (diff-list :pointer))

(defcfun %git-diff-num-deltas
    size-t
  (diff-list %diff-list))

(defcfun %git-diff-get-delta
    (:pointer (:struct git-diff-delta))
  (diff-list %diff-list)
  (index size-t))

(defcfun %git-diff-foreach
    %return-value
  (diff-list %diff-list)
  (file-callback :pointer)
  (hunk-callback :pointer)
  (data-callback :pointer)
  (payload :pointer))

(defcfun %git-patch-from-diff
    %return-value
  (patch :pointer)
  (diff-list %diff-list)
  (index size-t))

(defcfun %git-patch-get-delta
    (:pointer (:struct git-diff-delta))
  (patch %patch))

(defcfun %git-patch-to-buf
    %return-value
  (out (:pointer (:struct git-buf)))
  (patch %patch))

(defcfun %git-patch-free
    :void
  (patch :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign Type Translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod translate-to-foreign (value (type diff-options))
  (let ((ptr (foreign-alloc '(:struct git-diff-options))))
    (%git-diff-options-init ptr +git-diff-options-version+)
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((value diff-options) (type diff-options) ptr)
  (with-foreign-slots ((version flags oid-abbrev ignore-submodules context-lines interhunk-lines
                                old-prefix new-prefix max-size notify-cb payload)
                       ptr (:struct git-diff-options))
    (setf flags (diff-flags value))
    (setf oid-abbrev (diff-oid-abbrev value))
    (setf ignore-submodules (diff-ignore-submodules value))
    (setf context-lines (diff-context-lines value))
    (setf interhunk-lines (diff-interhunk-lines value))
    (setf old-prefix (diff-old-prefix value))
    (setf new-prefix (diff-new-prefix value))
    (convert-into-foreign-memory
     (diff-pathspec value)
     '(:struct git-strarray)
     (foreign-slot-pointer ptr '(:struct git-diff-options) 'pathspec))
    (setf max-size (diff-max-size value))
    (setf diff-notify-cb (diff-notify-cb value))
    ptr))

(defmethod free-translated-object (pointer (type diff-options) param)
  (declare (ignore param))
  (%git-strarray-free
   (foreign-slot-pointer pointer '(:struct git-diff-options) 'pathspec))
  (foreign-free pointer))


(defmethod translate-from-foreign (value (type git-diff-delta-tclass))
  (with-foreign-slots ((old-file new-file status similarity flags)
                       value (:struct git-diff-delta))
    (list :status status :similarity similarity :flags flags
          :file-a old-file
          :file-b new-file)))

(defmethod translate-from-foreign (value (type diff-list))
  (let ((diff-list (make-instance 'diff-list
                                  :free-function #'%git-diff-free
                                  :pointer value)))
    diff-list))

(defmethod translate-from-foreign (value (type patch))
  (let ((patch (make-instance 'patch
                              :free-function #'%git-patch-free
                              :pointer value)))
    patch))

(defvar *git-diff-deltas* nil
  "Used to handle return values from the git diff")

(defcallback collect-diff-files :int ((delta (:pointer (:struct git-diff-delta)))
                                      (progress :float) (data :pointer))
  (declare (ignore data progress))
  (push (convert-from-foreign delta '(:struct git-diff-delta)) *git-diff-deltas*)
  +success+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric diff (object-old object-new &optional options)
  (:documentation "Diff two objects."))

(defmethod diff ((repository repository) (index index) &optional (options (make-instance 'diff-options)))
  (with-foreign-objects ((diff-list :pointer))
    (%git-diff-index-to-workdir diff-list repository index options)
    (let ((diff-list (convert-from-foreign (mem-ref diff-list :pointer) '%diff-list)))
      ;; TODO (RS) this is a crap way to enable garbage collection
      (setf (facilitator diff-list) repository)
      (enable-garbage-collection diff-list)
      diff-list)))

(defmethod diff ((tree-old tree) (tree-new tree) &optional (options (make-instance 'diff-options)))
  (with-foreign-objects ((diff-list :pointer))
    (%git-diff-tree-to-tree diff-list (facilitator tree-old) tree-old tree-new options)
    (let ((diff-list (convert-from-foreign (mem-ref diff-list :pointer) '%diff-list)))
      ;; TODO (RS) this is a crap way to enable garbage collection
      (setf (facilitator diff-list) (facilitator tree-old))
      (enable-garbage-collection diff-list)
      diff-list)))

(defmethod diff ((tree tree) (index index) &optional (options (make-instance 'diff-options)))
  (with-foreign-objects ((diff-list :pointer))
    (%git-diff-tree-to-index diff-list (facilitator index) tree index options)
    (let ((diff-list (convert-from-foreign (mem-ref diff-list :pointer) '%diff-list)))
      ;; TODO (RS) this is a crap way to enable garbage collection
      (setf (facilitator diff-list) (facilitator tree))
      (enable-garbage-collection diff-list)
      diff-list)))

(defmethod diff ((commit commit) (index index)
                 &optional (options (make-instance 'diff-options)))
  (diff (commit-tree commit) index options))

(defmethod diff ((commit-old commit) (commit-new commit)
                 &optional (options (make-instance 'diff-options)))
  (diff (commit-tree commit-old) (commit-tree commit-new) options))

(defgeneric diff-deltas-count (diff-list)
  (:method ((diff-list diff-list))
    (%git-diff-num-deltas diff-list)))

(defgeneric diff-deltas-summary (diff-list)
  (:method ((diff-list diff-list))
    (let (*git-diff-deltas*)
      (%git-diff-foreach diff-list
                         (callback collect-diff-files)
                         (null-pointer)
                         (null-pointer)
                         (null-pointer))
      *git-diff-deltas*)))

(defgeneric make-patch1 (diff-list index)
  (:method ((diff diff-list) index)
    (with-foreign-objects ((patch :pointer))
      (%git-patch-from-diff patch diff index)
      (let* ((patch (convert-from-foreign (mem-ref patch :pointer) '%patch))
             (delta (convert-from-foreign (%git-patch-get-delta patch)
                                          '(:struct git-diff-delta))))

        ;; TODO (RS) this is a crap way to enable garbage collection
        (setf (facilitator patch) (facilitator diff))
        (enable-garbage-collection patch)
        (setf (getf delta :patch) (patch-to-string patch))
        delta))))

(defgeneric patch-to-string (patch)
  (:method ((patch patch))
   (with-foreign-object (buffer '(:struct git-buf))
     (with-foreign-slots ((ptr size asize) buffer (:struct git-buf))
       ;; Set the buffer's pointer to null so that libgit allocates the
       ;; memory needed.
       (setf ptr (null-pointer))
       (setf size 0)
       (setf asize 0)
       (%git-patch-to-buf buffer patch)
       (prog1
           (foreign-string-to-lisp ptr :encoding :utf-8 :count size)
         (%git-buf-free buffer))))))

(defgeneric make-patch (diff-list)
  (:method ((diff diff-list))
    (loop :for i :below (diff-deltas-count diff)
          :collect (make-patch1 diff i))))
