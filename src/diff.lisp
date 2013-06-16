;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2013 Russell Sim <russell.sim@gmail.com>
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


(defbitfield (git-diff-option-flags :unsigned-int)
  (:normal 0)
  (:reverse #.(ash 1 0))
  :force_text
  :ignore_whitespace
  :ignore_whitespace_change
  :ignore_whitespace_eol
  :ignore_submodules
  :patience
  :include_ignored
  :include_untracked
  :include_unmodified
  :recurse_untracked_dirs
  :disable_pathspec_match
  :deltas_are_icase
  :include_untracked_content
  :skip_binary_check
  :include_typechange
  :include_typechange_trees
  :ignore_filemode
  :recurse_ignored_dirs)

(defbitfield git-diff-flags
  (:binary #.(ash 1 0)) ;; file(s) treated as binary data
  :not_binary         ;; file(s) treated as text data
  :valid_oid)         ;; `oid` value is known correct

(defcenum git-delta-status
  :unmodified  ;; no changes
  :added       ;; entry does not exist in old version
  :deleted     ;; entry does not exist in new version
  :modified    ;; entry content changed between old and new
  :renamed     ;; entry was renamed between old and new
  :copied      ;; entry was copied from another old entry
  :ignored     ;; entry is ignored item in workdir
  :untracked   ;; entry is untracked item in workdir
  :typechange) ;; type of entry changed between old and new

(defcenum (git-diff-line :char)
  (:context #.(char-int #\Space))
  (:addition #.(char-int #\+))
  (:deletion #.(char-int #\-))
  (:add_eofnl #.(char-int #\Newline))
  (:del_eofnl 0)
  (:file_hdr #.(char-int #\F))
  (:hunk_hdr #.(char-int #\H))
  (:binary #.(char-int #\B)))

(eval-when (:compile-toplevel :execute)
  (defvar *diff-options-version* 1)
  (defvar *diff-context-lines* 3)
  (defvar *diff-interhunk-lines* 0)
  (defvar *diff-old-prefix* "a")
  (defvar *diff-new-prefix* "b")
  (defvar *diff-max-size* (* 512 1024 1024)))

(defcstruct git-diff-options
  (version :unsigned-int)
  (flags git-diff-option-flags)
  (context-lines :uint16)
  (interhunk-lines :uint16)
  (old-prefix :string)
  (new-prefix :string)
  (pathspec (:struct git-strings))
  (max-size off-t)  ;; defaults to 512MB
  (diff-notify-cb :pointer)  ;; this isn't really a pointer?
  (notify-payload :pointer))

(defcstruct git-diff-file
  (oid %oid)
  (path :string)
  (size off-t)
  (flags git-diff-flags)
  (mode git-file-mode))

(defcstruct git-diff-delta
  (old_file (:struct git-diff-file))
  (new_file (:struct git-diff-file))
  (status git-delta-status)
  (similarity :unsigned-int) ;;< for RENAMED and COPIED, value 0-100
  (flags :unsigned-int))

(defcstruct git-diff-range
  (old_start :int)  ;; Starting line number in old_file
  (old_lines :int)  ;; Number of lines in old_file
  (new_start :int)  ;; Starting line number in new_file
  (new_lines :int)) ;; Number of lines in new_file


(defcfun %git-diff-index-to-workdir
    %return-value
  (diff-list %diff-list)
  (repository %repository)
  (index %index)
  (options %diff-options))

(defcfun %git-diff-tree-to-tree
    %return-value
  (diff-list %diff-list)
  (repository %repository)
  (old_tree %tree)
  (new_tree %tree)
  (options (:pointer (:struct git-diff-options))))

(defcfun %git-diff-tree-to-index
    %return-value
  (diff-list %diff-list)
  (repository %repository)
  (old_tree %tree)
  (index %index)
  (options (:pointer (:struct git-diff-options))))

(defcfun %git-diff-list-free
    :void
  (diff-list %diff-list))

(defcfun %git-diff-patch-free
    :void
  (patch :pointer))

(defcfun %git-diff-num-deltas
    size-t
  (diff-list %diff-list))

(defcfun %git-diff-get-patch
    %return-value
  (patch :pointer)
  (delta :pointer)
  (diff-list %diff-list)
  (index size-t))

(defcfun %git-diff-patch-to-str
    %return-value
  (string :pointer)
  (patch %patch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign Type Translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-type diff-options ()
  ((version :reader diff-version :initarg :version :initform *diff-options-version*)
   (flags :accessor diff-flags :initarg :flags :initform '(:normal))
   (context-lines :accessor diff-context-lines :initarg :context-lines :initform *diff-context-lines*)
   (interhunk-lines :accessor diff-interhunk-lines :initarg :interhunk-lines :initform *diff-interhunk-lines*)
   (old-prefix :accessor diff-old-prefix :initarg :old-prefix :initform *diff-old-prefix*)
   (new-prefix :accessor diff-new-prefix :initarg :new-prefix :initform *diff-new-prefix*)
   (max-size :accessor diff-max-size :initarg :max-size :initform *diff-max-size*)
   (pathspec :accessor diff-pathspec :initarg :pathspec :initform nil)
   (notify-cb :accessor diff-notify-cb :initarg :notify-cb :initform (null-pointer))
   (notify-payload :accessor diff-notify-payload :initarg :notify-payload :initform (null-pointer)))
  (:actual-type :pointer)
  (:simple-parser %diff-options))

(defmethod translate-to-foreign ((value diff-options) (type diff-options))
  (let ((options (foreign-alloc '(:struct git-diff-options))))
    (with-foreign-slots ((version flags context-lines interhunk-lines old-prefix new-prefix
                                  max-size diff-notify-cb notify-payload)
                         options (:struct git-diff-options))
      (setf version (diff-version value))
      (setf flags (diff-flags value))
      (setf context-lines (diff-context-lines value))
      (setf interhunk-lines (diff-interhunk-lines value))
      (setf old-prefix (diff-old-prefix value))
      (setf new-prefix (diff-new-prefix value))
      (let ((pathspecs (diff-pathspec value))
            (strings-list (foreign-slot-pointer options '(:struct git-diff-options) 'pathspec)))
        (setf (cffi:foreign-slot-value strings-list '(:struct git-strings) 'count) (length pathspecs))
        (let ((array (foreign-alloc :pointer :initial-element (null-pointer) :count (length pathspecs))))
          (loop :for string :in pathspecs
                :for i :from 0 :below (length pathspecs)
                :for str = (foreign-string-alloc string)
                :do (setf (mem-aref array :pointer i) str))
          (setf (cffi:foreign-slot-value strings-list '(:struct git-strings) 'strings) array)))
      (setf max-size (diff-max-size value))
      (setf diff-notify-cb (diff-notify-cb value))
      (setf notify-payload (diff-notify-cb value))
      options)))

(defmethod free-translated-object (pointer (type diff-options) param)
  ;; TODO (RS) this currently leaks memory, needs to be extended to
  ;; cleanup the nested strarray.
  (foreign-free pointer))


(define-foreign-type diff-list (git-pointer-type)
  nil
  (:actual-type :pointer)
  (:simple-parser %diff-list))

(defmethod translate-from-foreign (value (type git-pointer-type))
  (setf (pointer type) value)
  ;; TODO (RS) hook up garbage collection here
  type)

(defmethod translate-from-foreign (value (type diff-list))
  (setf (pointer type) value)
  (setf (free-function type) #'%git-diff-list-free)
  (enable-garbage-collection type)  ;; TODO (RS) this should be moved
                                    ;; out to an after method.
  type)

(define-foreign-type patch (git-pointer-type)
  nil
  (:actual-type :pointer)
  (:simple-parser %patch))

(defmethod translate-from-foreign (value (type patch))
  (setf (pointer type) value)
  (setf (free-function type) #'%git-diff-patch-free)
  (enable-garbage-collection type)  ;; TODO (RS) this should be moved
                                    ;; out to an after method.
  type)

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
    (translate-from-foreign (mem-ref diff-list :pointer)
                            (make-instance 'diff-list :facilitator repository))))

(defmethod diff ((tree-old tree) (tree-new tree) &optional (options (make-instance 'diff-options)))
  (with-foreign-objects ((diff-list :pointer))
    (%git-diff-tree-to-tree diff-list (facilitator tree-old) tree-old tree-new options)
    (translate-from-foreign (mem-ref diff-list :pointer)
                            (make-instance 'diff-list :facilitator (facilitator tree-old)))))

(defmethod diff ((tree tree) (index index) &optional (options (make-instance 'diff-options)))
  (with-foreign-objects ((diff-list :pointer))
    (%git-diff-tree-to-index diff-list (facilitator index) tree index options)
    (translate-from-foreign (mem-ref diff-list :pointer)
                            (make-instance 'diff-list :facilitator (facilitator tree-old)))))

(defmethod make-patch ((diff diff-list) index)
  (with-foreign-objects ((patch :pointer) (delta :pointer))
    (%git-diff-get-patch patch delta diff index)
    (translate-from-foreign
     (mem-ref patch :pointer)
     (make-instance 'patch
                    :facilitator (facilitator diff)))))

(defmethod patch-to-string ((diff diff-list))
  (with-foreign-object (string :pointer)
    (%git-diff-patch-to-str string (make-patch diff 0))
    (prog1
        (foreign-string-to-lisp (mem-ref string :pointer) :encoding :utf-8)
      (foreign-free (mem-ref string :pointer)))))

(defmethod diff-list-size ((diff-list diff-list))
  (git-diff-num-deltas diff-list))
