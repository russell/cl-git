;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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

(defcenum (git-file-mode :uint16)
  (:new #o0000000)
  (:tree #o0040000)
  (:blob #o0100644)
  (:blob-executable #o0100755)
  (:link #o0120000)
  (:commit #o0160000))

(defbitfield git-diff-flags
  (:binary #.(ash 1 0)) ;; file(s) treated as binary data
  :not-binary         ;; file(s) treated as text data
  :valid-oid)         ;; `oid` value is known correct

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

(defcenum (git-diff-submodule-ignore :int16)
  (:reset -1)
  :none
  :untracked
  :dirty
  :all
  :default)

(define-foreign-type patch (git-pointer)
  nil
  (:actual-type :pointer)
  (:simple-parser %patch))

(defcstruct git-diff-file
  (:oid (:struct git-oid))
  (:path :string)
  (:size off-t)
  (:flags git-diff-flags)
  (:mode git-file-mode))

(defcstruct (git-diff-delta :class diff-delta-type)
  (status git-delta-status)
  (flags git-diff-flags)
  (similarity :uint16) ;;< for RENAMED and COPIED, value 0-100
  (number-files :uint16)
  (old-file (:struct git-diff-file))
  (new-file (:struct git-diff-file))
  )

(defcstruct git-diff-range
  (:start-a :int)  ;; Starting line number in old_file
  (:lines-a :int)  ;; Number of lines in old_file
  (:start-b :int)  ;; Starting line number in new_file
  (:lines-b :int)) ;; Number of lines in new_file

(defcstruct git-diff-options
  (version :unsigned-int)
  (flags git-diff-option-flags)
  (ignore-submodules git-diff-submodule-ignore)
  (pathspec (:struct git-strings))
  (diff-notify-cb :pointer)  ;; this isn't really a pointer?
  (notify-payload :pointer)
  (context-lines :uint16)
  (interhunk-lines :uint16)
  (oid-abbrev :uint16)
  (max-size off-t)  ;; defaults to 512MB
  (old-prefix :string)
  (new-prefix :string))

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
                     :initform :default)
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
   (notify-payload :accessor diff-notify-payload
                   :initarg :notify-payload
                   :initform (null-pointer)))
  (:actual-type :pointer)
  (:simple-parser %diff-options))

(define-foreign-type diff-list (git-pointer)
  nil
  (:actual-type :pointer)
  (:simple-parser %diff-list))

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

(defcfun %git-patch-to-str
    %return-value
  (string :pointer)
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
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((value diff-options) (type diff-options) ptr)
  (with-foreign-slots ((version flags oid-abbrev ignore-submodules context-lines interhunk-lines
                                old-prefix new-prefix max-size diff-notify-cb notify-payload)
                       ptr (:struct git-diff-options))
    (setf version (diff-version value))
    (setf flags (diff-flags value))
    (setf oid-abbrev (diff-oid-abbrev value))
    (setf ignore-submodules (diff-ignore-submodules value))
    (setf context-lines (diff-context-lines value))
    (setf interhunk-lines (diff-interhunk-lines value))
    (setf old-prefix (diff-old-prefix value))
    (setf new-prefix (diff-new-prefix value))
    (convert-into-foreign-memory
     (diff-pathspec value)
     '(:struct git-strings)
     (foreign-slot-pointer ptr '(:struct git-diff-options) 'pathspec))
    (setf max-size (diff-max-size value))
    (setf diff-notify-cb (diff-notify-cb value))
    (setf notify-payload (diff-notify-cb value))
    ptr))

(defmethod free-translated-object (pointer (type diff-options) param)
  (%git-strarray-free
   (foreign-slot-pointer pointer '(:struct git-diff-options) 'pathspec))
  (foreign-free pointer))


(defmethod translate-from-foreign (value (type diff-delta-type))
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

(defmethod diff-deltas-count ((diff-list diff-list))
  (%git-diff-num-deltas diff-list))

(defmethod diff-deltas-summary ((diff-list diff-list))
  (let (*git-diff-deltas*)
    (%git-diff-foreach diff-list
                       (callback collect-diff-files)
                       (null-pointer)
                       (null-pointer)
                       (null-pointer))
    *git-diff-deltas*))

(defmethod make-patch1 ((diff diff-list) index)
  (with-foreign-objects ((patch :pointer))
    (%git-patch-from-diff patch diff index)
    (let* ((patch (convert-from-foreign (mem-ref patch :pointer) '%patch))
           (delta (convert-from-foreign (%git-patch-get-delta patch)
                                        '(:struct git-diff-delta))))

      ;; TODO (RS) this is a crap way to enable garbage collection
      (setf (facilitator patch) (facilitator diff))
      (enable-garbage-collection patch)
      (setf (getf delta :patch) (patch-to-string patch))
      delta)))

(defmethod patch-to-string ((patch patch))
  (with-foreign-object (string :pointer)
    (%git-patch-to-str string patch)
    (prog1
        (foreign-string-to-lisp (mem-ref string :pointer) :encoding :utf-8)
      (foreign-free (mem-ref string :pointer)))))

(defmethod make-patch ((diff diff-list))
  (loop :for i :below (diff-deltas-count diff)
        :collect (make-patch1 diff i)))
