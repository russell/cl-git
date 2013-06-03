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


#+nil (defcfun ("git_tree_create_fromindex" git-tree-create-fromindex)
    %return-value
  (oid :pointer)
  (index %index))

(defcfun ("git_tree_id" git-tree-oid)
    %oid
  "Returns the oid of the tree."
  (tree %tree))

(defcfun ("git_tree_entrycount" git-tree-entry-count)
    :unsigned-int
  "Returns the number of tree entries in the tree object.
This does count the number of direct children, not recursively."
  (tree %tree))

(defcfun ("git_tree_entry_byindex" git-tree-entry-by-index)
    %tree-entry
  "Returns the tree entry at index"
  (tree %tree)
  (index :unsigned-int))

(defcfun ("git_tree_entry_name" git-tree-entry-name)
    :string
  "Returns the tree entry name."
  (tree %tree))

(defcfun ("git_tree_entry_filemode" git-tree-entry-filemode)
    :int
  "Returns the tree entry attributes."
  (tree %tree))

(defcfun ("git_tree_entry_type" git-tree-entry-type)
    git-object-type
  "Returns the tree entry type."
  (tree %tree))

(defcfun ("git_tree_entry_byname" git-tree-entry-byname)
    %tree-entry
  (tree %tree)
  (name :string))

(defcfun ("git_tree_entry_id" git-tree-entry-id)
    %oid
  (entry %tree-entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign type translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tree (object)
  ((filename :reader filename :initarg :filename :initform "")))

(defclass tree-mixin ()
  ((filemode :reader filemode :initarg :filemode :initform nil)
   (filename :reader filename :initarg :filename :initform nil))
  (:documentation "A git tree entry mixin."))

(defclass tree-blob (tree-mixin blob)
  ()
  (:documentation "A git tree blob."))

(defclass tree-tree (tree-mixin tree)
  ()
  (:documentation "A git tree entry."))

(defclass tree-commit (tree-mixin commit)
  ()
  (:documentation "A git tree commit."))

(defclass tree-link (tree-mixin)
  ()
  (:documentation "A git tree commit."))

(defun make-tree-entry (type filename mode oid repository)
  "Make a weak reference by name that can be looked-up later."
  (make-instance type
                 :filename (pathname filename)
                 :filemode mode
                 :oid oid
                 :facilitator repository
                 :free-function #'git-object-free))

(defmethod translate-from-foreign (value (type git-tree-entry-type))
  (unless (null-pointer-p value)
    (let ((type (git-tree-entry-type value))
          (filename (git-tree-entry-name value)))
      (list
       :filemode (git-tree-entry-filemode value)
       :filename (if (eq type :tree)
                     (make-pathname :directory `(:relative ,filename))
                     (pathname filename))
       :oid (git-tree-entry-id value)
       :type type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod get-object ((class (eql 'tree)) oid repository)
  (git-object-lookup oid class repository))

(defmethod git-entry-count ((object tree))
  (git-tree-entry-count object))

(defmethod git-entry-by-index ((object tree) index)
  (git-tree-entry-by-index object index))

;; Set up the object mappings
;; TODO (RS) this should be a function call.
(setf (getf object-type-mapping 'tree-link) :link)
(setf (getf object-type-mapping 'tree-commit) :commit)
(setf (getf object-type-mapping 'tree-tree) :tree)
(setf (getf object-type-mapping 'tree-blob) :blob)

(defun tree-entries (tree pathname)
  (loop
    :for index :from 0 :below (git-entry-count tree)
    :for entry = (git-entry-by-index tree index)
    :when (if pathname (pathname-match-p (getf entry :filename) pathname) t)
    :collect
    (destructuring-bind (&key type filename filemode oid)
        entry
        (let ((type
                (ecase type
                  (:commit 'tree-commit)
                  (:link 'tree-link)
                  (:tree 'tree-tree)
                  (:blob 'tree-blob))))
          (make-tree-entry type
                           (merge-pathnames filename (filename tree))
                           filemode
                           oid
                           (facilitator tree))))))

(defun chomp-pathname (pathname index)
  "Split a path by out into segments by index."
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname))
        (size index))
    (cond ((> (length (pathname-directory pathname)) size)
            (let ((chomped-directory (nth size (pathname-directory pathname))))
              (make-pathname :directory `(:relative ,chomped-directory))))
          (t
           (make-pathname
            :name name
            :type type)))))

(defun tree-recurse (object paths)
  (let ((path (car paths)))
    (if (> (length paths) 1)
        (loop :for subtree :in (tree-entries object path)
              :do (tree-recurse subtree (cdr paths)))
        (tree-entries object path))))


(defmethod tree-directory ((object tree) &optional pathname)
  "List objects from a tree.  Optional argument pathname a wild
pathname that the entries must match."
  (if pathname
      (let* ((pathname (if (pathnamep pathname) pathname (pathname pathname)))
             (paths (loop :for index :from 1 :upto (length (pathname-directory pathname))
                          :collect (chomp-pathname pathname index)))
             (path (car paths)))
        (if (> (length paths) 1)
            (let ((objects))
              (loop :for subtree :in (tree-entries object path)
                    :when (eq (type-of subtree) 'tree-tree)
                      :do (setf objects
                                (concatenate 'list objects
                                             (tree-recurse subtree (cdr paths)))))
              objects)
            (tree-entries object path)))
      (tree-entries object pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subtrees
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((object tree-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((not (null-pointer-p (slot-value object 'libgit2-pointer)))
       (format stream "~a" (filename object)))
      ((or (slot-value object 'libgit2-oid) (slot-value object 'libgit2-name))
       (format stream "~a (weak)" (filename object)))
      ((slot-value object 'libgit2-disposed)
       (princ "(disposed)" stream)))))
