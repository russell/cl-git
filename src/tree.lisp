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

(defcfun ("git_tree_entry_attributes" git-tree-entry-attributes)
    :unsigned-int
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


(defmethod translate-from-foreign (value (type git-tree-entry-type))
  (unless (null-pointer-p value)
    (list
     :attr (git-tree-entry-attributes value)
     :filename (git-tree-entry-name value)
     :oid (git-tree-entry-id value)
     :type (git-tree-entry-type value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tree (object)
  ())

(defmethod git-lookup ((class (eql :tree)) oid repository &key)
  (git-object-lookup oid class :repository repository))

#+nil (defun git-create-from-index (index)
  "Write the current index to the disk and return an oid to it."
  (assert (not (null-or-nullpointer index)))
  (with-foreign-object (oid 'git-oid)
    (git-tree-create-fromindex oid index)
    (convert-from-foreign oid '%oid)))


(defmethod git-entry-count ((object tree))
  (git-tree-entry-count object))

(defmethod git-entry-by-index ((object tree) index)
  (git-tree-entry-by-index object index))

(defmethod git-tree ((object tree) &key path repository)
  (with-foreign-string (%path path)
    (let ((entry (git-tree-entry-byname object %path)))
      (when entry
        (git-lookup :object (getf entry :oid) :repository repository)))))
