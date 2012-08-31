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


(define-foreign-type git-tree-entry-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %tree-entry))

(define-foreign-type tree (object)
  nil
  (:actual-type :pointer)
  (:simple-parser %tree))

(defcstruct git-tree-entry
  (attr :unsigned-int)
  (filename :string)
  (oid git-oid)
  (filename-len size)
  (removed :int))

(defcfun ("git_tree_create_fromindex" %git-tree-create-fromindex)
    %return-value
  (oid :pointer)
  (index :pointer))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign type translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod translate-from-foreign (value (type git-tree-entry-type))
  (with-foreign-slots ((attr filename oid removed) value git-tree-entry)
    (list :attr attr :filename filename
          :oid (convert-from-foreign oid '%oid) :removed removed)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun git-tree-lookup (oid)
  "Lookup a Git tree object."
  (git-object-lookup oid :tree))

(defun git-tree-entries (tree)
  "Return all direct children of TREE."
  (loop :repeat (git-tree-entry-count tree)
        :for index :from 0
        :collect (git-tree-entry-by-index tree index)))

(defun git-oid-from-index ()
  "Write the current index to the disk and return an oid to it."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (with-foreign-object (oid 'git-oid)
    (%git-tree-create-fromindex oid *git-repository-index*)
    (convert-from-foreign oid '%oid)))
