;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
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

(defctype size :unsigned-long)
(defctype size-t :unsigned-long)
(defctype off-t :uint64)


(defcenum git-file-mode
  (:new 0000000)
  (:tree 0040000)
  (:blob 0100644)
  (:blob-executable 0100755)
  (:link 0120000)
  (:commit 0160000))

(define-foreign-type git-object (git-pointer)
  ((libgit2-oid :initarg :oid :initform nil)
   (libgit2-name :initarg :name :initform nil)
   (libgit2-disposed :initform nil))
  (:actual-type :pointer)
  (:simple-parser %object)
  (:documentation "Class wrapping a pointer, handles finalization and
  freeing of the underlying object"))

(define-foreign-type git-commit (git-object)
  nil
  (:simple-parser %commit))

(define-foreign-type git-tag (git-object)
  nil
  (:simple-parser %tag))

(define-foreign-type git-blob (git-object)
  nil
  (:simple-parser %blob))

(define-foreign-type git-tree-entry-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %tree-entry))

(define-foreign-type git-tree (git-object)
  nil
  (:simple-parser %tree))

(define-foreign-type git-config (git-pointer)
  nil
  (:simple-parser %config))

(define-foreign-type git-remote (git-pointer)
  nil
  (:simple-parser %remote))

(define-foreign-type git-reference (git-object)
  nil
  (:simple-parser %reference))

(define-foreign-type git-reflog (git-pointer)
  nil
  (:simple-parser %reflog))

(define-foreign-type git-reflog-entry (git-pointer)
  nil
  (:simple-parser %reflog-entry))

(define-foreign-type git-repository (git-pointer)
  ()
  (:simple-parser %repository))

(define-foreign-type git-odb (git-pointer)
  ()
  (:simple-parser %odb))

(define-foreign-type git-index (git-pointer)
  ()
  (:simple-parser %index))

(define-foreign-type git-odb-object (git-pointer)
  ()
  (:simple-parser %odb-object))

(define-foreign-type git-revision-walker (git-pointer)
  ()
  (:simple-parser %revwalker))

(define-foreign-type oid-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %oid))

(define-foreign-type git-signature-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %git-signature))

(define-foreign-type git-strings-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %git-strings))

(define-foreign-type time-type ()
  nil
  (:actual-type :int64)
  (:simple-parser %time))

(define-foreign-type index-entry-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %index-entry))

(define-foreign-type refspec-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %refspec))

#+nil (define-foreign-type indexer-stats-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %indexer-stats))

;;; ERRORS
(define-foreign-type git-error-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %git-error))

(define-foreign-type return-value-type ()
  nil
  (:actual-type :int)
  (:simple-parser %return-value))
