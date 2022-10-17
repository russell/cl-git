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

(define-foreign-type time-t ()
  nil
  (:actual-type git-time-t)
  (:simple-parser %time-t))

(defcenum (git-index-filemode)
  (:new #o0000000)
  (:tree #o0040000)
  (:blob #o0100644)
  (:blob-executable #o0100755)
  (:link #o0120000)
  (:gitlink #o0160000))

(defcenum (git-filemode-t :uint16)
  (:unreadable #o0000000)
  (:tree #o0040000)
  (:blob #o0100644)
  (:blob-executable #o0100755)
  (:link #o0120000)
  (:commit #o0160000))

(define-foreign-type git-object (git-pointer)
  ((libgit2-oid :initarg :oid
                :initform nil)
   (libgit2-name :initarg :name
                 :initform nil)
   (libgit2-disposed :initform nil))
  (:actual-type :pointer)
  (:simple-parser %object)
  (:documentation "Class wrapping a pointer, handles finalization and
  freeing of the underlying object"))

(define-foreign-type tree (git-object)
  nil
  (:simple-parser %tree))

(define-foreign-type index (git-pointer)
  ()
  (:documentation "A git index")
  (:simple-parser %index))

(define-foreign-type index-entry-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %index-entry))

(define-foreign-type index-time-struct-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %index-time))

(defmacro enum-to-bitmask (name enum)
  `(defbitfield ,name
      ,@(loop :for key :in (cffi:foreign-enum-keyword-list enum)
              :collect (list key (cffi::foreign-enum-value enum key)))))

(enum-to-bitmask git-checkout-notify-t* git-checkout-notify-t)
(enum-to-bitmask git-checkout-strategy-t* git-checkout-strategy-t)
(enum-to-bitmask git-credential-t* git-credential-t)
(enum-to-bitmask git-diff-flag-t* git-diff-flag-t)
(enum-to-bitmask git-diff-option-t* git-diff-option-t)
(enum-to-bitmask git-feature-t* git-feature-t)
(enum-to-bitmask git-sort-t* git-sort-t)
(enum-to-bitmask git-status-t* git-status-t)
