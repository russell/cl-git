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

(define-foreign-type git-object ()
  ()
  (:actual-type :pointer)
  (:simple-parser %object))

(define-foreign-type git-commit (git-object)
  nil
  (:actual-type :pointer)
  (:simple-parser %commit))

(define-foreign-type git-tag (git-object)
  nil
  (:actual-type :pointer)
  (:simple-parser %tag))

(define-foreign-type git-blob (git-object)
  nil
  (:actual-type :pointer)
  (:simple-parser %blob))

(define-foreign-type git-tree-entry-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %tree-entry))

(define-foreign-type git-tree (git-object) ;; Is this needed????
  nil
  (:actual-type :pointer)
  (:simple-parser %tree))

(define-foreign-type git-config () 
  nil
  (:actual-type :pointer)
  (:simple-parser %config))

(define-foreign-type git-reference ()
  nil
  (:actual-type :pointer)
  (:simple-parser %reference))

(define-foreign-type git-repository (git-object)
  ()
  (:actual-type :pointer)
  (:simple-parser %repository))


;;; TODO FIX THIS!!!!
(define-foreign-type revision-walker ()
  ((%object :accessor pointer :initarg :pointer :initform (null-pointer))
   (%repository :accessor %repository :initarg :repository-pointer))
  (:actual-type :pointer)
  (:simple-parser %revwalker))

(define-foreign-type oid-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %oid))

(define-foreign-type git-signature-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %git-signature))

(define-foreign-type time-type ()
  nil
  (:actual-type :int64)
  (:simple-parser %time))


;;; ERRORS
(define-foreign-type git-error-type ()
  nil
  (:actual-type git-error)
  (:simple-parser %git-error))

(define-foreign-type return-value-type ()
  nil
  (:actual-type :int)
  (:simple-parser %return-value))
