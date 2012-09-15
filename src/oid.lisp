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

(defparameter *git-oid-size* 20)

(defparameter *git-oid-hex-size* (+ 40 1)
  "The size of a Git commit hash.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcstruct git-oid
  (id :unsigned-char :count 20)) ;; should be *git-oid-size* or +git-oid-size+


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign type translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod translate-to-foreign ((value number) (type oid-type))
  (declare (ignore type))
  (let ((c-oid (foreign-alloc 'git-oid)))
    (loop
      :for c-index :from 0 :below *git-oid-size*
      :for byte-index :downfrom (* 8 (1- *git-oid-size*)) :by 8
      :do
         (setf (mem-aref (foreign-slot-pointer c-oid 'git-oid 'id)
                         :unsigned-char c-index)
               (ldb (byte 8 byte-index) value)))
    c-oid))

(defmethod translate-to-foreign ((value string) (type oid-type))
  (translate-to-foreign (parse-integer value :radix 16) type))

(defmethod translate-to-foreign ((value t) (type oid-type))
  (if (pointerp value)
      (values value t)
      (error "Cannot convert type: ~A to git-oid struct" (type-of value))))

(defmethod translate-from-foreign (value (type oid-type))
  "Translates a pointer to a libgit2 oid structure to an integer, the lisp
version of the oid.  If the pointer is a C null pointer return nil.
This can happen for example when the oid is asked for a reference and the
reference is symbolic."
  (declare (ignore type))
  (if (null-pointer-p value)
      nil
      (let ((lisp-oid 0))
        (loop
          :for c-index :from 0 :below *git-oid-size*
          :for byte-index :downfrom (* 8 (1- *git-oid-size*)) :by 8
          :do
             (setf (ldb (byte 8 byte-index) lisp-oid)
                   (mem-aref (foreign-slot-pointer value 'git-oid 'id)
                             :unsigned-char c-index)))
        lisp-oid)))

(defmethod free-translated-object (pointer (type oid-type) do-not-free)
  (unless do-not-free (foreign-free pointer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helper functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oid-from-string-or-number (value)
  "Returns an OID (integer) from value.  This is a very 
limited function, it converts strings by interpreting them
as base 16 numbers and returns a number straight through."
  (typecase value
    (number value)
    (string (parse-integer value :radix 16))
    (t (error "Wrong type: ~A in oid-from-string-or-number" (type-of value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lookup-oid (&key sha head (repository *git-repository*))
  "Returns an oid for a single commit (or tag).  It takes a single
 keyword argument, either SHA or HEAD If the keyword argument is SHA
 the value should be a SHA1 id as a string, or an OID as a number.
 The value for the HEAD keyword should be a symbolic reference to a git commit.

TODO, this function is a bit messy, need to think about cleaning this up."
  (cond
    (head (let* ((original-ref (git-lookup :reference head :repository repository))
                 (resolved-ref (git-resolve original-ref)))
	    (prog1
		(git-reference-oid resolved-ref)
	      (git-free original-ref)
	      (git-free resolved-ref))))
    (sha (oid-from-string-or-number sha))))

(defun lookup-oids (&key sha head (repository *git-repository*))
  "Similar to lookup-commit, except that the keyword arguments also
 accept a list of references.  In that case it will return a list of
 oids instead of a single oid.  If the argument was a single
 reference, it will return a list containing a single oid."
  (flet ((lookup-loop (keyword lookup)
           (loop :for reference
                 :in (if (atom lookup) (list lookup) lookup)
                 :collect (lookup-oid keyword reference :repository repository))))
    (cond
      (head (lookup-loop :head head))
      (sha (lookup-loop :sha sha)))))
