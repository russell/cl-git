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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *git-oid-size* 20))

(defparameter *git-oid-hex-size* (+ 40 1)
  "The size of a Git commit hash.")


(defcstruct (git-oid :class oid-struct-type)
  (id :unsigned-char :count #.*git-oid-size*))

(defun oid-to-foreign (oid foreign)
  (loop
    :for c-index :from 0 :below *git-oid-size*
    :for byte-index :downfrom (* 8 (1- *git-oid-size*)) :by 8
    :do
       (setf (mem-aref foreign :unsigned-char c-index)
             (ldb (byte 8 byte-index) oid))))

(defmethod translate-into-foreign-memory (value (type oid-struct-type) ptr)
  (declare (ignore ptr))
  (if (pointerp value)
      value
      (error "Don't know how to translate ~A." value)))

(defmethod translate-into-foreign-memory ((value number) (type oid-struct-type) ptr)
  (oid-to-foreign value (foreign-slot-pointer ptr '(:struct git-oid) 'id)))

(defmethod translate-to-foreign ((value number) (type oid-struct-type))
  (let ((ptr (foreign-alloc '(:struct git-oid))))
    (translate-into-foreign-memory value type ptr)
    ptr))

(defmethod translate-to-foreign ((value string) (type oid-struct-type))
  (translate-to-foreign (parse-integer value :radix 16) type))

(defun oid-from-foreign (value)
  (let ((lisp-oid 0))
        (loop
          :for c-index :from 0 :below *git-oid-size*
          :for byte-index :downfrom (* 8 (1- *git-oid-size*)) :by 8
          :do
             (setf (ldb (byte 8 byte-index) lisp-oid)
                   (mem-aref value
                             :unsigned-char c-index)))
        lisp-oid))

(defmethod translate-from-foreign (value (type oid-struct-type))
  "Translates a pointer to a libgit2 oid structure to an integer, the lisp
version of the oid.  If the pointer is a C null pointer return nil.
This can happen for example when the oid is asked for a reference and the
reference is symbolic."
  (if (null-pointer-p value)
      nil
      (let ((oid (oid-from-foreign (foreign-slot-pointer value '(:struct git-oid) 'id))))
        oid)))

(defmethod free-translated-object (pointer (type oid-struct-type) do-not-free)
  (unless do-not-free (foreign-free pointer)))


(define-foreign-type oid-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %oid))

(defmethod translate-to-foreign (value (type oid-type))
  (convert-to-foreign value '(:struct git-oid)))

(defmethod translate-from-foreign (value (type oid-type))
  (convert-from-foreign value '(:struct git-oid)))

(defmethod free-translated-object (value (type oid-type) param)
  (free-converted-object value '(:struct git-oid) param))


(defcfun ("git_oid_tostr" git-oid-tostr)
    :pointer
  "Returns the buffer that the string is written into.  The size of
the input buffer should be equal to git oid hex size + 1."
  (buffer :pointer)
  (size size-t)
  (oid %oid))



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


(defun lookup-oid (&key sha head repository)
  "Returns an oid for a single commit (or tag).  It takes a single
 keyword argument, either SHA or HEAD If the keyword argument is SHA
 the value should be a SHA1 id as a string, or an OID as a number.
 The value for the HEAD keyword should be a symbolic reference to a git commit.

TODO, this function is a bit messy, need to think about cleaning this up."
  (assert (not (null-or-nullpointer repository)))
  (cond
    (head (let* ((original-ref (get-object 'reference head repository))
                 (resolved-ref (git-resolve original-ref)))
            (prog1
                (target resolved-ref)
              (free original-ref)
              (free resolved-ref))))
    (sha (oid-from-string-or-number sha))))

(defun lookup-oids (&key sha head repository)
  "Similar to lookup-commit, except that the keyword arguments also
 accept a list of references.  In that case it will return a list of
 oids instead of a single oid.  If the argument was a single
 reference, it will return a list containing a single oid."
  (assert (not (null-or-nullpointer repository)))
  (flet ((lookup-loop (keyword lookup)
           (loop :for reference
                 :in (if (atom lookup) (list lookup) lookup)
                 :collect (lookup-oid keyword reference :repository repository))))
    (cond
      (head (lookup-loop :head head))
      (sha (lookup-loop :sha sha)))))
