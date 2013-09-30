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


(defcstruct git-strings
  (strings :pointer)
  (count size-t))


(define-foreign-type git-strings-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %git-strings))


(defcfun ("git_strarray_free" %git-strarray-free)
    :void
  (strings :pointer))


(defmethod translate-to-foreign (value (type git-strings-type))
  (if (pointerp value)
      (values value t)
      (error "Cannot convert type: ~A to git-strings" (type-of value))))

(defmethod translate-to-foreign ((value list) (type git-strings-type))
  (let ((ptr (foreign-alloc '(:struct git-strings))))
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((object list) (type git-strings-type) ptr)
  (setf (cffi:foreign-slot-value ptr '(:struct git-strings) 'count) (length object))
  (let ((array (foreign-alloc :pointer :initial-element (null-pointer)
                                       :count (length object))))
    (loop :for string :in object
          :for i :from 0
          :for str = (foreign-string-alloc string)
          :do (setf (mem-aref array :pointer i) str))
    (setf (cffi:foreign-slot-value ptr '(:struct git-strings) 'strings) array))
  ptr)

(defmethod translate-from-foreign (value (type git-strings-type))
  (with-foreign-slots ((strings count) value (:struct git-strings))
    (loop :for i :below count
          :collect (foreign-string-to-lisp (mem-aref strings :pointer i)))))

(defmethod free-translated-object (pointer (type git-strings-type) do-not-free)
  (%git-strarray-free pointer)
  (unless do-not-free (foreign-free pointer)))

(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))
