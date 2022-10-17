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

(defmethod translate-to-foreign ((value list) (type git-strarray-tclass))
  (let ((ptr (foreign-alloc '(:struct git-strarray))))
    (translate-into-foreign-memory value type ptr)
    ptr))

(defmethod translate-into-foreign-memory ((object list) (type git-strarray-tclass) ptr)
  (setf (cffi:foreign-slot-value ptr '(:struct git-strarray) 'count) (length object))
  (let ((array (foreign-alloc :pointer :initial-element (null-pointer)
                                       :count (length object))))
    (loop :for string :in object
          :for i :from 0
          :for str = (foreign-string-alloc string)
          :do (setf (mem-aref array :pointer i) str))
    (setf (cffi:foreign-slot-value ptr '(:struct git-strarray) 'strings) array)))

(defmethod translate-from-foreign (value (type git-strarray-tclass))
  (with-foreign-slots ((strings count) value (:struct git-strarray))
    (loop :for i :below count
          :collect (foreign-string-to-lisp (mem-aref strings :pointer i)))))

(defmethod free-translated-object (ptr (type git-strarray-tclass) do-not-free)
  (%git-strarray-free ptr)
  (unless do-not-free (foreign-free ptr)))

(defcfun ("git_strarray_free" %git-strarray-free)
    :void
  (strings :pointer))
