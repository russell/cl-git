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


(defcstruct git-strings
  (strings :pointer)
  (count size))

(defcfun ("git_strarray_free" %git-strarray-free)
    :void
  (strings %git-strings))

;;; New translation

(defmethod translate-to-foreign ((value t) (type git-strings-type))
  (if (pointerp value)
      (values value t)
      (error "Cannot convert type: ~A to git-strings" (type-of value))))

(defmethod translate-from-foreign (value (type git-strings-type))
  (with-foreign-slots ((strings count) value git-strings)
    (loop :for i :below count
       :collect (foreign-string-to-lisp (mem-aref strings :pointer i)))))

(defmethod free-translated-object (pointer (type git-strings-type) do-not-free)
  (%git-strarray-free pointer)
  (unless do-not-free (foreign-free pointer)))

;;; Helper function for debugging
(defun null-or-nullpointer (obj)
  (or (not obj)
      (typecase obj
    (git-pointer (null-pointer-p (pointer obj)))
    (t (null-pointer-p obj)))))

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
