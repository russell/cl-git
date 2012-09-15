;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
;; Copyright (C) 2012 Willem Rein Oudshoorn <woudshoo@xs4all.nl>
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

(defbitfield git-capabilities
  (:threads 1)
  (:https 2))

(defcfun ("git_libgit2_capabilities" git-capabilities)
    git-capabilities)

(defcfun ("git_libgit2_version" %git-version)
    :void
  (major :pointer)
  (minor :pointer)
  (revision :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun git-version ()
  "Returns the libgit2 C-library version number as a list of three integers,
\(major minor revision\)
"
  (with-foreign-objects
      ((maj :int)
       (min :int)
       (rev :int))
    (%git-version maj min rev)
    (list (mem-ref maj :int) 
	  (mem-ref min :int)
	  (mem-ref rev :int))))
