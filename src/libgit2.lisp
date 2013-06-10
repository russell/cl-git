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

(defcfun ("git_libgit2_capabilities" libgit2-capabilities)
  git-capabilities
  "Return a list of the libgit2 capabilities, possible values in the
list return values are :THREADS and :HTTPS.")

(defcfun ("git_libgit2_version" %git-version)
    :void
  (major :pointer)
  (minor :pointer)
  (revision :pointer))

(defcfun ("git_threads_init" git-threads-init)
    :void
    "Init libgit2 threading.")

(defcfun ("git_threads_shutdown" git-threads-shutdown)
    :void
    "Shutdown libgit2 threading.")


;;; Init threading on load
(eval-when (:execute)
  (git-threads-init))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun libgit2-version ()
  "Returns the libgit2 C-library version number as a list of three integers,
\(major minor revision\)."
  (with-foreign-objects
      ((maj :int)
       (min :int)
       (rev :int))
    (%git-version maj min rev)
    (list (mem-ref maj :int)
      (mem-ref min :int)
      (mem-ref rev :int))))
