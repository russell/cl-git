;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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

(defvar +success+ 0
  "Used to signal success for C return values in callbacks.")

(defbitfield git-capabilities
  (:threads 1)
  (:https 2)
  (:ssh 4))

(define-foreign-library libgit2
  (:linux "libgit2.so.1.3.0")
  (:windows "libgit2.dll")
  (:darwin "libgit2.0.dylib")
  (:default "libgit2"))

(unless (foreign-library-loaded-p 'libgit2)
  (use-foreign-library libgit2))

(defcfun ("git_libgit2_features" libgit2-features)
  git-capabilities
  "Return a list of the libgit2 capabilities, possible values in the
list return values are :THREADS and :HTTPS.")

(defcfun ("git_libgit2_version" %git-version)
    :void
  (major :pointer)
  (minor :pointer)
  (revision :pointer))

(defcfun ("git_libgit2_init" git-init)
    :void
    "Init libgit2.")

(defcfun ("git_libgit_shutdown" git-shutdown)
    :void
    "Shutdown libgit2.")

;;; Init threading on load
(eval-when (:load-toplevel :execute)
  (git-init))


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

(defmethod translate-name-to-foreign ((lisp-name symbol)
                                      (package (eql *package*))
                                      &optional varp)
  (let ((name (translate-underscore-separated-name lisp-name)))
    (if varp
        (string-trim '(#\* #\%) name)
        (string-trim '(#\%) name))))
