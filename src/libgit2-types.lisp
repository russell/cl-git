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

(defctype off-t :int64)
(defctype object-size-t :uint64)

(define-foreign-type time-t ()
  nil
  (:actual-type :int64)
  (:simple-parser %time-t))

(defcenum (git-filemode-t :uint16)
  (:unreadable #o0000000)
  (:tree #o0040000)
  (:blob #o0100644)
  (:blob-executable #o0100755)
  (:link #o0120000)
  (:commit #o0160000))
