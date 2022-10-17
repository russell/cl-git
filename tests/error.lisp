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


(in-package #:cl-git-tests)

(in-suite :cl-git)

(def-test error-conditions ()
  "Verify all error conditions exist."
  (is (equal
       nil
       (loop :for key :in (cffi:foreign-enum-keyword-list 'cl-git::git-error-code)
             :unless (gethash (cffi:foreign-enum-value 'cl-git::git-error-code key)
                              cl-git::error-conditions)
               :collect (format nil "~A from ~S missing from ~S"
                                key
                                'cl-git::git-error-code
                                'cl-git::error-conditions)))))
