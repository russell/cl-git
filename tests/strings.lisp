;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2013 Russell Sim <russell.sim@gmail.com>
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

(test string-list
  (for-all ((strings-fixture (random-list)))
    (let ((str-pointer
            (cffi:translate-to-foreign strings-fixture
                                       (make-instance 'cl-git::git-strings-type))))
      (is
       (equal
        (cffi:translate-from-foreign str-pointer (make-instance 'cl-git::git-strings-type))
        strings-fixture))
      (cffi:free-translated-object str-pointer (make-instance 'cl-git::git-strings-type) nil))))
