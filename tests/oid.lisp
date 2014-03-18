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


(in-package #:cl-git-tests)

(in-suite :cl-git)


(def-test oid ()
  "Test translating an oid"
  (let ((oid (cffi:translate-to-foreign "20d7c3344c81a77ca1e685f115b7d86b11c7908f"
                                         (make-instance 'cl-git::oid-type))))
    (is (cffi:pointerp oid))
    (is (equal (cffi:translate-from-foreign oid (make-instance 'cl-git::oid-type))
               187499369576181252898993308018266208451623882895))
    (cffi:free-translated-object oid (make-instance 'cl-git::oid-type) t)))


(def-test oid-struct ()
  "Test translating an oid struct"
  (let ((oid (cffi:convert-to-foreign "20d7c3344c81a77ca1e685f115b7d86b11c7908f"
                                        '(:struct cl-git::oid))))
    (is (cffi:pointerp oid))
    (is (equal (cffi:convert-from-foreign oid '(:struct cl-git::oid))
               187499369576181252898993308018266208451623882895))
    (cffi:free-converted-object oid '(:struct cl-git::oid) t)))
