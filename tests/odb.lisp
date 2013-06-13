;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2013 Russell Sim <russell.sim@gmail.com>
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


(def-test odb-list (:fixture repository-with-commits)
  "list all the odb objects in the database."
  (let ((oids (list-objects :oid *test-repository*)))
    ;; XXX (RS) this test is limited because we aren't currently
    ;; storing a list of the trees and blobs in the
    ;; *test-repository-state* variable.
    (mapcar (lambda (v) (is (member (getf v :sha) oids)))
            *test-repository-state*)))

(def-test odb-load (:fixture repository-with-commits)
  "load an object from the database."
  (let* ((commit (next-test-commit))
         (object (get-object 'odb-object (getf commit :sha) *test-repository*)))
    (is
     (equal
      (oid object)
      (getf commit :sha)))
    (is
     (equal
      (odb-type0 object)
      :commit))
    (is
     (search (getf (getf commit :author) :name)
      (octets-to-string (odb-data object) :external-format :utf-8)))))
