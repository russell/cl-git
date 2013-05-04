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


(in-package #:cl-git-tests)

(in-suite :cl-git)

(test create-commit
  "create a repository and add a file to it."
  (with-test-repository
    (make-test-revisions 1)  ;; set up a fixture
    (let ((test-commit (next-test-commit)))  ;; get first commit
      (with-git-revisions
          (commit :sha (getf test-commit :sha))
        (commit-equal test-commit commit)))))


(test create-commit-custom-signature
  "test if the time is an integer, and test when there is no email
address specified."
  (with-test-repository
    (add-test-revision :author (list :name (random-string 50)
                                     :email (random-string 50)
                                     :time 1111111111)) ;; set up a fixture
    (let ((test-commit (first *test-repository-state*))) ;; get first commit
      (with-git-revisions
          (commit :sha (getf test-commit :sha))
        (commit-equal test-commit commit)))))


(test create-commit-default-time
  "Test to make sure that if there is no time in the signature then it
will be added automatically."
  (with-test-repository
    (let ((test-pre-create (timestamp-to-unix (now))))
      (add-test-revision :author (list :name (random-string 50)
                                       :email (random-string 50))) ;; set up a fixture
      (let ((test-post-create (timestamp-to-unix (now)))
            (test-commit (first *test-repository-state*))) ;; get first commit
        (with-git-revisions
            (commit :sha (getf test-commit :sha))
          (let ((created (getf (getf (commit-to-alist commit) :author) :time)))
            (is (<= (timestamp-to-unix created) test-post-create))
            (is (>= (timestamp-to-unix created) test-pre-create))
            (setf (getf (getf test-commit :author) :time) created))
          (commit-equal test-commit commit))))))


(test create-commit-custom-signature
  "test if the time is an integer, and test when there is no email
address specified."
  (with-test-repository
    (add-test-revision :author (list :name (random-string 50)
                                     :email (random-string 50)
                                     :time 1111111111)) ;; set up a fixture
    (let ((test-commit (first *test-repository-state*))) ;; get first commit
      (with-git-revisions
          (commit :sha (getf test-commit :sha))
        (commit-equal test-commit commit)))))
