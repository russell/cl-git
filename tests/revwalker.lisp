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


(test create-random-commits
  "create a repository and add several random commits to it. then
check that the commit messages match the expected messages."
  (with-test-repository ()
    (make-test-revisions 10)
    (let* ((commit-list *test-repository-state*)
           (tcommit (pop commit-list))
           (count 0))
      (with-git-revisions
          (commit :sha (getf tcommit :sha))
        (commit-equal tcommit commit)
        (setf count (1+ count))
        (setf tcommit (pop commit-list)))
      (is (equal count 10)))))


(test revision-walker-test
  "create a repository and add several random commits to it. then
check that the commit messages match the expected messages."
  (with-test-repository ()
    (make-test-revisions 10)
    (let* ((commit-list *test-repository-state*)
           (tcommit (pop commit-list))
           (count 0))
      (let ((walker (revision-walk (getf tcommit :sha) :flags :sha)))
        (do ((commit (git-next walker) (git-next walker)))
            ((null commit))
          (commit-equal tcommit commit)
          (setf count (1+ count))
          (setq tcommit (pop commit-list)))
        (is (equal count 10))))))
