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
  (tempory-repository
      (path)
    (cl-git:with-repository (path)
      (create-random-commits path 10))
    (cl-git:with-repository (path)
      (let* ((commit-list (create-random-commits path 10))
             (tcommit (pop commit-list)))
        (cl-git:with-git-revisions
            (commit :sha (assoc-default 'commit-sha tcommit))
          (is (equal (cl-git:git-message commit)
                     (assoc-default 'commit-message tcommit)))
          (let ((tauthor (assoc-default 'author tcommit))
                (author (cl-git:git-author commit)))
            (is (equal (getf author :name)
                       (assoc-default 'name tauthor)))
            (is (equal (getf author :email)
                       (assoc-default 'email tauthor))))
          (let ((tcommitter (assoc-default 'committer tcommit))
                (committer (cl-git:git-committer commit)))
            (is (equal (getf committer :name)
                       (assoc-default 'name tcommitter)))
            (is (equal (getf committer :email)
                       (assoc-default 'email tcommitter))))
          (setq tcommit (pop commit-list)))))))


(test revision-walker-test
  "create a repository and add several random commits to it. then
check that the commit messages match the expected messages."
  (tempory-repository
      (path)
    (cl-git:with-repository (path)
      (create-random-commits path 10))
    (cl-git:with-repository (path)
      (let* ((commit-list (create-random-commits path 10))
             (tcommit (pop commit-list)))
        (let ((walker (revision-walk (assoc-default 'commit-sha tcommit) :flags :sha)))
          (do ((commit (git-next walker) (git-next walker)))
              ((null commit))
            (is (equal (cl-git:git-message commit)
                       (assoc-default 'commit-message tcommit)))
            (let ((tauthor (assoc-default 'author tcommit))
                  (author (cl-git:git-author commit)))
              (is (equal (getf author :name)
                         (assoc-default 'name tauthor)))
              (is (equal (getf author :email)
                         (assoc-default 'email tauthor))))
            (let ((tcommitter (assoc-default 'committer tcommit))
                  (committer (cl-git:git-committer commit)))
              (is (equal (getf committer :name)
                         (assoc-default 'name tcommitter)))
              (is (equal (getf committer :email)
                         (assoc-default 'email tcommitter))))
            (setq tcommit (pop commit-list))))))))
