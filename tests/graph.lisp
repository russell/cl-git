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

#-(or libgit2-0.27 libgit2-0.28 libgit2-1.0 libgit2-1.1)
(def-test graph-parents (:fixture repository-with-commits)
  (let* ((test-commit (next-test-commit))
         (test-commit-parents (parents (get-object 'commit
                                                   (getf test-commit :sha)
                                                   *test-repository*))))
    (is (eql
         nil
         (reachable-from *test-repository*
                         (get-object 'commit
                                     (getf test-commit :sha)
                                     *test-repository*)
                         test-commit-parents)))
    (is (eql
         t
         (reachable-from *test-repository*
                         (car test-commit-parents)
                         (list
                          (get-object 'commit
                                     (getf test-commit :sha)
                                     *test-repository*)))))))
