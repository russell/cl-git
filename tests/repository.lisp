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

(test repository-init
  "Create a repository and open it."
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
           (progn
             (git-init :repository path :bare t)
             (is (typep (git-open :repository path) 'repository)))
        (progn
          (delete-directory-and-files path))))))

(test with-repository
  "Create a repository and test the with-repository macro."
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
	   (progn
	     (git-init :repository path :bare t)
	     (with-repository (path)
           (is (typep *git-repository* 'repository))))
	(progn
	  (delete-directory-and-files path))))))


(def-test repository-path (:fixture (repository :bare t))
  (is
   (equal
    (git-path *git-repository*)
    (namestring *repository-path*))))


(def-test repository-path-workdir-bare (:fixture (repository :bare t))
  (is
   (equal
    (git-workdir *git-repository*)
    nil)))

(def-test repository-path-workdir (:fixture (repository))
  (is
   (equal
    (git-workdir *git-repository*)
    (namestring *repository-path*))))

(def-test repository-head (:fixture (repository))
  (let ((test-commit (make-test-revision)))
    (is
     (commit-equal
      (git-target (git-head *git-repository*))
      test-commit))))

(def-test repository-head-detached (:fixture (repository))
  "Confirm that the current head is detached then check that not."
  (make-test-revision)
  ;; TODO add negative test
  (is (equal
       (git-head-detached *git-repository*)
       nil)))

(def-test is-repository-empty (:fixture (repository))
  "Check that the repository is empty."
  (is (eq (git-repository-is-empty *git-repository*) t))
  (make-test-revision)
  (is (eq (git-repository-is-empty *git-repository*) nil)))

(def-test is-repository-bare (:fixture (repository :bare t))
  "Check that the repository is bare."
  (is
   (eq
    (git-repository-is-bare *git-repository*)
    t)))

(def-test repository-head-orphaned (:fixture (repository))
  "Confirm that the current head is orphaned then check that not."
  ;; confirm head is orphaned
  (is (equal
       (git-head-orphaned *git-repository*)
       t))
  (make-test-revision)
  ;; confirm head no longer orphaned
  (is (equal
       (git-head-orphaned *git-repository*)
       nil)))
