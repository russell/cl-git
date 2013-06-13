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
             (init-repository path :bare t)
             (is (typep (open-repository path) 'repository)))
        (progn
          (delete-directory-and-files path))))))

(def-test repository-path (:fixture (repository :bare t))
  (is
   (equal
    (repository-path *test-repository*)
    *repository-path*)))


(def-test repository-path-workdir-bare (:fixture (repository :bare t))
  (is
   (equal
    (repository-workdir *test-repository*)
    nil)))

(def-test repository-path-workdir (:fixture (repository))
  (is
   (equal
    (repository-workdir *test-repository*)
    *repository-path*)))

(def-test repository-head (:fixture (repository))
  (let ((test-commit (make-test-revision)))
    (is
     (commit-equal
      (target (repository-head *test-repository*))
      test-commit))))

(def-test repository-head-detached (:fixture (repository))
  "Confirm that the current head is detached then check that not."
  (make-test-revision)
  ;; TODO add negative test
  (is (equal
       (head-detached-p *test-repository*)
       nil)))

(def-test is-repository-empty (:fixture (repository))
  "Check that the repository is empty."
  (is (eq (empty-p *test-repository*) t))
  (make-test-revision)
  (is (eq (empty-p *test-repository*) nil)))

(def-test is-repository-bare (:fixture (repository :bare t))
  "Check that the repository is bare."
  (is
   (eq
    (bare-p *test-repository*)
    t)))

(def-test repository-head-orphaned (:fixture (repository))
  "Confirm that the current head is orphaned then check that not."
  ;; confirm head is orphaned
  (is (equal
       (head-orphaned-p *test-repository*)
       t))
  (make-test-revision)
  ;; confirm head no longer orphaned
  (is (equal
       (head-orphaned-p *test-repository*)
       nil)))

(def-test with-repository ()
  "Create a repository and test the with-repository macro."
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
	   (progn
	     (init-repository :repository path :bare t)
	     (with-repository (repository path)
           (is (typep repository 'repository))))
	(progn
	  (delete-directory-and-files path))))))
