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

(def-test commit ()
  "create a repository and add a file to it."
  (with-test-repository ()
    (let ((test-commit (make-test-revision)))  ;; get first commit
      (bind-git-commits (((commit :sha (getf test-commit :sha))) *test-repository*)
        (commit-equal test-commit commit)))))


(def-test default-signature ()
  "Test to make sure that if there is no time or email address in the
signature then it will be added automatically."
  (with-test-repository ()
    (let ((test-pre-create (timestamp-to-unix (now))))
      (let ((test-post-create (timestamp-to-unix (now)))
            (test-commit (make-test-revision :author (list :name (random-string 50)))))
        (bind-git-commits (((commit :sha (getf test-commit :sha))) *test-repository*)
          ;; set the email address the test data to the default.
          (setf (getf (getf test-commit :author) :email) (cl-git::default-email))
          ;; test that the time is correct
          (let ((created (getf (getf (commit-to-alist commit) :author) :time)))
            (is (<= (timestamp-to-unix created) test-post-create))
            (is (>= (timestamp-to-unix created) test-pre-create))
            (setf (getf (getf test-commit :author) :time) created))
          (commit-equal test-commit commit))))))


(def-test custom-signature-time ()
  "test if the time is an integer then it will be parsed correctly."
  (with-test-repository ()
    (let ((test-commit (make-test-revision
                        :author (list :name (random-string 50)
                                      :email "test@localhost"
                                      :time 1111111111))))
      (bind-git-commits (((commit :sha (getf test-commit :sha))) *test-repository*)
        (commit-equal test-commit commit)))))


(def-test commit-parents (:fixture repository-with-commits)
  (let ((test-commit (next-test-commit)))
    (is (equal
         (oid (car (parents (get-object 'commit (getf test-commit :sha)
                                        *test-repository*))))
         (getf (next-test-commit) :sha)))
    (is (equal
         (length (parents (get-object 'commit (getf test-commit :sha)
                                      *test-repository*)))
         1))
    ;; XXX (RS) this is a very low level test to make sure that the
    ;; lazy oid loading is working.  It should probably be covered in
    ;; a higher level test once one is written.
    (is
     (pointerp
      (cl-git::pointer
       (car
        (parents
         (get-object 'commit (getf test-commit :sha)
                     *test-repository*))))))))
