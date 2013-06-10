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

(def-fixture reference-with-context ()
  (with-test-repository ()
    (let* ((test-commit (make-test-revision))
           (ref-default (make-object 'reference "refs/heads/oid"
                                     *test-repository*
                                     :target (getf test-commit :sha)))
           (ref-symbolic (make-object 'reference "refs/heads/symbolic"
                                      *test-repository*
                                      :target "refs/heads/oid"
                                      :type :symbolic)))
      (declare (ignorable ref-symbolic) (ignorable ref-default))
      (&body))))

(def-fixture reference ()
  (with-test-repository ()
    (let ((test-commit (make-test-revision)))
      (make-object 'reference "refs/heads/oid"
                   *test-repository*
                   :target (getf test-commit :sha)))
    (make-object 'reference "refs/heads/symbolic"
                 *test-repository*
                 :target "refs/heads/oid"
                 :type :symbolic)
    (&body)))

(def-test references-list-oid (:fixture reference)
  (is
   (equal
    (sort-strings (mapcar #'full-name (list-objects 'reference *test-repository* :test-not #'symbolic-p)))
    (sort-strings (list "refs/heads/oid" "refs/heads/master")))))

(def-test references-list-symbolic (:fixture reference)
  (is
   (equal ;; test the list-objects default args.
    (sort-strings (mapcar #'full-name (list-objects 'reference *test-repository*)))
    (sort-strings (list "refs/heads/oid" "refs/heads/symbolic" "refs/heads/master")))))

(def-test reference-lookup-oid (:fixture reference)
  (let ((ref (get-object 'reference "refs/heads/oid" *test-repository*)))
      (is
       (equal (full-name ref)
              "refs/heads/oid"))
    (is
     (equal (type-of ref)
            'reference))))

(def-test reference-lookup-symbolic (:fixture reference)
  (let ((ref (get-object 'reference "refs/heads/symbolic" *test-repository*)))
    (is
     (equal (full-name ref)
            "refs/heads/symbolic"))
    (is
     (equal (type-of ref)
            'reference))))

(def-test reference-accessors-oid (:fixture reference-with-context)
  "Create oid reference and check accessors."
  (is (equal
       (symbolic-p ref-default)
       nil))
  (is (equal
       (full-name ref-default)
       "refs/heads/oid"))
  (is (equal
       (short-name ref-default)
       "oid")))

(def-test reference-accessors-symbolic (:fixture reference-with-context)
  "Create symbolic reference and check it's name."
  (is (equal
       (symbolic-p ref-symbolic)
       t))
  (is (equal
       (full-name ref-symbolic)
       "refs/heads/symbolic"))
  (is (equal
       (short-name ref-symbolic)
       "symbolic")))

(def-test reference-target-oid (:fixture reference-with-context)
  "Check that the returned commit id matches the id from the reference
fixture"
  (is (equal
       (oid (target ref-default))
       (getf test-commit :sha))))

(def-test reference-target-symbolic (:fixture reference-with-context)
  "Check that the returned commit id matches the id from the reference
fixture"
  (is (equal
       (oid (target ref-symbolic))
       (getf test-commit :sha))))


(def-test reference-is-branch (:fixture reference)
  "Check that the ref is a branch."
  (is (equal
       (branch-p (get-object 'reference "refs/heads/oid" *test-repository*))
       t)))

(def-test reference-is-not-remote (:fixture reference)
  "Check that the ref is a branch."
  (is (equal
       (remote-p (get-object 'reference "refs/heads/oid" *test-repository*))
       nil)))

;; TODO add test for the positive case
(def-test reference-has-reflog (:fixture reference-with-context)
  "Check that the ref has a reflog."
  (is (equal
       (git-has-log ref-default)
       nil)))

(def-test reference-set-target (:fixture reference-with-context)
  "Check that the returned commit id matches the id from the reference
fixture"
  (let ((test-commit1 (make-test-revision)))
    (is (equal
         (oid (target ref-default))
         (getf test-commit :sha)))
    (setf (target ref-default) (getf test-commit1 :sha))
    (is (equal
         (oid (target ref-default))
         (getf test-commit1 :sha)))))
