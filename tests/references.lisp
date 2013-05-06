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
           (ref-default (git-create :reference "refs/heads/oid"
                                    :target (getf test-commit :sha)))
           (ref-symbolic (git-create :reference "refs/heads/symbolic"
                                     :target "refs/heads/oid"
                                     :type :symbolic)))
      (declare (ignorable ref-symbolic) (ignorable ref-default))
      (&body))))

(def-fixture reference ()
  (with-test-repository ()
      (let ((test-commit (make-test-revision)))
        (git-create :reference "refs/heads/oid"
                    :target (getf test-commit :sha)))
    (git-create :reference "refs/heads/symbolic"
                :target "refs/heads/oid"
                :type :symbolic)
    (&body)))

(def-test references-list-oid (:fixture reference)
  (is
   (equal ;; test the git-list default args.
    (sort-strings (list "refs/heads/oid" "refs/heads/master"))
    (sort-strings (git-list :reference)))))

(def-test references-list-symbolic (:fixture reference)
  (is
   (equal
    (sort-strings (list "refs/heads/oid" "refs/heads/symbolic" "refs/heads/master"))
    (sort-strings (git-list :reference :flags '(:oid :symbolic))))))

(def-test reference-lookup-oid (:fixture reference)
  (is
   (equal (git-name (git-lookup :reference "refs/heads/oid"))
          "refs/heads/oid")))

(def-test reference-lookup-symbolic (:fixture reference)
  (is
   (equal (git-name (git-lookup :reference "refs/heads/symbolic"))
          "refs/heads/symbolic")))

(def-test reference-accessors-oid (:fixture reference-with-context)
  "Create oid reference and check accessors."
  (is (equal
       (git-type ref-default)
       '(:OID)))
  (is (equal
       (git-name ref-default)
       "refs/heads/oid")))

(def-test reference-accessors-symbolic (:fixture reference-with-context)
  "Create symbolic reference and check it's name."
  (is (equal
       (git-type ref-symbolic)
       '(:SYMBOLIC)))
  (is (equal
       (git-name ref-symbolic)
       "refs/heads/symbolic")))

(def-test reference-target-oid (:fixture reference-with-context)
  "Check that the returned commit id matches the id from the reference
fixture"
  (is (equal
       (git-id (git-target ref-default))
       (getf test-commit :sha)))
  (is (equal
       (git-target ref-default :type :oid)
       (getf test-commit :sha))))

(def-test reference-target-symbolic (:fixture reference-with-context)
  "Check that the returned commit id matches the id from the reference
fixture"
  (signals unresolved-reference-error
    (git-id (git-target ref-symbolic)))
  (is (equal
       (git-id (git-target (git-resolve ref-symbolic)))
       (getf test-commit :sha))))


(def-test reference-is-branch (:fixture reference)
  "Check that the ref is a branch."
  (is (equal
       (git-is-branch (git-lookup :reference "refs/heads/oid"))
       t)))

(def-test reference-is-not-remote (:fixture reference)
  "Check that the ref is a branch."
  (is (equal
       (git-is-remote (git-lookup :reference "refs/heads/oid"))
       nil)))

;; TODO add test for the positive case
(def-test reference-has-reflog (:fixture reference-with-context)
  "Check that the ref has a reflog."
  (is (equal
       (git-has-log ref-default)
       nil)))
