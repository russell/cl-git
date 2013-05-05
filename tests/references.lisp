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

(test create-references
  "Create reference and check it's name."
  (with-test-repository
    (let* ((test-commit (make-test-revision))
           (ref-default (git-create :reference "refs/heads/oid"
                                    :target (getf test-commit :sha)))
           (ref-symbolic (git-create :reference "refs/heads/symbolic"
                                     :target "refs/heads/oid"
                                     :type :symbolic)))
      (is
       (equal ;; test the git-list default args.
        (sort-strings (list (git-name ref-default) "refs/heads/master"))
        (sort-strings (git-list :reference))))
      (is
       (equal
        (sort-strings (list (git-name ref-default) (git-name ref-symbolic) "refs/heads/master"))
        (sort-strings (git-list :reference :flags '(:oid :symbolic))))))))

(test reference-accessors-oid
  "Create oid reference and check accessors."
  (with-test-repository
    (let* ((test-commit (make-test-revision))
           (ref-default (git-create :reference "refs/heads/oid"
                                    :target (getf test-commit :sha))))
      (is (equal
           (git-type ref-default)
           '(:OID)))
      (is (equal
           (git-name ref-default)
           "refs/heads/oid")))))

(test reference-accessors-symbolic
  "Create symbolic reference and check it's name."
  (with-test-repository
    (let* ((test-commit (make-test-revision))
           (ref-default (git-create :reference "refs/heads/oid"
                                    :target (getf test-commit :sha)))
           (ref-symbolic (git-create :reference "refs/heads/symbolic"
                                     :target "refs/heads/oid"
                                     :type :symbolic)))
      (is (equal
           (git-type ref-symbolic)
           '(:SYMBOLIC)))
      (is (equal
           (git-name ref-symbolic)
           "refs/heads/symbolic")))))
