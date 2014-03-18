;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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


(def-fixture repository (&key bare)
  (with-test-repository (:bare bare)
    (&body)))


(def-fixture repository-with-commits ()
  (with-test-repository ()
    (make-test-revisions 10)
    (&body)))

(def-fixture repository-with-commit ()
  (with-test-repository ()
    (make-test-revisions 1)
    (&body)))


(def-fixture repository-with-changes ()
  (with-test-repository ()
    (let* ((commit1-content
             (make-test-commit
              (random-commit
               :files '((:filename "test-file"
                         :text "
Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Donec hendrerit tempor tellus.
Donec pretium posuere tellus.
Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.
Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.
Nulla posuere.
Donec vitae dolor.
Nullam tristique diam non turpis.
Cras placerat accumsan nulla.
Nullam rutrum.
Nam vestibulum accumsan nisl.
Aliquam erat volutpat.
Nunc eleifend leo vitae magna.
In id erat non orci commodo lobortis.
Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus.
Sed diam.
Praesent fermentum tempor tellus.
Nullam tempus.
Mauris ac felis vel velit tristique imperdiet.
Donec at pede.
Etiam vel neque nec dui dignissim bibendum.
Vivamus id enim.
Phasellus neque orci, porta a, aliquet quis, semper a, massa.
Phasellus purus.
Pellentesque tristique imperdiet tortor.
Nam euismod tellus id erat.

")))))
           (commit2-content
             (make-test-commit
              (random-commit
               :parents (getf commit1-content :sha)
               :files '((:filename "test-file"
                         :text "
Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Donec hendrerit tempor tellus.
Donec pretium posuere tellus.
Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.
Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.
Aliquam erat volutpat.
Nunc eleifend leo vitae magna.
In id erat non orci commodo lobortis.
Nullam tristique diam non turpis.
Cras placerat accumsan nulla.
Nullam rutrum.
Nam vestibulum accumsan nisl.
Aliquam erat volutpat.
Nunc eleifend leo vitae magna.
In id erat non orci commodo lobortis.
Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus.
Sed diam.
Praesent fermentum tempor tellus.
Nullam tempus.
Mauris ac felis vel velit tristique imperdiet.
Donec at pede.
Etiam vel neque nec dui dignissim bibendum.
Vivamus id enim.
Phasellus neque orci, porta a, aliquet quis, semper a, massa.
Phasellus purus.
Nam euismod tellus id erat.

"))))))
      (bind-git-commits (((commit1 :sha (getf commit1-content :sha))
                          (commit2 :sha (getf commit2-content :sha))) *test-repository* )
        (&body)))))

(eval-when (:compile-toplevel)
  (defvar repository-with-changes-diff1
    (with-open-file (stream (merge-pathnames "repository-with-changes.diff"
                                             *compile-file-truename*))
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string))))

(defvar repository-with-changes-diff repository-with-changes-diff1)
