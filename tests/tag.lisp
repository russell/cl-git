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


(def-test list-tags (:fixture repository)
  (is (equal (list-objects 'tag *test-repository*)
             nil))
  (let ((tag-name (random-string 50))
        (tag-message (random-string 500))
        (test-commit (make-test-revision :author (list :name (random-string 50)))))
    (bind-git-commits (((commit :sha (getf test-commit :sha))) *test-repository*)
      (let ((tag
              (make-tag tag-name tag-message
                        :repository *test-repository*
                        :target commit
                        :tagger (list :name (random-string 50)
                                      :email (random-string 50)
                                      :time (random-time)))))
        (is (equal (full-name tag)
                   (concatenate 'string "refs/tags/" tag-name)))
        (is (equal (short-name tag)
                   tag-name))))
    (is (equal (mapcar #'short-name (list-objects 'tag *test-repository*))
               (list tag-name)))))
