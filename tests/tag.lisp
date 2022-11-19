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


(def-test list-tags (:fixture repository)
  (is (equal (list-objects 'tag *test-repository*)
             nil))
  (let ((tag-name1 (random-string 50))
        (tag-name2 (random-string 50))
        (tag-message (random-string 500))
        (test-commit (make-test-revision :author (list :name (random-string 50)))))
    (bind-git-commits (((commit :sha (getf test-commit :sha))) *test-repository*)
      (let ((annotated-tag
              (make-object 'tag tag-name1
                            *test-repository*
                            :message tag-message
                            :target commit
                            :tagger `(:name ,(random-string 50)
                                      :email ,(random-string 50)
                                      :time ,(random-time))))
            (lightweight-tag
              (make-object 'tag tag-name2
                            *test-repository*
                            :type :lightweight
                            :target commit)))))
    (is (equal
         (sort
          (mapcar #'short-name (list-objects 'tag *test-repository*))
          #'string-greaterp)
         (sort
          (list tag-name1 tag-name2)
          #'string-greaterp)))))

(def-fixture annotated-tag-with-context ()
  (with-test-repository ()
    (let* ((test-commit (make-test-revision))
           (tag (make-object 'tag
                             "test-tag"
                             *test-repository*
                             :type :annotated
                             :target (get-object 'commit
                                                 (getf test-commit :sha)
                                                 *test-repository*)
                             :message "this is an annotated tag"
                             :tagger `(:name ,(random-string 50)
                                       :email ,(random-string 50)
                                       :time ,(random-time)))))
      (&body))))

(def-test tag-accessors-annotated (:fixture annotated-tag-with-context)
  "Create an annotated tag and check accessors."
  (is (equal
       (full-name tag)
       "refs/tags/test-tag"))
  (is (equal
       (short-name tag)
       "test-tag")))


(def-fixture lightweight-tag-with-context ()
  (with-test-repository ()
    (let* ((test-commit (make-test-revision))
           (tag (make-object 'tag
                             "test-tag"
                             *test-repository*
                             :type :lightweight
                             :target (get-object 'commit
                                                 (getf test-commit :sha)
                                                 *test-repository*))))
      (&body))))

(def-test tag-accessors-lightweight (:fixture lightweight-tag-with-context)
  "Create lightweight tag and check accessors."
  (is (equal
       (full-name tag)
       "refs/tags/test-tag"))
  (is (equal
       (short-name tag)
       "test-tag")))
