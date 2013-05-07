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

(def-fixture index-with-file (filename filetext)
  (with-test-repository ()
    (with-repository-index
      (write-string-to-file filename filetext)
      (&body))))

#+nil (def-test index-add-pathname (:fixture (index-with-file #P"test-file" "foo blah."))
  (git-add filename)
  (git-write *git-repository-index*)
  (is
   (equal
    (git-entries *git-repository-index*)
    t)))

#+nil (def-test index-add-string (:fixture (index-with-file "test-file" "foo blah."))
  (git-add filename)
  (git-write *git-repository-index*)
  (is
   (equal
    (git-entries *git-repository-index*)
    t)))

#+nil (def-test index-add-abspathname (:fixture (index-with-file
                                           (merge-pathnames (make-pathname :name "test-file")
                                                            *repository-path*)
                                           "foo blah."))
  (write-string-to-file filename text)
  (git-add filename)
  (is
   (equal
    (git-entries *git-repository-index*)
    t)))
