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

(def-test blob-content (:fixture repository-with-commit)
  ;; TODO (RS) this test sucks, it should be sorting the tree object
  ;; at the start.
  (let* ((commit (next-test-commit))
         (object (get-object 'commit (getf commit :sha) *test-repository*)))
    (is
     (equal
      (sort-strings
       (mapcar (compose #'namestring #'filename)
               (tree-directory (get-tree object))))
      (sort-strings
       (mapcar (lambda (e) (getf e :filename))
               (getf commit :files)))))
    (is
     (equal
      (sort-strings
        (mapcar (lambda (e) (getf e :text))
                (getf commit :files)))
      (sort-strings
       (mapcar (compose #'(lambda (o) (octets-to-string o :external-format :utf-8))
                        #'blob-content)
               (tree-directory (get-tree object))))))))
