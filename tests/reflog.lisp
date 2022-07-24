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

(def-test reflog-entries (:fixture repository-with-commits)
  (let ((entries
          (entries
           (reflog
            (get-object 'reference "refs/heads/master" *test-repository*))))
        (commits (loop :for commit :in *test-traverse-state*
                       :collect (getf commit :message))))
    (is (equal
         (mapcar #'message entries)
         (concatenate 'list
                      (mapcar (lambda (e) (format nil "commit: ~a" e))
                              (subseq commits 0 (1- (length commits))))
                      (list (format nil "commit (initial): ~a"
                                    (car (last commits)))))))))

(def-test reflog-entry-count (:fixture repository-with-commits)
  (let ((entries
          (entries
           (reflog
            (get-object 'reference "refs/heads/master" *test-repository*))))
        (commits (loop :for commit :in *test-traverse-state*
                       :collect (getf commit :message))))
    (is (equal
         (length commits)
         (length entries)))))
