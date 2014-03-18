;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;
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


(def-test diff-revisions (:fixture repository-with-changes)
  (let ((diffs (diff commit1 commit2)))
    (is (eq (diff-deltas-count diffs) 1))
    (is (equal (diff-deltas-summary diffs)
               '((:status :modified :similarity 0 :flags 0
                  :file-a (:mode 33188
                           :flags (:valid-oid)
                           :size 0
                           :path "test-file"
                           :oid 97787706012661474925191056142692387097255677107)
                  :file-b (:mode 33188
                           :flags (:valid-oid)
                           :size 0
                           :path "test-file"
                           :oid 243568240973109882797341286687005129339258402139)))))
    (is (equal (make-patch diffs)
               `((:patch ,repository-with-changes-diff
                  :status :modified
                  :similarity 0
                  :flags 2
                  :file-a (:mode 33188
                           :flags (:not-binary :valid-oid)
                           :size 902
                           :path "test-file"
                           :oid
                           97787706012661474925191056142692387097255677107)
                  :file-b (:mode 33188
                           :flags (:not-binary :valid-oid)
                           :size 919
                           :path "test-file"
                           :oid
                           243568240973109882797341286687005129339258402139)))))))
