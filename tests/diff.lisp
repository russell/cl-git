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

(defun sort-flags (object &rest key-path)
  (loop :for key :in (butlast key-path)
        :for obj = (getf (or obj object) key)
        :finally (setf (getf obj (car (last key-path)))
                       (stable-sort (getf obj (car (last key-path))) #'string<)))
  object)

(defun sort-diff-flags (diff)
  (loop :for patch :in diff
        :collect (sort-flags (sort-flags patch :file-a :flags) :file-b :flags)))

(def-test diff-revisions (:fixture repository-with-changes)
  (let ((diffs (diff commit1 commit2)))
    (is (eq (diff-deltas-count diffs) 1))
    (is (equal '((:status :modified
                  :similarity 0
                  :flags (:not-binary)
                  :file-a (:id-abbrev 40
                           :mode :blob
                           :flags (:not-binary :valid-oid :exists)
                           :size 902
                           :path "test-file"
                           :oid 97787706012661474925191056142692387097255677107)
                  :file-b (:id-abbrev 40
                           :mode :blob
                           :flags (:not-binary :valid-oid :exists)
                           :size 919
                           :path "test-file"
                           :oid 243568240973109882797341286687005129339258402139)))
               (diff-deltas-summary diffs)))
    (is (equal `((:patch ,repository-with-changes-diff
                  :status :modified
                  :similarity 0
                  :flags (:not-binary)
                  :file-a (:id-abbrev 40
                           :mode :blob
                           :flags (:exists :not-binary :valid-oid)
                           :size 902
                           :path "test-file"
                           :oid
                           97787706012661474925191056142692387097255677107)
                  :file-b (:id-abbrev 40
                           :mode :blob
                           :flags (:exists :not-binary :valid-oid)
                           :size 919
                           :path "test-file"
                           :oid
                           243568240973109882797341286687005129339258402139)))
               (sort-diff-flags (make-patch diffs))))))


(def-test diff-working (:fixture repository-with-unstaged)
  (let ((diffs (diff *test-repository* (open-index *test-repository*))))
    (is (eq (diff-deltas-count diffs) 1))
    (is (equal '((:status :modified
                  :similarity 0
                  :flags (:not-binary)
                  :file-a (:id-abbrev 40
                           :mode :blob
                           :flags (:not-binary :valid-oid :exists)
                           :size 902
                           :path "test-file"
                           :oid 97787706012661474925191056142692387097255677107)
                  :file-b (:id-abbrev 40
                           :mode :blob
                           :flags (:not-binary :valid-oid :exists)
                           :size 919
                           :path "test-file"
                           :oid 243568240973109882797341286687005129339258402139)))
               (diff-deltas-summary diffs)))
    (is (equal `((:patch ,repository-with-changes-diff
                  :status :modified
                  :similarity 0
                  :flags (:not-binary)
                  :file-a (:id-abbrev 40
                           :mode :blob
                           :flags (:exists :not-binary :valid-oid)
                           :size 902
                           :path "test-file"
                           :oid
                           97787706012661474925191056142692387097255677107)
                  :file-b (:id-abbrev 40
                           :mode :blob
                           :flags (:exists :not-binary :valid-oid)
                           :size 919
                           :path "test-file"
                           :oid
                           243568240973109882797341286687005129339258402139)))
               (sort-diff-flags (make-patch diffs))))))


(def-test diff-staged (:fixture repository-with-staged)
  (let ((diffs (diff commit1 (open-index *test-repository*))))
    (is (eq (diff-deltas-count diffs) 1))
    (is (equal '((:status :modified
                  :similarity 0
                  :flags (:not-binary)
                  :file-a (:id-abbrev 40
                           :mode :blob
                           :flags (:not-binary :valid-oid :exists)
                           :size 902
                           :path "test-file"
                           :oid 97787706012661474925191056142692387097255677107)
                  :file-b (:id-abbrev 40
                           :mode :blob
                           :flags (:not-binary :valid-oid :exists)
                           :size 919
                           :path "test-file"
                           :oid 243568240973109882797341286687005129339258402139)))
               (diff-deltas-summary diffs)))
    (is (equal `((:patch ,repository-with-changes-diff
                  :status :modified
                  :similarity 0
                  :flags (:not-binary)
                  :file-a (:id-abbrev 40
                           :mode :blob
                           :flags (:exists :not-binary :valid-oid)
                           :size 902
                           :path "test-file"
                           :oid
                           97787706012661474925191056142692387097255677107)
                  :file-b (:id-abbrev 40
                           :mode :blob
                           :flags (:exists :not-binary :valid-oid)
                           :size 919
                           :path "test-file"
                           :oid
                           243568240973109882797341286687005129339258402139)))
               (sort-diff-flags (make-patch diffs))))))
