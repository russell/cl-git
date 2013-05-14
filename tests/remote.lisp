;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2012-2013 Russell Sim <russell.sim@gmail.com>
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

(def-test list-remotes (:fixture repository-with-commits)
  "Create a new repository and check it's remotes."
  (is
   (eq
    (git-list :remote)
    nil)))

(def-test create-remote (:fixture repository-with-commits)
  "Create a remote and check it's details."
  (git-create :remote "origin" :url "/dev/null" :repository *git-repository*)
  (is
   (equal
    (git-list :remote)
    '("origin")))
  (is
   (equal
    (git-url (git-load :remote "origin"))
    "/dev/null")))


(def-test fetch-remotes (:fixture repository-with-commits)
  "Create a new repository and check it's remotes."
  (let ((remote-repo-path (gen-temp-path)))
    (unwind-protect
	     (git-init :repository remote-repo-path)
         (with-repository (remote-repo remote-repo-path)
           (git-create :remote "origin"
                       :url (concatenate 'string "file://" (namestring *repository-path*))
                       :repository remote-repo)
           (let ((remote (git-load :remote "origin" :repository remote-repo)))
             (git-connect remote)
             (git-download remote)))
      (progn
        (delete-directory-and-files remote-repo-path)))))
