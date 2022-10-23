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

(def-test clone-repository (:fixture repository-with-commits)
  "Create a new repository and clone it."
  (let ((remote-repo-path (gen-temp-path)))
    (unwind-protect
         (let ((cloned-repository
                 (clone-repository (namestring *repository-path*)
                                   remote-repo-path)))
           (is (eql
                (oid (repository-head *test-repository*))
                (oid (repository-head cloned-repository)))))
      (progn
        (delete-directory-and-files remote-repo-path)))))

(def-test clone-repository-https (:fixture repository-with-commits)
  "Create a new repository and clone it."
  (let ((remote-repo-path (gen-temp-path)))
    (unwind-protect
         (let ((cloned-repository
                 (clone-repository
                  "https://github.com/libgit2/TestGitRepository.git"
                  remote-repo-path
                  ;; TODO This is only really needed for my local
                  ;; machine that rewrites all github url's to be ssh.
                  ;; But it shuoldn't cause an issue here and still
                  ;; generally tests things.  Though not ideal
                  :credentials 'ssh-key-from-agent)))
           (is (eql
                ;; Hard coded oid
                417875169754799628935327977610761210040455787456
                (oid (repository-head cloned-repository)))))
      (progn
        (delete-directory-and-files remote-repo-path)))))

(def-test git-clone-options-struct ()
  "Verify initialising a GIT-CLONE-OPTIONS-STRUCT."
  (cffi:with-foreign-object (ptr '(:struct cl-git::git-clone-options))
    ;; change the last value in the struct to be something different
    (cffi:with-foreign-slots ((cl-git::remote-cb-payload)
                              ptr (:struct cl-git::git-clone-options))
      (setf cl-git::remote-cb-payload ptr))
    (cl-git::%git-clone-init-options ptr cl-git::+git-clone-options-version+)
    (cffi:with-foreign-slots ((cl-git::version cl-git::remote-cb-payload)
                               ptr (:struct cl-git::git-clone-options))
      ;; verify the first and last values of the struct, it should all be nil
      (is (eql cl-git::+git-clone-options-version+ cl-git::version))
      (is (cffi:null-pointer-p cl-git::remote-cb-payload)))))
