;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
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

(test repository-init
  "Create a repository and open it."
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
           (progn
             (git-init :repository path :bare t)
             (is (typep (git-open :repository path) 'repository)))
        (progn
          (delete-directory-and-files path))))))


(test with-repository
  "Create a repository and test the with-repository macro."
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
	   (progn
	     (git-init :repository path :bare t)
	     (with-repository (path)
           (is (typep *git-repository* 'repository))))
	(progn
	  (delete-directory-and-files path))))))
