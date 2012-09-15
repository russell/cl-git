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
  "create a repository and open it to make sure that works"
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
           (progn
             (cl-git:git-init :repository path :bare t)
             (cl-git:git-open :repository path))
        (progn
          (cl-fad:delete-directory-and-files path))))))


(test with-repository
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
	   (progn 
	     (cl-git:git-init :repository path :bare t)
	     (with-repository (path)))
	(progn
	  (cl-fad:delete-directory-and-files path))))))
