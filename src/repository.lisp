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


(in-package #:cl-git)

(defparameter *git-repository* nil
  "A global that stores a pointer to the current Git repository.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Git Repositories
(defctype git-repository :pointer)
(defctype git-repository-index :pointer)

;;; Git Config

(defcfun ("git_repository_config" %git-repository-config)
    :int
  (out :pointer)
  (repository :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun git-repository-init (path &optional bare)
  "Init a new Git repository.  A positive value for BARE init a bare
repository.  Returns the path of the newly created Git repository."
  (with-foreign-object (repo :pointer)
    (handle-git-return-code
     (foreign-funcall "git_repository_init"
                      git-repository repo
                      :string (namestring path)
                      :unsigned-int (if bare 1 0)
                      :int))
    (foreign-funcall "git_repository_free"
                     git-repository (mem-ref repo :pointer)
                     :void)
    path))

(defun git-repository-open (path)
  "Open an existing repository and set the global *GIT-REPOSITORY*
variable to the open repository.  If the PATH contains a .git
directory it will be opened instead of the specified path."
  (assert (null-or-nullpointer *git-repository-index*))
  (assert (null-or-nullpointer *git-repository*))
  (with-foreign-object (repository-ref :pointer)
    (let ((path (or (cl-fad:directory-exists-p
                     (merge-pathnames
                      #p".git/"
                      (cl-fad:pathname-as-directory path)))
                    (truename path))))
      (with-foreign-strings ((%path (namestring path)))
        (handle-git-return-code
         (foreign-funcall "git_repository_open"
                          git-repository repository-ref
                          :string (namestring path)
                          :int)))
      (with-foreign-object (repository-ref1 :pointer)
        (setf repository-ref1 (mem-ref repository-ref :pointer))
        (setf repository-ref (mem-ref repository-ref :pointer))
        (finalize repository-ref
                  (lambda ()
                    (foreign-funcall "git_repository_free"
                                     :pointer repository-ref1
                                     :void)))
        repository-ref))))


(defun ensure-git-repository-exist (path &optional bare)
  "Open a repository at location, if the repository doesn't exist
create it.  BARE is an optional keyword, if specified then the newly
created repository will be bare."
  (handler-case
      (progn
        (git-repository-open path)
        path)
    (git-error ()
      (git-repository-init path bare)
      path)))

(defun git-repository-config ()
  "Return the config object of the current open repository."
  (assert (not (null-or-nullpointer *git-repository*)))
  (with-foreign-object (config :pointer)
    (handle-git-return-code (%git-repository-config config *git-repository*))
    (mem-ref config :pointer)))

(defmacro with-git-repository ((path) &body body)
  "Evaluates the body with *GIT-REPOSITORY* bound to a newly opened
repositony at path."
  `(let ((*git-repository* (git-repository-open ,path)))
     ,@body))
