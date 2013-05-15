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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Git Repositories

;;(defctype git-repository-index :pointer) ;; TODO move to index.

;;; Git Config
(defcfun ("git_repository_init" %git-repository-init)
    :int
  (repository :pointer)
  (path :string)
  (bare :boolean))

(defcfun ("git_repository_open" %git-repository-open)
    %return-value
  (repository :pointer)
  (path :string))

(defcfun ("git_repository_free" git-repository-free)
    :void
  (repository %repository))

(defcfun ("git_repository_config" %git-repository-config)
    %return-value
  (out :pointer)
  (repository %repository))

(defcfun ("git_repository_index" %git-repository-index)
    %return-value
  (index :pointer)
  (repository %repository))

(defcfun ("git_repository_odb" %git-repository-odb)
    %return-value
  (odb :pointer)
  (repository %repository))

(defcfun ("git_repository_head" %git-repository-head)
    %return-value
  (out :pointer)
  (repository %repository))

(defcfun ("git_repository_is_empty" git-repository-is-empty)
    :boolean
  "Return T if the repository is empty and contains no references."
  (repository %repository))

(defcfun ("git_repository_is_bare" git-repository-is-bare)
    :boolean
  "Return T if the repository is bare."
  (repository %repository))

(defcfun ("git_repository_head_detached" git-head-detached)
    :boolean
  "Returns T if the HEAD in the repository is detached, in other words,
the HEAD reference is not a symbolic reference to a branch, but a
direct commit."
  (repository %repository))

(defcfun ("git_repository_head_orphan" git-head-orphaned)
    :boolean
  "Returns t if the HEAD points to a commit that doesn't exist."
  (repository %repository))

(defcfun ("git_repository_path" %git-repository-path)
    :string
  (repository %repository))

(defcfun ("git_repository_workdir" %git-repository-workdir)
    :string
  (repository %repository))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass repository (git-pointer) ()
  (:documentation "Repository is the root type, it
contains the object database."))


(defmethod git-init ((class (eql :repository)) (path string)
             &key bare &allow-other-keys)
  (with-foreign-object (repository-ref :pointer)
    (%git-repository-init repository-ref path bare)
    (make-instance 'repository
           :pointer (mem-ref repository-ref :pointer)
           :free-function #'git-repository-free)))

(defmethod git-init ((class (eql :repository)) (path pathname) &rest r)
  "Open an existing repository located at PATH."
  (apply #'git-init class (namestring path) r))

(defmethod git-open ((class (eql :repository)) (path string) &key &allow-other-keys)
  "Open an existing repository located at PATH."
  (with-foreign-object (repository-ref :pointer)
    (%git-repository-open repository-ref path)
    (make-instance 'repository
           :pointer (mem-ref repository-ref :pointer)
           :free-function #'git-repository-free)))

(defmethod git-open ((class (eql :repository)) (path pathname) &rest r)
  (apply #'git-open class (namestring path) r))


(defmethod git-path ((repository repository))
  "Returns the path the the .git directory of the repository.
Or for a bare repository to the repository itself."
  (%git-repository-path repository))

(defmethod git-workdir ((repository repository))
  "Returns the working directory for the repository."
  (%git-repository-workdir repository))

(defmethod git-config ((repository repository) &key level)
  (let ((config
          (with-foreign-object (config :pointer)
            (%git-repository-config config repository)
            (make-instance 'config
                           :pointer (mem-ref config :pointer)
                           :facilitator repository
                           :free-function #'git-config-free))))
    (if level
        (git-config config :level level)
        config)))

(defmethod git-index ((repository repository))
  "Return the index of the repository."
  (with-foreign-object (index :pointer)
    (%git-repository-index index repository)
    (make-instance 'index
           :pointer (mem-ref index :pointer)
           :facilitator repository
           :free-function #'%git-index-free)))

(defmethod git-odb ((repository repository))
  (with-foreign-object (odb :pointer)
    (%git-repository-odb odb repository)
    (make-instance 'odb
           :pointer (mem-ref odb :pointer)
           :facilitator repository
           :free-function #'%git-odb-free)))

(defmethod git-head ((repository repository))
  "Returns the resolved reference for HEAD."
  (with-foreign-object (head :pointer)
    (%git-repository-head head repository)
    (make-instance 'reference
		   :pointer (mem-ref head :pointer)
		   :facilitator repository
		   :free-function #'%git-reference-free)))


(defmacro with-repository ((var &optional pathname-or-string) &body body)
  "Evaluates the body with VAR bound to a newly opened located
repository at PATHNAME-OR-STRING.  Repository is freed upon exit of
this scope so any objects that leave this scope will no longer be able
to access the repository."
  (if pathname-or-string
      `(let ((,var (git-open :repository ,pathname-or-string)))
         (unwind-protect
              (progn
                ,@body)
           (git-free ,var)))
      ;; XXX Backwards compatible version of the macro
      `(let ((*git-repository* (git-open :repository ,var)))
         (unwind-protect
              (progn
                ,@body)
           (git-free *git-repository*)))))
