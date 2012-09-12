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

(defctype git-repository-index :pointer) ;; TODO move to index.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass repository (git-pointer) ())


(defmethod git-init ((class (eql 'repository)) (path string) 
		     &key bare &allow-other-keys)
  "Init a new Git repository.  A positive value for BARE init a bare
repository.  Returns the path of the newly created Git repository."
  (with-foreign-object (repository-ref :pointer)
    (%git-repository-init repository-ref path bare)
    (make-instance 'repository
		   :pointer (mem-ref repository-ref :pointer)
		   :free-function #'git-repository-free)))

(defmethod git-init ((class (eql 'repository)) (path pathname) &rest r)
  (apply #'git-init class (namestring path) r))

(defmethod git-open ((class (eql 'repository)) (path string) &key &allow-other-keys)
  "Open an existing repository located at PATH."
  (with-foreign-object (repository-ref :pointer)
    (%git-repository-open repository-ref path)
    (make-instance 'repository
		   :pointer (mem-ref repository-ref :pointer)
		   :free-function #'git-repository-free)))

(defmethod git-open ((class (eql 'repository)) (path pathname) &rest r)
  (apply #'git-open class (namestring path) r))



(defmethod git-config ((repository repository))
  (with-foreign-object (config :pointer)
    (%git-repository-config config repository)
    (make-instance 'config
		   :pointer (mem-ref config :pointer)
		   :facilitator repository
		   :free-function #'git-config-free)))

(defmethod git-index ((repository repository))
  (with-foreign-object (index :pointer)
    (%git-repository-index index repository)
    (make-instance 'index
		   :pointer (mem-ref index :pointer)
		   :facilitator repository
		   :free-function #'%git-index-free)))

(defmacro with-repository ((path) &body body)
  "Evaluates the body with *GIT-REPOSITORY* bound to a newly opened
repositony at path."
  `(let ((*git-repository* (git-open 'repository ,path)))
     (unwind-protect 
	  (progn 
	    ,@body)
       (git-free *git-repository*))))
