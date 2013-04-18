;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
;; Copyright (C) 2012 Willem Rein Oudshoorn <woudshoo@xs4all.nl>
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

(defcfun ("git_remote_list" %git-remote-list)
    %return-value
  (strings :pointer)
  (repository %repository))

(defcfun ("git_remote_load" %git-remote-load)
    %return-value
  (remote-out :pointer)
  (repository %repository)
  (name :string))

(defcfun ("git_remote_free" %git-remote-free)
    :void
  (remote %remote))

(defcfun ("git_remote_pushurl" git-push-url)
    :string
  (remote %remote))

(defcfun ("git_remote_url" git-url)
    :string
  (remote %remote))

(defcfun ("git_remote_name" %git-remote-name)
    :string
  (remote %remote))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass remote (git-pointer) ())

(defmethod git-list ((class (eql :remote))
             &key (repository *git-repository*))
  (with-foreign-object (string-array 'git-strings)
    (%git-remote-list string-array repository)
    (prog1 
	(convert-from-foreign string-array '%git-strings)
      (free-translated-object string-array '%git-strings t))))

(defmethod git-load ((class (eql :remote))
             name &key (repository *git-repository*))
  (with-foreign-object (remote-out :pointer)
    (%git-remote-load remote-out repository name)
    (make-instance 'remote
           :pointer (mem-ref remote-out :pointer)
           :facilitator repository
           :free-function #'%git-remote-free)))

(defmethod git-name ((remote remote))
  (%git-remote-name remote))
