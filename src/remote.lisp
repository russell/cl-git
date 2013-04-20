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

(defbitfield (refspec-flags :unsigned-int)
  :force
  :pattern
  :matching)

(defcstruct (git-refspec :class refspec-struct-type)
  (next :pointer)
  (src :string)
  (dst :string)
  (flags refspec-flags))

(defmethod translate-from-foreign (value (type refspec-struct-type))
  (translate-from-foreign value (make-instance 'refspec-type)))

(defmethod translate-from-foreign (value (type refspec-type))
  (unless (null-pointer-p value)
    (with-foreign-slots ((next src dst flags) value (:struct git-refspec))
			(cons (list  :src src
				     :dst dst
				     :flags flags)
			      (translate-from-foreign next type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defcfun ("git_remote_connected" %git-remote-connected)
  %bool
  (remote %remote))

(defcenum %direction
  :pull
  :push)

(defcfun ("git_remote_connect" %git-remote-connect)
  %return-value
  (remote %remote)
  (direction %direction))

(defcfun ("git_remote_disconnect" %git-remote-disconnect)
  :void
  (remote %remote))

(defcfun ("git_remote_fetchspec" %git-remote-fetchspec)
  %refspec
  (remote %remote))

(defcfun ("git_remote_pushspec" %git-remote-pushspec)
  %refspec
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

(defmethod git-connect ((remote remote) &key (direction :pull))
  (%git-remote-connect remote direction))

(defmethod git-connected ((remote remote))
  (%git-remote-connected remote))

(defmethod git-disconnected ((remote remote))
  (%git-remote-disconnect remote))

(defmethod git-pushspec ((remote remote))
  (%git-remote-pushspec remote))

(defmethod git-fetchspec ((remote remote))
  (%git-remote-fetchspec remote))
