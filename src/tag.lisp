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


(defcfun ("git_tag_type" git-tag-type)
    git-object-type
  (tag %tag))

(defcfun ("git_tag_target" %git-tag-target)
    %return-value
  (reference :pointer)
  (tag %tag))

(defcfun ("git_tag_target" %git-tag-peel)
    %return-value
  (reference :pointer)
  (tag %tag))

(defcfun ("git_tag_tagger" git-tag-tagger)
    %git-signature
  (tag %tag))

(defcfun ("git_tag_name" git-tag-name)
    :string
  "Returns the name of the tag"
  (tag %tag))

(defcfun ("git_tag_message" git-tag-message)
    :string
  "Returns the message of the tag"
  (tag %tag))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tag (object)
  ())

(defmethod git-lookup ((class (eql :tag))
               oid &key (repository *git-repository*))
  (git-object-lookup oid class :repository repository))

(defmethod git-name ((tag tag))
  (git-tag-name tag))

(defmethod git-tagger ((tag tag))
  (git-tag-name tag))

(defmethod git-type ((tag tag))
  (git-tag-type tag))

(defmethod git-message ((tag tag))
  (git-tag-message tag))

(defmethod git-target ((tag tag))
  (with-foreign-object (%object :pointer)
    (%git-tag-target %object tag)
    (make-instance-object :pointer (mem-ref %object :pointer)
              :facilitator (facilitator tag))))

(defmethod git-peel ((tag tag))
  "Peels layers of the tag until the resulting object is not a tag anymore.
Basically calls GIT-TARGET on tag and if the result of that is a TAG,
repeat the process."
  (with-foreign-object (%object :pointer)
    (%git-tag-peel %object tag)
    (make-instance-object :pointer (mem-ref %object :pointer)
              :facilitator (facilitator tag))))
