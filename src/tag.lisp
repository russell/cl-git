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

(defparameter *tag-values* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("git_tag_list" %git-tag-list)
    %return-value
  (strings :pointer)
  (repository %repository))

(defcfun ("git_tag_foreach" %git-tag-foreach)
    %return-value
  (repository %repository)
  (callback :pointer)
  (payload :pointer))

(defcfun ("git_tag_create" %git-tag-create)
    %return-value
  (oid %oid)
  (repo %repository)
  (tag-name :string)
  (target %object)
  (tagger %git-signature)
  (message :string)
  (force :boolean))

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

(defcfun ("git_tag_lookup_byname" %git-tag-lookup-byname)
    %return-value
  (out :pointer)
  (repository %repository)
  (name :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tag (object)
  ())


(defun make-tag (name message &key
                                repository
                                target
                                tagger
                                force)
  ;; TODO should catch exists error and offer a continue
  (with-foreign-object (newoid '(:struct git-oid))
    (with-foreign-strings ((%name name)
                           (%message message))
      (%git-tag-create newoid repository %name target tagger %message force))
    (git-lookup :tag (convert-from-foreign newoid '%oid) :repository repository)))

(defmethod git-list ((class (eql :tag))
		     &key (repository *git-repository*))
  "Returns a list of tag names for the repository.

Important Note:  This is the list of tag names as the user thinks of tags.
Tag objects in libgit2 and cl-git are however a different thing.

Also the list of these values are the strings the user see, but not the
strings they are identified with.  So this call is rather useless."
  (with-foreign-object (string-array '(:struct git-strings))
    (%git-tag-list string-array repository)
    (prog1
	(convert-from-foreign string-array '%git-strings)
      (free-translated-object string-array '%git-strings t))))

#|
(defcallback collect-tag-values :int ((tag-name :string) (oid %oid) (payload :pointer))
  (declare (ignore payload))
  (push (cons tag-name oid) *tag-values*)
  0)

(defmethod git-list ((class (eql :tag))
		     &key (repository *git-repository*))
  (let ((*tag-values* nil))
    (%git-tag-foreach repository
		      (callback collect-tag-values)
		      (null-pointer))
    *tag-values*))
|#

(defmethod git-lookup ((class (eql :tag))
               oid &key (repository *git-repository*))
  (git-object-lookup oid class :repository repository))

(defmethod git-lookup-byname ((class (eql :tag)) (name string)
			      &key (repository *git-repository*))
  (with-foreign-object (reference :pointer)
    (%git-tag-lookup-byname reference repository name)
    (make-instance 'reference
		   :pointer (mem-ref reference :pointer)
		   :facilitator repository
		   :free-function #'%git-reference-free)))

(defmethod git-name ((tag tag))
  (git-tag-name tag))

(defmethod git-tagger ((tag tag))
  (git-tag-tagger tag))

(defmethod git-type ((tag tag))
  (git-tag-type tag))

(defmethod git-message ((tag tag))
  (git-tag-message tag))

(defmethod git-target ((tag tag) &key (type :object))
  "Returns the target of a tag.
The optional :TYPE keyword arguments specifies in which form the
target is returned:

- if :TYPE is :OBJECT it will return a git object.
- if :TYPE is :OID, it will return an OID for the target object."
  (with-foreign-object (%object :pointer)
    (%git-tag-target %object tag)
    (case type
      (:object
       (make-instance-object :pointer (mem-ref %object :pointer)
			     :facilitator (facilitator tag)))
      (:oid (git-object-id (mem-ref %object :pointer)))
      (t (error "Unknown type, type should be either :oid or :object but got: ~A" type)))))

(defmethod git-peel ((tag tag))
  "Peels layers of the tag until the resulting object is not a tag anymore.
Basically calls GIT-TARGET on tag and if the result of that is a TAG,
repeat the process."
  (with-foreign-object (%object :pointer)
    (%git-tag-peel %object tag)
    (make-instance-object :pointer (mem-ref %object :pointer)
              :facilitator (facilitator tag))))
