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

(in-package #:cl-git)

(defparameter *tag-values* nil)


(define-foreign-type tag (git-object)
  nil
  (:documentation "Tags are used to identify interesting points in the
repositories history.")
  (:simple-parser %tag))


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
  (oid :pointer)
  (repo %repository)
  (tag-name :string)
  (target %object)
  (tagger %git-signature)
  (message :string)
  (force :boolean))

(defcfun %git-tag-create-lightweight
    %return-value
  (oid :pointer)
  (repo %repository)
  (tag-name :string)
  (target %object)
  (force :boolean))

(defcfun ("git_tag_target" %git-tag-target)
    %return-value
  (target-out :pointer)
  (tag %tag))

(defcfun ("git_tag_peel" %git-tag-peel)
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


(defgeneric tag-p (tag)
  (:documentation
   "Return T if the reference is within the git tag namespace.")
  (:method ((tag string))
    (when (eq 0 (search reference-tags-dir tag))
      t))
  (:method ((tag tag))
    (tag-p (full-name tag)))
  (:method (tag)
    (declare (ignore tag))
    nil))

(defun make-tag (name message &key
                                repository
                                target
                                tagger
                                force)
  "Create a tag that points to target.

NAME is the name of the tag.  MESSAGE will set the message body of the
tag.

REPOSITORY is the repository that the tag will be added to.  TARGET is
the object that the tag will point to.  TAGGER should be a signature
plist.

If FORCE is t the tag will be created, even if a tag with the same
name already exists.  If FORCE is nil, it will return an error if that
is the case."
  ;; TODO (RS) should catch exists error and offer a continue.
  (with-foreign-object (newoid '(:struct git-oid))
    (with-foreign-strings ((%name name)
                           (%message message))
      (%git-tag-create newoid repository %name target tagger %message force))
    (get-object 'tag (convert-from-foreign newoid '%oid) repository)))


(defun make-lightweight-tag (name &key
                                repository
                                target
                                force)
  "Create a tag that points to target.

NAME is the name of the tag.

REPOSITORY is the repository that the tag will be added to.  TARGET is
the object that the tag will point to.  TAGGER should be a signature
plist.

If FORCE is t the tag will be created, even if a tag with the same
name already exists.  If FORCE is nil, it will return an error if that
is the case."
  ;; TODO (RS) should catch exists error and offer a continue.
  (with-foreign-object (newoid '(:struct git-oid))
    (with-foreign-strings ((%name name))
      (%git-tag-create-lightweight newoid repository %name target force))
    (get-object 'reference (concatenate 'string reference-tags-dir name)
                repository)))


(defmethod make-object ((class (eql 'tag)) name repository
                       &key
                         (type :annotated)
                         force
                         target
                         signature
                         message)
  "Create a tag to TARGET.
The type of tag depends on TYPE.  If TYPE is :ANNOTATED the value of
TARGET should be an OID and a direct tag is created.  If TYPE is
:LIGHTWEIGHT, a reference is created and TARGET should be a
OID. SIGNATURE should be a signature plist.

If FORCE is t the tag will be created, even if a tag with
the same name already exists.  If FORCE is nil, it will return an
error if that is the case."
  (ecase type
    (:annotated
     (make-tag name message
               :repository repository
               :target target
               :tagger signature
               :force force))
    (:lightweight
     (make-lightweight-tag name
               :repository repository
               :target target
               :force force))))


(defmethod list-objects ((class (eql 'tag)) repository &key test test-not)
  "Returns a list of tag for the repository.  If the tag is an
annotated tag then a TAG object will be returned, otherwise it will be
a ref with the in the tag namespace."
  (with-foreign-object (string-array '(:struct git-strarray))
    (%git-tag-list string-array repository)
    (let ((refs
            (mapcar (lambda (ref-name)
                      (get-object 'tag
                                  (concatenate 'string reference-tags-dir ref-name)
                                  repository))
                    (prog1
                        (convert-from-foreign string-array '(:struct git-strarray))
                      (free-converted-object string-array '(:struct git-strarray) t)))))
      (cond (test
                (remove-if-not test refs))
            (test-not
             (remove-if test-not refs))
            (t
             refs)))))

(defmethod get-object ((class (eql 'tag)) name-or-oid repository)
  (if (tag-p name-or-oid)
      (let* ((ref (get-object 'reference name-or-oid repository))
             (target (target ref)))
        (case (type-of target)
          ('tag target)
          (t ref)))
      (git-object-lookup name-or-oid class repository)))

(defmethod full-name ((tag tag))
  (concatenate 'string reference-tags-dir (short-name tag)))

(defmethod short-name ((tag tag))
  (git-tag-name tag))

(defgeneric tagger (object)
  (:documentation "Returns the signature of the tagger of OBJECT.

The return value is a signature (a property list with
keys :NAME, :EMAIL and :TIME.  If the tag is not annotated then nil
will be returned.")
  (:method ((tag tag))
    (git-tag-tagger tag))
  (:method ((tag reference))
    nil))

(defmethod message ((tag tag))
  (git-tag-message tag))

(defmethod target ((tag tag))
  "Returns the target of a tag."
  (with-foreign-object (%object :pointer)
    (%git-tag-target %object tag)
    (make-instance-object :pointer (mem-ref %object :pointer)
                          :type :any
                          :facilitator (facilitator tag))))


(defmethod resolve ((object tag) &optional (stop-at '(commit)))
  "Resolve the tag target until the target object is not a tag anymore.
Basically calls TARGET on tag and result until there is a COMMIT
returned.

Using values returns the finally found object and a list of the
traversed objects."
  (let ((objects
          (do ((refs (list (target object) object)
                     (cons (target (car refs)) refs)))
              ((member (type-of (car refs)) stop-at)
               refs))))
    (values (car objects) (cdr objects))))
