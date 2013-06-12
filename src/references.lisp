;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2013 Russell Sim <russell.sim@gmail.com>
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

(defbitfield git-reference-flags
  (:oid 1)
  (:symbolic 2)
  (:packed 4)
  (:has-peel 8))

(defcfun ("git_reference_list" %git-reference-list)
    %return-value
  (strings :pointer)
  (repository %repository)
  (flags git-reference-flags))

(defcfun ("git_reference_target" %git-reference-target)
    %oid
  (reference %reference))

(defcfun ("git_reference_name" %git-reference-name)
    :string
  (reference %reference))

(defcfun ("git_reference_lookup" %git-reference-lookup)
    %return-value
  (reference :pointer)
  (repository %repository)
  (name :string))

(defcfun ("git_reference_resolve" %git-reference-resolve)
    %return-value
  (resolved-ref :pointer)
  (reference %reference))

(defcfun ("git_reference_create" %git-reference-create)
    %return-value
  (reference :pointer)
  (repository %repository)
  (name :string)
  (oid %oid)
  (force :boolean))

(defcfun ("git_reference_symbolic_create" %git-reference-symbolic-create)
    %return-value
  (reference :pointer)
  (repository %repository)
  (name :string)
  (target :string)
  (force :boolean))

(defcfun ("git_reference_set_target" %git-reference-set-target)
    %return-value
  (new-reference :pointer)
  (reference %reference)
  (oid %oid))

(defcfun ("git_reference_symbolic_target" %git-reference-symbolic-target)
    :string
  (reference %reference))

(defcfun ("git_reference_free" %git-reference-free)
    :void
  (reference %reference))

(defcfun ("git_reference_type" git-reference-type)
    git-reference-flags
  (reference %reference))

(defcfun ("git_reference_is_branch" %git-is-branch)
    :boolean
  "Returns t if the reference is a local branch."
  (reference %reference))

(defcfun ("git_reference_is_remote" %git-is-remote)
    :boolean
  "Returns t if the reference lives in the refs/remotes namespace."
  (reference %reference))

(defcfun ("git_reference_has_log" git-has-log)
    :boolean
  "Returns t if there exists a REFLOG for the reference."
  (reference %reference))

(define-condition unresolved-reference-error (error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass reference (git-pointer) ())

;;; XXX (RS) should probably look at using the groveller to get these
;;; values.
(defvar reference-dir "refs/")
(defvar reference-heads-dir (concatenate 'string reference-dir "heads/"))
(defvar reference-tags-dir (concatenate 'string  reference-dir "tags/"))
(defvar reference-remotes-dir (concatenate 'string reference-dir "remotes/"))


(defgeneric symbolic-p (reference)
  (:documentation
   "Return T if the reference is symbolic.")
  (:method ((reference reference))
    (let ((type (git-reference-type reference)))
      (assert (eql 1 (length type)))
      (ecase (car type)
        (:symbolic t)
        (:oid nil)))))

(defgeneric remote-p (reference)
  (:documentation
   "Return T if the reference is within the git remotes namespace.")
  (:method ((reference string))
    (when (eq 0 (search reference-remotes-dir reference))
      t))
  (:method ((reference reference))
      (remote-p (full-name reference))))

(defgeneric branch-p (reference)
  (:documentation
   "Return T if the reference is within the git heads namespace.")
  (:method ((reference string))
    (when (eq 0 (search reference-heads-dir reference))
      t))
  (:method ((reference reference))
    (branch-p (full-name reference))))

(defmethod %git-lookup-by-name ((class (eql 'reference)) name repository)
  "Lookup a reference by name and return a pointer to it.  This
pointer will need to be freed manually."
  (assert (not (null-or-nullpointer repository)))
  (with-foreign-object (reference :pointer)
    (%git-reference-lookup reference repository name)
    (mem-ref reference :pointer)))

(defmethod get-object ((class (eql 'reference)) name repository)
  "Find a reference by its full name e.g.: ref/heads/master
Note that this function name clashes with the generic lookup function.
We need to figure this out by using the type argument to do dispatch."
  (make-instance 'reference
                 :pointer (%git-lookup-by-name 'reference name repository)
                 :facilitator repository
                 :free-function #'%git-reference-free))

;; XXX (RS) not sure what to do about this functionality, it could be
;; expected that the user uses RESOLVE's second value to determine the
;; OID ref, so perhaps this is redundant?
(defun git-resolve (reference)
  "If the reference is symbolic, follow the it until it finds a non
symbolic reference."
  (with-foreign-object (resolved-ref :pointer)
    (%git-reference-resolve resolved-ref reference)
    (make-instance 'reference
           :pointer (mem-ref resolved-ref :pointer)
           :facilitator (facilitator reference)
           :free-function #'%git-reference-free)))


(defun make-reference-from-name (name repository)
  "Make a weak reference by name that can be looked-up later."
  (make-instance 'reference :name name
                            :facilitator repository
                            :free-function #'%git-reference-free))

(defmethod list-objects ((class (eql 'reference)) repository &key test test-not)
  "List all the refs the returned list can be filtered using a PREDICATE."
  (assert (not (null-or-nullpointer repository)))
  (with-foreign-object (string-array '(:pointer (:struct git-strings)))
    (%git-reference-list string-array repository '(:oid :symbolic :packed))
    (let ((refs
            (mapcar (lambda (ref-name)
                      (make-reference-from-name ref-name repository))
             (prog1
                 (convert-from-foreign string-array '%git-strings)
               (free-translated-object string-array '%git-strings t)))))
      (cond (test
             (remove-if-not test refs))
            (test-not
             (remove-if test-not refs))
            (t
             refs)))))


(defmethod make-object ((class (eql 'reference)) name repository
                       &key
                         (type :oid)
                         force
                         target)
  "Create a reference to TARGET.
The type of reference depends on TYPE.  If TYPE is :OID the value of
TARGET should be an OID and a direct reference is created.  If TYPE
is :SYMBOLIC, a symbolic reference is created and TARGET should be a
string.

If FORCE is t the reference will be created, even if a reference with
the same name already exists.  If FORCE is nil, it will return an
error if that is the case."
  (with-foreign-object (reference :pointer)
    (ecase type
      (:oid
       (%git-reference-create reference repository name target force))
      (:symbolic
       (%git-reference-symbolic-create reference repository name target force)))
    (make-instance 'reference
                   :pointer (mem-ref reference :pointer)
                   :facilitator repository
                   :free-function #'%git-reference-free)))


(defun find-oid (name repository &key (flags :both))
  "Find a head or sha that matches the NAME. Possible flags
are :SHA, :HEAD or :BOTH"
  (assert (not (null-or-nullpointer repository)))
  (flet ((and-both (flag other-flag)
           (find flag (list :both other-flag))))
  (acond
    ((and (and-both flags :head)
          (remove-if-not (lambda (ref) (equal ref name))
             (list-objects 'reference repository)))
     (lookup-oid :head (car it) :repository repository))
    ((numberp name)
     (lookup-oid :sha name :repository repository))
    ((and (and-both flags :sha)
          (find (length name) '(40 7))
          (not (loop :for char :across name
                     :when (not (find char "1234567890abcdef"))
                       :collect char)))
     (lookup-oid :sha name :repository repository))
    (t (error "Invalid reference named ~A." name)))))

(defun find-oids (name-or-names repository &key (flags :both))
  "Find a head or sha that matches the NAME. Possible flags
are :SHA, :HEAD or :BOTH"
  (assert (not (null-or-nullpointer repository)))
  (if (stringp name-or-names)
      (find-oid name-or-names repository :flags flags)
      (loop :for name :in name-or-names
            :collect (find-oid name repository :flags flags))))

(defmethod full-name ((object reference))
  (if (slot-value object 'libgit2-name)
      (slot-value object 'libgit2-name)
      (%git-reference-name object)))

(defmethod short-name ((object reference))
  (let ((name (full-name object)))
    (cond
      ((remote-p name)
       (subseq name (length reference-remotes-dir)))
      ((branch-p name)
       (subseq name (length reference-heads-dir)))
      (t name))))

(defmethod print-object ((object reference) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((not (null-pointer-p (slot-value object 'libgit2-pointer)))
       (format stream "~a" (full-name object)))
      ((or (slot-value object 'libgit2-oid) (slot-value object 'libgit2-name))
       (format stream "~a (weak)" (full-name object)))
      ((slot-value object 'libgit2-disposed)
       (princ "(disposed)" stream)))))

(defmethod oid ((reference reference))
  "Returns the oid that this reference points to.  If this reference
is a symbolic reference then it will be resolved to a real reference
first."
  (if (symbolic-p reference)
      (oid (target (git-resolve reference)))
      (oid (target reference))))

(defmethod target ((reference reference))
  "Returns the Object that this reference points to.  If the reference
is symbolic then the reference it points to will be returned."
  (if (symbolic-p reference)
      (get-object 'reference
                  (%git-reference-symbolic-target reference)
                  (facilitator reference))
      (get-object 'object
                  (%git-reference-target reference)
                  (facilitator reference))))

(defun (setf target) (val reference)
  (with-foreign-object (new-reference :pointer)
    (%git-reference-set-target
     new-reference
     reference
     (if (numberp val) val (oid val)))
    (let ((old-pointer (pointer reference)))
      ;; XXX (RS) Swap out the old pointer with a new one.  This
      ;; should be extracted out to a function.
      (setf (slot-value reference 'libgit2-pointer)
            (mem-ref new-reference :pointer))
      (cancel-finalization reference)
      (enable-garbage-collection reference)
      (%git-reference-free old-pointer))))


(defmethod resolve ((object reference) &optional (stop-at '(commit tag)))
  "Resolve the reference until the resulting object is a tag or
commit.  Basically calls TARGET until the returned object is a COMMIT
or TAG.

Using values returns the finally found object and a list of the
traversed objects."
  (let ((objects
          (do ((refs (list (target object) object)
                     (cons (target (car refs)) refs)))
              ((member (type-of (car refs)) stop-at)
               refs))))
    (values (car objects) (cdr objects))))
