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


(defcfun ("git_commit_create" %git-commit-create)
    %return-value
  (oid :pointer)
  (repo %repository)
  (update-ref :pointer)
  (author %git-signature)
  (committer %git-signature)
  (message-encoding :pointer)
  (message :pointer)
  (tree %tree)
  (parent-count :int)
  (parents :pointer))

(defcfun ("git_commit_message" git-commit-message)
    :string
  "Return a string containing the commit message."
  (commit %commit))

(defcfun ("git_commit_author" git-commit-author)
    %git-signature
  "Given a commit return the commit author's signature."
  (commit %commit))

(defcfun ("git_commit_committer" git-commit-committer)
    %git-signature
  "Given a commit return the commit committer's signature."
  (commit %commit))

(defcfun ("git_commit_parentcount" git-commit-parentcount)
    :int
  "Returns the number of parent commits of the argument."
  (commit %commit))

(defcfun ("git_commit_parent_id" git-commit-parent-oid)
    %oid
  "Returns the oid of the parent with index `parent-index' in the list
of parents of the commit `commit'."
  (commit %commit)
  (n :int))

(defcfun ("git_commit_tree" %git-commit-tree)
    %return-value
  (tree-out :pointer)
  (commit %commit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass commit (object)
  ()
  (:documentation "Commit objects link the state of the tree with a
description.  Commits contain a description of the author, commit
author and a message about the commit.  They also contain state
information about the current tree and links to any parent commits.
Commits that have more then one parent are considered to be merges."))

(defun make-commit (oid message &key
                                  (update-ref "HEAD")
                                  author
                                  committer
                                  parents
                                  (repository *git-repository*))
  "Create a new commit from the tree with the OID specified and
MESSAGE.  Optional :UPDATE-REF is the name of the reference that will
be updated to point to this commit.  The default value \"HEAD\" will
updote the head of the current branch.  If it's value is NULL then no
reference will be updated.  :AUTHOR is an optional instance of a
GIT-SIGNATURE that details the commit author.  :COMMITTER is an
optional instance of a GIT-SIGNATURE the details the committer.
:PARENTS is an optional list of parent commits sha1 hashes."

  (assert (not (null-or-nullpointer repository)))

  (let ((tree (git-lookup :object oid :type :tree :repository repository))
        (parents (if (listp parents) parents (list parents))))

    ;; lookup all the git commits
    (setq parents (mapcar #'(lambda (c)
                              (git-lookup :object
                                          (lookup-oid :sha c)
                                          :repository repository))
                          parents))

    (with-foreign-objects ((%parents :pointer (length parents))
                           (newoid '(:struct git-oid)))
      (with-foreign-strings ((%message message)
                             (%message-encoding "UTF-8")
                             (%update-ref update-ref))

        (loop :for parent :in parents
              :counting parent :into i
              :do (setf (mem-aref %parents :pointer (1- i)) (pointer parent)))
        (%git-commit-create
         newoid
         repository
         %update-ref
         author
         committer
         %message-encoding
         %message
         tree
         (length parents)
         %parents))
      (git-lookup :commit (convert-from-foreign newoid '%oid)
                  :repository repository))))

(defmethod git-lookup ((class (eql :commit))
               oid &key (repository *git-repository*))
  (git-object-lookup oid class :repository repository))

(defmethod git-name ((commit commit))
  (with-foreign-pointer-as-string (str 41 :encoding :ascii)
    (git-oid-tostr str 41 (git-id commit))))

(defmethod git-message ((commit commit))
  "Return a string containing the commit message."
  (git-commit-message commit))

(defmethod git-author ((commit commit))
  "Given a commit return the commit author's signature."
  (git-commit-author commit))

(defmethod git-committer ((commit commit))
  (git-commit-committer commit))

(defmethod git-parentcount ((commit commit))
  "Returns the number of parent commits of the argument COMMIT."
  (git-commit-parentcount commit))

(defmethod git-parent-oid ((commit commit) index)
  "Returns the oid of the parent with index INDEX in the list of
parents of the commit COMMIT."
  (git-commit-parent-oid commit index))

(defmethod git-tree ((commit commit) &key path (repository *git-repository*))
  "Returns the TREE object of the commit."
  (let ((tree (with-foreign-object (%tree :pointer)
                (%git-commit-tree %tree commit)
                (make-instance-object :pointer (mem-aref %tree :pointer)
                                      :facilitator (facilitator commit)
                                      :type :tree))))
    (if path
        (git-tree tree :path path :repository repository)
        tree)))

(defun git-commit-from-oid (oid &key (repository *git-repository*))
  "Returns a git-commit object identified by the `oid'.
This is an extended version of GIT-COMMIT-LOOKUP.
If the oid refers to a tag, this function will return the git-commit
pointed to by the tag.  The call git-commit-lookup will fail."
  (let ((git-object (git-object-lookup oid :any :repository repository)))
    (ecase (git-object-type git-object)
      (:tag (git-target git-object))
      (:commit git-object))))

(defun commit-oid-from-oid (oid &key (repository *git-repository*))
  "Returns the oid of a commit referenced by `oid'.
If the `oid' refers to a commit the function is basically a
no-op.  However if `oid' refers to a tag, it will return
the oid of the target of the tag."
  (let ((commit (git-commit-from-oid oid :repository repository)))
    (git-object-id commit)))

(defmacro bind-git-commits (bindings &body body)
  "Lookup commits specified in the bindings.  The bindings syntax is
similar to the LET syntax except instead of needing to specify an
initial form key arguments are used.  Atleast one key arguments SHA or
HEAD must be specified.  SHA is a hash of the commit.  HEAD is a full
ref path."
  `(let ,(mapcar #'(lambda (s)
                     `(,(car s) (null-pointer)))
          bindings)
     (unwind-protect
          (progn
            ,@(mapcar
               #'(lambda (s)
                   `(setf ,(car s)
                          (git-commit-from-oid
                           (lookup-oid ,@(cdr s)))))
               bindings)
            ,@body))))
