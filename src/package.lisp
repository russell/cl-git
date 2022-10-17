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


;;;; package.lisp

(defpackage #:cl-git
  (:nicknames :git)
  (:use #:cl)
  (:import-from #:anaphora
                #:awhen
                #:acond
                #:it)
  (:import-from #:trivial-garbage
                #:make-weak-pointer
                #:cancel-finalization
                #:make-weak-hash-table
                #:finalize)
  (:import-from #:cl-fad
                #:pathname-relative-p)
  (:import-from #:alexandria
                #:ensure-list)
  (:import-from #:closer-mop
                #:subclassp)
  (:import-from #:flexi-streams
                #:external-format-name
                #:make-external-format)
  (:import-from #:uiop
                #:getenv)
  (:import-from #:cffi
                #:define-parse-method
                #:define-foreign-type
                #:use-foreign-library
                #:foreign-library-loaded-p
                #:foreign-enum-value
                #:foreign-enum-keyword-list
                #:define-foreign-library
                #:translate-to-foreign
                #:translate-into-foreign-memory
                #:translate-from-foreign
                #:translate-name-to-foreign
                #:translate-underscore-separated-name
                #:free-translated-object
                #:convert-into-foreign-memory
                #:convert-to-foreign
                #:convert-from-foreign
                #:free-converted-object
                #:with-foreign-object
                #:with-foreign-objects
                #:with-foreign-slots
                #:with-foreign-strings
                #:with-foreign-string
                #:with-foreign-pointer-as-string
                #:foreign-slot-pointer
                #:foreign-alloc
                #:foreign-string-alloc
                #:null-pointer
                #:null-pointer-p
                #:pointerp
                #:mem-ref
                #:mem-aref
                #:defcfun
                #:defctype
                #:defcstruct
                #:defbitfield
                #:foreign-string-to-lisp
                #:defcallback
                #:callback
                #:foreign-free
                #:pointer-address
                #:defcenum)
  (:import-from #:local-time
                #:unix-to-timestamp
                #:timestamp-to-universal)
  (:export
   ;; LibGit2 Library
   #:libgit2-features
   #:libgit2-version

   ;; Config
   #:config
   #:git-config
   #:git-config-open-level
   #:git-values

   ;; Git Objects
   #:free
   #:full-name
   #:get-object
   #:list-objects
   #:make-object
   #:object
   #:object-type
   #:oid
   #:reflog
   #:short-name

   ;; Git Objects -- collections
   #:entries
   #:entry-count
   #:entry-by-index

   ;; Errors
   #:basic-error
   #:not-found
   #:exists
   #:ambiguous-error
   #:buffer-error
   #:connection-error
   #:user-error
   #:barerepo-error
   #:orphanedhead-error
   #:unmerged-error
   #:non-fast-forward-error
   #:invalid-spec-error
   #:merge-conflict-error
   #:passthrough
   #:stop-iteration
   #:unknown-error

   ;; Tags
   #:make-tag
   #:tag
   #:tagger

   ;; Commits
   #:author
   #:bind-git-commits
   #:commit
   #:committer
   #:make-commit
   #:commit-tree
   #:message
   #:message-encoding
   #:parents

   ;; Revision walker
   #:revision-walk
   #:next-revision

   ;; Blobs
   #:binary-p
   #:blob
   #:blob-content
   #:blob-size

   ;; References
   #:branch-p
   #:resolve
   #:head-p
   #:reference
   #:remote-p
   #:symbolic-p
   #:tag-p
   #:target
   #:unresolved-reference-error
   #:upstream

   ;; Index
   #:index
   #:index-add-file
   #:index-write
   #:index-entries
   #:index-to-tree
   #:open-index
   #:index-clear
   #:index-conflicts-p
   #:index-refresh
   #:with-index

   ;; Tree
   #:tree
   #:get-tree
   #:tree-directory

   ;; Tree-Entries (sub-classes of blob, commit, tree or tag)
   #:tree-blob
   #:tree-tree
   #:filemode
   #:filename

   ;; Odb
   #:odb
   #:odb-data
   #:odb-object
   #:odb-size
   #:odb-type
   #:open-odb

   ;; Remote
   #:remote
   #:remote-connect
   #:remote-connected-p
   #:remote-disconnect
   #:remote-download
   #:ls-remote
   #:remote-push-refspecs
   #:remote-fetch-refspecs
   #:remote-push-url
   #:remote-url

   ;; Repository
   #:bare-p
   #:empty-p
   #:git-has-log
   #:head-detached-p
   #:head-unborn-p
   #:init-repository
   #:open-repository
   #:clone-repository
   #:with-repository
   #:repository
   #:repository-head
   #:repository-path
   #:repository-status
   #:repository-workdir

   ;; Credentials
   #:ssh-key-from-agent
   #:ssh-key
   #:username-password

   ;; diff
   #:diff
   #:diff-deltas-count
   #:diff-deltas-summary
   #:make-patch
   #:patch-to-string))
