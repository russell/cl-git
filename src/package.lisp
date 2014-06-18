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
  (:import-from #:cffi
                #:define-parse-method
                #:define-foreign-type
                #:use-foreign-library
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
   #:git-entries
   #:git-entry-count
   #:git-entry-by-index
   #:list-objects
   #:make-object
   #:object
   #:object-type
   #:oid
   #:reflog
   #:short-name

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
   #:head-orphaned-p
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
   #:patch-to-string
   ))
