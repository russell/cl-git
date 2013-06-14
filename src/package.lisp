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
  (:import-from #:cffi
                #:define-parse-method
                #:define-foreign-type
                #:use-foreign-library
                #:define-foreign-library
                #:translate-to-foreign
                #:translate-from-foreign
                #:free-translated-object
                #:convert-from-foreign
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
  (:export
   ;; LibGit2 Library
   #:libgit2-capabilities
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
   #:message
   #:parents
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
   #:*git-repository-index*
   #:git-add
   #:git-write
   #:git-write-tree
   #:index
   #:index-clear
   #:index-conflicts-p
   #:index-refresh
   #:with-index

   ;; Tree
   #:tree
   #:get-tree
   #:tree-directory

   ;; Tree-Entries (sub-classes of blob, commit, tree or tag)
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
   #:git-ls
   #:remote
   #:remote-connect
   #:remote-connected-p
   #:remote-disconnect
   #:remote-download
   #:remote-fetchspec
   #:remote-push-url
   #:remote-pushspec
   #:remote-url

   ;; Repository
   #:bare-p
   #:empty-p
   #:git-has-log
   #:head-detached-p
   #:head-orphaned-p
   #:init-repository
   #:open-repository
   #:with-repository
   #:repository
   #:repository-head
   #:repository-path
   #:repository-status
   #:repository-workdir))
