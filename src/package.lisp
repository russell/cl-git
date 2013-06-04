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
   #:capabilities
   #:libgit2-version

   #:revision-walk
   #:walker-next

   #:git-resolve
   #:git-add
   #:git-write
   #:git-clear
   #:git-config
   #:git-config-open-level
   #:git-values
   #:blob-size
   #:blob-content
   #:binary-p
   #:repository-status
   #:oid
   #:git-message
   #:git-author
   #:git-committer
   #:parent-count
   #:parents
   #:git-tree
   #:get-object
   #:list-objects
   #:full-name
   #:short-name
   #:git-next
   #:git-tagger
   #:git-type
   #:target
   #:git-entry-count
   #:git-entry-by-index
   #:git-entries
   #:open-repository
   #:init-repository
   #:free

   ;; errors
   #:unresolved-reference-error

   ;; new objects
   #:make-commit
   #:make-tag
   #:make-object

   ;; Macros
   #:with-index
   #:with-git-revisions
   #:bind-git-commits

   ;; Classes.
   #:odb-object
   #:blob
   #:reference
   #:symbolic-reference
   #:commit
   #:tree
   #:remote
   #:config
   #:object
   #:index
   #:tag
   #:repository

   ;; predicates
   #:branch-p
   #:remote-p
   #:symbolic-p
   #:tag-p
   #:head-p

   ;; variables
   #:*git-repository-index*

   #:git-peel
   #:git-index
   #:git-load
   #:git-read
   #:index-conflicts-p
   #:git-data
   #:tree-directory

   ;; odb
   #:open-odb
   #:odb-data
   #:odb-size

   ;; remote
   #:remote-download
   #:remote-fetchspec
   #:remote-pushspec
   #:remote-connect
   #:remote-disconnect
   #:remote-connected-p
   #:remote-push-url
   #:remote-url
   #:git-ls
   #:git-is-head
   #:git-write-tree
   #:repository-head
   #:head-orphaned-p
   #:head-detached-p
   #:empty-p
   #:bare-p
   #:repository-path
   #:repository-workdir
   #:git-has-log
   #:git-is-remote
   #:git-is-branch
   #:upstream
   #:remote-name))
