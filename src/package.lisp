;;;; package.lisp

(defpackage #:cl-git
  (:nicknames :git)
  (:use #:cl)
  (:import-from #:anaphora
                #:acond
                #:it)
  (:import-from #:trivial-garbage
                #:make-weak-pointer
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
   #:revision-walk
   #:walker-next

   #:make-object
   #:git-resolve
   #:git-add
   #:git-write
   #:git-clear
   #:git-config
   #:git-config-open-level
   #:git-connect
   #:git-values
   #:git-raw-size
   #:git-raw-content
   #:git-status
   #:git-id
   #:git-message
   #:git-author
   #:git-committer
   #:git-parentcount
   #:git-parent-oid
   #:git-parent-oids
   #:git-tree
   #:git-lookup
   #:list-objects
   #:full-name
   #:short-name
   #:git-next
   #:git-tagger
   #:git-type
   #:git-target
   #:git-entry-count
   #:git-entry-by-index
   #:git-entries
   #:open-repository
   #:init-repository
   #:git-free

   ;; errors
   #:unresolved-reference-error

   ;; new objects
   #:make-commit
   #:make-tag

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

   #:git-capabilities
   #:git-tracking
   #:git-version
   #:git-peel
   #:git-index
   #:git-index-has-conflicts
   #:git-load
   #:git-read
   #:git-push-url
   #:git-url
   #:open-odb
   #:git-size
   #:git-data
   #:remote-download
   #:remote-fetchspec
   #:remote-pushspec
   #:remote-connect
   #:remote-disconnect
   #:remote-connected-p
   #:git-ls
   #:git-is-head
   #:git-write-tree
   #:repository-head
   #:head-orphaned-p
   #:head-detached-p
   #:git-repository-is-empty
   #:bare-p
   #:repository-path
   #:repository-workdir
   #:git-has-log
   #:git-is-remote
   #:git-is-branch
   #:git-upstream
   #:git-remote-name))
