;;;; package.lisp

(defpackage #:cl-git
  (:use #:cl)
  (:import-from #:anaphora
                #:acond
                #:it)
  (:import-from #:trivial-garbage
                #:make-weak-pointer
                #:finalize)
  (:import-from #:cl-fad
                #:pathname-relative-p)
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

   #:git-create
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
   #:git-list
   #:git-name
   #:git-next
   #:git-tagger
   #:git-type
   #:git-target
   #:git-entry-count
   #:git-entry-by-index
   #:git-entries
   #:git-open
   #:git-init
   #:git-free

   ;; errors
   #:unresolved-reference-error

   ;; new objects
   #:make-commit
   #:make-tag

   ;; Macros
   #:with-repository
   #:with-repository-index
   #:with-index
   #:with-git-revisions
   #:bind-git-commits

   ;; Classes.
   #:blob
   #:reference
   #:commit
   #:tree
   #:remote
   #:config
   #:object
   #:index
   #:tag
   #:repository

   ;; variables
   #:*git-repository-index*
   #:*git-repository*

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
   #:git-odb
   #:git-size
   #:git-data
   #:git-download
   #:git-fetchspec
   #:git-pushspec
   #:git-ls
   #:git-is-head
   #:git-write-tree
   #:git-head
   #:git-head-orphaned
   #:git-head-detached
   #:git-repository-is-empty
   #:git-repository-is-bare
   #:git-path
   #:git-workdir
   #:git-has-log
   #:git-is-remote
   #:git-is-branch
   #:git-upstream
   #:git-remote-name
   #:git-upstream-name
   #:git-lookup-byname))
