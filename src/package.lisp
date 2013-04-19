;;;; package.lisp

(defpackage #:cl-git
  (:use #:cl)
  (:import-from #:anaphora
                #:acond
                #:it)
  (:import-from #:trivial-garbage
                #:make-weak-pointer
                #:finalize)
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
   #:git-reference-oid
   #:git-resolve
   #:git-add
   #:git-write
   #:git-clear
   #:git-create-from-index
   #:git-config
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

   ;; Bigger functions
   #:make-commit

   ;; Macros
   #:with-repository
   #:with-repository-index
   #:with-git-revisions
   #:bind-git-commits

   ;; Classes.
   #:blob
   #:reference
   #:commit
   #:tree
   #:object
   #:index
   #:repository
   #:*git-repository-index*
   #:git-capabilities
   #:git-tracking
   #:git-version
   #:git-peel
   #:git-load
   #:git-push-url
   #:git-url
   #:git-odb
   #:git-size
   #:git-data))
