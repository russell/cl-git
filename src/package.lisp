;;;; package.lisp

(defpackage #:cl-git
  (:use #:cl)
  (:import-from #:anaphora
                #:acond
                #:it)
  (:import-from #:trivial-garbage
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
                #:defcenum)
  (:export
   #:with-git-revisions
   #:revision-walk
   #:walker-next
   #:bind-git-commits
   #:make-commit
   #:git-reference-list
   #:git-reference-create
   #:git-reference-lookup
   #:git-reference-oid
   #:git-reference-type
   #:git-resolve
   #:with-repository-index
   #:git-index-add
   #:git-index-write
   #:git-index-clear
   #:git-oid-from-index
   #:with-repository
   #:ensure-repository-exist
   #:git-config-free
   #:git-repository-config
   #:git-values
   #:git-object-lookup
;   #:git-object-id
;   #:git-object-type
   #:git-object-free
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
   #:git-name
   #:git-tagger
   #:git-type
   #:git-target
   #:git-entry-count
   #:git-entry-by-index
   #:git-entries
   #:git-repository-open
   #:git-repository-free
   #:git-free))
