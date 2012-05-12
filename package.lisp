;;;; package.lisp

(defpackage #:cl-git
  (:use #:cl)
  (:export
   #:*git-repository*
   #:*git-repository-index*
   #:git-repository-init
   #:git-repository-open
   #:git-repository-free
   #:with-git-revisions
   #:bind-git-commits
   #:git-commit-create
   #:git-commit-author
   #:git-commit-message
   #:git-commit-committer
   #:git-commit-parent-count
   #:git-commit-parent-oid
   #:git-commit-parent-oids
   #:git-reference-listall
   #:git-reference-create
   #:git-reference-resolve
   #:with-git-repository-index
   #:git-index-add
   #:git-index-write
   #:git-oid-from-index
   #:with-git-repository
   #:ensure-git-repository-exist
   #:git-tag-tagger
   #:git-tag-type
   #:git-tag-target
   #:git-config-free
   #:git-repository-config
   #:git-config-values
   #:git-tree-oid
   #:git-tree-entry-count
   #:git-tree-entry-by-index
   #:git-tree-lookup
   #:git-tree-close
   #:git-tree-entries))


