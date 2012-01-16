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
   #:with-git-commits
   #:git-signature-create
   #:git-commit-create
   #:git-commit-author
   #:git-commit-message
   #:git-commit-committer
   #:git-reference-listall
   #:with-git-repository-index
   #:git-index-add
   #:git-index-write
   #:git-oid-from-index
   #:with-git-repository
   #:ensure-git-repository-exist
))
