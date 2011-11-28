;;;; package.lisp

(defpackage #:cl-git
  (:use #:cl)
  (:export
   #:*git-repository*
   #:git-repository-init
   #:git-repository-open
   #:git-repository-free
   #:with-git-revisions
   #:git-commit-author
   #:git-commit-message
   #:git-commit-committer
   #:git-reference-listall
   #:with-git-repository
   #:ensure-git-repository-exist
))

