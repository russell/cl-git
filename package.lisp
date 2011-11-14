;;;; package.lisp

(defpackage #:cl-git
  (:use #:cl)
  (:export
   #:with-git-revisions
   #:git-commit-author
   #:git-commit-message
   #:git-commit-committer
   #:git-reference-listall
   #:with-git-repository
   #:ensure-git-repository-exist
))

