;;;; package.lisp

(defpackage #:cl-git
  (:use #:cl)
  (:export
   #:*git-repository*
   #:with-git-revisions
   #:git-commit-author
   #:git-commit-message
   #:git-commit-committer
   #:git-reference-listall
   #:with-git-repository
   #:ensure-git-repository-exist
))

