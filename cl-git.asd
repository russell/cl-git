;;;; cl-git.asd

(asdf:defsystem #:cl-git
  :description "A CFFI wrapper of libgit2."
  :version "0.2"
  :serial t
  :depends-on (#:cffi #:local-time #:cl-fad)
  :author "Russell Sim <russell.sim@gmail.com>"
  :licence "Lisp-LGPL"
  :components ((:file "package")
               (:file "cl-git")))

(asdf:defsystem #:cl-git-tests
  :depends-on (#:cl-git #:FiveAM #:cl-fad)
  :components ((:file "tests")))
