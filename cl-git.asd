;;;; cl-git.asd

(asdf:defsystem #:cl-git
  :description "A CFFI wrapper of libgit2."
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:local-time)
  :author "Russell Sim <russell.sim@gmail.com>"
  :licence "Lisp-LGPL"
  :components ((:file "package")
               (:file "cl-git")))

(asdf:defsystem #:cl-git-tests
  :depends-on (#:cl-git #:FiveAM #:cl-fad)
  :components ((:file "tests")))
