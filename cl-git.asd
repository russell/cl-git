;;;; cl-git.asd

(asdf:defsystem #:cl-git
  :description "A CFFI wrapper of libgit2."
  :version "0.2"
  :serial t
  :depends-on (#:cffi #:local-time #:cl-fad #:trivial-garbage #:anaphora)
  :author "Russell Sim <russell.sim@gmail.com>"
  :licence "Lisp-LGPL"
  :components ((:static-file "cl-git.asd")
               (:module "src"
                :serial t
                :components
                ((:file "package")
		 (:file "api")
                 (:file "cl-git")
                 (:file "utils")
                 (:file "error")
                 (:file "oid")
                 (:file "index")
                 (:file "repository")
                 (:file "config")
                 (:file "signature")
                 (:file "references")
                 (:file "object")
                 (:file "tree")
                 (:file "commit")
                 (:file "tag")
		 (:file "blob")
                 (:file "status")
                 (:file "revwalk")))))

(asdf:defsystem #:cl-git-tests
  :depends-on (#:cl-git #:FiveAM #:cl-fad #:unix-options #:inferior-shell #:local-time)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "common")
                 (:file "commit")
                 (:file "repository")
                 (:file "references")
                 (:file "revwalker"))))
  :in-order-to ((compile-op (load-op :cl-git))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-git))))
  (asdf:oos 'asdf:load-op :cl-git-tests)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-git))
