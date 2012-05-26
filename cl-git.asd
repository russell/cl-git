;;;; cl-git.asd

(asdf:defsystem #:cl-git
  :description "A CFFI wrapper of libgit2."
  :version "0.2"
  :serial t
  :depends-on (#:cffi #:local-time #:cl-fad #:trivial-garbage)
  :author "Russell Sim <russell.sim@gmail.com>"
  :licence "Lisp-LGPL"
  :components ((:static-file "cl-git.asd")
               (:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "cl-git")
                 (:file "utils")
                 (:file "error")
                 (:file "oid")
                 (:file "index")
                 (:file "repository")
                 (:file "config")
                 (:file "signature")
                 (:file "references")
                 (:file "tree")
                 (:file "object")
                 (:file "commit")
                 (:file "tag")
                 (:file "status")
                 (:file "revwalk")))))

(asdf:defsystem #:cl-git-tests
  :depends-on (#:cl-git #:FiveAM #:cl-fad #:unix-options #:inferior-shell)
  :components ((:module "tests"
                :components
                ((:file "tests"))))
  :in-order-to ((compile-op (load-op :cl-git))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-git))))
  (asdf:oos 'asdf:load-op :cl-git-tests)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-git))
