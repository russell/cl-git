;;;; cl-git.asd

(asdf:defsystem #:cl-git
  :description "A CFFI wrapper of libgit2."
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :defsystem-depends-on (:asdf)
  :depends-on (#:cffi #:local-time #:cl-fad #:trivial-garbage #:anaphora)
  :author "Russell Sim <russell.sim@gmail.com>"
  :licence "Lisp-LGPL"
  :components ((:static-file "cl-git.asd")
               (:module "src"
                :components
                ((:file "package")
                 (:file "cl-git" :depends-on ("package"))
                 (:file "api" :depends-on ("package"))
                 (:file "cffi-types" :depends-on ("package" "cl-git"))
                 (:file "git-pointer" :depends-on ("package"))
                 (:file "utils" :depends-on ("package"))
                 (:file "error" :depends-on ("package"))
                 (:file "libgit2" :depends-on ("package"))
                 (:file "oid" :depends-on ("package" "api" "cffi-types"))
                 (:file "object" :depends-on ("package" "api" "cffi-types"))
                 (:file "signature" :depends-on ("package" "api" "cffi-types"))
                 (:file "index" :depends-on ("package" "api" "cffi-types" "signature" "oid"))
                 (:file "repository" :depends-on ("package" "api" "cffi-types"))
                 (:file "references" :depends-on ("package" "api" "cffi-types"))
                 (:file "reflog" :depends-on ("package" "api" "cffi-types"))
                 (:file "branch" :depends-on ("package" "api" "cffi-types"))
                 (:file "tree" :depends-on ("package" "api" "cffi-types"))
                 (:file "commit" :depends-on ("package" "api" "cffi-types"))
                 (:file "tag" :depends-on ("package" "api" "cffi-types"))
                 (:file "blob" :depends-on ("package" "api" "cffi-types"))
                 (:file "config" :depends-on ("package" "api" "cffi-types"))
                 (:file "status" :depends-on ("package" "api" "cffi-types"))
                 (:file "revwalk" :depends-on ("package" "api" "cffi-types"))
                 (:file "remote" :depends-on ("package" "api" "cffi-types"))
                 (:file "odb" :depends-on ("package" "api" "cffi-types"))))))


(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-git))))
  (asdf:oos 'asdf:load-op :cl-git-tests)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-git))
