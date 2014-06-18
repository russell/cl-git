;;;; cl-git.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem #:cl-git
  :description "A CFFI wrapper of libgit2."
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :defsystem-depends-on (:asdf)
  :depends-on (#:cffi #:local-time #:cl-fad #:flexi-streams #:trivial-garbage #:anaphora #:alexandria #:closer-mop)
  :author "Russell Sim <russell.sim@gmail.com>"
  :licence "Lisp-LGPL"
  :components ((:static-file "cl-git.asd")
               (:module "src"
                :components
                ((:file "package")
                 (cffi-grovel:grovel-file "libgit2-types-grovel")
                 (:file "libgit2-types" :depends-on ("package" "libgit2-types-grovel"))
                 (:file "api" :depends-on ("package"))
                 (:file "libgit2" :depends-on ("package" "libgit2-types"))
                 (:file "buffer" :depends-on ("libgit2"))
                 (:file "error" :depends-on ("libgit2"))
                 (:file "utils" :depends-on ("libgit2"))
                 (:file "git-pointer" :depends-on ("libgit2"))
                 (:file "oid" :depends-on ("api" "libgit2"))
                 (:file "object" :depends-on ("git-pointer" "repository" "oid"))
                 (:file "signature" :depends-on ("libgit2"))
                 (:file "index" :depends-on ("git-pointer" "signature" "oid"))
                 (:file "repository" :depends-on ("git-pointer"))
                 (:file "references" :depends-on ("object"))
                 (:file "reflog" :depends-on ("git-pointer"))
                 (:file "branch" :depends-on ("object"))
                 (:file "commit" :depends-on ("object" "tree"))
                 (:file "tag" :depends-on ("object"))
                 (:file "diff" :depends-on ("git-pointer" "tree" "buffer"))
                 (:file "blob" :depends-on ("object"))
                 (:file "tree" :depends-on ("object" "blob"))
                 (:file "config" :depends-on ("git-pointer"))
                 (:file "status" :depends-on ("git-pointer"))
                 (:file "revwalk" :depends-on ("git-pointer"))
                 (:file "remote" :depends-on ("object"))
                 (:file "odb" :depends-on ("object"))
                 (:file "checkout" :depends-on ("object"))
                 (:file "clone" :depends-on ("checkout" "credentials"))
                 (:file "credentials" :depends-on ("object"))))))


(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-git))))
  (asdf:oos 'asdf:load-op :cl-git-tests)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-git))
