;;;; cl-git.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+quicklisp
  (ql:quickload 'cffi-grovel :silent t)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(defsystem "cl-git"
  :description "A CFFI wrapper of libgit2."
  :version (:read-file-form "version.lisp-expr")
  ;; https://github.com/quicklisp/quicklisp-client/issues/108 There
  ;; will be errors if loading without first calling `(ql:quickload
  ;; 'cffi :silent t)'
  :defsystem-depends-on (:asdf :cffi-grovel)
  :depends-on ("cffi"
               "local-time"
               "cl-fad"
               "flexi-streams"
               "trivial-garbage"
               "anaphora"
               "alexandria"
               "closer-mop"
               "uiop")
  :author "Russell Sim <russell.sim@gmail.com>"
  :licence "Lisp-LGPL"
  :pathname "src/"
  :components
  ((:file "package")
   (:file "git-pointer"
    :depends-on ("package"))
   (:file "libgit2" :depends-on ("package"
                                 "libgit2-lib"
                                 "libgit2-types"
                                 "libgit2-types-grovel1"))
   (:file "libgit2-lib" :depends-on ("package"))
   (:file "libgit2-types" :depends-on ("package"
                                       "git-pointer"
                                       "libgit2-features"
                                       "libgit2-types-grovel0"))
   (:file "libgit2-features" :depends-on ("package"
                                          "libgit2-lib"))
   (cffi-grovel:grovel-file "libgit2-types-grovel0"
                            :depends-on ("package"
                                         "libgit2-features"
                                         "libgit2-lib"))
   (cffi-grovel:grovel-file "libgit2-types-grovel1"
                            :depends-on ("package"
                                         "libgit2-features"
                                         "libgit2-types"
                                         "libgit2-types-grovel0"))
   (:file "api" :depends-on ("package"))
   (:file "buffer" :depends-on ("libgit2"))
   (:file "error" :depends-on ("libgit2"))
   (:file "strarray" :depends-on ("libgit2"))
   (:file "proxy" :depends-on ("libgit2"))
   (:file "oid" :depends-on ("api" "libgit2"))
   (:file "object" :depends-on ("git-pointer" "repository" "oid"))
   (:file "signature" :depends-on ("libgit2"))
   (:file "index" :depends-on ("git-pointer" "signature" "oid"))
   (:file "repository" :depends-on ("git-pointer"))
   (:file "references" :depends-on ("object"))
   (:file "reflog" :depends-on ("git-pointer"))
   (:file "branch" :depends-on ("object"))
   (:file "commit" :depends-on ("object" "tree"))
   (:file "graph" :depends-on ("oid" "repository"))
   (:file "tag" :depends-on ("object"))
   (:file "diff" :depends-on ("libgit2-types" "git-pointer" "tree" "buffer"))
   (:file "blob" :depends-on ("object"))
   (:file "tree" :depends-on ("object" "blob"))
   (:file "config" :depends-on ("git-pointer"))
   (:file "status" :depends-on ("git-pointer"))
   (:file "revwalk" :depends-on ("git-pointer"))
   (:file "remote" :depends-on ("object" "credentials"))
   (:file "odb" :depends-on ("object"))
   (:file "checkout" :depends-on ("object"))
   (:file "clone" :depends-on ("checkout" "credentials" "remote"))
   (:file "credentials" :depends-on ("object")))
  :in-order-to ((test-op (test-op "cl-git/tests"))))


(defsystem "cl-git/tests"
  :defsystem-depends-on (:asdf)
  :depends-on ("cl-git"
               "fiveam"
               "cl-fad"
               "unix-options"
               "inferior-shell"
               "local-time"
               "alexandria"
               "flexi-streams")
  :version (:read-file-form "version.lisp-expr")
  :licence "Lisp-LGPL"
  :pathname "tests/"
  :components
  ((:file "package")
   (:file "common" :depends-on ("package"))
   (:file "fixtures" :depends-on ("package"))
   (:file "commit" :depends-on ("common"))
   (:file "clone" :depends-on ("common" "fixtures"))
   (:file "checkout" :depends-on ("common"))
   (:file "index" :depends-on ("common" "fixtures"))
   (:file "repository" :depends-on ("common" "fixtures"))
   (:file "remote" :depends-on ("common" "fixtures"))
   (:file "strings" :depends-on ("common" "fixtures"))
   (:file "tag" :depends-on ("common" "fixtures"))
   (:file "diff" :depends-on ("common" "fixtures"))
   (:file "tree" :depends-on ("common" "fixtures"))
   (:file "graph" :depends-on ("common" "fixtures"))
   (:file "config" :depends-on ("common" "fixtures"))
   (:file "odb" :depends-on ("common" "fixtures"))
   (:file "blob" :depends-on ("common" "fixtures"))
   (:file "references" :depends-on ("common"))
   (:file "error" :depends-on ("common"))
   (:file "revwalker" :depends-on ("common"))
   (:file "libgit2" :depends-on ("common")))
  :in-order-to ((compile-op (load-op :cl-git)))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :cl-git)))
