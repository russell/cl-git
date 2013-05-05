(asdf:defsystem #:cl-git-tests
  :defsystem-depends-on (:asdf)
  :depends-on (#:cl-git #:FiveAM #:cl-fad #:unix-options #:inferior-shell #:local-time #:alexandria)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "common")
                 (:file "commit")
                 (:file "repository")
                 (:file "references")
                 (:file "revwalker"))))
  :in-order-to ((compile-op (load-op :cl-git))))
