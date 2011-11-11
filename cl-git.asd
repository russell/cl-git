;;;; cl-git.asd

(asdf:defsystem #:cl-git
  :serial t
  :depends-on (#:cffi #:local-time)
  :components ((:file "package")
               (:file "cl-git")))

