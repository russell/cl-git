;;;; cl-git.asd

(asdf:defsystem #:cl-git
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "cl-git")))

