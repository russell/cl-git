;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-


(deftask prepare-release (:summary "Prepare to release a new version.")
  (format t "working dir is ~a~%" (working-directory*))
  (with-changes (:changes (changes-from-git))
    (with-semantic-version (:current (highest-git-tag))
      (setf (next-version*) (version-from-conventional-commits))
      (write-version-sexp-file "version.lisp-expr")
      (write-changelog-file "CHANGELOG" :rst
                            :style :conventional-commits
                            :after "---------"
                            :header-char "~")

      (prin1 (changelog-lines :markdown :conventional-commits))
      (write-file ".git/RELEASE_CHANGELOG"
                  (format nil "Release ~a~%~%~a"
                          (next-version*)
                          (changelog-lines :markdown :conventional-commits))))))

(deftask release-new-version (:summary "Release a new version.")
  (format t "working dir is ~a~%" (working-directory*))
  (with-changes (:changes (changes-from-git))
    (with-semantic-version (:current (highest-git-tag))
      (setf (next-version*) (version-from-conventional-commits))
      (let ((message (read-file ".git/RELEASE_CHANGELOG")))
        (git-commit-changes
         :message message
         :author '(:name "Russell Sim"
                   :email "rsl@simopolis.xyz"))
        (git-create-tag
         :message message
         :author '(:name "Russell Sim"
                   :email "rsl@simopolis.xyz"))))))
