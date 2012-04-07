
(defpackage :cl-git-tests
  (:use
   :common-lisp
   :cl-git
   :it.bese.FiveAM)
    (:export
     #:repository-init
     #:create-commit
     #:create-commits))

(in-package #:cl-git-tests)


(defun gen-letter ()
  (gen-character :code (gen-integer :min (char-code #\a)
                                    :max (char-code #\z))))

(defun gen-temp-path ()
  (concatenate 'string "/tmp/cl-git.test-"
               (funcall
                (gen-string :length (gen-integer :min 5 :max 10)
                            :elements (gen-letter)))))

(test repository-init
  "create a repository and open it to make sure that works"
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
	   (progn
	     (cl-git:git-repository-init path :bare)
	     (cl-git:git-repository-open path)
	     (cl-git:git-repository-free))
	(progn
	  (cl-fad:delete-directory-and-files path))))))

(defun add-random-file-modification (repo-path filename)
    (let ((test-file (concatenate 'string repo-path "/" filename)))
      (with-open-file (stream test-file :direction :output :if-exists :supersede)
        (format stream "Random text: ~s.\n"
                (funcall
                 (gen-string :length (gen-integer :min 5 :max 10)
                             :elements (gen-letter)))))
      (cl-git:git-index-add filename)
      (cl-git:git-index-write)))

(defun commit-random-file-modification (repo-path filename commit-message &key (parents nil))
  (cl-git:with-git-repository-index
    (add-random-file-modification repo-path filename)
    (cl-git:git-commit-create (cl-git:git-oid-from-index) commit-message :parents parents)))

(test create-commit
      "create a repository and add a file to it."
      (let ((path (gen-temp-path)))
        (finishes
         (unwind-protect
              (progn
                (cl-git:git-repository-init path)
                (cl-git:with-git-repository (path)
                  (cl-git:with-git-revisions
                      (commit :sha
                              (commit-random-file-modification
                               path "test" "Test commit"))
                    (is (equal (cl-git:git-commit-message commit)
                               "Test commit"))))))
           (progn
             (cl-fad:delete-directory-and-files path)))))

(test create-commits
      "create a repository and add several commits to it."
      (let ((path (gen-temp-path)))
        (finishes
         (unwind-protect
              (progn
                (cl-git:git-repository-init path)
                (cl-git:with-git-repository (path)
                  (let ((commit-sha
                          (commit-random-file-modification
                           path "test" "Test commit3"
                           :parents
                           (commit-random-file-modification
                            path "test" "Test commit2"
                            :parents
                            (commit-random-file-modification
                             path "test" "Test commit1")))))
                    (let ((commit-messages (list "Test commit3"
                                                 "Test commit2"
                                                 "Test commit1")))
                      (cl-git:with-git-revisions (commit :sha commit-sha)
                        (is (equal (cl-git:git-commit-message commit)
                                   (car commit-messages)))
                        (setq commit-messages (cdr commit-messages))))))))
         (progn
           (cl-fad:delete-directory-and-files path)
           ))))
