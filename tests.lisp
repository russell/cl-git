
(defpackage :cl-git-tests
  (:use
   :common-lisp
   :cl-git
   :it.bese.FiveAM)
  (:export
   #:repository-init
   #:create-commit
   #:create-random-commits))

(in-package #:cl-git-tests)


(defun gen-letter ()
  (gen-character :code (gen-integer :min (char-code #\a)
                                    :max (char-code #\z))))

(defun gen-temp-path ()
  (concatenate 'string "/tmp/cl-git.test-"
               (funcall
                (gen-string :length (gen-integer :min 5 :max 10)
                            :elements (gen-letter)))))

(defun random-number (min max)
  (funcall (gen-integer :min min :max max)))

(defun random-string (length)
  (funcall
   (gen-string :length (gen-integer :min length :max length)
               :elements (gen-letter))))

(defun assoc-default (key alist)
  (cdr (assoc key alist)))

(defun sort-strings (strings)
  (sort strings #'string-lessp))

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
  "randomly modify the file in the repo at repo-path."
  (let ((test-file (concatenate 'string repo-path "/" filename))
        (content (with-output-to-string (stream)
                   (format stream "Random text: ~A.\n" (random-string 100)))))
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      content)
    (cl-git:git-index-add filename)
    (cl-git:git-index-write)
    content))

(defun add-new-random-file (repo-path)
  "add a new random file to the index of the repo located at
REPO-PATH."
  (let ((filename (random-string 25)))
    (cl-git:with-git-repository-index
      (let ((content (add-random-file-modification repo-path filename)))
        `((filename . ,filename)(content . ,content))))))

(defun create-random-signature ()
  "create a random commit signature and return both the commit and the
signature to the commit"
  (let ((name (random-string 25))
        (email (random-string 25)))
    (let ((signature (git-signature-create
                      :name name
                      :email email))
          (sig-alist `((name . ,name)
                       (email . ,email))))
    (values signature sig-alist))))

(defun commit-random-file (repo-path &key (parents nil))
  "create a new random file in the repository located at REPO-PATH
then add and commit it to the repository. returns a list containing
commit-message filename content."
  (let ((commit-message (with-output-to-string (stream)
                          (format
                           stream "Random commit: ~A.\n"
                           (random-string 100)))))
    (cl-git:with-git-repository-index
      (multiple-value-bind (author author-alist) (create-random-signature)
        (multiple-value-bind (committer committer-alist) (create-random-signature)
          (let* ((file (add-new-random-file repo-path))
                 (commit-sha (cl-git:git-commit-create
                          (cl-git:git-oid-from-index)
                          commit-message
                          :author author
                          :committer committer
                          :parents parents)))

        (cons `(commit-sha . ,commit-sha)
              (cons `(commit-message . ,commit-message)
                    (cons `(committer . ,committer-alist)
                          (cons `(author . ,author-alist)
                                file))))))))))


(defun commit-random-file-modification (repo-path
                                        filename
                                        commit-message
                                        &key (parents nil))
  (cl-git:with-git-repository-index
    (add-random-file-modification repo-path filename)
    (cl-git:git-commit-create
     (cl-git:git-oid-from-index)
     commit-message
     :parents parents)))

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


(defun create-random-commits (repo-path number)
  "create a number of random commits to random files."
  (if (> 1 (1- number))
      (let ((new-commit (commit-random-file repo-path)))
        (cons new-commit nil))
      (let* ((commit (create-random-commits repo-path (1- number)))
             (new-commit (commit-random-file
                          repo-path
                          :parents (assoc-default 'commit-sha (car commit)))))
        (cons new-commit commit))))


(test create-random-commits
      "create a repository and add several random commits to it. then
check that the commit messages match the expected messages."
      (let ((path (gen-temp-path)))
        (finishes
         (unwind-protect
              (progn
                (cl-git:git-repository-init path)
                (cl-git:with-git-repository (path)
                  (create-random-commits path 10))
                (cl-git:with-git-repository (path)
                  (let* ((commit-list (create-random-commits path 10))
                         (tcommit (pop commit-list)))
                    (cl-git:with-git-revisions
                        (commit :sha (assoc-default 'commit-sha tcommit))
                      (is (equal (cl-git:git-commit-message commit)
                                 (assoc-default 'commit-message tcommit)))
                      (let ((tauthor (assoc-default 'author tcommit))
                            (author (cl-git:git-commit-author commit)))
                        (is (equal (car author)
                                   (assoc-default 'name tauthor)))
                        (is (equal (second author)
                                   (assoc-default 'email tauthor))))
                      (let ((tcommitter (assoc-default 'committer tcommit))
                            (committer (cl-git:git-commit-committer commit)))
                        (is (equal (car committer)
                                   (assoc-default 'name tcommitter)))
                        (is (equal (second committer)
                                   (assoc-default 'email tcommitter))))
                      (setq tcommit (pop commit-list))))))
           (progn
             (cl-fad:delete-directory-and-files path))))))

(test create-references
  "create a repository and add a file to it and a commit, then create
a reference from the commit."
  (let ((path (gen-temp-path)))
    (finishes
      (unwind-protect
           (progn
             (cl-git:git-repository-init path)
             (cl-git:with-git-repository (path)
               (let ((sha (commit-random-file-modification
                           path "test" "Test commit")))
                 (let ((reference (cl-git:git-reference-create
                                   "refs/heads/test" :sha sha)))
                   (is
                    (equal
                     (sort-strings (list reference "refs/heads/master"))
                     (sort-strings (cl-git:git-reference-listall))))))))
        (progn
          (cl-fad:delete-directory-and-files path))))))
