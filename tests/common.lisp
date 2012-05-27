;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


(defpackage :cl-git-tests
  (:use :common-lisp :cl-git :it.bese.FiveAM))


(in-package #:cl-git-tests)

(def-suite :cl-git)

(defparameter *test-repository-path* #P"/tmp/")

(defun getpid ()
  #+SBCL
  (sb-posix:getpid))

(defmacro format-string (control-string &rest format-arguments)
  `(with-output-to-string (stream)
     (format stream ,control-string ,@format-arguments)))

(defun open-test-files-p ()
  "check if there were any files left open from a test."
  (loop :for line
        :in (cdr (inferior-shell:run/lines (format-string "lsof -Fn -p ~S" (getpid))))
        :when (eq 1 (search (namestring *test-repository-path*) line))
          :collect (subseq line 1 (length line))))

(defun gen-letter ()
  (gen-character :code (gen-integer :min (char-code #\a)
                                    :max (char-code #\z))))

(defun gen-temp-path ()
  (concatenate 'string (namestring
                        (merge-pathnames "cl-git.test-"
                                         *test-repository-path*))
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

(defmacro tempory-repository ((path) &body body)
  "Create a new repository and bind the randomly generated path of the
new repository to PATH. "
  `(let ((,path (gen-temp-path)))
     (finishes
       (unwind-protect
            (progn
              (cl-git::git-repository-init ,path)
              ,@body
              (let ((open-files (open-test-files-p)))
                (when open-files
                  (fail "The following files were left open ~S" open-files))))
         (progn
           (cl-fad:delete-directory-and-files ,path))))))

(defun add-random-file-modification (repo-path filename)
  "randomly modify the file in the repo at repo-path."
  (let ((test-file (concatenate 'string repo-path "/" filename))
        (content (with-output-to-string (stream)
                   (format stream "Random text: ~A.~%" (random-string 100)))))
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      content)
    (cl-git:git-index-add filename)
    (cl-git:git-index-write)
    content))

(defun add-new-random-file (repo-path)
  "add a new random file to the index of the repo located at
REPO-PATH."
  (let ((filename (random-string 25)))
    (cl-git:with-repository-index
      (let ((content (add-random-file-modification repo-path filename)))
        `((filename . ,filename)(content . ,content))))))

(defun create-random-signature ()
  "create a random commit signature and return both the commit and the
signature to the commit"
  (let ((name (random-string 25))
        (email (random-string 25)))
    (values `(:name ,name :email ,email)
            `((name . ,name)
              (email . ,email)))))

(defun commit-random-file (repo-path &key (parents nil))
  "create a new random file in the repository located at REPO-PATH
then add and commit it to the repository. returns a list containing
commit-message filename content."
  (let ((commit-message (with-output-to-string (stream)
                          (format
                           stream "Random commit: ~A.~%"
                           (random-string 100)))))
    (cl-git:with-repository-index
      (multiple-value-bind (author author-alist) (create-random-signature)
        (multiple-value-bind (committer committer-alist) (create-random-signature)
          (let* ((file (add-new-random-file repo-path))
                 (commit-sha (cl-git:make-commit
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
  (cl-git:with-repository-index
    (add-random-file-modification repo-path filename)
    (cl-git:make-commit
     (cl-git:git-oid-from-index)
     commit-message
     :parents parents)))


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
