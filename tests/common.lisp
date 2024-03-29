;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2022 Russell Sim <russell.sim@gmail.com>
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

(in-package #:cl-git-tests)

(def-suite :cl-git)

(defparameter *test-repository-path* #P"/tmp/")

(defun getpid ()
  #+clisp (os:process-id)
  #+(or cmu scl) (unix:unix-getpid)
  #+sbcl (sb-unix:unix-getpid)
  #+gcl (system:getpid)
  #+openmcl (ccl::getpid)
  #+lispworks (system::getpid)
  #+ecl (si:getpid)
  #+ccl (ccl::getpid)
  #-(or clisp cmu scl sbcl gcl openmcl lispworks ecl ccl) (getpid-from-environment))

(defmacro format-string (control-string &rest format-arguments)
  `(with-output-to-string (stream)
     (format stream ,control-string ,@format-arguments)))

(defun open-test-files-p (repository-path)
  "check if there were any files left open from a test."
  (loop :for line
        :in (cdr (inferior-shell:run/lines (format-string "lsof -Fn -p ~S" (getpid))))
        :when (eq 1 (search (namestring repository-path) line))
          :collect (subseq line 1 (length line))))


(defun alpha-or-whitespace-p (c)
  (when (member (char-int c)
                 `(,@(iota 25 :start 65)
                   ,@(iota 25 :start 97)))
    t))

(defun gen-alpha-numeric ()
  "return either a letter or number."
  (gen-character :code-limit 126
                 :alphanumericp #'alpha-or-whitespace-p))

(defun gen-temp-path ()
  (merge-pathnames
   (make-pathname
    :directory `(:relative
                ,(concatenate 'string  "cl-git-test-"
                             (funcall
                              (gen-string :length (gen-integer :min 5 :max 10)
                                          :elements (gen-alpha-numeric))))))
   *test-repository-path*))

(defun gen-temp-file-path ()
  (merge-pathnames
   *test-repository-path*
   (make-pathname
    :name (concatenate 'string  "cl-git-test-"
                       (funcall
                        (gen-string :length (gen-integer :min 5 :max 10)
                                    :elements (gen-alpha-numeric)))))))

(defun random-number (min max)
  (funcall (gen-integer :min min :max max)))

(defun random-time ()
  (unix-to-timestamp
   (funcall (gen-integer
             :min 0
             :max (timestamp-to-unix (now))))))

(defun random-string (length)
  (funcall
   (gen-string :length (gen-integer :min length :max length)
               :elements (gen-alpha-numeric))))

(defun random-list (&key (length (gen-integer :min 0 :max 10)))
  (gen-list :length length :elements (gen-string :elements (gen-alpha-numeric))))

(defun assoc-default (key alist)
  (cdr (assoc key alist)))

(defun sort-strings (strings)
  (sort strings #'string-lessp))

(defvar *repository-path* nil
  "the path to the current test repository.")

(defvar *test-repository* nil
  "store the state of the current test repository.")

(defvar *test-repository-state* nil
  "store the state of the current test repository.")

(defvar *test-traverse-state* nil
  "store the state of the current test repository traversal state, it
will update to the new head when a new commit is added.")

(defvar *test-repository-index* nil)

(defmacro with-test-repository ((&key bare) &body body)
  "Create a new repository and bind the randomly generated path."
  `(let ((*repository-path* (gen-temp-path))
         *test-repository-state*
         *test-traverse-state*)
     (finishes
       (unwind-protect
            (progn
              (init-repository *repository-path* :bare ,bare)
              (let ((*test-repository* (open-repository *repository-path*)))
                ,@body
                (free *test-repository*))
              (let ((open-files (open-test-files-p *repository-path*)))
                (when open-files
                  (fail "The following files were left open ~S" open-files))))
         (progn
           (delete-directory-and-files *repository-path*))))))

(defun write-string-to-file (filename content
                             &optional (repo-path *repository-path*))
  (let* ((filename (merge-pathnames
                    (if (pathnamep filename)
                        filename
                        (make-pathname :name filename))
                    repo-path))
         (test-file (namestring filename)))
    (with-open-file (stream test-file :direction :output
                                      :if-exists :supersede)
      (format stream content))))

(defun read-file-to-string (filename)
  (with-open-file (stream filename)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun make-test-commit (commit)
  "Make a commit to the current repository and return the updated
commit alist. The commit argument is an alist that should contain the
keys :FILES :MESSAGE :AUTHOR :COMMITTER the returned alist will also
contain the a :SHA containing the sha1 hash of the newly created
commit."
  (with-index (*test-repository-index* *test-repository*)
    (dolist (file (getf commit :files))
      (funcall #'write-string-to-file (getf file :filename) (getf file :text))
      (index-add-file (getf file :filename) *test-repository-index*))
    (index-write *test-repository-index*)
    (setf (getf commit :sha)
          (oid
           (make-commit
            (index-to-tree *test-repository-index*)
            (getf commit :message)
            :repository *test-repository*
            :parents (getf commit :parents)
            :author (getf commit :author)
            :committer (getf commit :committer)))))
  commit)

(defun random-commit (&key
                        (message (random-string 100))
                        (author (list :name (random-string 50)
                                      :email (random-string 50)
                                      :time (random-time)))
                        (committer (list :name (random-string 50)
                                         :email (random-string 50)
                                         :time (random-time)))
                        (file-count 1)
                        (files (loop :for count :upfrom 0
                                     :collect (list
                                               :filename (random-string 10)
                                               :text (random-string 100))
                                     :while (< count file-count)))
                        parents)
  (let ((parents (if (listp parents) parents (list parents))))
    (list
     :parents parents
     :message message
     :files files
     :author author
     :committer committer)))

(defun add-test-revision (&rest rest)
  (let ((args rest))
    (setf (getf args :parents) (getf (car *test-repository-state*) :sha))
    (push
     (make-test-commit
      (apply #'random-commit args))
     *test-repository-state*)
    (setf *test-traverse-state* *test-repository-state*)))

(defun next-test-commit ()
  "return the next commit from the traverser and move the traverser to
it's parent."
  (pop *test-traverse-state*))

(defun make-test-revisions (depth)
  "create a number of random commits to random files."
  (loop :for count :upfrom 0
        :while (< count depth)
        :do (add-test-revision)))

(defun make-test-revision (&rest rest)
  "Create one test commit and return the test alist representation."
  (apply #'add-test-revision rest)
  (next-test-commit))

(defun commit-to-alist (commit)
  "Convert a commit to an alist, that is the same format as the test
commit alist."
  (list
   :message (message commit)
   :committer (committer commit)
   :author (author commit)))

(defun time-to-unix (time)
  (if (integerp time)
      time
      (timestamp-to-unix time)))

(defun signature-equal (x y)
  "Compare two signatures and make sure they are equal."
  (is (equal (getf x :name)
             (getf y :name)))
  (is (equal (getf x :email)
             (getf y :email)))
  (is (equal (time-to-unix (getf x :time))
             (time-to-unix (getf y :time)))))

(defun commit-equal (x y)
  "return true if the commits are equal."
  (let ((x (if (typep x 'commit)
               (commit-to-alist x)
               x))
        (y (if (typep y 'commit)
               (commit-to-alist y)
               y)))
    (is (equal (getf x :message)
               (getf y :message)))
    ;; TODO test parents
    (signature-equal (getf x :author)
                     (getf y :author))
    (signature-equal (getf x :committer)
                     (getf y :committer))))
