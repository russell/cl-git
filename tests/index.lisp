;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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

(in-suite :cl-git)

(def-fixture index-with-file (filename filetext)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-test-repository ()
    (let ((filename (if (functionp filename)
                        (funcall filename)
                        filename)))
      (with-index (*test-repository-index* *test-repository*)
        (write-string-to-file filename filetext)
        (&body)))))

(defun approximately-now-p (a)
  (timestamp-approximately-equal-p a (now)))

(defun timestamp-approximately-equal-p (a b)
  (< (timestamp-difference a b) 2))


(defun plist-equal (a b)
  "Compare an plist, if the plist contains a function then use that as
  a predicate."
  (loop :for (key value) :on b :by #'cddr
        :do (cond
              ((functionp value)
               (is (funcall value (getf a key))))
              ((eql (type-of value) 'local-time:timestamp)
               (is (timestamp-approximately-equal-p (getf a key) value)))
              (t (is (equal (getf a key) value))))))

(defun index-path-test (filename filetext)
  (index-add-file filename *test-repository-index*)
  (index-write *test-repository-index*)
  (mapcar #'plist-equal
          (entries *test-repository-index*)
          `((:C-TIME ,#'approximately-now-p
             :M-TIME ,#'approximately-now-p
             :FILE-SIZE ,(length filetext)
             :OID 475587057170892494251873940086020553338329808131
             :FLAGS ,(length (namestring filename))
             :PATH ,(namestring filename)
             :STAGE 0))))

(def-test index-add-pathname (:fixture (index-with-file #P"test-file" "foo blah."))
  (index-path-test filename filetext))


(def-test index-add-string (:fixture (index-with-file "test-file" "foo blah."))
  (index-path-test filename filetext))

(def-test index-add-abspathname (:fixture (index-with-file
                                           (lambda ()
                                             (merge-pathnames (make-pathname :name "test-file")
                                                              *repository-path*))
                                           "foo blah."))
  (let ((filename (enough-namestring filename *repository-path*)))
    (index-path-test filename filetext)))

(def-test index-clear (:fixture (index-with-file #P"test-file" "foo blah."))
  "Add an entry to the index and clear it."
  (is (eq (entry-count *test-repository-index*) 0))
  (index-add-file filename *test-repository-index*)
  (is (eq (entry-count *test-repository-index*) 1))
  (index-clear *test-repository-index*)
  (is (eq (entry-count *test-repository-index*) 0)))


(def-test index-has-conflicts (:fixture repository)
  (let ((filename "test-file"))
    (with-index (test-index *test-repository*)
      (write-string-to-file filename "foo")
      (index-add-file filename test-index)
      (is (eq (index-conflicts-p test-index)
              nil)))))

(def-test index-open (:fixture repository)
  (let ((filename "test-file")
        (index-file-path (gen-temp-path)))
    (with-index (test-index *test-repository*)
      (write-string-to-file filename "foo")
      (index-add-file filename test-index)
      (unwind-protect
           (with-index (index-file index-file-path)
             (let ((entry (entry-by-index test-index 0)))
               ;; Add the entry from the other git index.
               (index-add-file entry index-file)
               (plist-equal entry
                            (entry-by-index index-file 0))))
        (when (file-exists-p index-file-path)
          (delete-file index-file-path))))))

(def-test index-new (:fixture repository)
  (let ((filename "test-file"))
    (with-index (test-index *test-repository*)
      (write-string-to-file filename "foo")
      (index-add-file filename test-index)
      (with-index (index-in-memory)
        (index-write test-index)
        (let ((entry (entry-by-index test-index 0)))
          ;; Add the entry from the other git index.
          (index-add-file entry index-in-memory)
          (let ((entry1 (entry-by-index index-in-memory 0)))
            (plist-equal entry entry1)))))))
