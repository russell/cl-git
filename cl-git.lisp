;;;; cl-git.lisp

(in-package #:cl-git)

(cffi:define-foreign-library libgit2
  (:linux "libgit2.so")
  (:windows "libgit2.dll")
  (:darwin "libgit2.0.dylib")
  (:default "libgit2"))

(cffi:use-foreign-library libgit2)

(cffi:defctype git-code :int)

(cffi:defctype git-repository :pointer)

(defun git-repository-init (path &optional bare)
  "return a new git repository"
  (let ((is-bare (if bare 1 0))
	(repo (cffi:foreign-alloc :pointer)))
    (if (= (cffi:foreign-funcall "git_repository_init"
			  git-repository repo
			  :string path
			  :unsigned-int is-bare 
			  git-code) 0)
	(cffi:mem-ref repo :pointer)
	(error "something happend"))))


(defun git-repository-free (repo)
  "deallocate repository"
  (cffi:foreign-funcall "git_repository_free"
			git-repository repo
			:void))
