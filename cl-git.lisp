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

(cffi:defctype size :unsigned-int)

(cffi:defcstruct timeval
    (tv-secs :long)
    (tv-usecs :long))

(cffi:defbitfield git-reference-flags
    (:invalid 0)
    (:oid 1)
    (:symbolic 2)
    (:packed 4)
    (:has-peel 8))

(cffi:defcstruct git-strings
  (strings :pointer)
  (count size))

(cffi:defcfun ("git_reference_listall" %git-reference-listall)
    :int
  (strings :pointer)
  (repository :pointer)
  (flags git-reference-flags))

(cffi:defcfun ("git_strarray_free" %git-strarray-free)
    :void
  (strings :pointer))


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


(defun git-repository-open (path)
  "open an existing repository"
  (let ((repo (cffi:foreign-alloc :pointer)))
    (if (= (cffi:foreign-funcall "git_repository_open"
			  git-repository repo
			  :string path
			  git-code) 0)
	(cffi:mem-ref repo :pointer)
	(error "something happend"))))


(defun git-repository-free (repo)
  "deallocate repository"
  (cffi:foreign-funcall "git_repository_free"
			git-repository repo
			:void))

(defun git-reference-listall (repo &optional flags)
  "list all the refs, filter by flag"
  (let ((git-flags (if flags flags '(:oid))))
    (cffi:with-foreign-object (string-array 'git-strings)
      (if (= (%git-reference-listall string-array repo
				     git-flags) 0)
	  (progn
	    (cffi:with-foreign-slots ((strings count) string-array git-strings)
	      (let ((refs
		     (loop for i below count collect
		       (cffi:foreign-string-to-lisp
			(cffi:mem-aref strings :pointer i)))))
		(%git-strarray-free string-array)
		refs)))
	  (error "something happend")))))