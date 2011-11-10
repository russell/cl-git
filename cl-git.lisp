;;;; cl-git.lisp

(in-package #:cl-git)

(cffi:define-foreign-library libgit2
  (:linux "libgit2.so")
  (:windows "libgit2.dll")
  (:darwin "libgit2.0.dylib")
  (:default "libgit2"))

(cffi:use-foreign-library libgit2)


;;; Git Common
(cffi:defctype git-code :int)

(cffi:defctype size :unsigned-int)

(cffi:defcstruct timeval
    (tv-secs :long)
    (tv-usecs :long))

(cffi:defcstruct git-strings
  (strings :pointer)
  (count size))

(cffi:defcstruct git-oid
  (id :unsigned-char :count 20))


;;; Git Repositories
(cffi:defctype git-repository :pointer)


;;; Git OID
(cffi:defcfun ("git_oid_fromstr" %git-oid-fromstr)
    :int
  (oid :pointer)
  (str :string))

;;; Git References
(cffi:defbitfield git-reference-flags
    (:invalid 0)
    (:oid 1)
    (:symbolic 2)
    (:packed 4)
    (:has-peel 8))

(cffi:defcfun ("git_reference_listall" %git-reference-listall)
    :int
  (strings :pointer)
  (repository :pointer)
  (flags git-reference-flags))


;;; Git Revision Walking
(cffi:defcfun ("git_revwalk_new" %git-revwalk-new)
    :int
  (revwalk :pointer)
  (repository :pointer))

(cffi:defcfun ("git_revwalk_free" %git-revwalk-free)
    :void
  (revwalk :pointer))

(cffi:defcfun ("git_revwalk_sorting" %git-revwalk-sorting)
    :void
  (revwalk :pointer)
  (sort-mode :unsigned-int))

;;; Git Utilities
(cffi:defcfun ("git_strarray_free" %git-strarray-free)
    :void
  (strings :pointer))

;;; Git Errors
(defun git-error-code-text (code)
  "Translate a function return code to a readable message"
  (case code
    (0 "Success.")
    (-1 "Error.")
    (-2 "Input was not a properly formatted Git object id.")
    (-3 "Input does not exist in the scope searched.")
    (-4 "Not enough space available.")
    (-5 "Consult the OS error information.")
    (-6 "The specified object is of invalid type.")
    (-7 "The specified repository is invalid.")
    (-8 "The object type is invalid or doesn't match.")
    (-9 "The object cannot be written because it's missing internal data.")
    (-10 "The packfile for the ODB is corrupted.")
    (-11 "Failed to acquire or release a file lock.")
    (-12 "The Z library failed to inflate/deflate an object's data.")
    (-13 "The queried object is currently busy.")
    (-14 "The index file is not backed up by an existing repository.")
    (-15 "The name of the reference is not valid.")
    (-16 "The specified reference has its data corrupted.")
    (-17 "The specified symbolic reference is too deeply nested.")
    (-18 "The pack-refs file is either corrupted or its format is not currently supported.")
    (-19 "The path is invalid.")
    (-20 "The revision walker is empty; there are no more commits left to iterate.")
    (-21 "The state of the reference is not valid.")
    (-22 "This feature has not been implemented yet.")
    (-23 "A reference with this name already exists.")
    (-24 "The given integer literal is too large to be parsed.")
    (-25 "The given literal is not a valid number.")
    (-26 "Streaming error.")
    (-27 "invalid arguments to function.")
    (-28 "The specified object has its data corrupted.")
    (-29 "The given short oid is ambiguous.")
    (-30 "Skip and passthrough the given ODB backend.")
    (-31 "The path pattern and string did not match.")
    (-32 "The buffer is too short to satisfy the request.")
    ))

(define-condition git-error (error)
  ((message
    :initarg :message
    :accessor git-error-message
    :initform nil
    :documentation "Text message indicating what went wrong.")
   (code
    :initarg :value
    :accessor git-error-code
    :initform nil
    :documentation "The value of the error code.")))

(defun handle-git-return-code (return-code)
     (unless (= return-code 0)
	  (error 'git-error
		 :mossage (git-error-code-text return-code)
		 :code return-code)))


(defun git-repository-init (path &optional bare)
  "return a new git repository"
  (let ((is-bare (if bare 1 0))
	(repo (cffi:foreign-alloc :pointer)))
    (let ((return-code (cffi:foreign-funcall "git_repository_init"
					     git-repository repo
					     :string path
					     :unsigned-int is-bare
					     git-code)))
      (if (= return-code 0)
	  (cffi:mem-ref repo :pointer)
	  (error 'git-error
		 :mossage (git-error-code-text return-code)
		 :code return-code)))))


(defun git-repository-open (path)
  "open an existing repository"
  (let ((repo (cffi:foreign-alloc :pointer)))
    (let ((return-code (cffi:foreign-funcall "git_repository_open"
			  git-repository repo
			  :string path
			  git-code)))
      (if (= return-code 0)
	  (cffi:mem-ref repo :pointer)
	  (error 'git-error
		 :mossage (git-error-code-text return-code)
		 :code return-code)))))


(defun git-repository-free (repo)
  "deallocate repository"
  (cffi:foreign-funcall "git_repository_free"
			git-repository repo
			:void))


(defun git-oid-fromstr (str)
  "convert a git hash to an oid"
  (cffi:with-foreign-object (oid 'git-oid)
    (handle-git-return-code (%git-oid-fromstr oid str))
    oid))

(defun git-reference-listall (repo &optional flags)
  "list all the refs, filter by flag"
  (let ((git-flags (if flags flags '(:oid))))
    (cffi:with-foreign-object (string-array 'git-strings)
      (let ((return-code (%git-reference-listall string-array repo
						 git-flags)))
	(if (= return-code 0)
	    (progn
	      (cffi:with-foreign-slots ((strings count) string-array git-strings)
		(let ((refs
		       (loop for i below count collect
			 (cffi:foreign-string-to-lisp
			  (cffi:mem-aref strings :pointer i)))))
		  (%git-strarray-free string-array)
		  refs)))
	    (error 'git-error
		   :mossage (git-error-code-text return-code)
		   :code return-code))))))
