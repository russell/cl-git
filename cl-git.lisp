;;;; cl-git.lisp

(in-package #:cl-git)

(defparameter *git-repository* (cffi:null-pointer)
  "A global that stores a pointer to the current git repository.")

(defparameter *git-repository-index* (cffi:null-pointer)
  "A global that stores a pointer to the current git repository index.")

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
    (secs :long)
    (usecs :long))

(cffi:defcstruct git-strings
  (strings :pointer)
  (count size))

(cffi:defcstruct git-oid
  (id :unsigned-char :count 20))

(cffi:defcstruct git-signature
  (name :string)
  (email :string)
  (time timeval))


;;; Git Repositories
(cffi:defctype git-repository :pointer)
(cffi:defctype git-repository-index :pointer)


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

(cffi:defcfun ("git_reference_oid" %git-reference-oid)
    :pointer
  (reference :pointer))

(cffi:defcfun ("git_reference_lookup" %git-reference-lookup)
    :int
  (reference :pointer)
  (repository :pointer)
  (name :string))


;;; Git Object
(cffi:defcenum git-object-type
  (:any -2)		; Object can be any of the following
  (:bad -1)		; Object is invalid.
  (:commit 1)		; A commit object.
  (:tree 2)		; A tree (directory listing) object.
  (:blob 3)		; A file revision object.
  (:tag 4)		; An annotated tag object.
  (:ofs_delta 6)	; A delta, base is given by an offset.
  (:ref_delta 7)	; A delta, base is given by object id.
)

(cffi:defcfun ("git_object_lookup" %git-object-lookup)
    :int
  (object :pointer)
  (repo :pointer)
  (oid :pointer)
  (type git-object-type))

(cffi:defcfun ("git_object_close" %git-object-close)
    :void
  (object :pointer))

;;; Git Commit
(cffi:defcfun ("git_commit_message" %git-commit-message)
    :string
  (commit :pointer))

(cffi:defcfun ("git_commit_author" %git-commit-author)
    git-signature
  (commit :pointer))

(cffi:defcfun ("git_commit_committer" %git-commit-committer)
    git-signature
  (commit :pointer))


;;; Git Revision Walking
(cffi:defbitfield git-revwalk-flags
    (:none 0)
    (:topological 1)
    (:time 2)
    (:reverse 4))

(cffi:defcfun ("git_revwalk_new" %git-revwalk-new)
    :int
  (revwalk :pointer)
  (repository :pointer))

(cffi:defcfun ("git_revwalk_free" %git-revwalk-free)
    :void
  (revwalk :pointer))

(cffi:defcfun ("git_revwalk_reset" %git-revwalk-reset)
    :void
  (revwalk :pointer))

(cffi:defcfun ("git_revwalk_next" %git-revwalk-next)
    :int
  (oid :pointer)
  (revwalk :pointer))

(cffi:defcfun ("git_revwalk_sorting" %git-revwalk-sorting)
    :void
  (oid :pointer)
  (sort-mode git-revwalk-flags))

(cffi:defcfun ("git_revwalk_push" %git-revwalk-push)
    :int
    (revwalk :pointer)
    (oid :pointer))


;;; Git Index
(cffi:defcfun ("git_index_add" %git-index-add)
    :int
    (index :pointer)
    (path :pointer)
    (stage :int))

(cffi:defcfun ("git_index_clear" %git-index-clear)
    :void
  (index :pointer))


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
					     :string (namestring path)
					     :unsigned-int is-bare
					     git-code)))
      (if (= return-code 0)
	  (setf *git-repository* (cffi:mem-ref repo :pointer))
	  ; TODO should free repo pointer here
	  (error 'git-error
		 :mossage (git-error-code-text return-code)
		 :code return-code)))))


(defun git-repository-open (path)
  "open an existing repository"
  (let ((repo (cffi:foreign-alloc :pointer)))
    (let ((return-code (cffi:foreign-funcall "git_repository_open"
			  git-repository repo
			  :string (namestring path)
			  git-code)))
      (if (= return-code 0)
	  (setf *git-repository* (cffi:mem-ref repo :pointer))
	  ; TODO should free repo pointer here
	  (error 'git-error
		 :mossage (git-error-code-text return-code)
		 :code return-code)))))


(defun git-repository-free ()
  "deallocate repository"
  (cffi:foreign-funcall "git_repository_free"
			git-repository *git-repository*
			:void)
  (setf *git-repository* (cffi:null-pointer)))

(defun ensure-git-repository-exist (path &optional bare)
  (handler-case
      (progn
	(git-repository-open path)
	(git-repository-free))
    (git-error (err)
      (princ err)
      (git-repository-init path bare)
      (git-repository-free))))


(defun git-repository-index (repr)
  "load a repository index"
  (let ((index (cffi:foreign-alloc :pointer)))
    (let ((return-code (cffi:foreign-funcall
			"git_repository_index"
			:pointer index
			git-repository repo
			git-code)))
      (if (= return-code 0)
	  (setf *git-repository-index* (cffi:mem-ref repo :pointer))
	  ; TODO should free repo index pointer here
	  (error 'git-error
		 :mossage (git-error-code-text return-code)
		 :code return-code)))))


(defun git-commit-lookup (oid)
  (let ((commit (cffi:foreign-alloc :pointer)))
    (handle-git-return-code (%git-object-lookup commit *git-repository* oid
			(cffi:foreign-enum-value 'git-object-type :commit)))
    (cffi:mem-ref commit :pointer)))

(defun git-commit-message (commit)
  "return a string containing the commit message"
   (%git-commit-message commit))

(defun git-commit-author (commit)
  (cffi:with-foreign-slots ((name email time)
			    (%git-commit-author commit)
			    git-signature)
    (cffi:with-foreign-slots ((secs usecs) time timeval)
      (list name email (local-time:unix-to-timestamp secs)))))

(defun git-commit-committer (commit)
  (cffi:with-foreign-slots ((name email time)
			    (%git-commit-committer commit)
			    git-signature)
    (cffi:with-foreign-slots ((secs usecs) time timeval)
      (list name email (local-time:unix-to-timestamp secs)))))

(defun git-commit-close (commit)
  (%git-object-close commit))

(defun git-oid-fromstr (str)
  "convert a git hash to an oid"
 (cffi:with-foreign-object (oid 'git-oid)
    (handle-git-return-code (%git-oid-fromstr oid str))
    oid))

(defun git-reference-lookup (name)
  (let ((reference (cffi:foreign-alloc :pointer)))
    (handle-git-return-code (%git-reference-lookup reference *git-repository* name))
    (cffi:mem-ref reference :pointer)))

(defun git-reference-oid (reference)
  "return the oid from within the reference, this will be deallocated
with the reference"
 (let ((oid (cffi:null-pointer)))
    (setf oid (%git-reference-oid reference))
    oid))

(defun git-reference-listall (&optional flags)
  "list all the refs, filter by flag"
  (let ((git-flags (if flags flags '(:oid))))
    (cffi:with-foreign-object (string-array 'git-strings)
      (handle-git-return-code (%git-reference-listall string-array *git-repository*
						      git-flags))
      (cffi:with-foreign-slots ((strings count) string-array git-strings)
	(let ((refs
	       (loop for i below count collect
		 (cffi:foreign-string-to-lisp
		  (cffi:mem-aref strings :pointer i)))))
	  (%git-strarray-free string-array)
	  refs)))))

(defun git-revwalk (oid)
  (let ((revwalker-pointer (cffi:foreign-alloc :pointer)))
    (handle-git-return-code (%git-revwalk-new revwalker-pointer *git-repository*))
    (let ((revwalker (cffi:mem-ref revwalker-pointer :pointer)))
      (cffi:foreign-free revwalker-pointer)
      (%git-revwalk-sorting revwalker :time)
      (handle-git-return-code (%git-revwalk-push revwalker oid))
      revwalker
)))

(defmacro with-git-repository ((path) &body body)
  "Evaluates the body with *GIT-REPOSITORY* bound to a newly opened
repositony at path."
  (let ((temp (gensym)))
  `(let ((,temp *git-repository*))
     (unwind-protect
	  (progn
	    (git-repository-open ,path)
	    ,@body)
       (progn
	 (git-repository-free)
	 (setq *git-repository* ,temp))))))

(defmacro with-git-revisions ((commit &key sha head) &body body)
  "Iterate aver all the revisions, the symbol specified by commit will
be bound to each commit during each iteration."
  `(let ((oid (gensym)))
     (progn
       (cond
	 (,head
	  (setq oid (git-reference-oid (git-reference-lookup ,head))))
	 (,sha
	  (setq oid (git-oid-fromstr ,sha))
	  ))
       (let ((revwalker (git-revwalk oid)))
	 (labels ((revision-walker ()
		    (progn
		      (let ((,commit (git-commit-lookup oid)))
			(unwind-protect
			     (progn ,@body)
			  (progn (git-commit-close ,commit))
			))
		      (if (= (%git-revwalk-next oid revwalker) 0)
			  (revision-walker)))))
	   (revision-walker))))
       ))