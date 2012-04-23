;;;; cl-git.lisp

(in-package #:cl-git)

(defparameter *git-repository* (cffi:null-pointer)
  "A global that stores a pointer to the current Git repository.")

(defparameter *git-repository-index* (cffi:null-pointer)
  "A global that stores a pointer to the current Git repository index.")

(cffi:define-foreign-library libgit2
  (:linux "libgit2.so.0")
  (:windows "libgit2.dll")
  (:darwin "libgit2.0.dylib")
  (:default "libgit2"))

(cffi:use-foreign-library libgit2)


(defparameter *git-oid-hex-size* (+ 40 1)
  "The size of a Git commit hash.")


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

;;; ODB
(cffi:defctype git-odb :pointer)

;;; Git OID
(cffi:defcfun ("git_oid_fromstr" %git-oid-fromstr)
    :int
  (oid :pointer)
  (str :string))

(cffi::defctype size-t :unsigned-long)

(cffi:defcfun ("git_oid_to_string"
               %git-oid-tostr)
    (:pointer :char)
  (out (:pointer :char))
  (n size-t)
  (oid :pointer))

;;; Git Error
(cffi:defcfun ("git_lasterror" git-lasterror) :pointer)


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
    git-oid
  (reference :pointer))

(cffi:defcfun ("git_reference_lookup" %git-reference-lookup)
    :int
  (reference :pointer)
  (repository :pointer)
  (name :string))

(cffi:defcfun ("git_reference_create_oid" %git-reference-create-oid)
    :int
  (reference :pointer)
  (repository :pointer)
  (name :string)
  (oid :pointer)
  (force :int))

(cffi:defcfun ("git_reference_free" %git-reference-free)
    :void
  (reference :pointer))


;;; Git Object
(cffi:defcfun ("git_object_id" %git-object-id)
    :pointer
  (object :pointer))


(cffi:defcenum git-object-type
  (:any -2)       ; Object can be any of the following
  (:bad -1)       ; Object is invalid.
  (:commit 1)     ; A commit object.
  (:tree 2)       ; A tree (directory listing) object.
  (:blob 3)       ; A file revision object.
  (:tag 4)        ; An annotated tag object.
  (:ofs_delta 6)  ; A delta, base is given by an offset.
  (:ref_delta 7)) ; A delta, base is given by object id.


(cffi:defcfun ("git_object_type" %git-object-type)
    git-object-type
  (object :pointer))

(cffi:defcfun ("git_object_lookup" %git-object-lookup)
    :int
  (object :pointer)
  (repo :pointer)
  (oid :pointer)
  (type git-object-type))

(cffi:defcfun ("git_object_free"
               %git-object-free)
    :void
  (object :pointer))

;;; Reference
(cffi:defcfun ("git_reference_type" %git-reference-type)
    git-reference-flags
  (reference :pointer))

;;; Git Commit
(cffi:defcfun ("git_commit_create" %git-commit-create)
    :int
  (oid :pointer)
  (repo :pointer)
  (update-ref :pointer)
  (author :pointer)
  (committer :pointer)
  (message-encoding :pointer)
  (message :pointer)
  (tree :pointer)
  (parent-count :int)
  (parents :pointer))

(cffi:defcfun ("git_commit_message" %git-commit-message)
    :string
  (commit :pointer))

(cffi:defcfun ("git_commit_author" %git-commit-author)
    git-signature
  (commit :pointer))

(cffi:defcfun ("git_commit_committer" %git-commit-committer)
    git-signature
  (commit :pointer))

(cffi:defcfun ("git_tag_type" %git-tag-type)
    git-object-type
  (tag :pointer))

(cffi:defcfun ("git_tag_target" %git-tag-target)
    :int
  (reference :pointer)
  (tag :pointer))

(cffi:defcfun ("git_tag_tagger" %git-tag-tagger)
    git-signature
  (tag :pointer))

;;; Git Tree
(cffi:defcfun ("git_tree_create_fromindex" %git-tree-create-fromindex)
    :int
  (oid :pointer)
  (index :pointer))


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
    (stage :int)) ; an int from 0 to 4

(cffi:defcfun ("git_index_clear" %git-index-clear)
    :void
  (index :pointer))

(cffi:defcfun ("git_index_free" %git-index-free)
    :void
  (index :pointer))

(cffi:defcfun ("git_index_write" %git-index-write)
    :int
  (index :pointer))

;;; Git Utilities
(cffi:defcfun ("git_strarray_free" %git-strarray-free)
    :void
  (strings :pointer))


(defun getenv (name &optional default)
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name)
     default))

(defun default-email ()
  (or (getenv "MAIL")
      (concatenate 'string (getenv "USERNAME") "@" (machine-instance))))

(defun git-signature-create (&key (name nil) (email nil) (time nil))
  "Create a new GIT-SIGNATURE if the NAME isn't specified then use the
USER environment variable.  If no EMAIL is specified then use the
USERNAME at hostname.  If there is no TIME specified then use the
current time."
  (let ((signature (cffi:foreign-alloc 'git-signature)))
    (setf
     (cffi:foreign-slot-value signature 'git-signature 'name)
     (or name (getenv "USER"))

     (cffi:foreign-slot-value signature 'git-signature 'email)
     (or email (default-email)))

    (if time
        (setf (cffi:foreign-slot-value signature 'git-signature 'time) time)
        (cffi:with-foreign-slots ((time) signature git-signature)
          (setf
           (cffi:foreign-slot-value time 'timeval 'secs)
           (local-time:timestamp-to-unix (local-time:now))
           (cffi:foreign-slot-value time 'timeval 'usecs)
           0)))
  signature))


;;;

(defmacro with-git-signature-return (form)
  `(cffi:with-foreign-slots ((name email time)
			     ,form
			     git-signature)
     (cffi:with-foreign-slots ((secs usecs) time timeval)
       (list name email (local-time:unix-to-timestamp secs)))))

;;; Git Errors
(define-condition git-error (error)
  ((message
    :initarg :message
    :accessor git-error-message
    :initform nil
    :documentation "Text message indicating what went wrong.")
   (code
    :initarg :code
    :accessor git-error-code
    :initform nil
    :documentation "The value of the error code."))
  (:report (lambda (condition stream)
	     (format stream "git error ~D: ~A"
		     (git-error-code condition) (git-error-message condition)))))

(defun handle-git-return-code (return-code)
     (unless (= return-code 0)
	  (error 'git-error
		 :message (cffi:foreign-string-to-lisp (git-lasterror))
		 :code return-code)))


(defun git-oid-tostr (oid)
  "Convert an OID to a string."
  (cffi:with-foreign-pointer-as-string (str *git-oid-hex-size*)
    (%git-oid-tostr str *git-oid-hex-size* oid)
    (cffi:foreign-string-to-lisp str)
    ))


(defun git-repository-init (path &optional bare)
  "Init a new Git repository.  A positive value for BARE init a bare
repository.  Returns the path of the newly created Git repository."
  (let ((is-bare (if bare 1 0))
        (repo (cffi:foreign-alloc :pointer)))
    (unwind-protect
         (progn
           (handle-git-return-code
            (cffi:foreign-funcall "git_repository_init"
                                  git-repository repo
                                  :string (namestring path)
                                  :unsigned-int is-bare
                                  git-code))
           path)
      (progn
        (cffi:foreign-funcall "git_repository_free"
                              git-repository (cffi:mem-ref repo :pointer)
                              :void)
        (cffi:foreign-free repo)))))


(defun git-repository-open (path)
  "Open an existing repository and set the global *GIT-REPOSITORY*
variable to the open repository.  If the PATH contains a .git
directory it will be opened instead of the specified path."
  (let ((repo (cffi:foreign-alloc :pointer))
	(path (or (cl-fad:directory-exists-p
		   (merge-pathnames
		    #p".git/"
		    (cl-fad:pathname-as-directory path)))
		  path)))
    (unwind-protect
	 (progn
	   (cffi:with-foreign-strings ((%path (namestring path)))
	     (handle-git-return-code
	      (cffi:foreign-funcall "git_repository_open"
				    git-repository repo
				    :string (namestring path)
				    git-code)))
	   (setf *git-repository* (cffi:mem-ref repo :pointer)))
      (progn
	(cffi:foreign-free repo)))))


(defun git-repository-free ()
  "Deallocate repository and re-initialise the *GIT-REPOSITORY*
variable as a null painter."
  (cffi:foreign-funcall "git_repository_free"
			git-repository *git-repository*
			:void))


(defun ensure-git-repository-exist (path &optional bare)
  "Open a repository at location, if the repository doesn't exist
create it.  BARE is an optional keyword, if specified then the newly
created repository will be bare."
  (handler-case
      (progn
	(git-repository-open path)
	(git-repository-free)
	path)
    (git-error (err)
      (git-repository-init path bare)
      (git-repository-free)
      path)))


(defmacro with-git-repository-index (&body body)
  "Load a repository index uses the current *GIT-REPOSITORY* as the
current repository and sets *GIT-REPOSITORY-INDEX* as the newly opened
index."
    `(let ((*git-repository-index* (cffi:null-pointer)))
       (unwind-protect
	    (progn
	      (let ((index (cffi:foreign-alloc :pointer)))
		(handle-git-return-code (cffi:foreign-funcall
					 "git_repository_index"
					 :pointer index
					 git-repository *git-repository*
					 git-code))
		(setf *git-repository-index* (cffi:mem-ref index :pointer))
		(cffi:foreign-free index))
	      ,@body)
	 (progn
	   (%git-index-free *git-repository-index*)))))


(defun git-commit-create (oid message &key
                                        (update-ref "HEAD")
                                        (author nil)
                                        (committer nil)
                                        (parents nil))
  "Create a new commit from the tree with the OID specified and
MESSAGE.  Optional UPDATE-REF is the name of the reference that will
be updated to point to this commit.  The default value \"HEAD\" will
updote the head of the current branch.  If it's value is NULL then no
reference will be updated.  AUTHOR is an optional instance of a
GIT-SIGNATURE that details the commit author.  COMMITTER is an
optional instance of a GIT-SIGNATURE the details the commit committer.
PARENTS is an optional list of parent commits sha1 hashes."
  (let ((tree (cffi:foreign-alloc :pointer))
        (newoid (cffi:foreign-alloc 'git-oid))
        (%author (or author (git-signature-create)))
        (%committer (or committer (git-signature-create)))
        (%tree (git-tree-lookup oid))
        (parents (if (listp parents) parents (list parents))))
    (unwind-protect
         (progn
           ; lookup all the git commits
           (setq parents (mapcar #'(lambda (c) (git-commit-lookup (lookup-commit :sha c))) parents))
           (cffi:with-foreign-object (%parents :pointer (length parents))
             (cffi:with-foreign-strings ((%message message)
                                         (%message-encoding "UTF-8")
                                         (%update-ref update-ref))
               (loop for parent in parents
                     counting parent into i
                     do (setf (cffi:mem-aref %parents :pointer (1- i)) parent))
               (handle-git-return-code
                (%git-commit-create
                 newoid
                 *git-repository*
                 %update-ref
                 %author
                 %committer
                 %message-encoding
                 %message
                 %tree
                 (length parents)
                 %parents))))
           (git-oid-tostr newoid))
      (progn
        (mapcar #'(lambda (c) (git-commit-close c)) parents)
        (cffi:foreign-free newoid)
        (git-tree-close tree)
        (cffi:foreign-free tree)))))

(defun git-object-id (object)
  "Returns the oid identifying `object'"
  (let ((oid (cffi:null-pointer)))
    (setf oid (%git-object-id object))
    oid))

(defun git-object-lookup (oid type)
  "Returns a reference to the git odb (object) which is identified by the oid.
The type argument specifies which type is expected.  If the found
object is not of the right type, an error will be signaled.  The type
is one of :any, :bad, :commit :tree :blob :tag :ofs_delta :refs_delta.
:any and :bad are special cases.  :any means return the object found,
do not do a typecheck and is a valid type, but should typically not
occur.

Note that the returned git object should be freed with git-object-free."
  (let ((obj (cffi:foreign-alloc :pointer)))
    (prog2
	(handle-git-return-code
	 (%git-object-lookup
	  obj *git-repository* oid
	  (cffi:foreign-enum-value 'git-object-type type)))
	(cffi:mem-ref obj :pointer)
      (cffi:foreign-free obj))))

(defun git-tree-lookup (oid)
  "Lookup a Git tree object, the value returned will need to be freed
manually with GIT-TREE-CLOSE."
  (git-object-lookup oid :tree))

(defun git-tree-close (tree)
  "Close the tree and free the memory allocated to the tree."
  (%git-object-free tree))

(defun git-commit-lookup (oid)
  "Look up a commit by oid, return the resulting commit.  This commit
will need to be freed manually with GIT-COMMIT-CLOSE."
  (git-object-lookup oid :commit))

(defun git-commit-message (commit)
  "Return a string containing the commit message."
   (%git-commit-message commit))

(defun git-commit-author (commit)
  "Given a commit return the commit author's signature."
  (with-git-signature-return (%git-commit-author commit)))

(defun git-commit-committer (commit)
  "Given a commit return the commit committer's signature."
  (with-git-signature-return (%git-commit-committer commit)))

(defun git-commit-close (commit)
  "Close the commit and free the memory allocated to the commit."
  (%git-object-free commit))

(defun git-tag-type (tag)
  (%git-tag-type tag))

(defun git-tag-target (tag)
  (let ((obj (cffi:foreign-alloc :pointer)))
    (prog2
	(handle-git-return-code
	 (%git-tag-target obj tag))
	(cffi:mem-ref obj :pointer)
      (cffi:foreign-free obj))))

(defun git-tag-tagger (tag)
  (with-git-signature-return (%git-tag-tagger tag)))

(defun git-oid-fromstr (str)
  "Convert a Git hash to an oid."
 (cffi:with-foreign-object (oid 'git-oid)
    (handle-git-return-code (%git-oid-fromstr oid str))
    oid))

(defun git-reference-lookup (name)
  (let ((reference (cffi:foreign-alloc :pointer)))
    (unwind-protect
	 (progn
	   (handle-git-return-code
	    (%git-reference-lookup reference *git-repository* name))
	   (cffi:mem-ref reference :pointer))
      (cffi:foreign-free reference))))

(defun git-reference-oid (reference)
  "Return the oid from within the reference, this will be deallocated
with the reference."
 (cffi:with-foreign-object (oid :pointer)
   (setf oid (%git-reference-oid reference))
    oid))

(defun git-reference-listall (&optional flags)
  "List all the refs, filter by flag."
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

(defun git-reference-create (name &key sha head force)
  "Create new reference in the current repository with NAME linking to
SHA or HEAD.  If FORCE is true then override if it already exists."
  (let ((reference (cffi:null-pointer))
        (oid (lookup-commit :sha sha :head head)))
    (cffi:with-foreign-string (ref-name name)
      (cffi:with-foreign-object (%force :int)
        (setf %force (if force 1 0))
        (unwind-protect
             (handle-git-return-code
              (%git-reference-create-oid
               reference *git-repository*
               ref-name oid %force))
          (progn
              (%git-reference-free reference)
              (cffi:foreign-free reference))))))
  name)


(defun git-revwalk (oid-or-oids)
  "Walk all the revisions from a specified OID, or OIDs.
OID can be a single object id, or a list of object ids.
The OIDs can be anything that can be resolved by commit-oid-from-oid.
In general this means, commits and tags."
  (let ((revwalker-pointer (cffi:foreign-alloc :pointer)))
    (handle-git-return-code
     (%git-revwalk-new revwalker-pointer *git-repository*))
    (let ((revwalker (cffi:mem-ref revwalker-pointer :pointer)))
      (cffi:foreign-free revwalker-pointer)
      (%git-revwalk-sorting revwalker :time)
      (loop for oid in (if (atom oid-or-oids) (list oid-or-oids) oid-or-oids) do
	   (handle-git-return-code (%git-revwalk-push revwalker
						      (commit-oid-from-oid oid))))
      revwalker)))


(defun git-index-add (path)
  "Add a file at PATH to the repository, the PATH should be relative
to the repository."
  (let ((path (namestring path)))
    (cffi:with-foreign-string (path-str path)
      (handle-git-return-code
       (%git-index-add *git-repository-index* path-str 0)))))

(defun git-index-clear ()
  "Remove all staged data from the index at *GIT-REPOSITORY-INDEX*."
  (handle-git-return-code
   (%git-index-clear *git-repository-index*)))

(defun git-index-write ()
  "Write the current index stored in *GIT-REPOSITORY-INDEX* to disk."
  (handle-git-return-code
   (%git-index-write *git-repository-index*)))

(defun git-oid-from-index ()
  "Write the curret index to the disk and return an oid to it,
returned oid will have to be freed manually."
  (let ((oid (cffi:foreign-alloc 'git-oid)))
    (handle-git-return-code
     (%git-tree-create-fromindex oid *git-repository-index*))
    oid))

(defmacro with-git-repository ((path) &body body)
  "Evaluates the body with *GIT-REPOSITORY* bound to a newly opened
repositony at path."
  `(let ((*git-repository* nil))
     (unwind-protect
	  (progn
	    (git-repository-open ,path)
	    ,path
	    ,@body)
       (progn
	 (git-repository-free)))))

(defun git-commit-from-oid (oid)
  "Returns a git-commit object identified by the `oid'.
This is an extended version of git-commit-lookup.
If theo oid refers to a tag, this function will return the git-commit
pointed to by the tag.  The call git-commit-lookup will fail."
  (let ((git-object (git-object-lookup oid :any)))
    (ecase (%git-object-type git-object)
      (:tag (prog1 (git-tag-target git-object) (%git-object-free git-object)))
      (:commit git-object))))

(defun commit-oid-from-oid (oid)
  "Returns the oid of a commit referenced by `oid'.
If the `oid' refers to a commit the function is basically a
no-op.  However if `oid' refers to a tag, it will return
the oid of the target of the tag."
  (let ((commit (git-commit-from-oid oid)))
    (prog1
	(git-object-id commit)
      (%git-object-free commit))))


(defun lookup-commit (&key sha head)
  "Returns an oid for a single commit (or tag).  It takes a single
 keyword argument, either SHA or HEAD If the keyword argument is SHA
 the value should be a SHA1 id as a string.  The value for the HEAD
 keyword should be a symbolic reference to a git commit."
    (cond
      (head (git-reference-oid (git-reference-lookup head)))
       (sha (git-oid-fromstr sha))))

(defun lookup-commits (&key sha head)
   "Similar to lookup-commit, except that the keyword arguments also
 except a list of references.  It will returns list of oids instead of
 a single oid.  If the argument was a single reference, it will return
 a list containing a single oid."
   (flet ((lookup-loop (keyword lookup)
          (loop for reference
                in
                (if (atom lookup) (list lookup) lookup)
                collect
                (lookup-commit keyword reference))))
     (cond
       (head (lookup-loop :head head))
       (sha (lookup-loop :sha sha)))))

(defmacro bind-git-commits (bindings &body body)
  "Lookup commits specified in the bindings.  The bindings syntax is
similar to the LET syntax except instead of needing to specify an
initial form key arguments are used.  Atleast one key arguments SHA or
HEAD must be specified.  SHA is a hash of the commit.  HEAD is a full
ref path."
  `(let ,(mapcar #'(lambda (s)
		     `(,(car s) (cffi:null-pointer)))
	  bindings)
     (unwind-protect
	  (progn
	    ,@(mapcar
	       #'(lambda (s)
		   `(setf ,(car s)
			  (git-commit-from-oid
			   (lookup-commit ,@(cdr s)))))
	       bindings)
	    ,@body)
       (progn
	 ,@(mapcar
	    #'(lambda (s)
		`(progn
		   (unless (cffi:null-pointer-p ,(car s))
		     (git-commit-close ,(car s)))))
		   ;(cffi:foreign-free ,(car s))))
	    bindings)))))


(defmacro with-git-revisions ((commit &rest rest &key sha head) &body body)
  "Iterate aver all the revisions, the symbol specified by commit will
be bound to each commit during each iteration.  This uses a return
special call to stop iteration."
  `(let ((oids (lookup-commits ,@rest)))
     (let ((revwalker (git-revwalk oids)))
       (cffi:with-foreign-object (oid 'git-oid)
         (block nil
           (labels ((revision-walker ()
                      (progn
                        (if (= (%git-revwalk-next oid revwalker) 0)
                            (progn
                              (let ((,commit (git-commit-from-oid oid)))
                                (unwind-protect
                                     (progn ,@body)
                                  (progn (git-commit-close ,commit))))
                              (revision-walker))))))
             (revision-walker)))))))
