;;;; cl-git.lisp

(in-package #:cl-git)

(defparameter *git-repository* nil
  "A global that stores a pointer to the current Git repository.")

(defparameter *git-repository-index* nil
  "A global that stores a pointer to the current Git repository index.")

(cffi:define-foreign-library libgit2
  (:linux "libgit2.so.0")
  (:windows "libgit2.dll")
  (:darwin "libgit2.0.dylib")
  (:default "libgit2"))

(cffi:use-foreign-library libgit2)

(defparameter *git-oid-hex-size* (+ 40 1)
  "The size of a Git commit hash.")

(defparameter *git-oid-size* 20)

;;; Helper function for debugging

(defun null-or-nullpointer (obj)
  (or (not obj) (cffi:null-pointer-p obj)))


;;;  Define types
(cffi:define-foreign-type oid-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %oid))

(cffi:define-foreign-type time-type ()
  nil
  (:actual-type :int64)
  (:simple-parser %time))

(cffi:define-foreign-type git-signature-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %git-signature))

(cffi:define-foreign-type git-tree-entry-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %tree-entry))

(cffi:define-foreign-type object ()
  ((%object :accessor pointer :initarg :pointer :initform (null-pointer))
   (%repository :accessor %repository :initarg :repository-pointer))
  (:actual-type :pointer)
  (:simple-parser %object))

(cffi:define-foreign-type commit (object-type)
  nil
  (:actual-type :pointer)
  (:simple-parser %commit))

(cffi:define-foreign-type tag (object-type)
  nil
  (:actual-type :pointer)
  (:simple-parser %tag))

;;; Git Common
(cffi:defctype git-code :int)

(cffi:defctype size :unsigned-long)

(cffi:defcstruct timeval
    (time %time)
    (offset :int))

(cffi:defcstruct git-strings
  (strings :pointer)
  (count size))

(cffi:defcstruct git-oid
  (id :unsigned-char :count 20)) ;; should be *git-oid-size* or +git-oid-size+

(cffi:defcstruct git-signature
  (name :string)
  (email :string)
  (time timeval))

(cffi:defcstruct git-tree-entry
  (attr :unsigned-int)
  (filename :string)
  (oid git-oid)
  (filename-len size)
  (removed :int))


;;; Foreign type translation

;; OID
(defmethod cffi:translate-to-foreign ((value number) (type oid-type))
  (declare (ignore type))
  (let ((c-oid (cffi:foreign-alloc 'git-oid)))
    (loop
       :for c-index :from 0 :below *git-oid-size*
       :for byte-index :downfrom (* 8 (1- *git-oid-size*)) :by 8
       :do
       (setf (cffi:mem-aref (cffi:foreign-slot-pointer c-oid 'git-oid 'id)
			    :unsigned-char c-index)
	     (ldb (byte 8 byte-index) value)))
    c-oid))

(defmethod cffi:translate-to-foreign ((value string) (type oid-type))
  (cffi:translate-to-foreign (parse-integer value :radix 16) type))

(defmethod cffi:translate-to-foreign ((value t) (type oid-type))
  (if (cffi:pointerp value)
      (values value t)
      (error "Cannot convert type: ~A to git-oid struct" (type-of value))))

(defmethod cffi:translate-from-foreign (value (type oid-type))
  "Translates a pointer to a libgit2 oid structure to an integer, the lisp
version of the oid.  If the pointer is a C null pointer return nil.
This can happen for example when the oid is asked for a reference and the
reference is symbolic."
  (declare (ignore type))
  (if (cffi:null-pointer-p value)
      nil
      (let ((lisp-oid 0))
	(loop
	   :for c-index :from 0 :below *git-oid-size*
	   :for byte-index :downfrom (* 8 (1- *git-oid-size*)) :by 8
	   :do
	   (setf (ldb (byte 8 byte-index) lisp-oid)
		 (cffi:mem-aref (cffi:foreign-slot-pointer value 'git-oid 'id)
				:unsigned-char c-index)))
	lisp-oid)))

(defmethod cffi:free-translated-object (pointer (type oid-type) do-not-free)
  (unless do-not-free (cffi:foreign-free pointer)))

;; git time

(defmethod cffi:translate-from-foreign (value (type time-type))
  "Translate a git time_t to a local-time"
  (local-time:unix-to-timestamp value))

(defmethod cffi:translate-to-foreign ((value local-time:timestamp) (type time-type))
  "Translate a local-time value to a git time_t"
  (local-time:timestamp-to-unix value))

(defmethod cffi:translate-to-foreign ((value integer) (type time-type))
  "Translate a universal-time to a git time_t"
  (cffi:translate-to-foreign
   (local-time:universal-to-timestamp (local-time:timestamp-to-unix value)) type))

;; git signature

(defmethod cffi:translate-to-foreign ((value list) (type git-signature-type))
  (declare (ignore type))
  (let ((signature (cffi:foreign-alloc 'git-signature)))
    (cffi:with-foreign-slots ((name email time) signature git-signature)
      (setf name (getf value :name (getenv "USER")))
      (setf email (getf value :email (default-email)))
      (cffi:with-foreign-slots ((time offset) time timeval)
	(let ((time-to-set (getf value :time (local-time:now))))
	  (setf time time-to-set)
	  (setf offset (/ (local-time:timestamp-subtimezone
			   time-to-set local-time:*default-timezone*)
			  60)))))
    signature))

(defmethod cffi:translate-to-foreign ((value t) (type git-signature-type))
  (if (cffi:pointerp value)
      (values value t)
      (error "Cannot convert type: ~A to git-signature struct" (type-of value))))

(defmethod cffi:translate-from-foreign (value (type git-signature-type))
  (cffi:with-foreign-slots ((name email time) value git-signature)
    (cffi:with-foreign-slots ((time) time timeval)
      (list :name name :email email :time time))))

(defmethod cffi:free-translated-object (pointer (type git-signature-type) do-not-free)
  (unless do-not-free (cffi:foreign-free pointer)))

;;;
(defmethod cffi:translate-from-foreign (value (type git-tree-entry-type))
  (cffi:with-foreign-slots ((attr filename oid removed) value git-tree-entry)
    (list :attr attr :filename filename
	  :oid  (cffi:convert-from-foreign oid '%oid) :removed removed)))


;;; Object

(defmethod translate-to-foreign (value (type object))
  (if (pointerp value)
      value
      (pointer value)))

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

;;; The return value should not be freed.
(cffi:defcfun ("git_oid_to_string"
               %git-oid-tostr)
    (:pointer :char)
  (out (:pointer :char))
  (n size-t)
  (oid %oid))

;;; Git Error
(cffi:defcfun ("git_lasterror" git-lasterror) :string)

;;; Git Config

(cffi:defcfun ("git_repository_config" %git-repository-config)
    :int
  (out :pointer)
  (repository :pointer))

(cffi:defcfun ("git_config_free" git-config-free)
    :void
  "Free the git configuration object that is acquired with git-repository-config."
  (config :pointer))

(cffi:defcfun ("git_config_foreach" %git-config-foreach)
    :int
  (config :pointer)
  (callback :pointer)
  (payload :pointer))

;;; Git Status
(cffi:defbitfield git-status-flags
  (:index-new        1)
  (:index-modified   2)
  (:index-deleted    4)
  (:worktree-new     8)
  (:worktree-modified 16)
  (:worktree-deleted  32)
  (:ignored           64))

(cffi:defcfun ("git_status_foreach" %git-status-for-each)
    :int
  (repository :pointer)
  (callback :pointer)
  (payload :pointer))

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

(cffi:defcfun ("git_reference_oid" git-reference-oid)
    %oid
  "Return the oid from within the reference."
  (reference :pointer))

(cffi:defcfun ("git_reference_lookup" %git-reference-lookup)
    :int
  (reference :pointer)
  (repository :pointer)
  (name :string))

(cffi:defcfun ("git_reference_resolve" %git-reference-resolve)
    :int
  (resolved-ref :pointer)
  (reference :pointer))

(cffi:defcfun ("git_reference_create_oid" %git-reference-create-oid)
    :int
  (reference :pointer)
  (repository :pointer)
  (name :string)
  (oid %oid)
  (force (:boolean :int)))

(cffi:defcfun ("git_reference_free" %git-reference-free)
    :void
  (reference :pointer))

;;; Git Object
(cffi:defcfun ("git_object_id" git-object-id)
    %oid
  "Returns the oid identifying `object'"
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


(cffi:defcfun ("git_object_type" git-object-type)
    git-object-type
  "Returns the type of the git object."
  (object :pointer))

(cffi:defcfun ("git_object_lookup" %git-object-lookup)
    :int
  (object %object)
  (repo :pointer)
  (oid %oid)
  (type git-object-type))

(cffi:defcfun ("git_object_free" git-object-free)
    :void
  "Free the git object."
  (object :pointer))

;;; Blobs
(cffi:defcfun ("git_blob_lookup" %git-blob-lookup)
    :int
  (blob-out :pointer)
  (repository :pointer)
  (oid %oid))

(cffi:defcfun ("git_blob_rawcontent" %git-blob-raw-content)
    :pointer
  (blob :pointer))

(cffi:defcfun ("git_blob_rawsize" git-blob-raw-size)
    size
  "The number of content bytes in the blob."
  (blob :pointer))

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
  (author %git-signature)
  (committer %git-signature)
  (message-encoding :pointer)
  (message :pointer)
  (tree :pointer)
  (parent-count :int)
  (parents :pointer))

(defcfun ("git_commit_message" commit-message)
    :string
  "Return a string containing the commit message."
  (commit %commit))

(defcfun ("git_commit_author" commit-author)
    %git-signature
  "Given a commit return the commit author's signature."
  (commit %commit))

(defcfun ("git_commit_committer" commit-committer)
    %git-signature
  "Given a commit return the commit committer's signature."
  (commit %commit))

(defcfun ("git_commit_parentcount" commit-parentcount)
    :int
  "Returns the number of parent commits of the argument."
  (commit %commit))

(defcfun ("git_commit_parent_oid" commit-parent-oid)
    %oid
  "Returns the oid of the parent with index `parent-index' in the list
of parents of the commit `commit'."
  (commit %commit)
  (n :int))

(cffi:defcfun ("git_commit_tree" %git-commit-tree)
    :int
  (tree-out :pointer)
  (commit :pointer))

;;; Tag functions
(cffi:defcfun ("git_tag_type" git-tag-type)
    git-object-type
  (tag :pointer))

(cffi:defcfun ("git_tag_target" %git-tag-target)
    :int
  (reference :pointer)
  (tag :pointer))

(cffi:defcfun ("git_tag_tagger" git-tag-tagger)
    %git-signature
  (tag :pointer))

(cffi:defcfun ("git_tag_name" git-tag-name)
    :string
  "Returns the name of the tag"
  (tag :pointer))

(cffi:defcfun ("git_tag_message" tag-message)
    :string
  "Returns the message of the tag"
  (tag %tag))

;;; Git Tree
(cffi:defcfun ("git_tree_create_fromindex" %git-tree-create-fromindex)
    :int
  (oid :pointer)
  (index :pointer))

(cffi:defcfun ("git_tree_id" git-tree-oid)
    %oid
  "Returns the oid of the tree."
  (tree :pointer))

(cffi:defcfun ("git_tree_entrycount" git-tree-entry-count)
    :unsigned-int
  "Returns the number of tree entries in the tree object.
This does count the number of direct children, not recursively."
  (tree :pointer))

(cffi:defcfun ("git_tree_entry_byindex" git-tree-entry-by-index)
    %tree-entry
  "Returns the tree entry at index"
  (tree :pointer)
  (index :unsigned-int))

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
  (walk :pointer)
  (sort-mode git-revwalk-flags))

(cffi:defcfun ("git_revwalk_push" %git-revwalk-push)
    :int
    (revwalk :pointer)
    (oid %oid))


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

;;;

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
		 :message (git-lasterror)
		 :code return-code)))


(defun git-oid-tostr (oid)
  "Convert an OID to a string."
  (cffi:with-foreign-pointer-as-string (str *git-oid-hex-size*)
    (%git-oid-tostr str *git-oid-hex-size* oid)
    (cffi:foreign-string-to-lisp str)))


(defun git-repository-init (path &optional bare)
  "Init a new Git repository.  A positive value for BARE init a bare
repository.  Returns the path of the newly created Git repository."
  (cffi:with-foreign-object (repo :pointer)
    (handle-git-return-code
            (cffi:foreign-funcall "git_repository_init"
                                  git-repository repo
                                  :string (namestring path)
                                  :unsigned-int (if bare 1 0)
                                  git-code))
           (cffi:foreign-funcall "git_repository_free"
                                 git-repository (cffi:mem-ref repo :pointer)
                                :void)
           path))


(defun git-repository-open (path)
  "Open an existing repository and set the global *GIT-REPOSITORY*
variable to the open repository.  If the PATH contains a .git
directory it will be opened instead of the specified path."
  (assert (null-or-nullpointer *git-repository-index*))
  (assert (null-or-nullpointer *git-repository*))
  (cffi:with-foreign-object (repository-ref :pointer)
    (let ((path (or (cl-fad:directory-exists-p
                     (merge-pathnames
                      #p".git/"
                      (cl-fad:pathname-as-directory path)))
                    (truename path))))
      (cffi:with-foreign-strings ((%path (namestring path)))
        (handle-git-return-code
         (cffi:foreign-funcall "git_repository_open"
                               git-repository repository-ref
                               :string (namestring path)
                               git-code)))
      (cffi:with-foreign-object (repository-ref1 :pointer)
        (setf repository-ref1 (cffi:mem-ref repository-ref :pointer))
        (setf repository-ref (cffi:mem-ref repository-ref :pointer))
        (finalize repository-ref
                  (lambda ()
                    (cffi:foreign-funcall "git_repository_free"
                                          :pointer repository-ref1
                                          :void)))
        repository-ref))))


(defun ensure-git-repository-exist (path &optional bare)
  "Open a repository at location, if the repository doesn't exist
create it.  BARE is an optional keyword, if specified then the newly
created repository will be bare."
  (handler-case
      (progn
        (git-repository-open path)
        path)
    (git-error ()
      (git-repository-init path bare)
      path)))

(defun git-repository-config ()
  "Return the config object of the current open repository."
  (assert (not (null-or-nullpointer *git-repository*)))
  (cffi:with-foreign-object (config :pointer)
    (handle-git-return-code (%git-repository-config config *git-repository*))
    (cffi:mem-ref config :pointer)))

(defparameter *config-values* nil)

(cffi:defcallback collect-config-values :int ((key :string) (value :string))
  (push (cons key value) *config-values*)
  0);;; replace with success

(defun git-config-values (config)
  "Returns the key value pairs in the config as an association list."
  (let ((*config-values* (list)))
    (handle-git-return-code
     (%git-config-foreach config
			  (cffi:callback collect-config-values)
			  (cffi:null-pointer)))
    *config-values*))

(defparameter *status-values* nil)

(cffi:defcallback collect-status-values :int ((path :string) (value git-status-flags) (payload :pointer))
  (declare (ignore payload))
  (push (cons path value) *status-values*)
  0)

(defun git-status ()
  (assert (not (null-or-nullpointer *git-repository*)))
  (let ((*status-values* (list)))
    (handle-git-return-code
     (%git-status-for-each *git-repository*
			   (cffi:callback collect-status-values)
			   (cffi:null-pointer)))
    *status-values*))

(defmacro with-git-repository-index (&body body)
  "Load a repository index uses the current *GIT-REPOSITORY* as the
current repository and sets *GIT-REPOSITORY-INDEX* as the newly opened
index."
    `(let ((*git-repository-index* (cffi:null-pointer)))
       (unwind-protect
	    (progn
	      (assert (not (null-or-nullpointer *git-repository*)))
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
  (assert (not (null-or-nullpointer *git-repository*)))

  (let ((newoid (cffi:foreign-alloc 'git-oid))
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
                     do (setf (cffi:mem-aref %parents :pointer (1- i)) (pointer parent)))
               (handle-git-return-code
                (%git-commit-create
                 newoid
                 *git-repository*
                 %update-ref
                 author
                 committer
                 %message-encoding
                 %message
                 %tree
                 (length parents)
                 %parents))))
           (git-oid-tostr newoid))
      (progn
        (git-tree-close %tree)
        (cffi:foreign-free newoid)))))

(defun git-commit-tree (commit)
  "Returns the tree object of the commit."
  (cffi:with-foreign-object (tree :pointer)
    (handle-git-return-code
     (%git-commit-tree tree commit))
    (cffi:mem-aref tree :pointer)))

(defun git-object-lookup (oid type)
  "Returns a reference to the git odb (object) which is identified by the oid.
The type argument specifies which type is expected.  If the found
object is not of the right type, an error will be signaled.  The type
is one of :any, :bad, :commit :tree :blob :tag :ofs_delta :refs_delta.
:any and :bad are special cases.  :any means return the object found,
do not do a typecheck and is a valid type, but should typically not
occur.

Note that the returned git object should be freed with git-object-free."

  (assert (not (null-or-nullpointer *git-repository*)))

  (cffi:with-foreign-object (obj-ptr :pointer)
    (let ((obj-type (ecase type
                      (:any 'object)
                      (:commit 'commit)
                      (:tag 'tag))))
      (handle-git-return-code
       (%git-object-lookup
        obj-ptr *git-repository* oid type))
      (let ((object (make-instance obj-type :pointer (mem-ref obj-ptr :pointer))))
        (with-foreign-object (finalizer-ptr :pointer)
          (setf finalizer-ptr (mem-ref obj-ptr :pointer))
          (finalize object
                    (lambda ()
                      (git-object-free finalizer-ptr))))
        object))))

(defun git-blob-lookup (oid)
  "Returns a blob identified by the oid."
  (assert (not (null-or-nullpointer *git-repository*)))

  (cffi:with-foreign-object (obj :pointer)
    (handle-git-return-code
     (%git-blob-lookup obj *git-repository* oid))
    (cffi:mem-ref obj :pointer)))

(defun git-blob-raw-content (blob)
  (let ((result (make-array (git-blob-raw-size blob)
			    :element-type '(unsigned-byte 8)
			    :initial-element 0))
	(content (%git-blob-raw-content blob)))
    (loop :for index :from 0
	 :repeat (length result)
	 :do
	 (setf (aref result index) (cffi:mem-aref content :unsigned-char index)))
    result))

(defun git-tree-lookup (oid)
  "Lookup a Git tree object, the value returned will need to be freed
manually with GIT-TREE-CLOSE."
  (git-object-lookup oid :tree))

(defun git-tree-close (tree)
  "Close the TREE and free the memory allocated to the tree."
  (git-object-free tree))

(defun git-tree-entries (tree)
  "Return all direct children of `tree'."
  (loop :repeat (git-tree-entry-count tree)
     :for index :from 0
     :collect (git-tree-entry-by-index tree index)))

(defun git-commit-lookup (oid)
  "Look up a commit by oid, return the resulting commit.  This commit
will need to be freed manually with GIT-COMMIT-CLOSE."
  (git-object-lookup oid :commit))

(defun git-commit-close (commit)
  "Close the commit and free the memory allocated to the commit."
  (git-object-free commit))

(defun git-tag-target (tag)
  (let ((obj (cffi:foreign-alloc :pointer)))
    (prog2
	(handle-git-return-code
	 (%git-tag-target obj tag))
	(cffi:mem-ref obj :pointer)
      (cffi:foreign-free obj))))

(defun git-oid-fromstr (str)
  "Convert a Git hash to an oid."
 (cffi:with-foreign-object (oid 'git-oid)
    (handle-git-return-code (%git-oid-fromstr oid str))
    (cffi:convert-from-foreign oid '%oid)))

(defun git-reference-lookup (name)
  "Find a reference by its full name e.g.: ref/heads/master"
  (assert (not (null-or-nullpointer *git-repository*)))
  (cffi:with-foreign-object (reference :pointer)
    (handle-git-return-code
     (%git-reference-lookup reference *git-repository* name))
    (cffi:mem-ref reference :pointer)))

(defun git-reference-resolve (reference)
  "If the reference is symbolic, follow the it until it finds a non
symbolic reference.  The result should be freed independently from the
argument."
  (cffi:with-foreign-object (resolved-ref :pointer)
    (handle-git-return-code
     (%git-reference-resolve resolved-ref reference))
    (cffi:mem-ref resolved-ref :pointer)))

(defun git-reference-listall (&rest flags)
  "List all the refs, filter by FLAGS.  The flag options
are :INVALID, :OID, :SYMBOLIC, :PACKED or :HAS-PEEL"
  (assert (not (null-or-nullpointer *git-repository*)))

  (let ((git-flags (if flags flags '(:oid))))
    (cffi:with-foreign-object (string-array 'git-strings)
      (handle-git-return-code (%git-reference-listall
                               string-array *git-repository*
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
  (assert (not (null-or-nullpointer *git-repository*)))

  (let ((oid (lookup-commit :sha sha :head head)))
    (cffi:with-foreign-object (reference :pointer)
      (unwind-protect
	   (handle-git-return-code
	    (%git-reference-create-oid
	     reference *git-repository*
	     name oid force))
	(progn
	  (%git-reference-free (cffi:mem-ref reference :pointer))))))
  name)


(defun git-revwalk (oid-or-oids)
  "Walk all the revisions from a specified OID, or OIDs.
OID can be a single object id, or a list of object ids.
The OIDs can be anything that can be resolved by commit-oid-from-oid.
In general this means, commits and tags."
  (assert (not (null-or-nullpointer *git-repository*)))

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
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (let ((path (namestring path)))
    (cffi:with-foreign-string (path-str path)
      (handle-git-return-code
       (%git-index-add *git-repository-index* path-str 0)))))

(defun git-index-clear ()
  "Remove all staged data from the index at *GIT-REPOSITORY-INDEX*."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (handle-git-return-code
   (%git-index-clear *git-repository-index*)))

(defun git-index-write ()
  "Write the current index stored in *GIT-REPOSITORY-INDEX* to disk."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (handle-git-return-code
   (%git-index-write *git-repository-index*)))

(defun git-oid-from-index ()
  "Write the current index to the disk and return an oid to it."
  (assert (not (null-or-nullpointer *git-repository-index*)))
  (cffi:with-foreign-object (oid 'git-oid)
    (handle-git-return-code
     (%git-tree-create-fromindex oid *git-repository-index*))
    (cffi:convert-from-foreign oid '%oid)))

(defmacro with-git-repository ((path) &body body)
  "Evaluates the body with *GIT-REPOSITORY* bound to a newly opened
repositony at path."
  `(let ((*git-repository* (git-repository-open ,path)))
     ,@body))

(defun git-commit-from-oid (oid)
  "Returns a git-commit object identified by the `oid'.
This is an extended version of git-commit-lookup.
If the oid refers to a tag, this function will return the git-commit
pointed to by the tag.  The call git-commit-lookup will fail."
  (let ((git-object (git-object-lookup oid :any)))
    (ecase (git-object-type git-object)
      (:tag (prog1 (git-tag-target git-object) (git-object-free git-object)))
      (:commit git-object))))

(defun commit-oid-from-oid (oid)
  "Returns the oid of a commit referenced by `oid'.
If the `oid' refers to a commit the function is basically a
no-op.  However if `oid' refers to a tag, it will return
the oid of the target of the tag."
  (let ((commit (git-commit-from-oid oid)))
	(git-object-id (pointer commit))))

(defun git-commit-parent-oids (commit)
  "Returns a list of oids identifying the parent commits of `commit'."
  (loop for index from 0 below (git-commit-parent-count commit)
       collect (git-commit-parent-oid commit index)))

(defun lookup-commit (&key sha head)
  "Returns an oid for a single commit (or tag).  It takes a single
 keyword argument, either SHA or HEAD If the keyword argument is SHA
 the value should be a SHA1 id as a string.  The value for the HEAD
 keyword should be a symbolic reference to a git commit."
    (cond
      (head (let* ((original-ref (git-reference-lookup head))
		   (resolved-ref (git-reference-resolve original-ref)))
	      (prog1 (git-reference-oid resolved-ref)
		(git-object-free resolved-ref)
		(git-object-free original-ref))))
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
	    ,@body))))


(defmacro with-git-revisions ((commit &rest rest &key sha head) &body body)
  "Iterate aver all the revisions, the symbol specified by commit will
be bound to each commit during each iteration.  This uses a return
special call to stop iteration."
  (declare (ignore sha))
  (declare (ignore head))
  `(let ((oids (lookup-commits ,@rest)))
     (let ((revwalker (git-revwalk oids)))
       (cffi:with-foreign-object (oid 'git-oid)
         (block nil
           (labels ((revision-walker ()
                      (progn
                        (if (= (%git-revwalk-next oid revwalker) 0)
                            (progn
                              (let ((,commit (git-commit-from-oid oid)))
                                ,@body)
                              (revision-walker))))))
             (unwind-protect
		  (revision-walker)
	       (%git-revwalk-free revwalker))))))))
