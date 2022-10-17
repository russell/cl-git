;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2022 Russell Sim <russell.sim@gmail.com>
;; Copyright (C) 2012 Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;; Copyright (C) 2014 Eric Timmons <etimmons@alum.mit.edu>
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

(in-package #:cl-git)

;; The remote callbacks data structure and supporting functions.

(defconstant +git-remote-callbacks-version+ 1
  "The version of the C remote callbacks structure that this code
uses.")

(defstruct remote-callback-db
  (last-used-id 0)
  (id->instance-map (make-weak-hash-table :weakness :value)))

(defvar *git-remote-callbacks* (make-remote-callback-db)
  "This database maps from a uint ID to a REMOTE-CALLBACKS
structure. When a C remote callbacks structure is created, the ID of
the Lisp structure it was created from is entered into the payload
slot. This way, Lisp can figure out which callback structure generated
the callback.")

(defun remote-callback-db-id-in-use-p (id &optional (db *git-remote-callbacks*))
  (multiple-value-bind (value present-p)
	  (gethash id (remote-callback-db-id->instance-map db))
	(declare (ignore value))
	present-p))

(defun remote-callbacks-by-id (id &optional (db *git-remote-callbacks*))
  (gethash id (remote-callback-db-id->instance-map db)))

(defun register-remote-callback (remote-callback &optional (db *git-remote-callbacks*))
  "Returns the ID of the remote callback in the database."
  ;; Note: this is NOT thread safe.
  (loop
	 with max-id = (1- (expt 2 32))
	 with id = (min max-id (remote-callback-db-last-used-id db))
	 ;; Repeat "only" 2^32 times
	 repeat (1+ max-id)
	 until (not (remote-callback-db-id-in-use-p id db))
	 do (if (= id max-id)
			(setf id 0)
			(incf id))
	 finally
	   (if (remote-callback-db-id-in-use-p id db)
		   ;; This means we were unable to find an unused 32bit
		   ;; ID. Signal an error.
		   (error "Unable to find an ID for remote callbcak.")
		   ;; We were able to find an unused ID.
		   (progn
			 (setf (gethash id (remote-callback-db-id->instance-map db))
				   remote-callback)
			 (setf (remote-callback-db-last-used-id db) id)
			 (return id)))))

(define-foreign-type remote-callbacks ()
  ((id
    :reader id)
   (credentials
    :initform nil
    :initarg :credentials
    :accessor credentials))
  (:simple-parser %remote-callbacks)
  (:actual-type :pointer))

(defcallback %git-remote-callback-acquire-credentials
    :int
    ((git-cred :pointer)
     (url :string)
     (username-from-url :string)
     (allowed-types git-credential-t*)
     (payload :pointer))
  "This is the callback we give to libgit. It finds the
REMOTE-CALLBACKS structure referenced by the PAYLOAD and dispatches on
its credentials slot to provide a pointer to credentials allocated in
foreign memory."
  ;; Convert the payload to an ID.
  (let* ((id (cffi:pointer-address payload))
		 (remote-callbacks (remote-callbacks-by-id id)))
	;; If no credentials have been provided, return a positive integer.
	(if remote-callbacks
		(acquire-credentials (credentials remote-callbacks) git-cred url username-from-url allowed-types payload)
		1)))

(defmethod initialize-instance :after ((inst remote-callbacks) &rest args)
  (declare (ignore args))
  (setf (slot-value inst 'id) (register-remote-callback inst)))

(defmethod translate-to-foreign (value (type remote-callbacks))
  (let ((ptr (foreign-alloc '(:struct git-remote-callbacks))))
    ;; First, initialize the structure with default values.
    (%git-remote-init-callbacks ptr +git-remote-callbacks-version+)
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((value remote-callbacks) (type remote-callbacks) ptr)
  "Translate a remote-callbacks class into a foreign structure."
  (with-foreign-slots ((credentials-cb payload) ptr (:struct git-remote-callbacks))
    ;; Use our callback to process credential requests.
    (setf credentials-cb (callback %git-remote-callback-acquire-credentials))
    (setf payload (cffi:make-pointer (id value))))
  ptr)


(defparameter *remote-ls-values* nil)

(define-foreign-type remote (git-object)
  nil
  (:simple-parser %remote))

(defbitfield (refspec-flags :unsigned-int)
  :force
  :pattern
  :matching)


;; TODO This was removed, but has no tests so it only came up when i
;; manually checked. This needs tests as well as having.

(defcstruct (git-refspec :class refspec-struct-type)
  (next :pointer)
  (src :string)
  (dst :string)
  (flags refspec-flags))


(define-foreign-type refspec-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %refspec))


(defmethod translate-from-foreign (value (type refspec-struct-type))
  (translate-from-foreign value (make-instance 'refspec-type)))


(defmethod translate-from-foreign (value (type refspec-type))
  (unless (null-pointer-p value)
    (with-foreign-slots ((next src dst flags) value (:struct git-refspec))
      (when (or src dst flags)
        (cons (list  :src src
                     :dst dst
                     :flags flags)
              (translate-from-foreign next type))))))

(defcfun %git-remote-refspec-count
    size-t
  (remote %remote))

(defcfun %git-remote-get-refspec
    %refspec
  (remote %remote)
  (index size-t))



#+nil (defmethod translate-from-foreign (value (type indexer-stats-struct-type))
  (translate-from-foreign value (make-instance 'indexer-stats-type)))

#+nil (defmethod translate-from-foreign (value (type indexer-stats-type))
  (with-foreign-slots ((total processed) value (:struct git-indexer-stats))
		      (list :processed processed
			    :total total)))

(define-foreign-type remote-head-type ()
  nil
  (:actual-type :pointer))

(defmethod translate-from-foreign (value (type remote-head-type))
  (with-foreign-slots ((local oid loid name symref-target) value (:struct git-remote-head))
    (list :local local
          :remote-oid oid
          :local-oid loid
          :name name
          :symref-target symref-target)))

(defconstant +git-fetch-options-version+ 1)

(define-foreign-type fetch-options ()
  ((remote-callbacks
    :initform (make-instance 'remote-callbacks)
    :accessor remote-callbacks))
  (:simple-parser %fetch-options)
  (:actual-type :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun %git-fetch-options-init
    %return-value
  (options :pointer)
  (version :uint))

(defmethod translate-to-foreign (value (type fetch-options))
  (let ((ptr (foreign-alloc '(:struct git-fetch-options))))
    ;; Init the structure with default values.
    ;; TODO(RS) this struct is leaked here, there is no freeing of it
    (%git-fetch-options-init ptr +git-fetch-options-version+)
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((value fetch-options) (type fetch-options) ptr)
  (with-foreign-slots (((:pointer callbacks))
                       ptr (:struct git-fetch-options))
    ;; Fill in the remote-callbacks structure.
    (translate-into-foreign-memory (remote-callbacks value) (remote-callbacks value) callbacks)
    )
  ptr)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("git_remote_create" %git-remote-create)
    %return-value
  (remote :pointer)
  (repository %repository)
  (name :string)
  (url :string))

(defcfun ("git_remote_list" %git-remote-list)
    %return-value
  (strings :pointer)
  (repository %repository))

(defcfun ("git_remote_lookup" %git-remote-lookup)
    %return-value
  (remote-out :pointer)
  (repository %repository)
  (name :string))

(defcfun ("git_remote_free" %git-remote-free)
    :void
  (remote :pointer))

(defcfun ("git_remote_pushurl" %git-remote-push-url)
    :string
  (remote %remote))

(defcfun ("git_remote_url" %git-remote-url)
    :string
  (remote %remote))

(defcfun ("git_remote_name" %git-remote-name)
    :string
  (remote %remote))

(defcfun ("git_remote_connected" %git-remote-connected)
    :boolean
  (remote %remote))

;; TODO(RS) should all the low level data structures have a % at the
;; start, we do that with functions.
(defcenum %direction
  :fetch
  :push)

(defcfun ("git_remote_connect" %git-remote-connect)
    %return-value
  (remote %remote)
  (direction %direction)
  (callbacks %remote-callbacks)
  (proxy-options %proxy-options)
  (custom-headers :pointer))

(defcfun ("git_remote_disconnect" %git-remote-disconnect)
    :void
  (remote %remote))

(defcfun ("git_remote_get_fetch_refspecs" %git-remote-get-fetch-refspecs)
    %return-value
  (fetchspec :pointer)
  (remote %remote))

(defcfun ("git_remote_get_push_refspecs" %git-remote-get-push-refspecs)
    %return-value
  (pushspec :pointer)
  (remote %remote))

(defcfun %git-remote-download
    %return-value
  (remote %remote)
  (refspecs :pointer)  ;; pointer to git_strarray
  (options %fetch-options))

(defcfun %git-remote-init-callbacks
    %return-value
  (callbacks :pointer)
  (version :uint))

(defcfun %git-remote-ls
    %return-value
  (output :pointer)
  (size :pointer)
  (remote %remote))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition connection-error (basic-error)
  ())

(defmethod make-object ((class (eql 'remote)) name repository
                       &key url)
  "Create a new remote."
  (let ((url (namestring url)))
    (with-foreign-object (remote :pointer)
      (%git-remote-create remote repository name url)
      (make-instance 'remote
                     :pointer (mem-ref remote :pointer)
                     :facilitator repository
                     :free-function #'%git-remote-free))))

(defmethod %git-lookup-by-name ((class (eql 'remote)) name repository)
  (assert (not (null-or-nullpointer repository)))
  (with-foreign-object (remote :pointer)
    (%git-remote-lookup remote repository name)
    (mem-ref remote :pointer)))

(defun make-remote-from-name (name repository)
  "Make a weak reference by name that can be looked-up later."
  (make-instance 'remote :name name
                         :facilitator repository
                         :free-function #'%git-remote-free))

(defmethod list-objects ((class (eql 'remote)) repository &key test test-not)
  (with-foreign-object (string-array '(:struct git-strarray))
    (%git-remote-list string-array repository)
    (let ((remotes
           (mapcar (lambda (remote-name)
                     (make-remote-from-name remote-name repository))
                   (prog1
                       (convert-from-foreign string-array '(:struct git-strarray))
                     (free-converted-object string-array '(:struct git-strarray) t)))))
      (cond (test
             (remove-if-not test remotes))
            (test-not
             (remove-if test-not remotes))
            (t
             remotes)))))

(defmethod get-object ((class (eql 'remote)) name repository)
  (with-foreign-object (remote-out :pointer)
    (%git-remote-lookup remote-out repository name)
    (make-instance 'remote
           :pointer (mem-ref remote-out :pointer)
           :facilitator repository
           :free-function #'%git-remote-free)))

(defmethod full-name ((remote remote))
  "The name of the remote."
  (if (slot-value remote 'libgit2-name)
      (slot-value remote 'libgit2-name)
      (%git-remote-name remote)))

(defmethod short-name ((remote remote))
  "The name of the remote."
  (if (slot-value remote 'libgit2-name)
      (slot-value remote 'libgit2-name)
      (%git-remote-name remote)))

(defmethod print-object ((object remote) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((not (null-pointer-p (slot-value object 'libgit2-pointer)))
       (format stream "~a" (full-name object)))
      ((or (slot-value object 'libgit2-oid) (slot-value object 'libgit2-name))
       (format stream "~a (weak)" (full-name object)))
      ((slot-value object 'libgit2-disposed)
       (princ "(disposed)" stream)))))

(defgeneric remote-connect (object &key direction credentials)
  (:documentation
   "Opens the remote connection.
The url used for the connection can be queried by GIT-URL.

The opened connection is one way, either data is retrieved from the
remote, or data is send to the remote.  The direction is specified
with the DIRECTION argument, :FETCH is for retrieving data, :PUSH is
for sending data.")
  (:method ((remote remote) &key (direction :fetch) credentials)
    (let ((callbacks (make-instance 'remote-callbacks :credentials credentials)))
      (with-foreign-objects ((proxy-options '(:struct git-proxy-options))
                             (headers '(:struct git-strarray)))
        (%git-remote-connect remote direction callbacks proxy-options headers)))))

(defgeneric remote-connected-p (remote)
  (:documentation "Returns t if the connection is open, nil otherwise.")
  (:method ((remote remote))
    (%git-remote-connected remote)))

(defgeneric remote-disconnect (remote)
  (:documentation "Disconnects an opened connection.")
  (:method ((remote remote))
    (%git-remote-disconnect remote)))

(defgeneric remote-push-refspecs (remote)
  (:documentation
   "Returns a list of push specifications of the remote. ")
  (:method ((remote remote))
    (with-foreign-object (string-array '(:struct git-strarray))
      (%git-remote-get-push-refspecs string-array remote)
      (prog1
          (convert-from-foreign string-array '(:struct git-strarray))
        (free-converted-object string-array '(:struct git-strarray) t)))))

(defgeneric remote-fetch-refspecs (remote)
  (:documentation
   "Returns a list of fetch specifications for the remote.")
  (:method ((remote remote))
    (with-foreign-object (string-array '(:struct git-strarray))
      (%git-remote-get-fetch-refspecs string-array remote)
      (prog1
          (convert-from-foreign string-array '(:struct git-strarray))
        (free-converted-object string-array '(:struct git-strarray) t)))))

(defgeneric remote-download (remote)
  (:documentation "Download the required packfile from the remote to
bring the repository into sync.")
  (:method ((remote remote))
    (unless (remote-connected-p remote)
      (error 'connection-error
             :message "Remote is not connected."))
    (%git-remote-download remote (null-pointer) (make-instance 'fetch-options))))

(defgeneric ls-remote (remote)
  (:documentation "Lists the current refs at the remote.  Return a
list of the refs described by NAME, REMOTE-OID, LOCAL-OID and a
LOCAL bool that is true if the ref has a local copy.")
  (:method ((remote remote))
    (unless (remote-connected-p remote)
      (error 'connection-error
             :message "Remote is not connected."))
    (with-foreign-objects ((count 'size-t)
                           (remotes :pointer))
      (%git-remote-ls remotes count remote)
      (loop :for i :below (mem-ref count :int)
            :collect (translate-from-foreign
                      (mem-aref (mem-ref remotes :pointer) :pointer i)
                      (make-instance 'remote-head-type))))))

(defgeneric remote-push-url (remote)
  (:method ((remote remote))
    (%git-remote-push-url remote)))

(defgeneric remote-url (remote)
  (:documentation
   "Return the url to the remote.")
  (:method ((remote remote))
    (%git-remote-url remote)))
