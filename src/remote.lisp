;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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

(defconstant +git-remote-callbacks-version+ 1)

(defparameter *remote-ls-values* nil)

(defbitfield (refspec-flags :unsigned-int)
  :force
  :pattern
  :matching)

(defcstruct git-remote-callbacks
  (version :uint)
  (sideband-progress :pointer)
  (completion :pointer)
  (credentials :pointer)
  (transfer-progress :pointer)
  (update-tips :pointer)
  (payload :pointer))

(define-foreign-type remote-callbacks ()
  ((credentials
    :initform nil
    :initarg :credentials
    :accessor credentials))
  (:simple-parser %remote-callbacks)
  (:actual-type :pointer))

(defmethod translate-to-foreign (value (type remote-callbacks))
  (let ((ptr (foreign-alloc '(:struct git-remote-callbacks))))
    ;; First, initialize the structure with default values.
    (%git-remote-init-callbacks ptr +git-remote-callbacks-version+)
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((value remote-callbacks) (type remote-callbacks) ptr)
  "Translate a remote-callbacks class into a foreign structure. This
assumes that *available-credentials* has a new binding established."
  (with-foreign-slots ((credentials) ptr (:struct git-remote-callbacks))
    ;; Use our callback to process credential requests.
    (setf credentials (callback git-cred-acquire-cb))
    (setf *available-credentials* (credentials value)))
  ptr)


(defcstruct (git-refspec :class refspec-struct-type)
  (next :pointer)
  (src :string)
  (dst :string)
  (flags refspec-flags))


(define-foreign-type remote (git-object)
  nil
  (:simple-parser %remote))


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


(defcstruct (git-indexer-stats :class indexer-stats-struct-type)
  (total :unsigned-int)
  (processed :unsigned-int))

#+nil (defmethod translate-from-foreign (value (type indexer-stats-struct-type))
  (translate-from-foreign value (make-instance 'indexer-stats-type)))

#+nil (defmethod translate-from-foreign (value (type indexer-stats-type))
  (with-foreign-slots ((total processed) value (:struct git-indexer-stats))
		      (list :processed processed
			    :total total)))

(defcstruct (git-remote-head :class remote-head-struct-type)
  (local :boolean)
  (oid (:struct git-oid))
  (loid (:struct git-oid))
  (name :string))

(defmethod translate-from-foreign (value (type remote-head-struct-type))
  (with-foreign-slots ((local oid loid name) value (:struct git-remote-head))
    (list :local local
          :remote-oid oid
          :local-oid loid
          :name name)))

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

(defcfun ("git_remote_load" %git-remote-load)
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

(defcenum %direction
  :fetch
  :push)

(defcfun ("git_remote_connect" %git-remote-connect)
  %return-value
  (remote %remote)
  (direction %direction))

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

(defcfun ("git_remote_download" %git-remote-download)
  %return-value
  (remote %remote)
  (callback :pointer)
  (payload :pointer))

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
  "Lookup a reference by name and return a pointer to it.  This
pointer will need to be freed manually."
  (assert (not (null-or-nullpointer repository)))
  (with-foreign-object (remote :pointer)
    (%git-remote-load remote repository name)
    (mem-ref remote :pointer)))

(defun make-remote-from-name (name repository)
  "Make a weak reference by name that can be looked-up later."
  (make-instance 'remote :name name
                         :facilitator repository
                         :free-function #'%git-remote-free))

(defmethod list-objects ((class (eql 'remote)) repository &key test test-not)
  (with-foreign-object (string-array '(:struct git-strings))
    (%git-remote-list string-array repository)
    (let ((remotes
           (mapcar (lambda (remote-name)
                     (make-remote-from-name remote-name repository))
                   (prog1
                       (convert-from-foreign string-array '(:struct git-strings))
                     (free-converted-object string-array '(:struct git-strings) t)))))
      (cond (test
             (remove-if-not test remotes))
            (test-not
             (remove-if test-not remotes))
            (t
             remotes)))))

(defmethod get-object ((class (eql 'remote)) name repository)
  (with-foreign-object (remote-out :pointer)
    (%git-remote-load remote-out repository name)
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

(defgeneric remote-connect (object &key direction)
  (:documentation
   "Opens the remote connection.
The url used for the connection can be queried by GIT-URL.

The opened connection is one way, either data is retrieved from the
remote, or data is send to the remote.  The direction is specified
with the DIRECTION argument, :FETCH is for retrieving data, :PUSH is
for sending data.")
  (:method ((remote remote) &key (direction :fetch))
    (%git-remote-connect remote direction)))

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
    (with-foreign-object (string-array '(:struct git-strings))
      (%git-remote-get-push-refspecs string-array remote)
      (prog1
          (convert-from-foreign string-array '(:struct git-strings))
        (free-converted-object string-array '(:struct git-strings) t)))))

(defgeneric remote-fetch-refspecs (remote)
  (:documentation
   "Returns a list of fetch specifications for the remote.")
  (:method ((remote remote))
    (with-foreign-object (string-array '(:struct git-strings))
      (%git-remote-get-fetch-refspecs string-array remote)
      (prog1
          (convert-from-foreign string-array '(:struct git-strings))
        (free-converted-object string-array '(:struct git-strings) t)))))

(defgeneric remote-download (remote)
  (:documentation "Download the required packfile from the remote to
bring the repository into sync.")
  (:method ((remote remote))
    (unless (remote-connected-p remote)
      (error 'connection-error
             :message "Remote is not connected."))
    (%git-remote-download remote (null-pointer) (null-pointer))))

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
            :collect (mem-ref (mem-aref (mem-ref remotes :pointer) :pointer i)
                              '(:struct git-remote-head))))))

(defgeneric remote-push-url (remote)
  (:method ((remote remote))
    (%git-remote-push-url remote)))

(defgeneric remote-url (remote)
  (:documentation
   "Return the url to the remote.")
  (:method ((remote remote))
    (%git-remote-url remote)))
