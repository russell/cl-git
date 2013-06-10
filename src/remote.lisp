;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
;; Copyright (C) 2012 Willem Rein Oudshoorn <woudshoo@xs4all.nl>
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

(defparameter *remote-ls-values* nil)

(defbitfield (refspec-flags :unsigned-int)
  :force
  :pattern
  :matching)

(defcstruct (git-refspec :class refspec-struct-type)
  (next :pointer)
  (src :string)
  (dst :string)
  (flags refspec-flags))

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
			    :oid oid
			    :loid loid
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
  (remote %remote))

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

(defcfun ("git_remote_fetchspec" %git-remote-fetchspec)
  %refspec
  (remote %remote))

(defcfun ("git_remote_pushspec" %git-remote-pushspec)
  %refspec
  (remote %remote))


(defcfun ("git_remote_download" %git-remote-download)
  %return-value
  (remote %remote)
  (callback :pointer)
  (payload :pointer))

(defcallback collect-remote-ls-values :int ((remote-head
					     (:pointer (:struct git-remote-head)))
					    (payload :pointer))
  (declare (ignore payload))
  (push (convert-from-foreign remote-head '(:struct git-remote-head))
	*remote-ls-values*)
  0)

(defcfun ("git_remote_ls" %git-remote-ls)
  %return-value
  (remote %remote)
  (callback :pointer)
  (payload :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass remote (git-pointer) ())


(defmethod make-object ((class (eql 'remote)) name repository
                       &key url)
  "Create a new remote."
  (let ((url (if (pathnamep url) (namestring url) url)))
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
                       (convert-from-foreign string-array '%git-strings)
                     (free-translated-object string-array '%git-strings t)))))
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

(defmethod remote-connect ((remote remote) &key (direction :fetch))
  "Opens the remote connection.
The url used for the connection can be queried by GIT-URL.

The opened connection is one way, either data is retrieved from the
remote, or data is send to the remote.  The direction is specified
with the DIRECTION argument, :FETCH is for retrieving data, :PUSH is
for sending data."
  (%git-remote-connect remote direction))

(defmethod remote-connected-p ((remote remote))
  "Returns t if the connection is open, nil otherwise."
  (%git-remote-connected remote))

(defmethod remote-disconnect ((remote remote))
  "Disconnects an opened connection."
  (%git-remote-disconnect remote))

(defmethod remote-pushspec ((remote remote))
  "Returns a list of push specifications of the remote.
Each specification is property list with the following keys:

- SRC, a string matching the source references,
- DST, the pattern used to rewrite the references at the remote.
- FLAGS, a combination of the following flags :FORCE, :PATTERN, :MATCHING."
  (%git-remote-pushspec remote))

(defmethod remote-fetchspec ((remote remote))
  "Returns a list of fetch specifications for the remote.
Each specification is propety list with the keys: SRC, DST and FLAGS.

See also git-pushspec."
  (%git-remote-fetchspec remote))

(defmethod remote-download ((remote remote))
  "Download the required packfile from the remote to bring the
repository into sync."
  (%git-remote-download remote (cffi-sys:null-pointer) (cffi-sys:null-pointer)))

(defmethod git-ls ((remote remote))
  (let ((*remote-ls-values* (list)))
    (%git-remote-ls remote (callback collect-remote-ls-values) (null-pointer))
    *remote-ls-values*))

(defmethod remote-push-url ((remote remote))
  (%git-remote-push-url remote))

(defmethod remote-url ((remote remote))
  "Return the url to the remote."
  (%git-remote-url remote))
