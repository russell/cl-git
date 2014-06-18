;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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

(defconstant +git-clone-options-version+ 1)

(defcenum git-clone-local-t
  (:auto)
  (:local)
  (:no-local)
  (:local-no-links))

(defcstruct git-clone-options
  (version :uint)
  (checkout-options (:struct git-checkout-options))
  (remote-callbacks (:struct git-remote-callbacks))
  (bare :boolean)
  (ignore-cert-errors :boolean)
  (local git-clone-local-t)
  (remote-name :string)
  (checkout-branch :string)
  (signature (:pointer (:struct git-signature))))

(define-foreign-type clone-options ()
  ((remote-callbacks
    :initform (make-instance 'remote-callbacks)
    :accessor remote-callbacks))
  (:simple-parser %clone-options)
  (:actual-type :pointer))

(defcfun %git-clone-init-options
    %return-value
  (options :pointer)
  (version :uint))

(defcfun %git-clone
    %return-value
  (out :pointer)
  (url :string)
  (local-path :string)
  (options %clone-options))

(defmethod initialize-instance :after ((opts clone-options) &rest initargs &key credentials)
  (declare (ignore initargs))
  (when credentials
    (setf (remote-callbacks opts) (make-instance 'remote-callbacks :credentials credentials))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Translation Methods
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod translate-to-foreign (value (type clone-options))
  (let ((ptr (foreign-alloc '(:struct git-clone-options))))
    ;; Init the structure with default values.
    (%git-clone-init-options ptr +git-clone-options-version+)
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((value clone-options) (type clone-options) ptr)
  (with-foreign-slots (((:pointer remote-callbacks))
                       ptr (:struct git-clone-options))
    ;; Fill in the remote-callbacks structure.
    (translate-into-foreign-memory (remote-callbacks value) (remote-callbacks value) remote-callbacks))
  ptr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-repository ((url string) (path pathname) &key credentials)
  (clone-repository url (namestring path) :credentials credentials))

(defmethod clone-repository ((url string) (path string) &key credentials)
  "Clone a repository from URL to PATH. CREDENTIALS "
  (with-foreign-object (repository-ref :pointer)
    ;; make a new binding for the available-credentials callback to
    ;; use.
    (let (*available-credentials*)
      (%git-clone repository-ref url path (make-instance 'clone-options :credentials credentials)))
    (make-instance 'repository
           :pointer (mem-ref repository-ref :pointer)
           :free-function #'git-repository-free)))
