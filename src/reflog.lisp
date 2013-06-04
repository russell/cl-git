;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-Level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun ("git_reflog_free" %git-reflog-free)
  :void
  (reflog %reflog))

(defcfun ("git_reflog_read" %git-reflog-read)
    %return-value
  (out :pointer)
  (reference %reference))

(defcfun ("git_reflog_entrycount" %git-reflog-entry-count)
    :unsigned-int
  (reflog %reflog))

(defcfun ("git_reflog_entry_byindex" %git-reflog-entry-by-index)
    :pointer
  (reflog %reflog)
  (index size-t))

(defcfun ("git_reflog_entry_committer" %git-reflog-entry-commiter)
    %git-signature
  (reflog-entry %reflog-entry))

(defcfun ("git_reflog_entry_message" %git-reflog-entry-message)
    :string
  (reflog-entry %reflog-entry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; High-Level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass reflog (git-pointer) ())
(defclass reflog-entry (git-pointer) ())

(defmethod reflog ((reference reference))
  (with-foreign-object (reflog :pointer)
    (%git-reflog-read reflog reference)
    (make-instance 'reflog
		   :pointer (mem-ref reflog :pointer)
		   :facilitator (facilitator reference)
		   :free-function #'%git-reflog-free)))


(defmethod git-entry-count ((reflog reflog))
  (%git-reflog-entry-count reflog ))

(defmethod git-entry-by-index ((reflog reflog) index)
  (make-instance 'reflog-entry
		 :pointer (%git-reflog-entry-by-index reflog index)
		 :facilitator reflog
		 :free-function #'identity))

(defmethod committer ((reflog-entry reflog-entry))
  (%git-reflog-entry-commiter reflog-entry))

(defmethod message ((reflog-entry reflog-entry))
  (%git-reflog-entry-message reflog-entry))
