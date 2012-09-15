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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("git_blob_rawcontent" %git-blob-raw-content)
    :pointer
  (blob %blob))

(defcfun ("git_blob_rawsize" git-raw-size)
    size
  "The number of content bytes in the blob."
  (blob %blob))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass blob (object) ())

(defmethod git-lookup ((class (eql :blob))
		       oid &key (repository *git-repository*))
  (git-object-lookup oid class :repository repository))

(defun git-raw-content (blob)
  "Returns the content of the blob BLOB as an array of UNSIGNED-BYTE's"
  (let ((result (make-array (git-raw-size blob)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
        (content (%git-blob-raw-content blob)))
    (loop :for index :from 0
          :repeat (length result)
          :do (setf (aref result index)
                    (mem-aref content :unsigned-char index)))
    result))
