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

(defparameter *oid-values* nil)

(defcfun ("git_odb_free" %git-odb-free)
    :void
  (odb %odb))

(defcfun ("git_odb_foreach" %git-odb-for-each)
    %return-value
  (odb %odb)
  (callback :pointer)
  (payload :pointer))

(defcfun ("git_odb_read" %git-odb-read)
    %return-value
  (out :pointer)
  (odb %odb)
  (oid %oid))

(defcfun ("git_odb_object_free" %git-odb-object-free)
    :void
  (obd-object %odb-object))

(defcfun ("git_odb_object_type" %git-odb-object-type)
    git-object-type
  (object %odb-object))

(defcfun ("git_odb_object_id" git-odb-object-id)
    %oid
  (object %odb-object))

(defcfun ("git_odb_object_size" %git-odb-object-size)
    size-t
  (objecgt %odb-object))

(defcfun ("git_odb_object_data" %git-odb-object-data)
    :pointer
  (object %odb-object))

(defcallback collect-oid-values :int ((oid %oid) (payload :pointer))
  (declare (ignore payload))
  (push oid *oid-values*)
  0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass odb (git-pointer) ())
(defclass odb-object (git-pointer) ())

(defmethod list-objects ((class (eql :oid)) (repository odb) &key test test-not)
  (let ((*oid-values* (list)))
    (%git-odb-for-each
     repository
     (callback collect-oid-values)
     (null-pointer))
    *oid-values*))

(defmethod list-objects ((class (eql :oid)) (repository repository) &key test test-not)
  (list-objects class (open-odb repository) :test test :test-not test-not))

(defmethod get-object ((class (eql 'odb-object)) oid (odb odb))
  (with-foreign-object (out :pointer)
    (%git-odb-read out odb oid)
    (make-instance 'odb-object
                   :pointer (mem-ref out :pointer)
                   :facilitator odb
                   :free-function #'%git-odb-object-free)))

(defmethod get-object ((class (eql 'odb-object)) oid (repository repository))
  (get-object class oid (open-odb repository)))

(defmethod oid ((object odb-object))
  (git-odb-object-id object))

(defmethod odb-type ((object odb-object))
  (%git-odb-object-type object))

(defmethod odb-size ((object odb-object))
  (%git-odb-object-size object))

(defmethod odb-data ((object odb-object))
  (let ((result (make-array (odb-size object)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
        (content (%git-odb-object-data object)))
    (loop :for index :from 0
          :repeat (length result)
          :do (setf (aref result index)
                    (mem-aref content :unsigned-char index)))
    result))
