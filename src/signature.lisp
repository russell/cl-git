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

(defcstruct timeval
  (time %time)
  (offset :int))

(defcstruct git-signature
  (name :string)
  (email :string)
  (time (:struct timeval)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign type translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Time

(defmethod translate-from-foreign (value (type time-type))
  "Translate a git time_t to a local-time"
  (local-time:unix-to-timestamp value))

(defmethod translate-to-foreign ((value local-time:timestamp) (type time-type))
  "Translate a local-time value to a git time_t"
  (local-time:timestamp-to-unix value))

(defmethod translate-to-foreign ((value integer) (type time-type))
  "Translate a universal-time to a git time_t"
  (translate-to-foreign
   (local-time:universal-to-timestamp (local-time:timestamp-to-unix value)) type))

;; Signatures

(defmethod translate-to-foreign ((value list) (type git-signature-type))
  (declare (ignore type))
  (let ((signature (foreign-alloc '(:struct git-signature))))
    (with-foreign-slots ((name email) signature (:struct git-signature))
      (setf name (getf value :name (getenv "USER")))
      (setf email (getf value :email (default-email)))
      (with-foreign-slots ((time offset) 
			   (foreign-slot-pointer signature '(:struct git-signature) 'time) 
			   (:struct timeval))
        (let ((time-to-set (getf value :time (local-time:now))))
          (setf time time-to-set)
          (setf offset (/ (local-time:timestamp-subtimezone
                           time-to-set local-time:*default-timezone*)
                          60)))))
    signature))

(defmethod translate-to-foreign ((value t) (type git-signature-type))
  (if (pointerp value)
      (values value t)
      (error "Cannot convert type: ~A to git-signature struct" (type-of value))))

(defmethod translate-from-foreign (value (type git-signature-type))
  (with-foreign-slots ((name email time) value (:struct git-signature))
    (list :name name :email email :time (getf time 'time))))

(defmethod free-translated-object (pointer (type git-signature-type) do-not-free)
  (unless do-not-free (foreign-free pointer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-email ()
  (or (getenv "MAIL")
      (concatenate 'string (getenv "USERNAME") "@" (machine-instance))))
