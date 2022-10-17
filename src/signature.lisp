;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2022 Russell Sim <russell.sim@gmail.com>
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

(define-foreign-type git-signature-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %git-signature))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign type translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Time

(defmethod translate-from-foreign (value (type time-t))
  "Translate a git time_t to a local-time"
  (local-time:unix-to-timestamp value))

(defmethod translate-to-foreign ((value local-time:timestamp) (type time-t))
  "Translate a local-time value to a git time_t"
  (local-time:timestamp-to-unix value))

(defmethod translate-to-foreign ((value integer) (type time-t))
  "Translate a universal-time to a git time_t"
  (translate-to-foreign (local-time:unix-to-timestamp value) type))

(defgeneric timezone-offset (time)
  (:documentation "Calculate the current timezone offset."))

(defmethod timezone-offset ((time local-time:timestamp))
  (/ (local-time:timestamp-subtimezone
                           time local-time:*default-timezone*)
                          60))

(defmethod timezone-offset ((time integer))
  (timezone-offset (local-time:unix-to-timestamp time)))


;; Signatures
(defmethod translate-to-foreign (value (type git-signature-type))
  (declare (optimize (debug 3)))
  (cond
    ((pointerp value)
     (values value t))
    ((listp value)
     (let ((signature (foreign-alloc '(:struct git-signature))))
       (with-foreign-slots ((time offset sign)
                            (foreign-slot-pointer signature '(:struct git-signature) 'when)
                            (:struct git-time))
         (let ((time-to-set (getf value :time (local-time:now))))
           (setf time time-to-set)
           (setf offset (timezone-offset time))
           (setf sign (char-int (if (< offset 0) #\- #\+)))))
       (with-foreign-slots ((name email when) signature (:struct git-signature))
         (setf name (getf value :name (getenv "USER")))
         (setf email (getf value :email (default-email))))
       signature))
    (t
     (error "Cannot convert type: ~A to git-signature struct" (type-of value)))))

(defun make-timezone (offset)
  "Return a new timezone based on the number on minutes in the
OFFSET."
  (let ((offset (* 60 offset)))
    (local-time::%make-simple-timezone
     "Explicit Offset From Git"
     (multiple-value-bind (offset-hours offset-secs)
         (floor offset local-time:+seconds-per-hour+)
       (format nil "~c~2,'0d~2,'0d"
               (if (minusp offset-hours) #\- #\+)
               (abs offset-hours)
               (truncate (abs offset-secs)
                         local-time:+seconds-per-minute+)))
     offset)))

(defmethod translate-from-foreign (value (type git-signature-type))
  (with-foreign-slots ((name email when) value (:struct git-signature))
    (list :name name
          :email email
          :time (getf when 'time)
          :timezone (make-timezone (getf when 'offset)))))

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
