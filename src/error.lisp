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

(defcstruct git-error
  (message :string)
  (klass :int))

(defcfun ("giterr_last" giterr-last) %git-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign type translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod translate-from-foreign (value (type git-error-type))
  (if (null-pointer-p value)
      nil
      (with-foreign-slots ((message klass) value (:struct git-error))
	(list klass message))))

(defmethod translate-from-foreign (return-value (type return-value-type))
  (if (or (not return-value) (and (< return-value 0) (> return-value -30)))
      (let ((last-error (giterr-last)))
	(error 'git-error
	       :code return-value
	       :message (cadr last-error)
	       :class (car last-error)))
      return-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    :documentation "The error value returned by a function.")
   (class
    :initarg :class
    :accessor git-error-class
    :initform nil
    :documentation "The error code/class returned by git_lasterr."))
  (:report (lambda (condition stream)
             (format stream "git error ~D/~D: ~A"
                     (git-error-code condition)
		     (git-error-class condition)
		     (git-error-message condition)))))
