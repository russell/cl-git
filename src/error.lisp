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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar error-conditions (make-hash-table)))

(defcstruct git-error
  (message :string)
  (klass git-error-t))


(define-foreign-type git-error-type ()
  nil
  (:actual-type :pointer)
  (:simple-parser %git-error))


(define-foreign-type return-value-type ()
  nil
  (:actual-type :int)
  (:simple-parser %return-value))


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
  ;; TODO (RS) this is stopping propagation of errors below 30, to
  ;; allow iteration exiting to be handled externally.  This should
  ;; probably be using condition handling instead.
  (if (or (not return-value) (< return-value 0))
      (let ((last-error (giterr-last)))
        (error (gethash return-value error-conditions 'unknown-error)
               :code return-value
               :message (cadr last-error)
               :class (car last-error)))
      return-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :execute :load-toplevel)
  (define-condition basic-error (simple-error)
    ((message
      :initarg :message
      :accessor error-message
      :initform nil
      :documentation "Text message indicating what went wrong.")
     (code
      :initarg :code
      :accessor error-code
      :initform nil
      :documentation "The error value returned by a function.")
     (class
      :initarg :class
      :accessor error-class
      :initform nil
      :documentation "The error code/class returned by git_lasterr."))
    (:report (lambda (condition stream)
               (format stream "Error ~S, ~A"
                       (or (error-class condition) (class-name (class-of condition)))
                       (error-message condition))))))

(eval-when (:compile-toplevel :execute :load-toplevel)
 (defmacro define-git-condition (error-type error-key)
   `(progn
      (eval-when (:compile-toplevel :execute :load-toplevel)
        (define-condition ,error-type (basic-error) ()))
      (eval-when (:execute :load-toplevel)
        (setf (gethash ,(foreign-enum-value 'git-error-code error-key) error-conditions) (quote ,error-type))))))

(define-git-condition ok :ok)

(define-git-condition general-error :error)
(define-git-condition not-found-error :enotfound)
(define-git-condition exists-error :eexists)
(define-git-condition ambiguous-error :eambiguous)
(define-git-condition buffer-error :ebufs)  ;; buffer is too small

(define-git-condition user-error :euser)

(define-git-condition barerepo-error :ebarerepo)
(define-git-condition unborn-branch-error :eunbornbranch)
(define-git-condition unmerged-error :eunmerged)
(define-git-condition non-fast-forward-error :enonfastforward)
(define-git-condition invalid-spec-error :einvalidspec)
(define-git-condition conflict-error :econflict)
(define-git-condition locked-error :elocked)
(define-git-condition modified-error :emodified)
(define-git-condition auth-error :eauth)
(define-git-condition certificate-error :ecertificate)
(define-git-condition applied-error :eapplied)
(define-git-condition peel-error :epeel)
(define-git-condition eof-error :eeof)
(define-git-condition invalid-error :einvalid)
(define-git-condition uncommitted-error :euncommitted)
(define-git-condition directory-error :edirectory)
(define-git-condition merge-conflict-error :emergeconflict)

(define-git-condition passthrough :passthrough)
(define-git-condition stop-iteration :iterover)
(define-git-condition retry :retry)
(define-git-condition mismatch-error :emismatch)
(define-git-condition index-dirty-error :eindexdirty)
(define-git-condition apply-fail-error :eapplyfail)

(define-condition unknown-error (basic-error) ()
  (:documentation "This return value is not expected."))
