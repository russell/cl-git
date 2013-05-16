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

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter error-type-list
    '((ok 0)
      (git-error -1)
      (not-found -3)
      (exists -4)
      (ambiguous-error -5)
      (buffer-error -6)  ;; buffer is too small
      (user-error -7)
      (barerepo-error -8)
      (orphanedhead-error -9)
      (unmerged-error -10)
      (non-fast-forward-error -11)
      (invalid-spec-error -12)
      (merge-conflict-error -13)
      (passthrough -30)
      (stop-iteration -31)))

  (defvar error-conditions (make-hash-table)))

(defcenum %error-class-list
  :no-memory
  :os
  :invalid
  :reference
  :zlib
  :repository
  :config
  :regex
  :odb
  :index
  :object
  :net
  :tag
  :tree
  :indexer
  :ssl
  :submodule
  :thread
  :stash
  :checkout
  :fetchhead
  :merge)


(defcstruct git-error
  (message :string)
  (klass %error-class-list))

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
  ;; TODO (RS) this is stopping prorogation of errors below 30, to
  ;; allow iteration exiting to be handled externally.  This should
  ;; probably be using condition handling instead.
  (if (or (not return-value) (and (< return-value 0)
                                  (> return-value -30)))
      (let ((last-error (giterr-last)))
        (error (gethash return-value error-conditions 'git-error)
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
             (format stream "Error ~S, ~A"
                     (git-error-class condition)
                     (git-error-message condition)))))

(defmacro git-define-condition (error-type)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-condition ,error-type (git-error) ())))

(eval-when (:compile-toplevel :load-toplevel)
  (loop :for (error-type error-number) :in error-type-list
        :do (git-define-condition error-type)
        :do (setf (gethash error-number error-conditions) error-type)))
