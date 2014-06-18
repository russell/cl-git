;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
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

(defcenum %error-class-list
  :none
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
  :merge
  :ssh
  :filter
  :revert
  :callback
  :cherrypick)


(defcstruct git-error
  (message :string)
  (klass %error-class-list))


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


(define-condition basic-error (error)
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
                     (error-message condition)))))

(defmacro define-git-condition (error-type error-number)
  `(progn
     (eval-when (:compile-toplevel :execute :load-toplevel)
       (define-condition ,error-type (basic-error) ()))
     (eval-when (:execute :load-toplevel)
       (setf (gethash ,error-number error-conditions) (quote ,error-type)))))

(setf (gethash -1 error-conditions) 'basic-error)
(define-git-condition not-found -3)
(define-git-condition exists -4)
(define-git-condition ambiguous-error -5)
(define-git-condition buffer-error -6)  ;; buffer is too small
(define-git-condition user-error -7)
(define-git-condition barerepo-error -8)
(define-git-condition orphanedhead-error -9)
(define-git-condition unmerged-error -10)
(define-git-condition non-fast-forward-error -11)
(define-git-condition invalid-spec-error -12)
(define-git-condition merge-conflict-error -13)
(define-git-condition passthrough -30)
(define-git-condition stop-iteration -31)

(define-condition unknown-error (basic-error) ()
  (:documentation "This return value is not expected."))
