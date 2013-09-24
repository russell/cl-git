;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
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


;;; Memory management
;;;
;;; The tricky thing is memory management, we use trivial-garbage:finalize, but
;;; that only takes a function with no arguments, so we do not know what
;;; we are finalizing.  Now of course we can create a closure containing
;;; extra information, but if that refers to the object it will keep
;;; the object alive so it is never collected, so we have to create a parallel
;;; 'object' containing the relevant information.
;;;
;;; What do we need for 'finalize'
;;;
;;; 1 - The libgit pointer to call free on
;;; 2 - All dependend objects that need to be invalidated.
;;;



(define-foreign-type git-pointer ()
  ((libgit2-pointer :initarg :pointer
                    :accessor pointer
                    :initform (null-pointer)
                    :documentation "A CFFI pointer from libgit2.
This is the git object that is wrapped by the instance of this class.")
   (free-function :accessor free-function :initarg :free-function :initform nil)
   (facilitator :accessor facilitator :initarg :facilitator :initform nil)
   (finalizer-data :accessor finalizer-data :initform (cons t nil)))
  (:actual-type :pointer))

(defmethod translate-to-foreign (value (type git-pointer))
  (if (pointerp value)
      value
      (if (null-pointer-p (pointer value))
          (error "Object hasn't been initialised correctly")
          (pointer value))))

(defvar object-type-mapping (list 'commit :commit 'tree :tree 'blob :blob 'tag :tag))

(defmethod pointer ((object git-object))
  "Try and resolve the pointer if it isn't set."
  (when (and (null-pointer-p (slot-value object 'libgit2-pointer))
             (not (slot-value object 'libgit2-disposed)))
    (cond
      ;; Resolve by oid
      ((slot-value object 'libgit2-oid)
       (setf (slot-value object 'libgit2-pointer)
             (git-object-lookup-ptr (slot-value object 'libgit2-oid)
                                    (getf object-type-mapping (type-of object))
                                    (facilitator object)))
       (enable-garbage-collection object)
       (setf (slot-value object 'libgit2-oid) nil))
      ;; Resolve by name
      ((slot-value object 'libgit2-name)
       (setf (slot-value object 'libgit2-pointer)
             (%git-lookup-by-name (type-of object)
                                  (slot-value object 'libgit2-name)
                                  (facilitator object)))
       (enable-garbage-collection object)
       (setf (slot-value object 'libgit2-name) nil))
      (t (error "Unable to lookup object in git repository."))))
  (slot-value object 'libgit2-pointer))


(defun mapc-weak (function list)
  "Same as mapc, but for lists containing weak-pointers.  The function
is applied to WEAK-POINTER-VALUE of the objects in the LIST.  Except
when WEAK-POINTER-VALUE is nul of course, because in that case the
object is gone."
  (mapc
   (lambda (o)
     (let ((real-o (trivial-garbage:weak-pointer-value o)))
       (when real-o (funcall function real-o))))
   list))


(defun internal-dispose (finalizer-data pointer free-function)
  "Helps disposing objects.
This function implements most of the disposing/freeing logic, but
because it is called from the finalize method it cannot access the relevant
object directly."
  (when (car finalizer-data)
    (setf (car finalizer-data) nil)            ;; mark as disposed
    (mapc-weak #'dispose (cdr finalizer-data)) ;; dispose dependencies
    (funcall free-function pointer)))          ;; free git object


(defgeneric dispose (object)
  (:documentation "This interface is used to mark as invalid git objects when for example
the repository is closed.")
  (:method ((object git-pointer))
    "Dispose of the object and any association to the facilitator."
    (setf (facilitator object) nil)
    (when (car (finalizer-data object))
      (internal-dispose (finalizer-data object)
                        (pointer object)
                        (free-function object))
      (setf (slot-value object 'libgit2-pointer) nil))))

(defmethod print-object ((object git-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((not (null-pointer-p (slot-value object 'libgit2-pointer)))
       (format stream "~X" (pointer-address (pointer object))))
      ((or (slot-value object 'libgit2-oid) (slot-value object 'libgit2-name))
       (princ "(weak)" stream))
      ((slot-value object 'libgit2-disposed)
       (princ "(disposed)" stream)))))

(defmethod free ((object git-pointer))
  (dispose object)
  nil)

(defmethod enable-garbage-collection (instance)
  (when (slot-value instance 'libgit2-pointer)
    (let ((finalizer-data (finalizer-data instance))
          (pointer (slot-value instance 'libgit2-pointer))
          (free-function (free-function instance)))

      (unless finalizer-data (error "No Finalizer data"))
      (unless free-function (error "No Free function"))

      (when (facilitator instance)
        (push (make-weak-pointer instance)
              (cdr (finalizer-data (facilitator instance)))))

      (finalize instance
                (lambda ()
                  (internal-dispose finalizer-data pointer free-function))))))

(defmethod initialize-instance :after ((instance git-object) &rest r)
  "Setup the finalizer to call internal-dispose with the right arguments."
  (when (getf r :pointer)
    (enable-garbage-collection instance)))
