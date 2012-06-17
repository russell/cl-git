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

(define-foreign-type revision-walker ()
  ((%object :accessor pointer :initarg :pointer :initform (null-pointer))
   (%repository :accessor %repository :initarg :repository-pointer))
  (:actual-type :pointer)
  (:simple-parser %revwalker))

(defbitfield git-revwalk-flags
  (:none 0)
  (:topological 1)
  (:time 2)
  (:reverse 4))

(defcfun ("git_revwalk_new" %git-revwalk-new)
    %return-value
  (revwalk :pointer)
  (repository :pointer))

(defcfun ("git_revwalk_free" %git-revwalk-free)
    :void
  (revwalk %revwalker))

(defcfun ("git_revwalk_reset" %git-revwalk-reset)
    :void
  (revwalk %revwalker))

(defcfun ("git_revwalk_next" %git-revwalk-next)
    :int
  (oid :pointer)
  (revwalk %revwalker))

(defcfun ("git_revwalk_sorting" %git-revwalk-sorting)
    :void
  (walk :pointer)
  (sort-mode git-revwalk-flags))

(defcfun ("git_revwalk_push" %git-revwalk-push)
    %return-value
  (revwalk %revwalker)
  (oid %oid))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign Type Translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod translate-to-foreign (value (type revision-walker))
  (if (pointerp value)
      value
      (pointer value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod walker-next ((walker revision-walker))
  "return a git-commit or nil if there are no more commits"
  (with-foreign-object (oid 'git-oid)
    (let ((return-code (%git-revwalk-next oid walker))
          (*git-repository* (%repository walker)))
      (when (= return-code 0)
        (git-commit-from-oid oid)))))


(defun git-revwalk (oid-or-oids &optional (ordering :time))
  "Walk all the revisions from a specified OID, or OIDs.
OID can be a single object id, or a list of object ids.
The OIDs can be anything that can be resolved by commit-oid-from-oid.
In general this means, commits and tags."

  (assert (not (null-or-nullpointer *git-repository*)))

  (with-foreign-object (revwalker-pointer :pointer)
    (%git-revwalk-new revwalker-pointer *git-repository*)
    (let ((revwalker (mem-ref revwalker-pointer :pointer)))
      (%git-revwalk-sorting revwalker ordering)
      (loop
        :for oid
        :in (if (atom oid-or-oids) (list oid-or-oids) oid-or-oids)
        :do (%git-revwalk-push revwalker (commit-oid-from-oid oid)))
      revwalker)))


(defun make-instance-revwalker (&key object-ptr repository-ptr)
  (let ((object (make-instance 'revision-walker
                               :pointer object-ptr
                               :repository-pointer (or repository-ptr *git-repository*))))
    (with-foreign-object (finalizer-ptr :pointer)
      (setf finalizer-ptr object-ptr)
      (finalize object
                (lambda ()
                  (%git-revwalk-free finalizer-ptr))))
    object))


(defmacro with-git-revisions ((commit &rest rest &key sha head) &body body)
  "Iterate aver all the revisions, the symbol specified by commit will
be bound to each commit during each iteration.  This uses a return
special call to stop iteration."
  (declare (ignore sha))
  (declare (ignore head))
;;  (warn "with-git-revisions is depricated, please use revision-walk instead.")
  `(let ((oids (lookup-oids ,@rest)))
     (let ((revwalker (git-revwalk oids)))
       (with-foreign-object (oid 'git-oid)
         (block nil
           (labels ((revision-walker ()
                      (progn
                        (if (= (%git-revwalk-next oid revwalker) 0)
                            (progn
                              (let ((,commit (git-commit-from-oid oid)))
                                ,@body)
                              (revision-walker))))))
             (unwind-protect
                  (revision-walker)
               (%git-revwalk-free revwalker))))))))


(defun revision-walk (name-or-names &optional (flags :both))
  "Create a revision walker starts iteration from the commits listed
in NAME-OR-NAMES. A head or sha that matches can be filterd using the
flags :SHA, :HEAD or :BOTH.

Once created iteration over commits can be done with the method
WALKER-NEXT."
  (let ((oids (find-oids name-or-names flags)))
     (let ((revwalker (make-instance-revwalker :object-ptr (git-revwalk oids))))
       revwalker)))
