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

(defbitfield git-revwalk-flags
  (:none 0)
  (:topological 1)
  (:time 2)
  (:reverse 4))

(defcfun ("git_revwalk_new" %git-revwalk-new)
    %return-value
  (revwalk :pointer)
  (repository %repository))

(defcfun ("git_revwalk_free" %git-revwalk-free)
    :void
  (revwalk %revwalker))

(defcfun ("git_revwalk_reset" %git-revwalk-reset)
    :void
  (revwalk %revwalker))

(defcfun ("git_revwalk_next" %git-revwalk-next)
    %return-value
  (oid (:pointer (:struct git-oid)))
  (revwalk %revwalker))

(defcfun ("git_revwalk_sorting" %git-revwalk-sorting)
    :void
  (walk %revwalker)
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


(defmethod translate-to-foreign (value (type git-revision-walker))
  (if (pointerp value)
      value
      (pointer value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass revision-walker (git-pointer) ())

(defmethod next-revision ((walker revision-walker))
  "Return a git-commit or nil if there are no more commits."
  (with-foreign-object (oid '(:struct git-oid))
    (handler-case
        (progn
          (%git-revwalk-next oid walker)
          (get-object 'commit oid (facilitator walker)))
      (stop-iteration nil))))

(defun make-revwalk (repository)
  "Create a new, empty, revwalker"
  (with-foreign-object (revwalker-pointer :pointer)
    (%git-revwalk-new revwalker-pointer repository)
  (make-instance 'revision-walker
                 :pointer (mem-ref revwalker-pointer :pointer)
                 :facilitator repository
                 :free-function #'%git-revwalk-free)))

(defun revision-walk (commits &key (ordering :time))
  "Create a revision walker starts iteration from the COMMITS listed.

The ordering can be adjusted to :NONE :TOPOLOGICAL :REVERSE or :TIME,
which is the default.

Once created iteration over commits can be done with the method
NEXT-REVISION method."
  (let* ((commits (ensure-list commits))
         (revwalker (make-revwalk (facilitator (car commits)))))
    (%git-revwalk-sorting revwalker ordering)
    (loop :for commit :in commits
          :do (%git-revwalk-push revwalker (oid commit)))
    revwalker))
