;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2022 Russell Sim <russell.sim@gmail.com>
;; Copyright (C) 2014 Eric Timmons <etimmons@alum.mit.edu>
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

(defconstant +git-checkout-options-version+ 1)

(defbitfield git-checkout-strategy
  :none
  :safe
  :force
  :recreate-missing
  ;; (ash 1 3) is misssing
  (:allow-conflicts #.(ash 1 4))
  :remove-untracked
  :remove-ignored
  :update-only
  :dont-update-index
  :no-refresh
  :skip-unmerged
  :use-ours
  :use-theirs
  (:disable-pathspec-match #.(ash 1 13))
  (:skip-locked-directories #.(ash 1 18))
  :dont-overwrite-ignored
  :conflict-style-merge
  :conflict-style-diff3
  :dont-remove-existing
  :dont-write-index
  :dry-run
  :conflict-style-zdiff3

  ;; Not implemented yet in libgit2
  ;; https://github.com/libgit2/libgit2/blob/61f1e31a146f220a633252ecb1054535f090745b/include/git2/checkout.h#L190
  (:update-submodules #.(ash 1 16))
  :update-submodules-if-changed)

(defbitfield git-checkout-notify
  :none
  :conflict
  :dirty
  :updated
  :untracked
  :ignored
  ;; TODO(RS) :all can't be implemented yet, needs to be 0x0FFFFu but
  ;; not sure how to add that
  ;; https://github.com/libgit2/libgit2/blob/61f1e31a146f220a633252ecb1054535f090745b/include/git2/checkout.h#L244
  )

(defcstruct git-checkout-options
  (version :uint)
  (checkout-strategy git-checkout-strategy)
  (disable-filters :boolean)
  (dir-mode :uint)
  (file-mode :uint)
  (file-open-flags :int)
  (notify-flags git-checkout-notify)
  (notify-cb :pointer)
  (notify-payload :pointer)
  (progress-cb :pointer)
  (progress-payload :pointer)
  (paths (:struct git-strings))
  (baseline %tree)
  (baseline-index %index)
  (target-directory :string)
  (ancestor-label :string)
  (our-label :string)
  (their-label :string)
  (perfdata-cb :pointer)
  (perfdata-payload :pointer))

(define-foreign-type checkout-options ()
  ()
  (:simple-parser %checkout-options)
  (:actual-type :pointer))

(defcfun %git-checkout-init-options
    %return-value
  (options :pointer)
  (version :uint))

;;; Translation methods

(defmethod translate-to-foreign (value (type checkout-options))
  (let ((ptr (foreign-alloc '(:struct git-checkout-options))))
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((value checkout-options) (type checkout-options) ptr)
  ;; First, initialize the structure with default values.
  (%git-checkout-init-options ptr +git-checkout-options-version+)
  ptr)
