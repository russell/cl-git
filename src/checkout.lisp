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

(define-foreign-type checkout-options ()
  ()
  (:simple-parser %checkout-options)
  (:actual-type :pointer))

(defcfun %git-checkout-init-options
    %return-value
  (options :pointer)
  (version :uint))

(defcfun %git-checkout-tree
    %return-value
  (repository %repository)
  (object %object)
  (checkout-options %checkout-options))

;;; Translation methods

(defmethod translate-to-foreign (value (type checkout-options))
  (let ((ptr (foreign-alloc '(:struct git-checkout-options))))
    (translate-into-foreign-memory value type ptr)))

(defmethod translate-into-foreign-memory ((value checkout-options) (type checkout-options) ptr)
  ;; First, initialize the structure with default values.
  (%git-checkout-init-options ptr +git-checkout-options-version+)
  ptr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric checkout (object repository)
  :documentation "Checkout an object and replace the working tree and index with it's
content.  If passed a reference update the head ot point at the
reference.")

(defmethod checkout ((object reference) repository)
  "Checkout the git REFERENCE over the top of the contents of the
working directory. Then update the heod to point at the REFERENCE."
  (checkout (resolve object) repository)
  (%git-repository-set-head repository (%git-reference-name object))
  object)

(defmethod checkout (object repository)
  "Checkout the git COMMIT or TREE over the top of the contents of the
working directory."
  (%git-checkout-tree
   repository object
   (make-instance 'checkout-options))
  object)
