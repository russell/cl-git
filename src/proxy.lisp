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

(defconstant +git-proxy-options-version+ 1)

(defcenum git-proxy
  :none
  :auto
  :specified)

(defcstruct (git-proxy-options :class proxy-options-struct-type)
  (version :unsigned-int)
  (type git-proxy)
  (url :string)

  ;; This will be called if the remote host requires authentication in
  ;; order to connect to it.
  ;;
  ;; Returning GIT_PASSTHROUGH will make libgit2 behave as though this
  ;; field isn't set.
  (credentials-cb :pointer)

  ;; If cert verification fails, this will be called to let the user
  ;; make the final decision of whether to allow the connection to
  ;; proceed. Returns 0 to allow the connection or a negative value to
  ;; indicate an error.
  (certificate-check-cb :pointer)
  (payload :pointer))


(define-foreign-type proxy-options ()
  ()
  (:simple-parser %proxy-options)
  (:actual-type :pointer))


(defcfun %git-proxy-options-init
    %return-value
  (options :pointer)
  (version :uint))


(defmethod translate-to-foreign (value (type proxy-options))
  (let ((ptr (foreign-alloc '(:struct git-proxy-options))))
    ;; Init the structure with default values.
    (%git-proxy-options-init ptr +git-proxy-options-version+)
    ptr))
