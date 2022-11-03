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


(define-foreign-type proxy-options ()
  ()
  (:simple-parser %proxy-options)
  (:actual-type :pointer))


#-(or libgit2-0.28 libgit2-0.27)
(defcfun %git-proxy-options-init
    %return-value
  (options :pointer)
  (version :uint))

#+(or libgit2-0.28 libgit2-0.27)
(defcfun ("git_proxy_init_options" %git-proxy-options-init)
    %return-value
  (options :pointer)
  (version :uint))


(defmethod translate-to-foreign (value (type proxy-options))
  (let ((ptr (foreign-alloc '(:struct git-proxy-options))))
    ;; Init the structure with default values.
    (%git-proxy-options-init ptr +git-proxy-options-version+)
    ptr))
