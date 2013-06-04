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

(defcenum git-config-level
  (:system 1)
  (:xdg 2)
  (:global 3)
  (:local 4)
  (:highest-level -1))

(defcstruct git-config-entry
  (name :string)
  (value :string)
  (level git-config-level))

(defcfun ("git_config_free" git-config-free)
    :void
  "Free the git configuration object that is acquired with
GIT-REPOSITORY-CONFIG."
  (config %config))

(defcfun ("git_config_foreach" %git-config-for-each)
    %return-value
  (config %config)
  (callback :pointer)
  (payload :pointer))

(defcfun ("git_config_open_level" %git-config-open-level)
    %return-value
  "Limit return a new git config object that is limited to the level specified."
  (config :pointer)
  (parent %config)
  (level git-config-level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for callbackes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *config-values* nil)

(defcallback collect-config-values :int ((entry git-config-entry) (data :pointer))
  (declare (ignore data))
  (with-foreign-slots ((name value level) entry (:struct git-config-entry))
    (push (list :name name :value value :level level) *config-values*))
  0);;; replace with success


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass config (object) ())

(defmethod git-values ((config config))
  "Returns the key value pairs in the config as an association list."
  (let ((*config-values* (list)))
    (%git-config-for-each config
                         (callback collect-config-values)
                         (null-pointer))
    *config-values*))

(defmethod git-config ((config config) &key level)
  "Returns the key value pairs in the config as an association list."
  (if level
      (with-foreign-object (%config :pointer)
        (%git-config-open-level %config config level)
        (make-instance-object :pointer (mem-aref %config :pointer)
                              :facilitator (facilitator config)
                              :type 'config))
      config))
