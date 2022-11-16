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

(defcfun %git-message-trailers
    %return-value
  (trailer-array :pointer)
  (message :string))

(defcfun %git-message-trailer-array-free
    :void
  (trailer-array :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric message-trailers (message)
  (:documentation "Read the message trailers and return a list of them.")
  (:method ((message commit))
    (message-trailers (message message)))
  (:method ((message string))
    (with-foreign-object (ptr :pointer)
      (unwind-protect
           (progn
             (%git-message-trailers ptr message)
             (with-foreign-slots
                 ((trailers count) ptr (:struct git-message-trailer-array))
               (loop :for i :below count
                     :collect
                     (with-foreign-slots
                         ((key value)
                          (mem-aptr trailers '(:struct git-message-trailer) i)
                          (:struct git-message-trailer))
                       (list :key key :value value)))))
        (%git-message-trailer-array-free ptr))))
)
