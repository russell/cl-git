;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2013 Russell Sim <russell.sim@gmail.com>
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


(defpackage #:cl-git-tests
  (:use #:common-lisp #:cl-git #:it.bese.FiveAM)
  (:import-from #:alexandria
                #:compose
                #:iota)
  (:import-from #:cl-fad
                #:file-exists-p
                #:delete-directory-and-files)
  (:import-from #:flexi-streams
                #:octets-to-string)
  (:import-from #:local-time
                #:unix-to-timestamp
                #:timestamp-to-unix
                #:timestamp-difference
                #:now)
  (:import-from #:cffi
                #:pointerp))
