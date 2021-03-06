;;; Copyright 2008 Ben Deane

;;; This file is part of the common lisp package com.elbeno.vector.

;;; The package is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; The package is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with the package.  If not, see <http://www.gnu.org/licenses/>.

;;;; package.lisp

(defpackage #:com.elbeno.vector
  (:use #:cl)
  (:export #:make-vector
           #:xcoord
           #:ycoord
           #:+vector
           #:-vector
           #:scale-vector
           #:dot-product
           #:interp
           #:rotate-vector
           #:vector-angle
           #:vector-length
           #:normalize
           #:normal
           #:vector-distance
           #:transform-point))
