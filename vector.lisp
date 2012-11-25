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

;;;; vector.lisp

(in-package #:com.elbeno.vector)

;;; "vector" goes here. Hacks and glory await!

;; a vector
(defun make-vector (x y)
  (cons x y))

(defun xcoord (p)
  (car p))

(defun ycoord (p)
  (cdr p))

(defun -vector (a b)
  (make-vector (- (xcoord a) (xcoord b))
               (- (ycoord a) (ycoord b))))

(defun +vector (a b)
  (make-vector (+ (xcoord a) (xcoord b))
               (+ (ycoord a) (ycoord b))))

;; scale a vector
(defun scale-vector (v s)
  (make-vector (* s (xcoord v))
               (* s (ycoord v))))

;; dot product
(defun dot-product (a b)
  (+ (* (xcoord a) (xcoord b))
     (* (ycoord a) (ycoord b))))

;; interpolate between two points
(defun interp-single (x1 x2 k)
  (let ((j (- 1 k)))
    (+ (* x1 j) (* x2 k))))

(defun interp (a b k)
  (make-vector (interp-single (xcoord a) (xcoord b) k)
               (interp-single (ycoord a) (ycoord b) k)))

;; rotate a vector ACW by a given angle
(defun rotate-vector (v angle)
  (let ((c (cos angle))
        (s (sin angle))
        (x (xcoord v))
        (y (ycoord v)))
    (let ((newx (- (* x c) (* y s)))
          (newy (+ (* x s) (* y c))))
      (make-vector newx newy))))

;; compute the angle of a vector
(defun vector-angle (v)
  (let ((x (xcoord v))
        (y (ycoord v)))
    (if (equalp x 0)
        (if (> y 0)
            (/ pi 2)
            (* 3 (/ pi 2)))
        (atan y x))))

;; compute the length of a vector
(defun vector-length (v)
  (let ((x (xcoord v))
        (y (ycoord v)))
    (sqrt (+ (* x x) (* y y)))))

;; normalize a vector
(defun normalize (v)
  (scale-vector v (/ 1 (vector-length v))))

;; compute the normal vector to a line segment
;; defined by two points
(defun normal (a b)
  (let ((vec (-vector b a)))
    (let ((n (rotate-vector vec (/ pi 2))))
      (normalize n))))

;; compute the distance between two points
(defun vector-distance (a b)
  (let ((vec (make-vector (- (xcoord b) (xcoord a))
                          (- (ycoord b) (ycoord a)))))
    (vector-length vec)))

;; transform a point
(defun transform-point (pt trans rot)
  (+vector trans (rotate-vector pt rot)))
