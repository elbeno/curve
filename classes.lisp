;;; Copyright 2008 Ben Deane

;;; This file is part of the common lisp package com.elbeno.curve.

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

(in-package #:com.elbeno.curve)

;;; classes
;; curves are beziers, splines, ellipses, circles, lines or polylines

(defclass curve ()
  ())

(defclass bezier (curve)
  ((start-point
    :initarg :p0
    :initform (error "Must supply a bezier start point (p0)")
    :accessor p0)
   (control-point-1
    :initarg :p1
    :initform (error "Must supply a bezier control point 1 (p1)")
    :accessor p1)
   (control-point-2
    :initarg :p2
    :initform (error "Must supply a bezier control point 2 (p2)")
    :accessor p2)
   (end-point
    :initarg :p3
    :initform (error "Must supply a bezier end point (p3)")
    :accessor p3)))

(defclass spline (curve)
  ((bezier-list
    :initarg :bezier-list
    :initform nil
    :accessor bezier-list)))

(defclass ellipse (curve)
  ((center
    :initarg :center
    :initform (make-vector 0 0)
    :accessor center)
   (radius-x
    :initarg :a
    :initform (error "Must supply an x radius (a)")
    :accessor a)
   (radius-y
    :initarg :b
    :initform (error "Must supply a y radius (b)")
    :accessor b)
   (orientation
    :initarg :o
    :initform 0
    :accessor o)))

(defclass circle (curve)
  ((center
    :initarg :center
    :initform (make-vector 0 0)
    :accessor center)
   (radius
    :initarg :r
    :initform (error "Must supply a radius (r)")
    :accessor r)))

(defclass line (curve)
  ((start-point
    :initarg :p0
    :initform (error "Must supply a line start point (p0)")
    :accessor p0)
   (end-point
    :initarg :p1
    :initform (error "Must supply a line end point (p1)")
    :accessor p1)))

(defclass polyline (curve)
  ((line-list
    :initarg :line-list
    :initform nil
    :accessor line-list)))
