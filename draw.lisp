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

;;; draw functions

(defgeneric draw (c)
  (:documentation "Draw the curve with vecto, first moving."))

(defmethod draw ((c bezier))
  (with-slots (start-point control-point-1 control-point-2 end-point) c
    (move-to (xcoord start-point) (ycoord start-point))
    (curve-to (xcoord control-point-1) (ycoord control-point-1)
              (xcoord control-point-2) (ycoord control-point-2)
              (xcoord end-point) (ycoord end-point))))

(defun draw-bezier-no-move (c)
  (with-slots (control-point-1 control-point-2 end-point) c
    (curve-to (xcoord control-point-1) (ycoord control-point-1)
              (xcoord control-point-2) (ycoord control-point-2)
              (xcoord end-point) (ycoord end-point))))

(defmethod draw ((c spline))
  (draw (car (bezier-list c)))
  (mapcar #'draw-bezier-no-move (cdr (bezier-list c))))

; my doctored version of vecto allows ellipse draw with orientation
;(defmethod draw ((c ellipse))
;  (with-slots (center radius-x radius-y orientation) c
;    (centered-ellipse-path (xcoord center) (ycoord center) radius-x radius-y orientation)))

; vecto's built-in ellipse draw doesn't
; allow for orientation, so draw the 4 beziers
(defmethod draw ((c ellipse))
  (with-slots (center radius-x radius-y orientation) c
    (let ((krx (* radius-x +kappa+))
          (kry (* radius-y +kappa+))
          (left (transform-point (make-vector (- radius-x) 0) center orientation))
          (right (transform-point (make-vector radius-x 0) center orientation))
          (top (transform-point (make-vector 0 radius-y) center orientation))
          (bottom (transform-point (make-vector 0 (- radius-y)) center orientation)))
      (let ((cp-tl-1 (transform-point (make-vector (- radius-x) kry) center orientation))
            (cp-tl-2 (transform-point (make-vector (- krx) radius-y) center orientation))
            (cp-tr-1 (transform-point (make-vector krx     radius-y) center orientation))
            (cp-tr-2 (transform-point (make-vector radius-x kry) center orientation))
            (cp-br-1 (transform-point (make-vector radius-x (- kry)) center orientation))
            (cp-br-2 (transform-point (make-vector krx     (- radius-y)) center orientation))
            (cp-bl-1 (transform-point (make-vector (- krx) (- radius-y)) center orientation))
            (cp-bl-2 (transform-point (make-vector (- radius-x) (- kry)) center orientation)))
        (move-to (xcoord left) (ycoord left))
        (curve-to (xcoord cp-tl-1) (ycoord cp-tl-1)
                  (xcoord cp-tl-2) (ycoord cp-tl-2)
                  (xcoord top) (ycoord top))
        (curve-to (xcoord cp-tr-1) (ycoord cp-tr-1)
                  (xcoord cp-tr-2) (ycoord cp-tr-2)
                  (xcoord right) (ycoord right))
        (curve-to (xcoord cp-br-1) (ycoord cp-br-1)
                  (xcoord cp-br-2) (ycoord cp-br-2)
                  (xcoord bottom) (ycoord bottom))
        (curve-to (xcoord cp-bl-1) (ycoord cp-bl-1)
                  (xcoord cp-bl-2) (ycoord cp-bl-2)
                  (xcoord left) (ycoord left))))))

(defmethod draw ((c circle))
  (with-slots (center radius) c
    (centered-circle-path (xcoord center) (ycoord center) radius)))

(defmethod draw ((c line))
  (with-slots (start-point end-point) c
    (move-to (xcoord start-point) (ycoord start-point))
    (line-to (xcoord end-point) (ycoord end-point))))

(defun draw-line-no-move (c)
  (with-slots (end-point) c
    (line-to (xcoord end-point) (ycoord end-point))))

(defmethod draw ((c polyline))
  (draw (car (line-list c)))
  (mapcar #'draw-line-no-move (cdr (line-list c))))
