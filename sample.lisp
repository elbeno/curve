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

;;; sampling functions (by parameter)

(defgeneric make-param-sampler (c)
  (:documentation "Make a sampler function for the curve parameterised between 0 and 1."))

;; bezier sampler is decastlejau's algorithm
(defun decasteljau (p0 p1 p2 p3 k)
  (let ((q0 (interp p0 p1 k))
        (q1 (interp p1 p2 k))
        (q2 (interp p2 p3 k)))
    (let ((r0 (interp q0 q1 k))
          (r1 (interp q1 q2 k)))
      (values (interp r0 r1 k)
              (atan (- (ycoord r1) (ycoord r0))
                    (- (xcoord r1) (xcoord r0)))))))

(defun bezier-decasteljau (bez k)
  (with-slots (start-point control-point-1 control-point-2 end-point) bez
    (decasteljau start-point control-point-1 control-point-2 end-point k)))

(defmethod make-param-sampler ((c bezier))
  (with-slots (start-point control-point-1 control-point-2 end-point) c
    (lambda (k)
      (decasteljau start-point control-point-1 control-point-2 end-point k))))

(defun spline-decasteljau (bezier-list k)
  (multiple-value-bind (k-int k-frac)
      (floor k)
    (if (eq k-int (length bezier-list))
        (let ((bezier (car (last bezier-list))))
          (bezier-decasteljau bezier 1))
        (let ((bezier (nth k-int bezier-list)))
          (bezier-decasteljau bezier k-frac)))))

(defmethod make-param-sampler ((c spline))
  (let* ((bl (bezier-list c))
         (len (length bl)))
    (lambda (k)
      (spline-decasteljau bl (* k len)))))

(defmethod make-param-sampler ((c ellipse))
  (with-slots (center radius-x radius-y orientation) c
    (lambda (k)
      (let* ((ang (* k pi 2))
             (x (* radius-x (cos ang)))
             (y (* radius-y (sin ang))))
        (values (transform-point (make-vector x y) center orientation)
                (let* ((tangentx (* (- radius-x) (sin ang)))
                       (tangenty (* radius-y (cos ang))))
                  (+ (vector-angle (make-vector tangentx tangenty)) orientation)))))))

(defmethod make-param-sampler ((c circle))
  (with-slots (center radius) c
    (lambda (k)
      (let* ((ang (* k pi 2))
             (x (* radius (cos ang)))
             (y (* radius (sin ang))))
        (values (make-vector (+ x (xcoord center)) (+ y (ycoord center)))
                (+ ang (/ pi 2)))))))

(defmethod make-param-sampler ((c line))
  (with-slots (start-point end-point) c
    (lambda (k)
      (values (interp start-point end-point k)
              (atan (- (ycoord end-point) (ycoord start-point))
                    (- (xcoord end-point) (xcoord start-point)))))))

(defun polyline-sample (line-list k)
  (multiple-value-bind (k-int k-frac)
      (floor k)
    (if (eq k-int (length line-list))
        (with-slots (start-point end-point) (car (last line-list))
          (values end-point
                  (atan (- (ycoord end-point) (ycoord start-point))
                        (- (xcoord end-point) (xcoord start-point)))))
        (let ((line (nth k-int line-list)))
          (with-slots (start-point end-point) line
            (values (interp start-point end-point k-frac)
                    (atan (- (ycoord end-point) (ycoord start-point))
                          (- (xcoord end-point) (xcoord start-point)))))))))

(defmethod make-param-sampler ((c polyline))
  (let* ((bl (line-list c))
         (len (length bl)))
    (lambda (k)
      (polyline-sample bl (* k len)))))

;;; sampling functions (by length)

(defgeneric make-length-sampler (c n)
  (:documentation "Make a sampler function for the curve (by length) between 0 and 1. The expected number of samples is n."))

;; circle and line length samplers are the same as the param samplers
(defmethod make-length-sampler ((c circle) n)
  (values (make-param-sampler c)
          (* 2 pi (r c))))

(defmethod make-length-sampler ((c line) n)
  (values (make-param-sampler c)
          (vector-length (-vector (p1 c) (p0 c)))))

;; sample an arbitrary curve
;; accumulating length along the curve
(defun sample-curve (sampler-fn n)
  (let ((arr (make-array (1+ n)))
        (len 0)
        (lastp (funcall sampler-fn 0)))
    (loop
       for i from 0 to n
       for k from 0 to 1 by (/ 1 n)
       do (let ((pt (funcall sampler-fn k)))
            (setf len (+ len (vector-distance lastp pt)))
            (setf lastp pt)
            (setf (elt arr i) (cons len k))))
    arr))

;; interpolate between two values to find a curve param
(defun interp-curve-param (pt-array idx distance)
  (if (eq idx (1- (length pt-array)))
      (cdr (elt pt-array idx))
      (let ((lena (car (elt pt-array idx)))
            (lenb (car (elt pt-array (1+ idx))))
            (ka (cdr (elt pt-array idx)))
            (kb (cdr (elt pt-array (1+ idx)))))
        (let ((frac (/ (- distance lena)
                       (- lenb lena))))
          (+ ka
             (* (- kb ka) frac))))))

;; binary search for the curve parameter
;; given distance along curve required
(defun find-curve-param (pt-array distance &optional (start 0) (end (length pt-array)))
  (if (eq (1+ start) end)
      (interp-curve-param pt-array start distance)
      (let* ((pivot (floor (+ start end) 2))
             (len (car (elt pt-array pivot))))
        (cond ((< distance len)
               (find-curve-param pt-array distance start pivot))
              (t
               (find-curve-param pt-array distance pivot end))))))

;; now the rest of the curve length samplers
(defmethod make-length-sampler ((c curve) n)
  (let* ((sample-points (sample-curve (make-param-sampler c) n))
         (len (car (elt sample-points n)))
         (param-sampler (make-param-sampler c)))
    (values (lambda (k)
              (funcall param-sampler (find-curve-param sample-points (* k len))))
            len)))
