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

;;; modulation functions

(defgeneric transform-curve (c start end len)
  (:documentation "Transform c given start point, end point, and length of segment."))

(defmethod transform-curve ((c circle) start end len)
  (let* ((p0 (car start))
         (p1 (car end))
         (r (/ (vector-length (-vector p1 p0)) 2)))
    (make-instance 'circle :center p0 :r r)))

(defmethod transform-curve ((c line) start end len)
  (with-slots (start-point end-point) c
    (let ((p0 (car start))
          (p1 (car end))
          (rot0 (cdr start))
          (rot1 (cdr end))
          (xextent (- (xcoord end-point) (xcoord start-point))))
      (let* ((scale-factor (/ len xextent))
             (sp (scale-vector (make-vector 0 (ycoord start-point)) scale-factor))
             (ep (scale-vector (make-vector 0 (ycoord end-point)) scale-factor)))
        (make-instance 'line :p0 (transform-point sp p0 rot0)
                       :p1 (transform-point ep p1 rot1))))))

(defmethod transform-curve ((c ellipse) start end len)
  (with-slots (center radius-x radius-y orientation) c
    (let* ((p0 (car start))
           (p1 (car end))
           (a (/ (vector-length (-vector p1 p0)) 2))
           (b (* radius-y (/ a radius-x)))
           (o (+ (cdr start) orientation)))
      (make-instance 'ellipse :center p0 :a a :b b :o o))))

; bezier curves MUST match the notional x-axis sign
; so reverse them if they don't

(defmethod transform-curve ((c bezier) start end len)
  (with-slots (start-point control-point-1 control-point-2 end-point) c
    (if (> (xcoord end-point) (xcoord start-point))
        (trans-bez-curve c start end len)
        (trans-bez-curve (reverse-bez-curve c) start end len))))

(defun reverse-bez-curve (c)
  (with-slots (start-point control-point-1 control-point-2 end-point) c
    (make-instance 'bezier :p0 end-point
                   :p1 control-point-2
                   :p2 control-point-1
                   :p3 start-point)))

(defun trans-bez-curve (c start end len)
  (with-slots (start-point control-point-1 control-point-2 end-point) c
    (let ((p0 (car start))
          (p1 (car end))
          (rot0 (cdr start))
          (rot1 (cdr end))
          (xextent (- (xcoord end-point) (xcoord start-point))))
      (let* ((scale-factor (/ len xextent))
             (sp (scale-vector (make-vector 0 (ycoord start-point)) scale-factor))
             (cp1 (scale-vector (make-vector (- (xcoord control-point-1) (xcoord start-point))
                                             (ycoord control-point-1))
                                scale-factor))
             (cp2 (scale-vector (make-vector (- (xcoord control-point-2) (xcoord end-point))
                                             (ycoord control-point-2))
                                scale-factor))
             (ep (scale-vector (make-vector 0 (ycoord end-point)) scale-factor)))
        (make-instance 'bezier :p0 (transform-point sp p0 rot0)
                       :p1 (transform-point cp1 p0 rot0)
                       :p2 (transform-point cp2 p1 rot1)
                       :p3 (transform-point ep p1 rot1))))))

; splines MUST match the x-axis sign
; so reverse them if they don't

(defun transform-spline (c start-param end-param len sampler)
  (with-slots (bezier-list) c
    (let ((firstx (xcoord (p0 (car bezier-list))))
          (lastx (xcoord (p3 (car (last bezier-list))))))
    (if (> lastx firstx)
        (trans-spline c start-param end-param len sampler)
        (trans-spline (reverse-spline c) start-param end-param len sampler)))))

(defun reverse-spline (c)
    (make-instance 'spline :bezier-list (reverse (mapcar #'reverse-bez-curve (bezier-list c)))))

(defun x-fracs (ptlist)
  (let* ((firstx (xcoord (car ptlist)))
         (xextent (- (xcoord (car (last ptlist))) firstx)))
    (mapcar (lambda (pt)
              (/ (- (xcoord pt) firstx) xextent))
            ptlist)))

(defun trans-spline (c start-param end-param len sampler)
  (with-slots (bezier-list) c
    (make-instance 'spline :bezier-list
                   (let* ((ptlist (cons (p0 (car bezier-list))
                                        (mapcar (lambda (b) (p3 b)) bezier-list)))
                          (xfracs-list (x-fracs ptlist))
                          (param-range (- end-param start-param))
                          (param-list (mapcar (lambda (xfrac)
                                                (+ start-param (* xfrac param-range)))
                                              xfracs-list)))
                     (loop
                        for p on param-list
                        for b in bezier-list
                        collect (transform-curve b
                                                 (multiple-value-bind (pt rot) (funcall sampler (car p)) (cons pt rot))
                                                 (multiple-value-bind (pt rot) (funcall sampler (cadr p)) (cons pt rot))
                                                 (* len (- (cadr p) (car p)))))))))

; polylines MUST match the x-axis sign too
; but we just assume they do

(defun transform-polyline (c start-param end-param len sampler)
  (with-slots (line-list) c
    (make-instance 'polyline :line-list
                   (let* ((ptlist (cons (p0 (car line-list))
                                        (mapcar (lambda (b) (p1 b)) line-list)))
                          (xfracs-list (x-fracs ptlist))
                          (param-range (- end-param start-param))
                          (param-list (mapcar (lambda (xfrac)
                                                (+ start-param (* xfrac param-range)))
                                              xfracs-list)))
                     (loop
                        for p on param-list
                        for l in line-list
                        collect (transform-curve l
                                                 (multiple-value-bind (pt rot) (funcall sampler (car p)) (cons pt rot))
                                                 (multiple-value-bind (pt rot) (funcall sampler (cadr p)) (cons pt rot))
                                                 (* len (- (cadr p) (car p)))))))))

(defgeneric modulate (c0 c1 n)
  (:documentation "Modulate a curve c1 onto a base curve c0 n times."))

;; modulate by default produces a list of curves
(defmethod modulate ((c0 curve) (c1 curve) n)
  (multiple-value-bind (sampler len) (make-length-sampler c0 (* 2 n))
    (let ((spaced-points (loop
                            for i from 0 to 1 by (/ 1 n)
                            collect (multiple-value-bind (pt rot) (funcall sampler i)
                                      (cons pt rot)))))
      (loop
         for i from 0 to (1- n)
         for start in spaced-points
         for end in (cdr spaced-points)
         collect (transform-curve c1 start end (/ len n))))))

;; modulate on a bezier or a spline produces a spline
;; for ease of drawing and filling
(defmethod modulate ((c0 curve) (c1 bezier) n)
  (list (make-instance 'spline :bezier-list (call-next-method))))

(defmethod modulate ((c0 curve) (c1 spline) n)
  (list (make-instance 'spline :bezier-list
                       (reduce (lambda (a b) (append a (bezier-list b)))
                               (multiple-value-bind (sampler len) (make-length-sampler c0 (* 2 n (length (bezier-list c1))))
                                 (loop
                                    for i from 0 to (1- n)
                                    collect (transform-spline c1 (/ i n) (/ (1+ i) n) len sampler)))
                               :initial-value nil))))

;; modulate on a polyline produces a polyline
;; for ease of drawing and filling
(defmethod modulate ((c0 curve) (c1 polyline) n)
  (list (make-instance 'polyline :line-list
                       (reduce (lambda (a b) (append a (line-list b)))
                               (multiple-value-bind (sampler len) (make-length-sampler c0 (* 2 n (length (line-list c1))))
                                 (loop
                                    for i from 0 to (1- n)
                                    collect (transform-polyline c1 (/ i n) (/ (1+ i) n) len sampler)))
                               :initial-value nil))))
