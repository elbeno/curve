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

;; see http://www.spaceroots.org/documents/ellipse/elliptical-arc.pdf

;; cubic bezier error coefficients

; 0 < b/a < 1/4
(defparameter +cubic-error-coeffs-0+
  (make-array '(2 4 4) :initial-contents
              '((( 3.85268  -21.229      -0.330434   0.0127842)
                 (-1.61486    0.706564    0.225945   0.263682)
                 (-0.910164   0.388383    0.00551445 0.00671814)
                 (-0.630184   0.192402    0.0098871  0.0102527))
                ((-0.162211   9.94329     0.13723    0.0124084)
                 (-0.253135   0.00187735  0.0230286  0.01264)
                 (-0.0695069 -0.0437594   0.0120636  0.0163087)
                 (-0.0328856 -0.00926032 -0.00173573 0.00527385)))))


; 1/4 <= b/a <= 1
(defparameter +cubic-error-coeffs-1+
  (make-array '(2 4 4) :initial-contents
              '((( 0.0899116 -19.2349    -4.11711     0.183362)
                 ( 0.138148   -1.45804    1.32044     1.38474)
                 ( 0.230903   -0.450262   0.219963    0.414038)
                 ( 0.0590565  -0.101062   0.0430592   0.0204699))
                (( 0.0164649   9.89394    0.0919496   0.00760802)
                 ( 0.0191603  -0.0322058  0.0134667  -0.0825018)
                 ( 0.0156192  -0.017535   0.00326508 -0.228157)
                 (-0.0236752   0.0405821 -0.0173086   0.176187)))))

;; ellipse functions

(defun ellipse-val (cx cy a b theta eta)
  (make-vector
   (+ cx
      (* a (cos theta) (cos eta))
      (* (- b) (sin theta) (sin eta)))
   (+ cy
      (* a (sin theta) (cos eta))
      (* b (cos theta) (sin eta)))))

(defun ellipse-deriv-val (a b theta eta)
  (make-vector
   (+ (* (- a) (cos theta) (sin eta))
      (* (- b) (sin theta) (cos eta)))
   (+ (* (- a) (sin theta) (sin eta))
      (* b (cos theta) (cos eta)))))

;; approximate an elliptical arc with a cubic bezier spline
;; (ellipse orientation theta, center (cx,cy), radii a,b
;; and arc between angles eta1 and eta2)
;; da/deta and db/deta specify radius rates of change
;; for spiral arcs

;; we require that a > b
;; if not, swap a and b, and rotate the ellipse pi/2
;; to get an equivalent arc

(defun approximate-elliptical-arc (cx cy a b theta eta1 eta2 &optional (da/deta 0) (db/deta 0) (err 0.5))
  "Approximate an elliptical arc with a cubic bezier spline. Requirement: eta1 < eta2."
  (make-instance 'spline :bezier-list
                 (if (> b a)
                     (approximate-arc cx cy b a
                                      (+ theta (/ pi 2))
                                      (- eta1 (/ pi 2)) (- eta2 (/ pi 2))
                                      db/deta da/deta err)
                     (approximate-arc cx cy a b
                                      theta
                                      eta1 eta2
                                      da/deta db/deta err))))

;; approximate an arc with a single bezier curve
;; it is assumed that a > b
;; and eta1 < eta2 <= eta1 + pi/2

(defun approximate-arc-single (cx cy a b theta eta1 eta2 da/deta db/deta)
  (let* ((etadiff (- eta2 eta1))
         (k (tan (/ etadiff 2)))
         (alpha (* (sin etadiff)
                   (/ (1- (sqrt (+ 4 (* 3 k k)))) 3)))
         (a2 (+ a (* da/deta etadiff)))
         (b2 (+ b (* db/deta etadiff))))
    (let* ((p1 (ellipse-val cx cy a b theta eta1))
           (p2 (ellipse-val cx cy a2 b2 theta eta2))
           (q1 (+vector p1 (scale-vector (ellipse-deriv-val a b theta eta1) alpha)))
           (q2 (-vector p2 (scale-vector (ellipse-deriv-val a2 b2 theta eta2) alpha))))
      (make-instance 'bezier :p0 p1
                     :p1 q1
                     :p2 q2
                     :p3 p2))))

;; safe addition to deal with precision issues
;; when approximating a large range spiral arc

(defun safe-add (a b)
  (let ((c (+ a b)))
    (if (> (- c a) b)
        (safe-add a (* b 0.999))
        c)))

;; approximate an arc within an error by subdividing
;; return a list of beziers

(defun approximate-arc (cx cy a b theta eta1 eta2 da/deta db/deta err)
  (cond ((< eta2 eta1)
         (error "approximate-arc: eta2 must be bigger than eta1"))
        ((> (- eta2 eta1) (/ pi 2))
         (let* ((etamid (safe-add eta1 (/ pi 2)))
                (etadiff (- etamid eta1))
                (amid (+ a (* da/deta etadiff)))
                (bmid (+ b (* db/deta etadiff))))
           (nconc (approximate-arc cx cy a b theta eta1 etamid da/deta db/deta err)
                  (approximate-arc cx cy amid bmid theta etamid eta2 da/deta db/deta err))))
        (t (if (> err (bezier-error a b eta1 eta2))
               (list (approximate-arc-single cx cy a b theta eta1 eta2 da/deta db/deta))
               (let* ((etamid (/ (+ eta1 eta2) 2))
                      (etadiff (- etamid eta1))
                      (amid (+ a (* da/deta etadiff)))
                      (bmid (+ b (* db/deta etadiff))))
                 (nconc (approximate-arc cx cy a b theta eta1 etamid da/deta db/deta err)
                        (approximate-arc cx cy amid bmid theta etamid eta2 da/deta db/deta err)))))))

;; compute the error of a cubic bezier
;; that approximates an elliptical arc
;; with radii a, b
;; between angles eta1 and eta2

;; this does not take into account
;; da/deta or db/deta for spirals

(defun calc-c-term (i b/a etasum arr)
  (loop
     for j from 0 to 3
     sum (* (/ (+ (* (aref arr i j 0) b/a b/a)
                  (* (aref arr i j 1) b/a)
                  (aref arr i j 2))
               (+ (aref arr i j 3) b/a))
            (cos (* j etasum)))))

(defun bezier-error (a b eta1 eta2)
  (let* ((b/a (/ b a))
         (etadiff (- eta2 eta1))
         (etasum (+ eta2 eta1))
         (arr (if (< b/a 0.25)
                  +cubic-error-coeffs-0+
                  +cubic-error-coeffs-1+)))
    (* (/ (+ (* 0.001 b/a b/a) (* 4.98 b/a) 0.207)
          (+ b/a 0.0067))
       a
       (exp (+ (calc-c-term 0 b/a etasum arr)
               (* (calc-c-term 1 b/a etasum arr) etadiff))))))
