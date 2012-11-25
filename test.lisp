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

;;; tests

(defparameter *bezier* (make-instance 'bezier :p0 '(0 . 0)
                                       :p1 '(15 . 20)
                                       :p2 '(15 . -20)
                                       :p3 '(30 . 0)))

(defparameter *spline* (make-instance 'spline :bezier-list
                                      (list (make-instance 'bezier :p0 '(10 . 100) :p1 '(100 . 100) :p2 '(120 . 120) :p3 '(80 . 150))
                                            (make-instance 'bezier :p0 '(80 . 150) :p1 '(40 . 180) :p2 '(60 . 200) :p3 '(150 . 200))
                                            (make-instance 'bezier :p0 '(150 . 200) :p1 '(240 . 200) :p2 '(260 . 180) :p3 '(220 . 150))
                                            (make-instance 'bezier :p0 '(220 . 150) :p1 '(180 . 120) :p2 '(200 . 100) :p3 '(290 . 100)))))

(defparameter *circle* (make-instance 'circle :center (make-vector 250 250) :r 150))

(defparameter *ellipse* (make-instance 'ellipse :center (make-vector 250 250) :a 200 :b 150 :o (/ pi 4)))

(defparameter *line* (make-instance 'line :p0 (make-vector 0 -10) :p1 (make-vector 20 10)))

(defparameter *polyline* (make-instance 'polyline :line-list
                                        (list (make-instance 'line :p0 (make-vector 0 -10) :p1 (make-vector 20 10))
                                              (make-instance 'line :p0 (make-vector 20 10) :p1 (make-vector 40 -10)))))

(defun test-mod-curve (filename c0 c1)
  (with-canvas (:width 500 :height 500)
    (set-rgb-fill 1 1 1)
    (clear-canvas)
    (set-rgb-fill 0.5 1 1)
    (set-rgb-stroke 0 0 0)
    (let ((curves (modulate c0 c1 24)))
      (mapcar #'draw curves))
    (set-line-width 3)
    (stroke)
    (save-png filename)))

(defun test-mod-circle (filename)
  (test-mod-curve filename *ellipse* *circle*))

(defun test-mod-ellipse (filename)
  (test-mod-curve filename *ellipse* *ellipse*))

(defun test-mod-line (filename)
  (test-mod-curve filename *ellipse* *line*))

(defun test-mod-bezier (filename)
  (test-mod-curve filename *ellipse* *bezier*))

(defun test-mod-spline (filename)
  (test-mod-curve filename *ellipse* *spline*))

(defun test-mod-polyline (filename)
  (test-mod-curve filename *ellipse* *polyline*))

;; modulate tests for an elliptical arc
(defun test-mod-arc1 (filename)
  (test-mod-curve filename (approximate-elliptical-arc 250 250 200 150 (/ pi 4) 0 pi)
                  *spline*))

(defun test-mod-arc2 (filename)
  (test-mod-curve filename *ellipse*
                  (approximate-elliptical-arc 0 0 10 15 pi 0 pi)))

;; compare a large ellipse generated with bezier approximations
;; to the ellipse built by vecto in quadrants

(defun test-arc-approx ()
  (let ((a 450)
        (b 250)
        (theta 0)
        (cx 500)
        (cy 500))
    (with-canvas (:width (* cx 2) :height (* cy 2))
      (set-rgb-fill 1 1 1)
      (clear-canvas)
      (set-rgb-stroke 0 0 0)
      (draw (approximate-elliptical-arc cx cy a b theta 0 (* pi 2)))
      (stroke)
      (save-png "test-arc.png"))
    (with-canvas (:width (* cx 2) :height (* cy 2))
      (set-rgb-fill 1 1 1)
      (clear-canvas)
      (set-rgb-stroke 0 0 0)
      (centered-ellipse-path cx cy a b)
      (stroke)
      (save-png "test-arc-compare.png"))))

;; draw spirals

(defun test-spirals ()
  (let ((a 1)
        (b 1)
        (da/deta (/ 150 (* pi 2)))
        (db/deta (/ 100 (* pi 2)))
        (theta (/ pi 4))
        (cx 500)
        (cy 500))
    (with-canvas (:width (* cx 2) :height (* cy 2))
      (set-rgb-fill 1 1 1)
      (clear-canvas)
      (set-rgb-stroke 0 0 0)
      (draw (approximate-elliptical-arc cx cy a a 0 0 (* pi 4) da/deta da/deta))
      (stroke)
      (save-png "test-circ-spiral.png"))
    (with-canvas (:width (* cx 2) :height (* cy 2))
      (set-rgb-fill 1 1 1)
      (clear-canvas)
      (set-rgb-stroke 0 0 0)
      (draw (approximate-elliptical-arc cx cy a b theta 0 (* pi 4) da/deta db/deta))
      (stroke)
      (save-png "test-ellipse-spiral.png"))))


(defun test-ellipse (filename)
  (with-canvas (:width 500 :height 500)
    (set-rgb-fill 1 1 1)
    (rectangle 0 0 500 500)
    (fill-path)
    (set-rgb-fill 0.5 1 1)
    (set-rgb-stroke 0 0 0)
    (draw *ellipse*)
    (fill-and-stroke)
    (save-png filename)))

(defun runtests ()
  (test-ellipse "ellipse.png")
  (test-mod-polyline "test-polyline.png")
  (test-mod-line "test-line.png")
  (test-mod-ellipse "test-ellipse.png")
  (test-mod-circle "test-circle.png")
  (test-mod-bezier "test-bezier.png")
  (test-mod-spline "test-spline.png")
  (test-mod-arc1 "test-spline-arc.png")
  (test-mod-arc2 "test-arc-ellipse.png")
  (test-arc-approx))
