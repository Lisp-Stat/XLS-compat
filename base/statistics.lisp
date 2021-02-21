;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: XLS -*-
;;; Copyright (c) 2020, 2017 by Symbolics Pte Ltd. All rights reserved.

(in-package #:xls)

;;; Statistical functions. NOTE: Currently depends on num-utils
;;; elementwise maths package, #:nuem

;;(declaim (inline quantile))

(defun quantile (sample p)
  "Returns the P-th quantile(s) of sequence SAMPLE. P can be a number or a sequence."
;  (assert (and (plusp (length sample)) (<= 0.0 p 1.0))) ; TODO: Implement <= for sequence
  (let* ((x (sort (copy-sequence 'vector sample) #'<))
         (n (length x))
         (np (e* p (- n 1)))
         (low (nu::efloor np))
         (high (nu::eceiling np)))
    (e/ (e+ (select x low) (select x high)) 2)))

(defun interquartile-range (x)
  "Args: (number-data)
Returns the interquartile range of the elements of X."
  (apply #'- (coerce (quantile x #(0.75 0.25)) 'list)))

(defun fivnum (x)
  "Args: (number-data)
Returns the five number summary (min, 1st quartile, median, 3rd quartile, max) of the elements X."
  (quantile x #(0 .25 .5 .75 1)))


