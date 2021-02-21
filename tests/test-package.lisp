;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021, 2019, 2017 by Symbolics Pte Ltd. All rights reserved.

(in-package :cl-user)

(defpackage :xlisp-stat-tests
  (:documentation "Tests for XLisp-Stat")
  (:use :cl
	:xls
	:fiveam)
  (:export #:run!
	   #:all-tests))
