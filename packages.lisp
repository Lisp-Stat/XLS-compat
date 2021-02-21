;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;; Copyright (c) 2021, 2018 by Symbolics Pte Ltd. All rights reserved.

(uiop:define-package :xls-compat
  (:nicknames #:xls)
  (:documentation "Functions and variables for base XLisp-Stat.")
  (:shadowing-import-from #:alexandria #:mean #:median #:variance)
  (:use #:cl #:alexandria #:nu #:select)
  (:export #:variables
	   #:def
	   #:undef
	   #:savevar
	   #:load-data
	   #:stem-and-leaf-plot
	   #:stem-and-leaf-plot2))
