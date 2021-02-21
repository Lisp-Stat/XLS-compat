;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;; Copyright (c) 2021, 2019, 2017 by Symbolics Pte Ltd. All rights reserved.

(asdf:defsystem :xls-compat
  :name "XLisp Statistics"
  :version "0.0.1"
  :description "An incomplete, undocumented, port of parts of XLisp-Stat to Common Lisp."
  :author "Steve Nunez <steve@symbolics.tech>"
  :license :MS-PL
  :depends-on (#:alexandria
	       #:num-utils
	       #:select)
  :in-order-to ((test-op (test-op "xls-compat/tests")))
  :components ((:file #:packages)
	       (:file #:xls-init)
	       (:module "base"
		  :serial t
		  :components ((:file #:type-checks)
			       (:file #:compound)
			       (:file #:variables)
			       (:file #:statistics)))
	       (:module "plot"
		  :components ((:file #:stem-and-leaf-stdout)))))

(defsystem :xls-compat/tests
  :description "Unit tests for XLS"
  :author "Steve Nunez <steve@symbolics.tech>"
  :license :MS-PL
  :depends-on (#:xls-compat #:fiveam)
  :serial t
  :pathname "tests/"
  :components ((:file #:test-package)
	       (:file #:variables-test))
  :perform (test-op (o s)
  		    (uiop:symbol-call :fiveam :run!
  				      (uiop:find-symbol* :all-tests
							 :xlisp-stat-tests))))

