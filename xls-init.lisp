;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: XLS -*-
;;; Copyright (c) 2017-2021 by Symbolics Pte. Ltd. All rights reserved.

(in-package :xls)

;;; Logical pathname setup

;; Logical pathnames are not well supported in CCL. For example the
;; backquote macro cannot be used in the *.pathname-translations
;; files, as demonstrated in the (setup-xls-translations) below, so
;; they must be hard-coded there. It is for that reason that I created
;; these functions, with the intention that the user can call it after
;; XLS starts, or place in their init file.

(defun setup-xls-translations ()
  (setf (logical-pathname-translations "xls")
	`(("DATA;**;*.*.*" ,(merge-pathnames "datasets/**/*.*" (asdf:system-source-directory 'xls-compat))))))

(setup-xls-translations)

