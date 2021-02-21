;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: XLS -*-
;;; Copyright (c) 2014-2015 by Symbolics Pte. Ltd. All rights reserved.
;;; Copyright (c) 1991 by Luke Tierney.

;;; See bottom of file for code of defvar and defparam; use as template
;;; if overriding defvar & defparam in CLS.

(in-package #:xls)

;;;
;;; Listing and Saving Variables
;;;

(defvar *variables* nil)
(defvar *ask-on-redefine* nil)

;;; TODO: Fix the documentation string
(defmacro def (name value &optional (documentation nil documentation-p))
  "Args: (var form)
VAR is not evaluated and must be a symbol.  Assigns the value of FORM to
VAR and adds VAR to the list *VARIABLES* of def'ed variables. Returns VAR.
If VAR is already bound and the global variable *ASK-ON-REDEFINE*
is not nil then you are asked if you want to redefine the variable."
  `(progn (declaim (special ,name))
	  (unless (and *ask-on-redefine*
		       (boundp ',name)
		       (not (y-or-n-p "Variable has a value. Redefine?")))
	    ,(when documentation-p
		   `(setf (documentation ',name 'variable) ',documentation))
	    (setf (symbol-value ',name) ,value)
	    (pushnew ',name *variables*))
	  ',name))

;;; Example: (undef 'urban)
(defun undef (v)
"Args: (v)
If V is the symbol of a defined variable the variable it is unbound and
removed from the list of defined variables. If V is a list of variable
names each is unbound and removed. Returns V."
  (dolist (s (if (listp v) v (list v)))
          (when (member s *variables*)
                (setf *variables* (delete s *variables*))
                (makunbound s)))
  v)

;; Should this also return package location? If using a package for a
;; data frame, perhaps add a keyword argument :all to loop through all
;; packages, otherwise use current package.
(defun variables ()
"Args:()
Returns a list of the names of all def'ed variables."
(if *variables*
    (sort (copy-list *variables*) #'string<=)))

;;;
;;; File operations with variables
;;;

;; Only here to quiet the compiler when evaluating savevar below. Still needed?
;;(defgeneric save (data)
;;  (:documentation "Save the data."))

;; Example usage: (savevar 'urban "urban")
(defun savevar (vars file &optional (suffix ".lisp"))
  "Args: (vars-symbol-or-list file-name-root &optional suffix-string)
NOTE: Ensure vars doesn't contain CLOS objects that don't have a save
method.  VARS is a symbol or a list of symbols. FILE-NAME-ROOT is a
string (or a symbol whose print name is used) not ending in
SUFFIX (defaults to \".lisp\"). The VARS and their current values are
written to the file FILE-NAME-ROOT.lisp in a form suitable for use with
the load command."

  (let ((filename (concatenate 'string (namestring file) suffix)))
    (when (or (not (probe-file filename))
	      (y-or-n-p "File ~S exits. Overwrite?" filename))
      (with-open-file (f filename :direction :output)
	(let ((vars (if (consp vars) vars (list vars))))
	  (flet ((save-one (x)
		   (let ((v (symbol-value x)))
		     (format f "(def ~s '~s)~%" x v)

		     #+nil
	             (if (typep v 'standard-object) ; no support for saving xlisp-stat objects
			 (format f "(def ~s ~s)~%" x (save v))
			 (format f "(def ~s '~s)~%" x v))

		     )))
	    (mapcar #'save-one vars))
	  vars)))))

#| FILE SELECTION DIALOG
;; The ftw package has an example of a dialog for MS Windows.
;; See: https://github.com/fjames86/ftw
 (defun open-file-dialog ()
  "provide a selection of files and query user."
  (error "open-file-dialog not implemented yet."))

Once working we can chane the below function signatures to:
(defun read-data-file (&optional (file (open-file-dialog)))
(defun read-data-columns (&optional (file (open-file-dialog))
                                    (cols (if file
                                              (count-file-columns file))))

and if the file name is not provided (as it's now &optional), the user
will be prompted by the OS file selection dialog.
|#

(defun count-file-columns (fname)
  "Args: (fname)
Returns the number of lisp items on the first nonblank line of file FNAME."
  (with-open-file (f fname)
    (if f
        (let ((line (do ((line (read-line f) (read-line f)))
                        ((or (null line) (< 0 (length line))) line))))
          (if line
              (with-input-from-string (s line)
                (do ((n 0 (+ n 1)) (eof (gensym)))
                    ((eq eof (read s nil eof)) n))))))))

;; TODO: Make return values vectors
(defun read-data-file (fname)
"Args:  (fname)
Returns a list of all lisp objects in FILE. FILE can be a string or a symbol,
in which case the symbol'f print name is used."
  (if fname
      (let ((eof (gensym)))
        (with-open-file (f fname)
          (if f
	      (do* ((r (read f nil eof) (read f nil eof))
		    (x (list nil))
		    (tail x (cdr tail)))
		   ((eq r eof) (cdr x))
		   (setf (cdr tail) (list r))))))))

;; Use this until we have a proper matrix transpose function and
;; implement vector versions of all the load/read functions
(defun list-transpose (list-of-lists)
  "Sames as a matrix transpose, but with lists of lists"
  (apply #'mapcar #'list list-of-lists))

;; TODO: Make return values vectors
; (defun read-data-columns (&optional (file (open-file-dialog))
(defun read-data-columns (fname &optional (cols (if fname
						    (count-file-columns fname))))
"Args: (file &optional cols)
Reads the data in FILE as COLS columns and returns a list of lists representing the columns."
  (if (and fname cols)
      (list-transpose (split-list (read-data-file fname) cols))))
;;      (transpose (split-list (read-data-file fname) cols))))

(defun load-data (fname)
"Args: (file) as string
Read in data file from the System DATA library.  Return true if success, failure value otherwise. The .lisp extention is not required. E.g. (load-data \"heating\")."
(if fname
    (load (merge-pathnames fname (translate-logical-pathname #P"xls:datasets;")) :verbose T :print T)))










;;;; Future CLS work. Experiment here in XLS first

;;;
;;; Shadow & redefine CL:DEFVAR & CL:DEFPARAMETER
;;;
#| Taken from the CLHS, for use in getting the DEF XLS-compat function right.
defparameter and defvar might be defined as follows:

 (defmacro defparameter (name initial-value 
                         &optional (documentation nil documentation-p))
   `(progn (declaim (special ,name))
           (setf (symbol-value ',name) ,initial-value)
           ,(when documentation-p
              `(setf (documentation ',name 'variable) ',documentation))
           ',name))

 (defmacro defvar (name &optional
                        (initial-value nil initial-value-p)
                        (documentation nil documentation-p))
   `(progn (declaim (special ,name))
           ,(when initial-value-p
              `(unless (boundp ',name)
                 (setf (symbol-value ',name) ,initial-value)))
           ,(when documentation-p
              `(setf (documentation ',name 'variable) ',documentation))
           ',name))
|#
