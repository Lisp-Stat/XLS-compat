;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: XLISP-STAT-TESTS  -*-

(in-package :xlisp-stat-tests)

;;; To run this test file, execute "(asdf:test-system :xlisp-stat) from the REPL
;;; This automatically reloads sources.

(def-suite all-tests
    :description "The master suite of all lisp-stat tests.")

(in-suite all-tests)

(defun test-xls ()
  (run! 'all-tests))

(test variables
  :description "Test defining a data variable."

  (dolist (var (variables))		; Ensure a clean start
    (undef var))

  (def water (list 31.5 27.0 25.9 39.1 39.2 38.3 33.9 33.8 27.9 33.1 26.3 37.8 34.6 36.4))
  (def depth (vector 8.9 36.6 36.8 6.1 6.9 6.9 7.3 8.4 6.5 8.0 4.5 9.9 2.9 2.0))
  (defvar w (list 31.5 27.0 25.9 39.1 39.2 38.3 33.9 33.8 27.9 33.1 26.3 37.8 34.6 36.4))
  (is-true (and (subsetp w water) (subsetp water w))) ; Two lists contain the same elements iff they are subsets of each other
  (is (listp water))
  (is (vectorp depth))
  (is (= 2 (length (variables))))
  (savevar '(water depth) #P"xls:data;saved-variables-test")
  (undef 'depth)
  (is (= 1 (length (variables))))
  (undef 'water)
  (is (= 0 (length (variables))))

  ;; Load saved variables from file and repeat tests
  (load  #P"xls:data;saved-variables-test")
  (delete-file #P"xls:data;saved-variables-test.lisp")
  (is-true (and (subsetp w water) (subsetp water w))) ; Two lists contain the same elements iff they are subsets of each other
  (is (listp water))
  (is (vectorp depth))
  (is (= 2 (length (variables))))
  (undef 'depth)
  (is (= 1 (length (variables))))
  (undef 'water)
  (is (= 0 (length (variables)))))
