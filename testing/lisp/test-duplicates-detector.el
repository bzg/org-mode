;;; test-duplicates-detector.el --- Tests for finding duplicates in Org tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ilya Chernyshov
;; Authors: Ilya Chernyshov <ichernyshovvv@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:

;; Unit tests that check for duplicate forms and tests in all Org test files.

;; Forms are considered duplicate if they:

;; 1. are `equal-including-properties',
;; 2. have the same nesting path,
;; 3. either are `should-' macros or have `should-' macros inside.

;; To ignore a form or a group of forms, wrap them in
;; `org-test-ignore-duplicate'.

;; `ert-deftest' are considered duplicate if their body are
;; `equal-including-properties.' When comparing, the docstrings are not taken
;; into account.

;;; Code:

(require 'org-test "../testing/org-test")

;;;; Variables

(defvar test-duplicates-progn-forms
  '( progn prog1 let dolist dotimes
     org-test-with-temp-text
     org-test-with-temp-text-in-file
     org-test-at-id
     org-test-ignore-duplicate)
  "List of forms equivalent to `progn'.
Immediate children inside these are not checked for duplicates.")

(defvar test-duplicates-detector-file-path
  (expand-file-name "test-duplicates-detector.el"
                    (expand-file-name "lisp" org-test-dir)))

(defvar test-duplicates-detector-files
  (remove
   test-duplicates-detector-file-path
   (directory-files
    (expand-file-name "lisp" org-test-dir) t "\\.el$")))

(defvar test-duplicates-detector-duplicate-forms nil
  "A list where each element is either:

  ((file test-name [(form-1 . numerical-order)
                    (form-2 . numerical-order) ...])
    (dup-form-1 . (numerical-order [numerical-order ...]))
  [ (dup-form-2 . (numerical-order [numerical-order ...]))
    (dup-form-3 . (numerical-order [numerical-order ...]))
     ...])

or

  (test-1-symbol . duplicate-of-test-1-symbol)


Where

  (file test-name [(form-1 . numerical-order)
                   (form-2 . numerical-order) ...])

is a path to duplicates.  For example, the path for the
duplicates in the following test:

                                             test-file.el

  (ertdeftest test-name ()
    \"Docstring.\"
    (let ((var-1 \"value\"))
     (when var-1
       (should-not
        (equal 2 (some-func \"string\" \"x\" nil)))
       (some-func \"string\" \"x=2\")
       (should-not
        (equal 2 (some-func \"string\" \"x\" nil)))
       (some-func \"string\" \"x=2\"))))

would look like this:

  (\"/absolute/path/to/test-file.el\"
    test-name
    (let . 4) (when . 2))

And the records about the duplicates would look like this:

  ((should-not
    (equal 2 (some-func \"string\" \"x\" nil))) 4 2)")

(defvar test-duplicates-detector-forms nil
  "Nested alist of found forms and paths to them (not filtered).")

;;;; Macros

(defmacro org-test-ignore-duplicate (&rest body)
  "Eval BODY forms sequentially and return value of last one.

The macro's body will be ignored by `test-duplicates-detector.el'
tests to skip duplicate forms inside the body."
  (declare (indent 0))
  `(progn ,@body))

;;;; ERT tests

(ert-deftest test-org-tests/find-duplicates ()
  "Try to find duplicate forms and ert-deftests in FILES."
  (should-not
   (test-duplicates-detector--find-duplicates
    test-duplicates-detector-files)))

;;;; Auxiliary functions

(defun test-duplicates-detector--find-duplicates (files)
  "Try to find duplicate forms and ert-deftests in FILES.

Duplicate forms will be written to
`test-duplicates-detector-duplicate-forms'.

`message' paths to them in a human-readable format."
  (setq test-duplicates-detector-forms nil)
  (let (found-deftests duplicate-tests)
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
	(save-excursion
          (goto-char (point-min))
          (while (search-forward "(ert-deftest" nil t)
            (goto-char (match-beginning 0))
            (let (deftest test-name)
              (ignore-errors
                (while (setq deftest (read (current-buffer)))
                  (setq test-name (cadr deftest))
                  (when (eq (car deftest) 'ert-deftest)
		    (if-let* ((f (seq-find
			          (lambda (x)
			            (equal-including-properties
				     ;; if cadddr is a docstring
				     (if (stringp (cadddr deftest))
				         (cddddr deftest)
				       (cdddr deftest))
				     (if (stringp (cadddr x))
				         (cddddr x)
				       (cdddr x))))
			          found-deftests)))
                        (push (cons test-name (cadr f)) duplicate-tests)
		      (push deftest found-deftests)
                      (test-duplicates-detector--search-forms-recursively
		       deftest (list file test-name)))))))))))
    (setq test-duplicates-detector-duplicate-forms
          (seq-filter
	   #'cdr
	   (mapcar
            (lambda (file)
              (cons
               (car file)
               (seq-filter
		(lambda (x)
		  (and (caddr x)
		       (seq-intersection
			'(should-not should should-error)
			(flatten-list (car x)))))
		(cdr file))))
            test-duplicates-detector-forms)))
    (when test-duplicates-detector-duplicate-forms
      (message
       "Found duplicates (To ignore the duplicate forms,
wrap them in `org-test-ignore-duplicate'):
%s"
       (mapconcat
	(lambda (path)
	  (let* ((file (file-relative-name (caar path)))
		 (test-name (symbol-name (cadar path)))
		 (string-path (append (list file test-name)
				      (mapcar (lambda (x)
						(symbol-name (car x)))
					      (cddar path))))
		 (indent -1)
		 (print-level 3))
	    (concat
	     (mapconcat
	      (lambda (x)
		(concat (make-string (* (setq indent (1+ indent)) 2) ? )
			x "\n"))
	      string-path
              "")
	     (mapconcat
	      (lambda (x)
		(format "%s%S: %d times\n"
			(make-string (* indent 2) ? )
			(car x)
			(length (cdr x))))
	      (cdr path)
              ""))))
	test-duplicates-detector-duplicate-forms
        "")))
    (when duplicate-tests
      (message "Duplicate ERT tests found:\n%s\n"
	       (mapconcat (lambda (x) (format "%S" x))
			  duplicate-tests "\n")))
    (append test-duplicates-detector-duplicate-forms
	    duplicate-tests)))

(defun test-duplicates-detector--search-forms-recursively (form form-path)
  "Search for forms recursively in FORM.

FORM-PATH is list of the form:
  (\"file-path\" ert-test-symbol
    (symbol-1 . sexp-order-1) (symbol-2 . sexp-order-2))

Write each form to `test-duplicates-detector-forms'"
  (let ((idx 0))
    (dolist (sub-form form)
      (when (consp sub-form)
        (unless (memq (car-safe form) test-duplicates-progn-forms)
          (push idx (alist-get
		     sub-form
                     (alist-get form-path test-duplicates-detector-forms
                                nil nil #'equal)
                     nil nil #'equal-including-properties)))
        (unless (memq (car sub-form)
		      '(should-not should should-error))
	  (test-duplicates-detector--search-forms-recursively
           sub-form
           (append form-path (list (cons (car sub-form) idx))))))
      (cl-incf idx))))

;;;; Testing the detector itself

(ert-deftest test-org-tests/test-duplicates-detector-testing-find-duplicates ()
  "Test `test-duplicates-detector--find-duplicates'."
  (should
   (equal
    (test-duplicates-detector--find-duplicates
     (list test-duplicates-detector-file-path))
    `(((,test-duplicates-detector-file-path
	test-org-tests/test-with-nested-duplicates)
       ((let ((var "string")) (should (message "123 %s" var))) 6 4))
      ((,test-duplicates-detector-file-path
	test-org-tests/test-with-duplicates-at-root)
       ((should (message "123")) 6 4))
      (test-org-tests/duplicate-test-2 . test-org-tests/duplicate-test-1)))))

;;;;; Tests with duplicate forms

(ert-deftest test-org-tests/test-with-duplicates-at-root ()
  "Test with duplicates at the root."
  (should (message "123"))
  (format "%s" "string")
  (should
   (message "123")))

(ert-deftest test-org-tests/test-with-nested-duplicates ()
  "Test with nested duplicates."
  (let ((var "string"))
    (should
     (message "123 %s" var)))
  (format "%s" "string")
  (let ((var "string"))
    (should (message "123 %s" var)))
  (format "%s" "string"))

;;;;; Tests without duplicates

(ert-deftest test-org-tests/test-without-duplicates-1 ()
  "Test without duplicates."
  (let ((var-1 "asd"))
    (concat "string" var-1))
  (should
   (let ((var-1 "asd"))
     (concat "string" var-1))))

(ert-deftest test-org-tests/test-without-duplicates-2 ()
  "Test without duplicates.
Equal `should' macros, but different nesting paths."
  (let ((var "string"))
    (should (format "123 %s" "asd")))
  (+ 5 6 9)
  (should (format "123 %s" "asd")))

;;;;; Duplicate deftests (maybe different names, but same body)

(ert-deftest test-org-tests/duplicate-test-1 ()
  "Docstring of duplicate-test-1."
  (let ((var 99))
    (+ 5 6 9 var)
    (should (format "123 %s" "asd")))
  (should (format "123 %s" "asd")))

(ert-deftest test-org-tests/duplicate-test-2 ()
  "Docstring of duplicate-test-2."
  (let ((var 99))
    (+ 5 6 9 var)
    (should (format "123 %s" "asd")))
  (should (format "123 %s" "asd")))

(provide 'test-duplicates-detector)

;; Local Variables:
;;   outline-regexp: "\\(;\\{3,\\} \\)"
;; End:

;;; test-duplicates-detector.el ends here
