;;; org-collector --- collect properties into tables

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte dot eric at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp, experimentation,
;;           organization, properties
;; Homepage: http://orgmode.org
;; Version: 0.01

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Pass in an alist of columns, each column can be either a single
;; property or a function which takes column names as arguments.
;;
;; For example the following propview block would collect the value of
;; the 'amount' property from each header in the current buffer
;;
;; #+BEGIN: propview :cols (ITEM amount)
;; | "ITEM"              | "amount" |
;; |---------------------+----------|
;; | "December Spending" |        0 |
;; | "Grocery Store"     |    56.77 |
;; | "Athletic club"     |     75.0 |
;; | "Restaurant"        |    30.67 |
;; | "January Spending"  |        0 |
;; | "Athletic club"     |     75.0 |
;; | "Restaurant"        |    50.00 |
;; |---------------------+----------|
;; |                     |          |
;; #+END:
;;
;; This slightly more selective propview block will limit those
;; headers included to those in the subtree with the id 'december'
;; in which the spendtype property is equal to "food"
;;
;; #+BEGIN: propview :id "december" :conds ((string= spendtype "food")) :cols (ITEM amount)
;; | "ITEM"          | "amount" |
;; |-----------------+----------|
;; | "Grocery Store" |    56.77 |
;; | "Restaurant"    |    30.67 |
;; |-----------------+----------|
;; |                 |          |
;; #+END:
;;
;; Org Collector allows arbitrary processing of the property values
;; through elisp in the cols: property.  This allows for both simple
;; computations as in the following example
;;
;; #+BEGIN: propview :id "results" :cols (ITEM f d list (apply '+ list) (+ f d))
;; | "ITEM" | "f" | "d" | "list"                  | "(apply (quote +) list)" | "(+ f d)" |
;; |--------+-----+-----+-------------------------+--------------------------+-----------|
;; | "run1" |   2 |  33 | (quote (9 2 3 4 5 6 7)) | 36                       |        35 |
;; | "run2" |   2 |  34 | :na                     | :na                      |        36 |
;; | "run3" |   2 |  35 | :na                     | :na                      |        37 |
;; | "run4" |   2 |  36 | :na                     | :na                      |        38 |
;; |        |     |     |                         |                          |           |
;; #+END:
;;
;; or more complex computations as in the following example taken from
;; an org file where each header in "results" subtree contained a
;; property "sorted_hits" which was passed through the
;; "average-precision" elisp function
;;
;; #+BEGIN: propview :id "results" :cols (ITEM (average-precision sorted_hits))
;; | "ITEM"    | "(average-precision sorted_hits)" |
;; |-----------+-----------------------------------|
;; | run (80)  |                          0.105092 |
;; | run (70)  |                          0.108142 |
;; | run (10)  |                          0.111348 |
;; | run (60)  |                          0.113593 |
;; | run (50)  |                          0.116446 |
;; | run (100) |                          0.118863 |
;; #+END:
;; 

;;; Code:
(require 'org)
(require 'org-table)

(defvar org-propview-default-value 0
  "Default value to insert into the propview table when the no
value is calculated either through lack of required variables for
a column, or through the generation of an error.")

(defun and-rest (list)
  (if (listp list)
      (if (> (length list) 1)
	  (and (car list) (and-rest (cdr list)))
	(car list))
    list))

(put 'org-collector-error
     'error-conditions
     '(error column-prop-error org-collector-error))

(defun org-read-prop (prop)
  "Convert the string property PROP to a number if appropriate.
If prop looks like a list (meaning it starts with a '(') then
read it as lisp expression, otherwise return it unmodified as a
string.

Results of calling:
\(org-read-prop \"12\") -> 12
\(org-read-prop \"(1 2 3)\") -> (1 2 3)
\(org-read-prop \"+0\") -> 0
\(org-read-prop \"aaa\") -> \"aaa\""
  (if (and (stringp prop) (not (equal prop "")))
      (let ((out (string-to-number prop)))
	(if (equal out 0)
	    (cond
	     ((or
	       (equal "(" (substring prop 0 1))
	       (equal "'" (substring prop 0 1)))

	      (condition-case nil
		  (read prop)
		(error prop)))
	     ((string-match "^\\(+0\\|-0\\|0\\)$" prop)
	      0)
	     (t
	      (set-text-properties 0 (length prop) nil prop)
	      prop))
	  out))
    prop))

(defun org-dblock-write:propview (params)
  "collect the column specification from the #+cols line
preceeding the dblock, then update the contents of the dblock."
  (interactive)
  (condition-case er
      (let ((cols (plist-get params :cols))
	    (conds (plist-get params :conds))
	    (match (plist-get params :match))
	    (scope (plist-get params :scope))
	    (content-lines (org-split-string (plist-get params :content) "\n"))
	    id table line pos)
	(save-excursion
	  (when (setq id (plist-get params :id))
	    (cond ((not id) nil)
		  ((eq id 'global) (goto-char (point-min)))
		  ((eq id 'local)  nil)
		  ((setq idpos (org-find-entry-with-id id))
		   (goto-char idpos))
		  (t (error "Cannot find entry with :ID: %s" id))))
	  (org-narrow-to-subtree)
	  (setq table (org-propview-to-table (org-propview-collect cols conds match scope)))
	  (widen))
	(setq pos (point))
	(when content-lines
	  (while (string-match "^#" (car content-lines))
	    (insert (pop content-lines) "\n")))
	(insert table) (insert "\n|--") (org-cycle) (move-end-of-line 1)
	(message (format "point-%d" pos))
	(while (setq line (pop content-lines))
	  (when (string-match "^#" line)
	    (insert "\n" line)))
	(goto-char pos)
	(org-table-recalculate 'all))
    (org-collector-error (widen) (error "%s" er))
    (error (widen) (error "%s" er))))

(defun org-propview-eval-w-props (props body)
  "evaluate the BODY-FORMS binding the variables using the
variables and values specified in props"
  (condition-case nil ;; catch any errors
      (eval `(let ,(mapcar
		    (lambda (pair) (list (intern (car pair)) (cdr pair)))
		    props)
	       ,body))
    (error nil)))

(defun org-propview-collect (cols &optional conds match scope)
  (interactive)
  ;; collect the properties from every header
  (let* ((header-props
	  (let ((org-trust-scanner-tags t))
	    (org-map-entries (quote (cons (cons "ITEM" (org-get-heading t))
					  (org-entry-properties)))
			     match scope)))
	 ;; read property values
	 (header-props (mapcar (lambda (props)
				 (mapcar (lambda (pair) (cons (car pair) (org-read-prop (cdr pair))))
					 props))
			       header-props))
	 ;; collect all property names
	 (prop-names (mapcar 'intern (delete-dups
				      (apply 'append (mapcar (lambda (header)
							       (mapcar 'car header))
							     header-props))))))
    (append
     (list
      (mapcar (lambda (el) (format "%S" el)) cols) ;; output headers
      'hline) ;; ------------------------------------------------
     (mapcar ;; calculate the value of the column for each header
      (lambda (props) (mapcar (lambda (col) (let ((result (org-propview-eval-w-props props col)))
					      (if result result org-propview-default-value)))
			      cols))
      (if conds
	  ;; eliminate the headers which don't satisfy the property
	  (delq nil
		(mapcar
		 (lambda (props)
		   (if (and-rest (mapcar (lambda (col) (org-propview-eval-w-props props col)) conds))
		       props))
		 header-props))
	  header-props)))))

(defun org-propview-to-table (results)
  ;; (message (format "cols:%S" cols))
  (orgtbl-to-orgtbl
   (mapcar
    (lambda (row)
      (if (equal row 'hline)
	  'hline
	(mapcar (lambda (el) (format "%S" el)) row)))
    (delq nil results)) '()))

(provide 'org-collector)
;;; org-collector ends here
