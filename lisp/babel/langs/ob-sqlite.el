;;; ob-sqlite.el --- org-babel functions for sqlite database interaction

;; Copyright (C) 2010  Free Software Foundation

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating sqlite source code.

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:sqlite '())

(defun org-babel-expand-body:sqlite
  (body params &optional processed-params) body)

(defvar org-babel-sqlite3-command "sqlite3")

(defun org-babel-execute:sqlite (body params)
  "Execute a block of Sqlite code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Sqlite source code block")
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
	(vars (org-babel-ref-variables params))
	(headers-p (equal "yes" (cdr (assoc :colnames params)))))
    (with-temp-buffer
      (insert
       (shell-command-to-string
	(format "%s %s -csv %s %S"
		org-babel-sqlite3-command
		(if headers-p "-header" "")
		(cdr (assoc :db params))
		(org-babel-sqlite-expand-vars body vars))))
      (if (or (member "scalar" result-params)
	      (member "code" result-params))
	  (buffer-string)
	(org-table-convert-region (point-min) (point-max))
	(org-babel-sqlite-table-or-scalar
	 (org-babel-sqlite-offset-colnames
	  (org-table-to-lisp) headers-p))))))

(defun org-babel-sqlite-expand-vars (body vars)
  "Expand the variables held in VARS in BODY."
  (mapc
   (lambda (pair)
     (setq body (replace-regexp-in-string
		 (format "\$%s" (car pair))
		 (format "%S" (cdr pair))
		 body)))
   vars)
  body)

(defun org-babel-sqlite-table-or-scalar (result)
  "If RESULT looks like a trivial table, then unwrap it."
  (if (and (equal 1 (length result))
	   (equal 1 (length (car result))))
      (caar result)
    result))

(defun org-babel-sqlite-offset-colnames (table headers-p)
  "If HEADERS-P is non-nil then offset the first row as column names."
  (if headers-p
      (cons (car table) (cons 'hline (cdr table)))
    table))

(defun org-babel-prep-session:sqlite (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "sqlite sessions not yet implemented"))

(provide 'ob-sqlite)

;; arch-tag: 5c03d7f2-0f72-48b8-bbd1-35aafea248ac

;;; ob-sqlite.el ends here
