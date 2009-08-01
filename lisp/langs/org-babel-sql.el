;;; org-babel-sql.el --- org-babel functions for sql evaluation

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating sql source code.
;;
;; SQL is somewhat unique in that there are many different engines for
;; the evaluation of sql (Mysql, PostgreSQL, etc...), so much of this
;; file will have to be implemented engine by engine.
;;
;; Also SQL evaluation generally takes place inside of a database.

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "sql")

(add-to-list 'org-babel-tangle-langs '("sql" "sql"))

(defun org-babel-execute:sql (body params)
  "Execute a block of Sql code with org-babel.  This function is
called by `org-babel-execute-src-block' via multiple-value-bind."
  (message "executing Sql source code block")
  )

(defun org-babel-prep-session:sql (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  ;; (message "params=%S" params) ;; debugging
  )

;; helper functions

(defun org-babel-sql-var-to-sql (var)
  "Convert an elisp var into a string of sql source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-sql-var-to-sql var ", ") "]")
    (format "%S" var)))

(defun org-babel-sql-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(provide 'org-babel-sql)
;;; org-babel-sql.el ends here
