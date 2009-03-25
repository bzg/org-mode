;;; litorgy-lisp.el --- litorgy functions for lisp code evaluation

;; Copyright (C) 2009 Eric Schulte, Dan Davison, Austin F. Frank

;; Author: Eric Schulte, Dan Davison, Austin F. Frank
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

;; Litorgy support for evaluating lisp code

;;; Code:
(require 'litorgy)

(litorgy-add-interpreter "emacs-lisp")

(defun litorgy-execute:emacs-lisp (body params)
  "Execute a block of emacs-lisp code with litorgy.  This
function is called by `litorgy-execute-src-block'."
  (save-window-excursion
    (let ((vars (litorgy-reference-variables params))
          (print-level nil) (print-length nil) results)
      (message "prefix")
      (message (format "-%S-" body))
      (message (format "%S" (stringp body)))
      (message (format "vars = %S" vars))
      (setq vars (mapcar (lambda (pair)
                         (list (intern (car pair))
                               (mapcar (lambda (row)
                                         (mapcar #'litorgy-read-cell row))
                                       (cdr pair))))
                       vars))
      (message (format "let = %S" vars))
      ;; need to find a way of assigning the variables in vars for the
      ;; context in which body is executed
      (message "executing emacs-lisp code block...")
      (format "%S"
              (eval `(let ,(mapcar (lambda (var)
                                     `(,(car var) ',(cdr var)))
                                   vars)
                       (message "inside")
                       (message (format "- %S -" table))
                       (message (format "- %S -" body))
                       ,(read body)))))))

(defun litorgy-read-cell (cell)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if cell looks like a list (meaning it starts with a
'(') then read it as lisp, otherwise return it unmodified as a
string.

This is taken almost directly from `org-read-prop'."
  (if (and (stringp cell) (not (equal cell "")))
      (let ((out (string-to-number cell)))
	(if (equal out 0)
	    (if (or (equal "(" (substring cell 0 1))
                    (equal "'" (substring cell 0 1)))
                (read cell)
	      (if (string-match "^\\(+0\\|-0\\|0\\)$" cell)
		  0
		(progn (set-text-properties 0 (length cell) nil cell)
		       cell)))
	  out))
    cell))

(provide 'litorgy-lisp)
;;; litorgy-lisp.el ends here
