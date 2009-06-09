;;; org-babel-R.el --- org-babel functions for R code evaluation

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, R, statistics
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

;; Org-Babel support for evaluating R code

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "R")

(defvar org-babel-R-buffer "org-babel-R"
  "Holds the buffer for the current R process")

(defun org-babel-R-initiate-R-buffer ()
  "If there is not a current R process then create one."
  (unless (org-babel-comint-buffer-livep org-babel-R-buffer)
    (save-window-excursion (R) (setf org-babel-R-buffer (current-buffer)))))

(defun org-babel-execute:R (body params)
  "Execute a block of R code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing R source code block...")
  (save-window-excursion
    (let ((vars (org-babel-ref-variables params))
          (results-params (split-string (or (cdr (assoc :results params)) "")))
          results)
      ;; (message (format "%S" results-params))
      (org-babel-R-initiate-R-buffer)
      (mapc (lambda (pair) (org-babel-R-assign-elisp (car pair) (cdr pair))) vars)
      (cond
       ((member "script" results-params) ;; collect all output
        (let ((tmp-file (make-temp-file "org-babel-R-script-output")))
          (org-babel-comint-input-command org-babel-R-buffer (format "sink(%S)" tmp-file))
          (org-babel-comint-input-command org-babel-R-buffer body)
          (org-babel-comint-input-command org-babel-R-buffer "sink()")
          (with-temp-buffer (insert-file-contents tmp-file) (buffer-string))))
       ((member "last" results-params) ;; the value of the last statement
        (org-babel-comint-input-command org-babel-R-buffer body)
        (org-babel-R-last-value-as-elisp))))))

(defun org-babel-R-quote-tsv-field (s)
  "Quote field S for export to R."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-R-assign-elisp (name value)
  "Read the elisp VALUE into a variable named NAME in the current
R process in `org-babel-R-buffer'."
  (unless org-babel-R-buffer (error "No active R buffer"))
  (org-babel-comint-input-command
   org-babel-R-buffer
   (if (listp value)
       (let ((transition-file (make-temp-file "org-babel-R-import")))
	 ;; ensure VALUE has an orgtbl structure (depth of at least 2)
	 (unless (listp (car value)) (setq value (list value)))
	 (with-temp-file transition-file
	   (insert (orgtbl-to-tsv value '(:fmt org-babel-R-quote-tsv-field)))
	   (insert "\n"))
	 (format "%s <- read.table(\"%s\", header=FALSE, sep=\"\\t\", as.is=TRUE)"
		 name transition-file))
     (format "%s <- %s" name (org-babel-R-quote-tsv-field value)))))

(defun org-babel-R-last-value-as-elisp ()
  "Return the last value returned by R as Emacs lisp."
  (let ((tmp-file (make-temp-file "org-babel-R")) result)
    (org-babel-comint-input-command
     org-babel-R-buffer
     (format "write.table(.Last.value, file=\"%s\", sep=\"\\t\", na=\"nil\",row.names=FALSE, col.names=FALSE, quote=FALSE)"
             tmp-file))
    (with-temp-buffer
      (condition-case nil
          (progn
            (org-table-import tmp-file nil)
            (delete-file tmp-file)
            (setq result (mapcar (lambda (row)
                                   (mapcar #'org-babel-R-read row))
                                 (org-table-to-lisp))))
        (error nil))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
          (if (consp (car result))
              (if (null (cdr (car result)))
                  (caar result)
                result)
            (car result))
        result))))

(defun org-babel-R-read (cell)
  "Strip nested \"s from around strings in exported R values."
  (org-babel-read (or (and (stringp cell)
                         (string-match "\\\"\\(.+\\)\\\"" cell)
                         (match-string 1 cell))
                    cell)))

(provide 'org-babel-R)
;;; org-babel-R.el ends here
