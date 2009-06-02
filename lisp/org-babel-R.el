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

(defvar org-babel-R-func-name "org_babel_R_main"
  "This is the main function which wraps each R source code
block.")

(defun org-babel-execute:R (body params)
  "Execute a block of R code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing R source code block...")
  (save-window-excursion
    (let ((vars (org-babel-ref-variables params))
          results)
      (org-babel-R-initiate-R-buffer)
      (mapc (lambda (pair) (org-babel-R-assign-elisp (car pair) (cdr pair))) vars)
      (org-babel-R-input-command
       (format "%s <- function ()\n{\n%s\n}" org-babel-R-func-name body))
      (org-babel-R-to-elisp org-babel-R-func-name))))

(defun org-babel-R-quote-tsv-field (s)
  "Quote field S for export to R."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-R-assign-elisp (name value)
  "Read the elisp VALUE into a variable named NAME in the current
R process in `org-babel-R-buffer'."
  (unless org-babel-R-buffer (error "No active R buffer"))
  (org-babel-R-input-command
   (if (listp value)
       (let ((transition-file (make-temp-file "org-babel-R-import"))
	     has-header)
	 ;; ensure VALUE has an orgtbl structure (depth of at least 2)
	 (unless (listp (car value)) (setq value (list value)))
	 (setq has-header (and (symbolp (cadr value)) (equal (cadr value) 'hline)))
	 (with-temp-file transition-file
	   (insert (orgtbl-to-tsv value '(:fmt org-babel-R-quote-tsv-field)))
	   (insert "\n"))
	 (format "%s <- read.table(\"%s\", header=%s, sep=\"\\t\", as.is=TRUE)"
		 name transition-file (if has-header "TRUE" "FALSE")))
     (format "%s <- %s" name (org-babel-R-quote-tsv-field value)))))

(defun org-babel-R-to-elisp (func-name)
  "Return the result of calling the function named FUNC-NAME in
`org-babel-R-buffer' as Emacs lisp."
  (let ((tmp-file (make-temp-file "org-babel-R")) result)
    (org-babel-R-input-command
     (format "write.table(%s(), \"%s\", , ,\"\\t\", ,\"nil\", , FALSE, FALSE)" func-name tmp-file))
    (with-temp-buffer
      (message "before condition")
      (condition-case nil
          (progn
            (org-table-import tmp-file nil)
            (delete-file tmp-file)
            (setq result (mapcar (lambda (row)
                                   (mapcar #'org-babel-R-read row))
                                 (org-table-to-lisp))))
        (error nil))
      (message "after condition")
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

;; functions for evaluation of R code
(defvar org-babel-R-buffer nil
  "Holds the buffer for the current R process")

(defun org-babel-R-initiate-R-buffer ()
  "If there is not a current R process then create one."
  ;; DED: Ideally I think we should use ESS mechanisms for this sort
  ;; of thing. See ess-force-buffer-current.
  (unless (and (buffer-live-p org-babel-R-buffer) (get-buffer org-babel-R-buffer))
    (save-excursion
      (R)
      (setf org-babel-R-buffer (current-buffer))
      (org-babel-R-wait-for-output)
      (org-babel-R-input-command ""))))

(defun org-babel-R-command-to-string (command)
  "Send a command to R, and return the results as a string."
  (org-babel-R-input-command command)
  (org-babel-R-last-output))

(defun org-babel-R-input-command (command)
  "Pass COMMAND to the R process running in `org-babel-R-buffer'."
  (save-excursion
    (save-match-data
      (set-buffer org-babel-R-buffer)
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (insert command)
      (comint-send-input)
      (org-babel-R-wait-for-output))))

(defun org-babel-R-wait-for-output ()
  "Wait until output arrives"
  (save-excursion
    (save-match-data
      (set-buffer org-babel-R-buffer)
      (while (progn
	       (goto-char comint-last-input-end)
	       (not (re-search-forward comint-prompt-regexp nil t)))
	(accept-process-output (get-buffer-process (current-buffer)))))))

(defun org-babel-R-last-output ()
  "Return the last R output as a string"
  (save-excursion
    (save-match-data
      (set-buffer org-babel-R-buffer)
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let ((raw (buffer-substring comint-last-input-end (- (point) 1)))
            output output-flag)
        (mapconcat
         (lambda (el)
           (if (stringp el)
               (format "%s" el)
             (format "%S" el)))
         (delq nil
               (mapcar
                (lambda (line)
                  (unless (string-match "^>" line)
                    (and (string-match "\\[[[:digit:]]+\\] *\\(.*\\)$" line)
                         (match-string 1 line))))
                ;; drop first, because it's the last line of input
                (cdr (split-string raw "[\n\r]")))) "\n")))))

(provide 'org-babel-R)
;;; org-babel-R.el ends here
