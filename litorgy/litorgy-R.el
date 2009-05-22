;;; litorgy-R.el --- litorgy functions for R code evaluation

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

;; Litorgy support for evaluating R code

;;; Code:
(require 'litorgy)

(litorgy-add-interpreter "R")

(defvar litorgy-R-func-name "litorgy_R_main"
  "This is the main function which wraps each R source code
block.")

(defun litorgy-execute:R (body params)
  "Execute a block of R code with litorgy.  This function is
called by `litorgy-execute-src-block'."
  (message "executing R source code block...")
  (save-window-excursion
    (let ((vars (litorgy-ref-variables params))
          results)
      (litorgy-R-initiate-R-buffer)
      (mapc (lambda (pair) (litorgy-R-assign-elisp (car pair) (cdr pair))) vars)
      (litorgy-R-input-command
       (format "%s <- function ()\n{\n%s\n}" litorgy-R-func-name body))
      (litorgy-R-to-elisp litorgy-R-func-name))))

(defun litorgy-R-quote-tsv-field (s)
  "Quote field S for export to R."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun litorgy-R-assign-elisp (name value)
  "Read the elisp VALUE into a variable named NAME in the current
R process in `litorgy-R-buffer'."
  (unless litorgy-R-buffer (error "No active R buffer"))
  (litorgy-R-input-command
   (if (listp value)
       (let ((transition-file (make-temp-file "litorgy-R-import")))
         ;; ensure VALUE has an orgtbl structure (depth of at least 2)
         (unless (listp (car value)) (setq value (list value)))
         (with-temp-file transition-file
           (insert (orgtbl-to-tsv value '(:fmt litorgy-R-quote-tsv-field)))
           (insert "\n"))
         (format "%s <- read.table(\"%s\", sep=\"\\t\", as.is=TRUE)" name transition-file))
     (format "%s <- %s" name (litorgy-R-quote-tsv-field value)))))

(defun litorgy-R-to-elisp (func-name)
  "Return the result of calling the function named FUNC-NAME in
`litorgy-R-buffer' as Emacs lisp."
  (let ((tmp-file (make-temp-file "litorgy-R")) result)
    (litorgy-R-input-command
     (format "write.table(%s(), \"%s\", , ,\"\\t\", ,\"nil\", , FALSE, FALSE)" func-name tmp-file))
    (with-temp-buffer
      (org-table-import tmp-file nil)
      (delete-file tmp-file)
      (setq result (mapcar (lambda (row)
                             (mapcar #'litorgy-R-read row))
                           (org-table-to-lisp)))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
          (if (consp (car result))
              (if (null (cdr (car result)))
                  (caar result)
                result)
            (car result))
        result))))

(defun litorgy-R-read (cell)
  "Strip nested \"s from around strings in exported R values."
  (litorgy-read (or (and (stringp cell)
                         (string-match "\\\"\\(.+\\)\\\"" cell)
                         (match-string 1 cell))
                    cell)))

;; functions for evaluation of R code
(defvar litorgy-R-buffer nil
  "Holds the buffer for the current R process")

(defun litorgy-R-initiate-R-buffer ()
  "If there is not a current R process then create one."
  ;; DED: Ideally I think we should use ESS mechanisms for this sort
  ;; of thing. See ess-force-buffer-current.
  (unless (and (buffer-live-p litorgy-R-buffer) (get-buffer litorgy-R-buffer))
    (save-excursion
      (R)
      (setf litorgy-R-buffer (current-buffer))
      (litorgy-R-wait-for-output)
      (litorgy-R-input-command ""))))

(defun litorgy-R-command-to-string (command)
  "Send a command to R, and return the results as a string."
  (litorgy-R-input-command command)
  (litorgy-R-last-output))

(defun litorgy-R-input-command (command)
  "Pass COMMAND to the R process running in `litorgy-R-buffer'."
  (save-excursion
    (save-match-data
      (set-buffer litorgy-R-buffer)
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (insert command)
      (comint-send-input)
      (litorgy-R-wait-for-output))))

(defun litorgy-R-wait-for-output ()
  "Wait until output arrives"
  (save-excursion
    (save-match-data
      (set-buffer litorgy-R-buffer)
      (while (progn
	       (goto-char comint-last-input-end)
	       (not (re-search-forward comint-prompt-regexp nil t)))
	(accept-process-output (get-buffer-process (current-buffer)))))))

(defun litorgy-R-last-output ()
  "Return the last R output as a string"
  (save-excursion
    (save-match-data
      (set-buffer litorgy-R-buffer)
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

(provide 'litorgy-R)
;;; litorgy-R.el ends here
