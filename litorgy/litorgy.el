;;; litorgy.el --- literate programing in org-mode

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

;; See rorg.org in this directory for more information

;;; Code:
(require 'org)

(defun litorgy-execute-src-block-maybe ()
  "Detect if this is context for a litorgical src-block and if so
then run `litorgy-execute-src-block'."
  (let ((case-fold-search t))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at litorgy-src-block-regexp))
        (progn (call-interactively 'litorgy-execute-src-block)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

(add-hook 'org-ctrl-c-ctrl-c-hook 'litorgy-execute-src-block-maybe)

(defvar litorgy-src-block-regexp nil
  "Regexp used to test when inside of a litorgical src-block")

(defun litorgy-set-interpreters (var value)
  (set-default var value)
  (setq litorgy-src-block-regexp
	(concat "#\\+begin_src \\("
		(mapconcat 'regexp-quote value "\\|")
		"\\)"
                "\\([ \t]+\\([^\n]+\\)\\)?\n" ;; match header arguments
                "\\([^\000]+?\\)#\\+end_src")))

(defun litorgy-add-interpreter (interpreter)
  "Add INTERPRETER to `litorgy-interpreters' and update
`litorgy-src-block-regexp' appropriately."
  (unless (member interpreter litorgy-interpreters)
    (setq litorgy-interpreters (cons interpreter litorgy-interpreters))
    (litorgy-set-interpreters 'litorgy-interpreters litorgy-interpreters)))

(defcustom litorgy-interpreters '()
  "Interpreters allows for evaluation tags.
This is a list of program names (as strings) that can evaluate code and
insert the output into an Org-mode buffer.  Valid choices are

R          Evaluate R code
emacs-lisp Evaluate Emacs Lisp code and display the result
sh         Pass command to the shell and display the result
perl       The perl interpreter
python     The python interpreter
ruby       The ruby interpreter

The source block regexp `litorgy-src-block-regexp' is updated
when a new interpreter is added to this list through the
customize interface.  To add interpreters to this variable from
lisp code use the `litorgy-add-interpreter' function."
  :group 'litorgy
  :set 'litorgy-set-interpreters
  :type '(set :greedy t
              (const "R")
	      (const "emacs-lisp")
              (const "sh")
	      (const "perl")
	      (const "python")
	      (const "ruby")))

;;; functions
(defun litorgy-execute-src-block (&optional arg)
  "Execute the current source code block, and dump the results
into the buffer immediately following the block.  Results are
commented by `org-toggle-fixed-width-section'.  With optional
prefix don't dump results into buffer but rather return the
results in raw elisp (this is useful for automated execution of a
source block)."
  (interactive "P")
  (let* ((info (litorgy-get-src-block-info))
         (lang (first info))
         (body (second info))
         (params (third info))
         (cmd (intern (concat "litorgy-execute:" lang)))
         result)
    (unless (member lang litorgy-interpreters)
      (error "Language is not in `litorgy-interpreters': %s" lang))
    (setq result (funcall cmd body params))
    (if arg
        result
      (litorgy-insert-result result (cdr (assoc :results params))))))

(defun litorgy-eval-buffer (&optional arg)
  "Replace EVAL snippets in the entire buffer."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward litorgy-regexp nil t)
      (litorgy-eval-src-block arg))))

(defun litorgy-eval-subtree (&optional arg)
  "Replace EVAL snippets in the entire subtree."
  (interactive "P")
  (save-excursion
    (org-narrow-to-subtree)
    (litorgy-eval-buffer)
    (widen)))

(defun litorgy-get-src-block-info ()
  "Return the information of the current source block (the point
should be on the '#+begin_src' line) as a list of the following
form.  (language body header-arguments-alist)"
  (unless (save-excursion
            (beginning-of-line 1)
            (looking-at litorgy-src-block-regexp))
    (error "not looking at src-block"))
  (let ((lang (litorgy-clean-text-properties (match-string 1)))
        (args (litorgy-clean-text-properties (or (match-string 3) "")))
        (body (litorgy-clean-text-properties (match-string 4))))
    (list lang body (litorgy-parse-header-arguments args))))

(defun litorgy-parse-header-arguments (arg-string)
  "Parse a string of header arguments returning an alist."
  (delq nil
        (mapcar
         (lambda (arg) (if (string-match "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]*\\([^ \f\t\n\r\v]*\\)" arg)
                           (cons (intern (concat ":" (match-string 1 arg))) (match-string 2 arg))))
         (split-string (concat " " arg-string) "[ \f\t\n\r\v]+:"))))

(defun litorgy-insert-result (result &optional insert)
  "Insert RESULT into the current buffer after the end of the
current source block.  With optional argument INSERT controls
insertion of results in the org-mode file.  INSERT can take the
following values...

t ------ the default options, simply insert the results after the
         source block
         
replace - insert results after the source block replacing any
          previously inserted results

silent -- no results are inserted"
  (message (format "-%S-" result))
  (if (stringp result)
      (setq result (litorgy-clean-text-properties result))
    (unless (listp result) (setq result (format "%S" result))))
  (if (and insert (string-equal insert "replace"))
      (litorgy-remove-result (listp result)))
  (if (= (length result) 0)
      (message "no result returned by source block")
    (unless (and insert (string-equal insert "silent"))
      (when (and (stringp result)
                 (not (or (string-equal (substring result -1) "\n")
                          (string-equal (substring result -1) "\r"))))
        (setq result (concat result "\n")))
      (save-excursion
        (if (re-search-forward "^#\\+end_src" nil t)
            (progn (open-line 1) (forward-char 2))
          (progn (open-line 1) (forward-char 1)))
        (if (stringp result) ;; assume the result is a table if it's not a string
            (litorgy-examplize-region (point) (progn (insert result) (point)))
          (progn
            (insert 
             (concat (orgtbl-to-orgtbl
                      (if (consp (car result)) result (list result))
                      '(:fmt (lambda (cell) (format "%S" cell)))) "\n"))
            (forward-line -1)
            (org-cycle)))))))

(defun litorgy-result-to-org-string (result)
  "Return RESULT as a string in org-mode format.  This function
relies on `litorgy-insert-result'."
  (with-temp-buffer (litorgy-insert-result result) (buffer-string)))

(defun litorgy-remove-result (&optional table)
  "Remove the result following the current source block.  If
optional argument TABLE is supplied then remove the table
following the block rather than the fixed width example."
  (save-excursion
    (re-search-forward "^#\\+end_src" nil t)
    (forward-char 1)
    (delete-region (point)
                   (save-excursion (forward-line 1)
                                   (if table
                                       (org-table-end)
                                     (while (if (looking-at ": ")
                                                (progn (while (looking-at ": ")
                                                         (forward-line 1)) t))
                                       (forward-line 1))
                                     (forward-line -1)
                                     (point))))))

(defun litorgy-examplize-region (beg end)
  "Comment out region using the ': ' org example quote."
  (interactive "*r")
  (let ((size (abs (- (line-number-at-pos end)
		      (line-number-at-pos beg)))))
    (if (= size 0)
	(let ((result (buffer-substring beg end)))
	  (delete-region beg end)
	  (insert (concat ": " result)))
      (save-excursion
        (goto-char beg)
        (dotimes (n size)
          (move-beginning-of-line 1) (insert ": ") (forward-line 1))))))

(defun litorgy-clean-text-properties (text)
  "Strip all properties from text return."
  (set-text-properties 0 (length text) nil text) text)

(provide 'litorgy)
;;; litorgy.el ends here
