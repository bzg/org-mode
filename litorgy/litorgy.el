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
  "Detect if this is context for a litorgical src-block and run if
so then run `litorgy-execute-src-block'."
  (let ((case-fold-search t))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at litorgy-src-block-regexp))
        (progn (call-interactively 'litorgy-execute-src-block)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

(add-hook 'org-ctrl-c-ctrl-c-hook 'litorgy-execute-src-block-maybe)

(defcustom litorgy-example-size-cutoff 10
  "Number at lines at which to switch from using the ': '
org-mode quote sytax to using a '#+BEGIN_EXAMPLE' block"
  :group 'litorgy
  :type 'integer)

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

(defcustom litorgy-interpreters '()
  "Interpreters allows for evaluation tags.
This is a list of program names (as strings) that can evaluate code and
insert the output into an Org-mode buffer.  Valid choices are 

R          Evaluate R code
emacs-lisp Evaluate Emacs Lisp code and display the result
sh         Pass command to the shell and display the result
perl       The perl interpreter
python     The python interpreter
ruby       The ruby interpreter"
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
commented by `litorgy-make-region-example'.  With optional prefix
don't dump results into buffer."
  (interactive "P")
  (let* ((info (litorgy-get-current-src-block-info))
         (lang (first info))
         (body (second info))
         (params (third info))
         (cmd (intern (concat "litorgy-execute:" lang)))
         result)
    (unless (member lang litorgy-interpreters)
      (error "Language is not in `litorgy-interpreters': %s" lang))
    (setq result (funcall cmd body params))
    (unless arg (litorgy-insert-result result))))

(defun litorgy-eval-subtree (&optional arg)
  "Replace EVAL snippets in the entire subtree."
  (interactive "P")
  (save-excursion
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (while (re-search-forward litorgy-regexp nil t)
      (litorgy-eval-src-block arg))
    (widen)))

(defun litorgy-get-current-src-block-info ()
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
    (list lang body
          (mapc (lambda (arg)
                  (if (string-match "\\([^ :]+\\):\\([^ :]+\\)" arg)
                      (cons (match-string 1 arg) (match-string 2 arg))))
                (split-string args)))))

(defun litorgy-insert-result (result)
  (save-excursion
    (re-search-forward "^#\\+end_src" nil t) (open-line 1) (forward-char 2)
    (let ((beg (point))
          (end (progn (insert result)
                      (point))))
      (message (format "from %S %S" beg end))
      (litorgy-make-region-example beg end))))

(defun litorgy-make-region-example (beg end)
  "Comment out region using either the '^:' or the BEGIN_EXAMPLE
syntax based on the size of the region as compared to
`litorgy-example-size-cutoff'."
  (interactive "*r")
  (let ((size (abs (- (line-number-at-pos end)
		      (line-number-at-pos beg)))))
    (if (= size 0)
	(let ((result (buffer-substring beg end)))
	  (delete-region beg end)
	  (insert (concat ": " result)))
      (if (<= size litorgy-example-size-cutoff)
	  (save-excursion
	    (goto-char beg)
	    (dotimes (n size)
	      (move-beginning-of-line 1) (insert ": ") (forward-line 1)))
	(let ((result (buffer-substring beg end)))
	  (delete-region beg end)
	  (insert (concat "#+BEGIN_EXAMPLE\n" result "#+END_EXAMPLE\n")))))))  

(defun litorgy-clean-text-properties (text)
  "Strip all properties from text return."
  (set-text-properties 0 (length text) nil text) text)

(provide 'litorgy)
;;; litorgy.el ends here
