;;; org-eval.el --- Display result of evaluating code in various languanges
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.03
;;
;; This file is not yet part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This modules allows to include output from various commands into an
;; Org-mode buffer.  This technique has been copied from Emacs-Muse, and
;; we try to make it work here in a way as simila as possible to
;; Muse.

(require 'org)

;;; Customization

(defgroup org-eval nil
  "Options concerning including output from commands into the Org-mode buffer."
  :tag "Org Eval"
  :group 'org)

(defface org-eval
  (org-compatible-face nil
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey40"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey60"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face for command output that is included into an Org-mode buffer."
  :group 'org-eval
  :group 'org-faces
  :version "22.1")

(defvar org-eval-regexp nil)

(defun org-eval-set-interpreters (var value)
  (set-default var value)
  (setq org-eval-regexp
	(concat "<\\("
		(mapconcat 'regexp-quote value "\\|")
		"\\)"
		"\\([^>]\\{0,50\\}?\\)>"
		"\\([^\000]+?\\)</\\1>")))

(defcustom org-eval-interpreters '("lisp")
  "Interpreters allows for evaluation tags.
This is a list of program names (as strings) that can evaluate code and
insert the output into an Org-mode buffer.  Valid choices are 

lisp    Interpret Emacs Lisp code and display the result
shell   Pass command to the shell and display the result
perl    The perl interpreter
python  Thy python interpreter
ruby    The ruby interpreter"
  :group 'org-eval
  :set 'org-eval-set-interpreters
  :type '(set :greedy t
	      (const "lisp")
	      (const "perl")
	      (const "python")
	      (const "ruby")
	      (const "shell")))
  
(defun org-eval-handle-snippets (limit &optional replace)
  "Evaluate code nisppets and display the results as display property.
When REPLACE is non-nil, replace the code region with the result (used
for export)."
  (let (a)
    (while (setq a (text-property-any (point) (or limit (point-max))
				      'org-eval t))
      (remove-text-properties
       a (next-single-property-change a 'org-eval nil limit)
       '(display t intangible t org-eval t))))
  (while (re-search-forward org-eval-regexp limit t)
    (let* ((beg (match-beginning 0))
	   (end (match-end 0))
	   (kind (match-string 1))
	   (attr (match-string 2))
	   (code (match-string 3))
	   (value (org-eval-code kind code))
	   markup lang)
      (if replace
	  (progn
	    (setq attr (save-match-data (org-eval-get-attributes attr))
		  markup (cdr (assoc "markup" attr))
		  lang  (cdr (assoc "lang" attr)))
	    (replace-match
	     (concat (if markup (format "#+BEGIN_%s" (upcase markup)))
		     (if (and markup (equal (downcase markup) "src"))
			 (concat " " (or lang "fundamental")))
		     "\n"
		     value
		     (if markup (format "\n#+END_%s\n" (upcase markup))))
	     t t))
	(add-text-properties
	 beg end
	 (list 'display value 'intangible t 'font-lock-multiline t
	       'face 'org-eval
	       'org-eval t))))))

(defun org-eval-replace-snippts ()
  "Replace EVAL snippets in the entire buffer.
This should go into the `org-export-preprocess-hook'."
  (goto-char (point-min))
  (org-eval-handle-snippets nil 'replace))

(add-hook 'org-export-preprocess-hook 'org-eval-replace-snippts)
(add-hook 'org-font-lock-hook 'org-eval-handle-snippets)

(defun org-eval-get-attributes (str)
  (let ((start 0) key value rtn)
    (while (string-match "\\<\\([a-zA-Z]+\\)\\>=\"\\([^\"]+\\)\"" str start)
      (setq key (match-string 1 str)
	    value (match-string 2 str)
	    start (match-end 0))
      (push (cons key value) rtn))
    rtn))

(defun org-eval-code (interpreter code)
  (cond
   ((equal interpreter "lisp")
    (org-eval-lisp (concat "(progn\n" code "\n)")))
   ((equal interpreter "shell")
    (shell-command-to-string code))
   ((member interpreter '("perl" "python" "ruby"))
    (org-eval-run (executable-find interpreter) code))
   (t (error "Cannot evaluate code type %s" interpreter))))

(defun org-eval-lisp (form)
  "Evaluate the given form and return the result as a string."
  (require 'pp)
  (save-match-data
    (condition-case err
        (let ((object (eval (read form))))
          (cond
           ((stringp object) object)
           ((and (listp object)
                 (not (eq object nil)))
            (let ((string (pp-to-string object)))
              (substring string 0 (1- (length string)))))
           ((numberp object)
            (number-to-string object))
           ((eq object nil) "")
           (t
            (pp-to-string object))))
      (error
       (org-display-warning (format "%s: Error evaluating %s: %s"
                                     "???" form err))
       "; INVALID LISP CODE"))))

(defun org-eval-run (cmd code)
  (with-temp-buffer
    (insert code)
    (shell-command-on-region (point-min) (point-max) cmd nil 'replace)
    (buffer-string)))  

(provide 'org-eval)

;;; org-eval.el ends here

