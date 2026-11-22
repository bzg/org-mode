;;; org-cdlatex.el --- CDLaTeX minor mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file defines org-cdlatex minor mode - CDLaTeX support in Org
;; mode.

;;; Code:

(require 'org-keys)
(require 'org-element)

(declare-function cdlatex-math-symbol "ext:cdlatex")
(declare-function cdlatex-compute-tables "ext:cdlatex" ())
(declare-function cdlatex-tab "ext:cdlatex" ())
(declare-function cdlatex-environment "ext:cdlatex" (environment item))
(declare-function cdlatex-math-modify "ext:cdlatex")
(declare-function cdlatex-sub-superscript "ext:cdlatex")

(declare-function org-return "org" (&optional indent arg interactive))
(declare-function org-inside-LaTeX-fragment-p "org" (&optional element))

(defvar org-cdlatex-mode-map (make-sparse-keymap)
  "Keymap for the minor `org-cdlatex-mode'.")

(org-defkey org-cdlatex-mode-map (kbd "_") #'org-cdlatex-underscore-caret)
(org-defkey org-cdlatex-mode-map (kbd "^") #'org-cdlatex-underscore-caret)
(org-defkey org-cdlatex-mode-map (kbd "`") #'cdlatex-math-symbol)
(org-defkey org-cdlatex-mode-map (kbd "'") #'org-cdlatex-math-modify)
(org-defkey org-cdlatex-mode-map (kbd "C-c {") #'org-cdlatex-environment-indent)

(defvar org-cdlatex-texmathp-advice-is-done nil
  "Flag remembering if we have applied the advice to texmathp already.")

;;;###autoload
(define-minor-mode org-cdlatex-mode
  "Toggle the minor `org-cdlatex-mode'.
This mode supports entering LaTeX environment and math in LaTeX fragments
in Org mode.
\\{org-cdlatex-mode-map}"
  :lighter " OCDL"
  (when org-cdlatex-mode
    ;; Try to load texmathp before cdlatex.  Otherwise, cdlatex can
    ;; bind `cdlatex--texmathp' to `ignore', not using `texmathp' at
    ;; all.
    (org-require-package 'texmathp "Auctex")
    (org-require-package 'cdlatex)
    (run-hooks 'cdlatex-mode-hook)
    (cdlatex-compute-tables))
  (unless org-cdlatex-texmathp-advice-is-done
    (setq org-cdlatex-texmathp-advice-is-done t)
    (advice-add 'texmathp :around #'org--math-p)))

(defvar texmathp-why)
(defun org--math-p (orig-fun &rest args)
  "Return t inside math fragments or running `cdlatex-math-symbol'.
This function is intended to be an :around advice for `texmathp'.

If Org mode thinks that point is actually inside
an embedded LaTeX environment, return t when the environment is math
or let `texmathp' do its job otherwise.
`\\[org-cdlatex-mode-map]'"
  (cond
   ((not (derived-mode-p 'org-mode)) (apply orig-fun args))
   ((eq this-command 'cdlatex-math-symbol)
    (setq texmathp-why '("cdlatex-math-symbol in org-mode" . 0))
    t)
   (t
    (let ((element (org-element-context)))
      (when (org-inside-LaTeX-fragment-p element)
        (pcase (substring-no-properties
                (org-element-property :value element)
                0 2)
          ((or "\\(" "\\[" (pred (string-match-p (rx string-start "$"))))
           (setq texmathp-why '("Org mode embedded math" . 0))
           t)
          (_ (apply orig-fun args))))))))

;;;###autoload
(defun turn-on-org-cdlatex ()
  "Unconditionally turn on `org-cdlatex-mode'."
  (org-cdlatex-mode 1))

(defun org-try-cdlatex-tab ()
  "Check if it makes sense to execute `cdlatex-tab', and do it if yes.
It makes sense to do so if `org-cdlatex-mode' is active and if the cursor is
  - inside a LaTeX fragment, or
  - after the first word in a line, where an abbreviation expansion could
    insert a LaTeX environment."
  (when org-cdlatex-mode
    (cond
     ;; Before any word on the line: No expansion possible.
     ((save-excursion (skip-chars-backward " \t") (bolp)) nil)
     ;; Just after first word on the line: Expand it.  Make sure it
     ;; cannot happen on headlines, though.
     ((save-excursion
	(skip-chars-backward "a-zA-Z0-9*")
	(skip-chars-backward " \t")
	(and (bolp) (not (org-at-heading-p))))
      (cdlatex-tab) t)
     ((org-inside-LaTeX-fragment-p) (cdlatex-tab) t))))

(defun org-cdlatex-underscore-caret (&optional _arg)
  "Execute `cdlatex-sub-superscript' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P" org-mode)
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively #'cdlatex-sub-superscript)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defun org-cdlatex-math-modify (&optional _arg)
  "Execute `cdlatex-math-modify' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P" org-mode)
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively #'cdlatex-math-modify)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defun org-cdlatex-environment-indent (&optional environment item)
  "Execute `cdlatex-environment' and indent the inserted environment.

ENVIRONMENT and ITEM are passed to `cdlatex-environment'.

The inserted environment is indented to current indentation
unless point is at the beginning of the line, in which the
environment remains unintended."
  (interactive nil org-mode)
  ;; cdlatex-environment always return nil.  Therefore, capture output
  ;; first and determine if an environment was selected.
  (let* ((beg (point-marker))
	 (end (copy-marker (point) t))
	 (inserted (progn
		     (ignore-errors (cdlatex-environment environment item))
		     (< beg end)))
	 ;; Figure out how many lines to move forward after the
	 ;; environment has been inserted.
	 (lines (when inserted
		  (save-excursion
		    (- (cl-loop while (< beg (point))
				with x = 0
				do (forward-line -1)
				(cl-incf x)
				finally return x)
		       (if (progn (goto-char beg)
				  (and (progn (skip-chars-forward " \t") (eolp))
				       (progn (skip-chars-backward " \t") (bolp))))
			   1 0)))))
	 (env (org-trim (delete-and-extract-region beg end))))
    (when inserted
      ;; Get indentation of next line unless at column 0.
      (let ((ind (if (bolp) 0
		   (save-excursion
		     (org-return t)
		     (prog1 (current-indentation)
		       (when (progn (skip-chars-forward " \t") (eolp))
			 (delete-region beg (point)))))))
	    (bol (progn (skip-chars-backward " \t") (bolp))))
	;; Insert a newline before environment unless at column zero
	;; to "escape" the current line.  Insert a newline if
	;; something is one the same line as \end{ENVIRONMENT}.
	(insert
	 (concat (unless bol "\n") env
		 (when (and (skip-chars-forward " \t") (not (eolp))) "\n")))
	(unless (zerop ind)
	  (save-excursion
	    (goto-char beg)
	    (while (< (point) end)
	      (unless (eolp) (indent-line-to ind))
	      (forward-line))))
	(goto-char beg)
	(forward-line lines)
	(indent-line-to ind)))
    (set-marker beg nil)
    (set-marker end nil)))

(provide 'org-cdlatex)

;;; org-cdlatex.el ends here


