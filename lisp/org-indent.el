;;; org-indent.el --- Dynamic indentation for  Org-mode
;; Copyright (C) 2009-2011 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This is an implementation of dynamic virtual indentation.  It works
;; by adding text properties to a buffer to make sure lines are
;; indented according to outline structure.

;;; Code:

(require 'org-macs)
(require 'org-compat)
(require 'org)

(eval-when-compile
  (require 'cl))

(declare-function org-inlinetask-get-task-level "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function org-inlinetask-outline-regexp "org-inlinetask" ())

(defgroup org-indent nil
  "Options concerning dynamic virtual outline indentation."
  :tag "Org Indent"
  :group 'org)

(defconst org-indent-max 40
  "Maximum indentation in characters.")
(defconst org-indent-max-levels 40
  "Maximum indentation in characters.")

(defvar org-indent-strings nil
  "Vector with all indentation strings.
It will be set in `org-indent-initialize'.")
(defvar org-indent-stars nil
  "Vector with all indentation star strings.
It will be set in `org-indent-initialize'.")
(defvar org-hide-leading-stars-before-indent-mode nil
  "Used locally.")

(defcustom org-indent-boundary-char ?\   ; comment to protect space char
  "The end of the virtual indentation strings, a single-character string.
The default is just a space, but if you wish, you can use \"|\" or so.
This can be useful on a terminal window - under a windowing system,
it may be prettier to customize the org-indent face."
  :group 'org-indent
  :set (lambda (var val)
	 (set var val)
	 (and org-indent-strings (org-indent-initialize)))
  :type 'character)

(defcustom org-indent-mode-turns-off-org-adapt-indentation t
  "Non-nil means setting the variable `org-indent-mode' will \
turn off indentation adaptation.
For details see the variable `org-adapt-indentation'."
  :group 'org-indent
  :type 'boolean)

(defcustom org-indent-mode-turns-on-hiding-stars t
  "Non-nil means setting the variable `org-indent-mode' will \
turn on `org-hide-leading-stars'."
  :group 'org-indent
  :type 'boolean)

(defcustom org-indent-indentation-per-level 2
  "Indentation per level in number of characters."
  :group 'org-indent
  :type 'integer)

(defcustom org-indent-fix-section-after-idle-time 0.2
  "Seconds of idle time before fixing virtual indentation of section.
The hooking-in of virtual indentation is not yet perfect.  Occasionally,
a change does not trigger to proper change of indentation.  For this we
have a timer action that fixes indentation in the current section after
a short amount idle time.  If we ever get the integration to work perfectly,
this variable can be set to nil to get rid of the timer."
  :group 'org-indent
  :type '(choice
	  (const "Do not install idle timer" nil)
	  (number :tag "Idle time")))

(defun org-indent-initialize ()
  "Initialize the indentation strings and set the idle timer."
  ;; We use an idle timer to "repair" the current section, because the
  ;; redisplay seems to have some problems.
  (unless org-indent-strings
    (when org-indent-fix-section-after-idle-time
      (run-with-idle-timer
       org-indent-fix-section-after-idle-time
       t 'org-indent-refresh-view)))
  ;; Initialize the indentation and star vectors
  (setq org-indent-strings (make-vector (1+ org-indent-max) nil))
  (setq org-indent-stars (make-vector (1+ org-indent-max) nil))
  (aset org-indent-strings 0 nil)
  (aset org-indent-stars 0 nil)
  (loop for i from 1 to org-indent-max do
	(aset org-indent-strings i
	      (org-add-props
		  (concat (make-string (1- i) ?\ )
			  (char-to-string org-indent-boundary-char))
		  nil 'face 'org-indent)))
  (loop for i from 1 to org-indent-max-levels do
	(aset org-indent-stars i
	      (org-add-props (make-string i ?*)
		  nil 'face 'org-hide))))

;;;###autoload
(define-minor-mode org-indent-mode
  "When active, indent text according to outline structure.

Internally this works by adding `line-prefix' and `wrap-prefix'
properties to all lines. These properties are updated locally in idle
time."
  nil " Ind" nil
  (cond
   ((org-bound-and-true-p org-inhibit-startup)
    (setq org-indent-mode nil))
   ((and org-indent-mode (featurep 'xemacs))
    (message "org-indent-mode does not work in XEmacs - refusing to turn it on")
    (setq org-indent-mode nil))
   ((and org-indent-mode
	 (not (org-version-check "23.1.50" "Org Indent mode" :predicate)))
    (message "org-indent-mode can crash Emacs 23.1 - refusing to turn it on!")
    (ding)
    (sit-for 1)
    (setq org-indent-mode nil))
   (org-indent-mode
    ;; mode was turned on.
    (org-set-local 'indent-tabs-mode nil)
    (or org-indent-strings (org-indent-initialize))
    (org-indent-indent-buffer)
    (when org-indent-mode-turns-off-org-adapt-indentation
      (org-set-local 'org-adapt-indentation nil))
    (when org-indent-mode-turns-on-hiding-stars
      (org-set-local 'org-hide-leading-stars-before-indent-mode
		     org-hide-leading-stars)
      (org-set-local 'org-hide-leading-stars t))
    (make-local-variable 'buffer-substring-filters)
    (add-to-list 'buffer-substring-filters
		 'org-indent-remove-properties-from-string)
    (org-add-hook 'org-after-demote-entry-hook
		  'org-indent-refresh-subtree nil 'local)
    (org-add-hook 'org-after-promote-entry-hook
		  'org-indent-refresh-subtree nil 'local)
    (and font-lock-mode (org-restart-font-lock)))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (save-excursion
      (save-restriction
	(org-indent-remove-properties (point-min) (point-max))
	(kill-local-variable 'org-adapt-indentation)
	(when (boundp 'org-hide-leading-stars-before-indent-mode)
	  (org-set-local 'org-hide-leading-stars
			 org-hide-leading-stars-before-indent-mode))
	(setq buffer-substring-filters
	      (delq 'org-indent-remove-properties-from-string
		    buffer-substring-filters))
	(remove-hook 'org-after-promote-entry-hook
		     'org-indent-refresh-subtree 'local)
	(remove-hook 'org-after-demote-entry-hook
		     'org-indent-refresh-subtree 'local)
	(and font-lock-mode (org-restart-font-lock))
	(redraw-display))))))


(defface org-indent
  (org-compatible-face nil nil)
  "Face for outline indentation.
The default is to make it look like whitespace.  But you may find it
useful to make it ever so slightly different."
  :group 'org-faces)

(defun org-indent-indent-buffer ()
  "Add indentation properties for the whole buffer."
  (interactive)
  (when org-indent-mode
    (save-excursion
      (save-restriction
	(widen)
	(org-indent-remove-properties (point-min) (point-max))
	(org-indent-add-properties (point-min) (point-max))))))

(defun org-indent-remove-properties (beg end)
  "Remove indentations between BEG and END."
  (let ((inhibit-modification-hooks t))
    (with-silent-modifications
      (remove-text-properties beg end '(line-prefix nil wrap-prefix nil)))))

(defun org-indent-remove-properties-from-string (string)
  "Remove indentation properties from STRING."
  (remove-text-properties 0 (length string)
			  '(line-prefix nil wrap-prefix nil) string)
  string)

(defun org-indent-add-properties (beg end)
  "Add indentation properties between BEG and END."
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    ;; 1. Initialize prefix at BEG. This is done by storing two
    ;;    variables: INLINE-PF and PF, representing respectively
    ;;    current `line-prefix' when line is inside an inline task or
    ;;    not.
    (let* ((inhibit-modification-hooks t)
	   (case-fold-search t)
	   (limited-re (org-get-limited-outline-regexp))
	   (inline-end-re (and (featurep 'org-inlinetask)
			       (concat (org-inlinetask-outline-regexp)
				       "end[ \t]*$")))
	   (pf (org-with-limited-levels
		(save-excursion
		  (and (ignore-errors (org-back-to-heading t))
		       (looking-at org-outline-regexp)
		       (aref org-indent-strings
			     (- (match-end 0) (match-beginning 0)))))))
	   (pf-inline (and inline-end-re
			   (org-inlinetask-in-task-p)
			   (aref org-indent-strings
				 (1+ (org-inlinetask-get-task-level))))))
      ;; 2. For each line, `line-prefix' is based on the value of the
      ;;    previous `line-prefix' (stored in PF and INLINE-PF).
      ;;    `wrap-prefix' computation is done with the current
      ;;    `line-prefix' value.
      (with-silent-modifications
	(while (< (point) end)
	  (cond
	   ;; Empty line: do nothing.
	   ((eolp) (forward-line 1))
	   ;; List item: `line-prefix' doesn't change, but
	   ;; `wrap-prefix' is set where body starts.
	   ((org-at-item-p)
	    (let* ((line (or pf-inline pf))
		   (wrap (aref org-indent-strings
			       (+ (org-list-item-body-column (point))
				  (length line)))))
	      (add-text-properties (point) (point-at-eol)
				   `(line-prefix ,line wrap-prefix ,wrap))
	      (forward-line 1)))
	   ;; Normal line: `line-prefix' doesn't change, but
	   ;; `wrap-prefix' also takes into account indentation.
	   ((not (looking-at org-outline-regexp))
	    (let* ((line (or pf-inline pf))
		   (wrap (aref org-indent-strings
			       (+ (length line) (org-get-indentation)))))
	      (add-text-properties (point) (point-at-eol)
				   `(line-prefix ,line wrap-prefix ,wrap))
	      (forward-line 1)))
	   ;; Headline: `line-prefix' is nil, `wrap-prefix' is set
	   ;; where headline starts and its value becomes a reference
	   ;; for following lines.
	   ((looking-at limited-re)
	    (let ((wrap (aref org-indent-strings
			      (- (match-end 0) (match-beginning 0)))))
	      (add-text-properties (point) (point-at-eol)
				   `(line-prefix nil wrap-prefix ,wrap))
	      (setq pf wrap)
	      (forward-line 1)))
	   ;; End of inline task: both `line-prefix' and `wrap-prefix'
	   ;; are nil. PF-INLINE is also nil, as following lines are
	   ;; out of the inline task.
	   ((looking-at inline-end-re)
	    (add-text-properties (point) (point-at-eol)
				 '(line-prefix nil wrap-prefix nil))
	    (setq pf-inline nil)
	    (forward-line 1))
	   ;; Beginnig of inline task: determine if the tasks contains
	   ;; text (and set PF-INLINE accordingly) or is only one line
	   ;; long by looking the status of the following line. In any
	   ;; case, `line-prefix' is nil and `wrap-prefix' is set
	   ;; where headline starts.
	   (t
	    (let ((wrap (progn
			  (looking-at org-outline-regexp)
			  (aref org-indent-strings
				(- (match-end 0) (match-beginning 0))))))
	      (add-text-properties (point) (point-at-eol)
				   `(line-prefix nil wrap-prefix ,wrap))
	      (forward-line 1)
	      (setq pf-inline (and (not (eobp))
				   (org-inlinetask-in-task-p)
				   wrap))))))))))

(defun org-indent-refresh-view (&rest ignore)
  "Refresh indentation properties in the visible portion of buffer.
IGNORE all arguments that might be passed to the function."
  (interactive)
  (when org-indent-mode
    (save-excursion
      (let ((beg (window-start))
	    (end (window-end nil t)))
	(org-indent-add-properties beg end)))))

(defun org-indent-refresh-subtree ()
  "Refresh indentation properties in the current outline subtree.
Point is assumed to be at an headline."
  (interactive)
  (when org-indent-mode
    (save-excursion
      (let ((beg (point-at-bol))
	    (end (save-excursion (org-end-of-subtree t t))))
	(org-indent-add-properties beg end)))))

(defun org-indent-refresh-buffer ()
  "Refresh indentation properties in the whole buffer."
  (interactive)
  (when org-indent-mode
    (org-indent-mode -1)
    (org-indent-mode 1)))

(provide 'org-indent)

;;; org-indent.el ends here
