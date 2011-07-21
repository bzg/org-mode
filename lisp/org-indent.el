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
(declare-function org-list-item-body-column "org-list" (item))

(defgroup org-indent nil
  "Options concerning dynamic virtual outline indentation."
  :tag "Org Indent"
  :group 'org)

(defconst org-indent-max 40
  "Maximum indentation in characters.")
(defconst org-indent-max-levels 20
  "Maximum added level through virtual indentation, in
characters.

It is computed by multiplying `org-indent-indentation-per-level'
minus one by actual level of the headline minus one.")

(defvar org-indent-strings nil
  "Vector with all indentation strings.
It will be set in `org-indent-initialize'.")
(defvar org-indent-stars nil
  "Vector with all indentation star strings.
It will be set in `org-indent-initialize'.")
(defvar org-hide-leading-stars-before-indent-mode nil
  "Used locally.")
(defvar org-indent-deleted-headline-flag nil
  "Non nil if the last deletion acted on an headline.
It is modified by `org-indent-notify-deleted-headline'.")


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

(defun org-indent-initialize ()
  "Initialize the indentation strings."
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
properties, after each buffer modifiation, on the modified zone."
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
    (org-add-hook 'after-change-functions 'org-indent-refresh-maybe nil 'local)
    (org-add-hook 'before-change-functions
		  'org-indent-notify-deleted-headline nil 'local)
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
	(remove-hook 'after-change-functions 'org-indent-refresh-maybe 'local)
	(remove-hook 'before-change-functions
		     'org-indent-notify-deleted-headline 'local)
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
  (if (not (org-mode-p))
      (error "Buffer major mode must be Org")
    (message "Setting buffer indentation. It may take a few seconds...")
    (org-with-wide-buffer
     (with-silent-modifications
       (org-indent-remove-properties (point-min) (point-max))
       (org-indent-add-properties (point-min) (point-max))))
    (message "Indentation of buffer set.")))

(defsubst org-indent-remove-properties (beg end)
  "Remove indentations between BEG and END."
  (remove-text-properties beg end '(line-prefix nil wrap-prefix nil)))

(defun org-indent-remove-properties-from-string (string)
  "Remove indentation properties from STRING."
  (remove-text-properties 0 (length string)
			  '(line-prefix nil wrap-prefix nil) string)
  string)

(defun org-indent-add-properties (beg end)
  "Add indentation properties between BEG and END."
  (org-with-wide-buffer
    (goto-char beg)
    (beginning-of-line)
    ;; 1. Initialize prefix at BEG. This is done by storing two
    ;;    variables: INLINE-PF and PF, representing respectively
    ;;    length of current `line-prefix' when line is inside an
    ;;    inline task or not.
    (let* ((case-fold-search t)
	   (limited-re (org-get-limited-outline-regexp))
	   (added-ind-per-lvl (1- org-indent-indentation-per-level))
	   (pf (let ((outline-regexp limited-re))
		 (save-excursion
		   (and (ignore-errors (org-back-to-heading t))
			(looking-at org-outline-regexp)
			(+ (* org-indent-indentation-per-level
			      (- (match-end 0) (match-beginning 0) 2)) 2)))))
	   (pf-inline (and (featurep 'org-inlinetask)
			   (org-inlinetask-in-task-p)
			   (+ (* org-indent-indentation-per-level
				 (1- (org-inlinetask-get-task-level))) 2)))
	   (set-prop-and-move
	    (function
	     ;; Set prefix properties `line-prefix' and `wrap-prefix'
	     ;; in current line to, respectively, length L and W and
	     ;; move forward. If H is non-nil, `line-prefix' will be
	     ;; starred. Assume point is at bol.
	     (lambda (l w h)
	       (let ((line (if h (aref org-indent-stars
				       (min l org-indent-max-levels))
			     (aref org-indent-strings
				   (min l org-indent-max))))
		     (wrap (aref org-indent-strings (min w org-indent-max))))
		(add-text-properties (point) (point-at-eol)
				     `(line-prefix ,line wrap-prefix ,wrap)))
	       (forward-line 1)))))
      ;; 2. For each line, set `line-prefix' and `wrap-prefix'
      ;;    properties depending on the type of line (headline, inline
      ;;    task, item or other).
      (while (< (point) end)
	(cond
	 ;; Empty line: do nothing.
	 ((eolp) (forward-line 1))
	 ;; Headline or inline task.
	 ((looking-at "\\*+ ")
	  (let* ((nstars (- (match-end 0) (match-beginning 0) 1))
		 (line (* added-ind-per-lvl (1- nstars)))
		 (wrap (+ line (1+ nstars))))
	    (cond
	     ;; Headline: new value for PF.
	     ((looking-at limited-re)
	      (funcall set-prop-and-move line wrap t)
	      (setq pf wrap))
	     ;; End of inline task: PF-INLINE is now nil.
	     ((looking-at "\\*+ end[ \t]*$")
	      (funcall set-prop-and-move line wrap t)
	      (setq pf-inline nil))
	     ;; Start of inline task. Determine if it contains text,
	     ;; or is only one line long. Set PF-INLINE accordingly.
	     (t (funcall set-prop-and-move line wrap t)
		(setq pf-inline (and (org-inlinetask-in-task-p) wrap))))))
	 ;; List item: `wrap-prefix' is set where body starts.
	 ((org-at-item-p)
	  (let* ((line (or pf-inline pf 0))
		 (wrap (+ (org-list-item-body-column (point)) line)))
	    (funcall set-prop-and-move line wrap nil)))
	 ;; Normal line: use PF-INLINE, PF or nil as prefixes.
	 (t (let* ((line (or pf-inline pf 0))
		   (wrap (+ line (org-get-indentation))))
	      (funcall set-prop-and-move line wrap nil))))))))

(defun org-indent-notify-deleted-headline (beg end)
  "Set `org-indent-deleted-headline-flag' depending on the current command.

BEG and END are the positions of the beginning and end of the
range of deleted text.

This function is meant to be called by `before-change-functions'.
Flag will be non-nil if command is going to delete an headline."
  (setq org-indent-deleted-headline-flag
	(and (/= beg end)
	     (save-excursion
	       (goto-char beg)
	       (save-match-data
		 (re-search-forward org-outline-regexp-bol end t))))))

(defun org-indent-refresh-maybe (beg end dummy)
  "Refresh indentation properties in an adequate portion of buffer.
BEG and END are the positions of the beginning and end of the
range of inserted text.  DUMMY is an unused argument.

This function is meant to be called by `after-change-functions'."
  (when org-indent-mode
    (save-match-data
      (cond
       ;; An headline was deleted.
       (org-indent-deleted-headline-flag
	(setq org-indent-deleted-headline-flag nil)
	(let ((end (save-excursion (outline-next-heading) (point))))
	  (org-indent-remove-properties beg end)
	  (org-indent-add-properties beg end)))
       ;; An headline was inserted.
       ((and (/= beg end)
	     (save-excursion
	       (goto-char beg)
	       (re-search-forward org-outline-regexp-bol end t)))
	(let ((end (save-excursion
		     (goto-char end) (outline-next-heading) (point))))
	  (org-indent-remove-properties beg end)
	  (org-indent-add-properties beg end)))
       ;; At an headline, modifying stars.
       ((save-excursion (goto-char beg)
			(and (org-at-heading-p) (< beg (match-end 0))))
	(let ((beg (point-at-bol))
	      (end (save-excursion (outline-next-heading) (point))))
	  (org-indent-remove-properties beg end)
	  (org-indent-add-properties beg end)))
       ;; Else, refresh properties of modified area.
       (t (org-indent-add-properties beg end))))))

(provide 'org-indent)

;;; org-indent.el ends here
