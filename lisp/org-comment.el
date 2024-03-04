;;; org-comment.el --- Org commenting support                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Author: Visuwesh <visuweshm@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library provides Org mode integration with Emacs comment handling.

;;; Code:

;;; Comments

;; Org comments syntax is quite complex.  It requires the entire line
;; to be just a comment.  Also, even with the right syntax at the
;; beginning of line, some elements (e.g., verse-block or
;; example-block) don't accept comments.  Usual Emacs comment commands
;; cannot cope with those requirements.  Therefore, Org replaces them.

;; Org still relies on 'comment-dwim', but cannot trust
;; 'comment-only-p'.  So, 'comment-region-function' and
;; 'uncomment-region-function' both point
;; to 'org-comment-or-uncomment-region'.  Eventually,
;; 'org-insert-comment' takes care of insertion of comments at the
;; beginning of line.

;; 'org-setup-comments-handling' install comments related variables
;; during 'org-mode' initialization.

(require 'org-element)
(eval-when-compile (require 'org-src)) ; `org-babel-do-in-edit-buffer'
(defvar org-complex-heading-regexp)

(defun org-setup-comments-handling ()
  (interactive)
  (setq-local comment-use-syntax nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "^\\s-*#\\(?: \\|$\\)")
  (setq-local comment-insert-comment-function 'org-insert-comment)
  (setq-local comment-region-function 'org-comment-or-uncomment-region)
  (setq-local uncomment-region-function 'org-comment-or-uncomment-region))

(defun org-insert-comment ()
  "Insert an empty comment above current line.
If the line is empty, insert comment at its beginning.  When
point is within a source block, comment according to the related
major mode."
  (declare-function org-indent-line "org-indent-static" ())
  (declare-function org-edit-src-exit "org-src")
  (declare-function org-edit-src-code "org-src")
  (declare-function org-babel-where-is-src-block-head "org-src")
  (if (let ((element (org-element-at-point)))
	(and (org-element-type-p element 'src-block)
	     (< (save-excursion
		  (goto-char (org-element-post-affiliated element))
		  (line-end-position))
		(point))
	     (> (save-excursion
		  (goto-char (org-element-end element))
		  (skip-chars-backward " \r\t\n")
		  (line-beginning-position))
		(point))))
      (progn
        (require 'org-src)
        (org-babel-do-in-edit-buffer (call-interactively #'comment-dwim)))
    (forward-line 0)
    (if (looking-at "\\s-*$") (delete-region (point) (line-end-position))
      (open-line 1))
    (require 'org-indent-static)
    (org-indent-line)
    (insert "# ")))

(defvar comment-empty-lines)		; From newcomment.el.
(defun org-comment-or-uncomment-region (beg end &rest _)
  "Comment or uncomment each non-blank line in the region.
Uncomment each non-blank line between BEG and END if it only
contains commented lines.  Otherwise, comment them.  If region is
strictly within a source block, use appropriate comment syntax."
  (if (let ((element (org-element-at-point)))
	(and (org-element-type-p element 'src-block)
	     (< (save-excursion
		  (goto-char (org-element-post-affiliated element))
		  (line-end-position))
		beg)
	     (>= (save-excursion
		  (goto-char (org-element-end element))
		  (skip-chars-backward " \r\t\n")
		  (line-beginning-position))
		end)))
      ;; Translate region boundaries for the Org buffer to the source
      ;; buffer.
      (let (src-end)
        (save-excursion
          (goto-char end)
          (org-babel-do-in-edit-buffer
           (setq src-end (point))))
	(save-excursion
	  (goto-char beg)
	  (org-babel-do-in-edit-buffer
	   (comment-or-uncomment-region (point) src-end))))
    (save-restriction
      ;; Restrict region
      (narrow-to-region (save-excursion (goto-char beg)
					(skip-chars-forward " \r\t\n" end)
					(line-beginning-position))
			(save-excursion (goto-char end)
					(skip-chars-backward " \r\t\n" beg)
					(line-end-position)))
      (let ((uncommentp
	     ;; UNCOMMENTP is non-nil when every non blank line between
	     ;; BEG and END is a comment.
	     (save-excursion
	       (goto-char (point-min))
	       (while (and (not (eobp))
			   (let ((element (org-element-at-point)))
			     (and (org-element-type-p element 'comment)
				  (goto-char (min (point-max)
						  (org-element-property
						   :end element)))))))
	       (eobp))))
	(if uncommentp
	    ;; Only blank lines and comments in region: uncomment it.
	    (save-excursion
	      (goto-char (point-min))
	      (while (not (eobp))
		(when (looking-at "[ \t]*\\(#\\(?: \\|$\\)\\)")
		  (replace-match "" nil nil nil 1))
		(forward-line)))
	  ;; Comment each line in region.
	  (let ((min-indent (point-max)))
	    ;; First find the minimum indentation across all lines.
	    (save-excursion
	      (goto-char (point-min))
	      (while (and (not (eobp)) (not (zerop min-indent)))
		(unless (looking-at "[ \t]*$")
		  (setq min-indent (min min-indent (org-current-text-indentation))))
		(forward-line)))
	    ;; Then loop over all lines.
	    (save-excursion
	      (goto-char (point-min))
	      (while (not (eobp))
		(unless (and (not comment-empty-lines) (looking-at "[ \t]*$"))
		  ;; Don't get fooled by invisible text (e.g. link path)
		  ;; when moving to column MIN-INDENT.
		  (let ((buffer-invisibility-spec nil))
		    (org-move-to-column min-indent t))
		  (insert comment-start))
		(forward-line)))))))))

(defun org-toggle-comment ()
  "Change the COMMENT state of an entry."
  (interactive nil org-mode)
  (declare-function org-back-to-heading "org-move" (&optional invisible-ok))
  (save-excursion
    (require 'org-move)
    (org-back-to-heading)
    (let ((case-fold-search nil))
      (looking-at org-complex-heading-regexp))
    (goto-char (or (match-end 3) (match-end 2) (match-end 1)))
    (skip-chars-forward " \t")
    (unless (memq (char-before) '(?\s ?\t)) (insert " "))
    (if (org-in-commented-heading-p t)
	(delete-region (point)
		       (progn (search-forward " " (line-end-position) 'move)
			      (skip-chars-forward " \t")
			      (point)))
      (insert org-comment-string)
      (unless (eolp) (insert " ")))))

(defun org-comment-dwim (_arg)
  "Call the comment command you mean.
Call `org-toggle-comment' if on a heading, otherwise call
`comment-dwim'."
  (interactive "*P" org-mode)
  (cond ((org-at-heading-p)
	 (call-interactively #'org-toggle-comment))
	(t (call-interactively #'comment-dwim))))

(provide 'org-comment)

;;; org-comment.el ends here
