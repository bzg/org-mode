;;; org-fill.el --- Org filling support                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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

;; This library provides Org mode support for Emacs fill API.
;; We use our own fill-paragraph and auto-fill functions.

;; `org-fill-paragraph' relies on adaptive filling and context
;; checking.  Appropriate `fill-prefix' is computed with
;; `org-adaptive-fill-function'.

;; `org-auto-fill-function' takes care of auto-filling.  It calls
;; `do-auto-fill' only on valid areas with `fill-prefix' shadowed with
;; `org-adaptive-fill-function' value.  Internally,
;; `org-comment-line-break-function' breaks the line.

;; `org-setup-filling' installs filling and auto-filling related
;; variables during `org-mode' initialization.

;;; Code:

(require 'org-element)
(require 'org-move)
(declare-function org-at-timestamp-p "org" (&optional extended))
(defvar org-mode-transpose-word-syntax-table)

(defun org-setup-filling ()
  ;; Prevent auto-fill from inserting unwanted new items.
  (setq-local fill-nobreak-predicate
              (org-uniquify
               (append fill-nobreak-predicate
                       '(org-fill-line-break-nobreak-p
                         org-fill-n-macro-as-item-nobreak-p
                         org-fill-paragraph-with-timestamp-nobreak-p))))
  (let ((paragraph-ending (substring org-element-paragraph-separate 1)))
    (setq-local paragraph-start paragraph-ending)
    (setq-local paragraph-separate paragraph-ending))
  (setq-local fill-paragraph-function 'org-fill-paragraph)
  (setq-local fill-forward-paragraph-function
              (lambda (&optional arg)
                (let ((org--single-lines-list-is-paragraph nil))
                  (org-forward-paragraph arg))))
  (setq-local auto-fill-inhibit-regexp nil)
  (setq-local adaptive-fill-function 'org-adaptive-fill-function)
  (setq-local normal-auto-fill-function 'org-auto-fill-function)
  (setq-local comment-line-break-function 'org-comment-line-break-function))

(defun org-fill-line-break-nobreak-p ()
  "Non-nil when a new line at point would create an Org line break."
  (save-excursion
    (skip-chars-backward " \t")
    (skip-chars-backward "\\\\")
    (looking-at "\\\\\\\\\\($\\|[^\\]\\)")))

(defun org-fill-paragraph-with-timestamp-nobreak-p ()
  "Non-nil when a new line at point would split a timestamp."
  (and (org-at-timestamp-p 'lax)
       (not (looking-at org-ts-regexp-both))))

(defun org-fill-n-macro-as-item-nobreak-p ()
  "Non-nil when a new line at point would create a new list."
  ;; During export, a "n" macro followed by a dot or a closing
  ;; parenthesis can end up being parsed as a new list item.
  (looking-at-p "[ \t]*{{{n\\(?:([^\n)]*)\\)?}}}[.)]\\(?:$\\| \\)"))

(defun org-adaptive-fill-paragraph-function ()
  "Compute a fill prefix for the current line in paragraph.
Return fill prefix, as a string, or nil if current line isn't meant to
be filled.  Use `adaptive-fill-regexp', but ignore Org markup at the
beginning of the line."
  (let ((context (org-element-context)))
    ;; Skip over markup symbols, if any.
    (defvar org-element-all-objects) ; org-element.el
    (when (and (org-element-type-p context org-element-all-objects)
               (org-element-contents-begin context)
               (> (org-element-contents-begin context)
                  (org-element-begin context)))
      (goto-char (org-element-contents-begin context)))
    ;; Delegate to `fill-match-adaptive-prefix' to handle
    ;; `adaptive-fill-regexp'.  *Assume* that
    ;; `fill-match-adaptive-prefix' matches at point.
    (let (adaptive-fill-function) (fill-match-adaptive-prefix))))

(defun org-adaptive-fill-function ()
  "Compute a fill prefix for the current line.
Return fill prefix, as a string, or nil if current line isn't
meant to be filled.  For convenience, if `adaptive-fill-regexp'
matches in paragraphs or comments, use it."
  (org-with-wide-buffer
   (unless (org-at-heading-p)
     (let* ((p (line-beginning-position))
	    (element (save-excursion
		       (forward-line 0)
		       (org-element-at-point)))
	    (type (org-element-type element))
	    (post-affiliated (org-element-post-affiliated element)))
       (unless (or (not element) (< p post-affiliated))
	 (cl-case type
	   (comment
	    (save-excursion
	      (forward-line 0)
	      (looking-at "[ \t]*")
	      (concat (match-string 0) "# ")))
	   (footnote-definition "")
	   ((item plain-list)
	    (make-string (org-list-item-body-column post-affiliated) ?\s))
	   (paragraph
	    ;; Fill prefix is usually the same as the current line,
	    ;; unless the paragraph is at the beginning of an item.
	    (let ((parent (org-element-parent element)))
	      (save-excursion
		(forward-line 0)
		(cond ((org-element-type-p parent 'item)
		       (make-string (org-list-item-body-column
				     (org-element-begin parent))
				    ?\s))
		      ((and adaptive-fill-regexp
			    ;; Locally override
			    ;; `adaptive-fill-function' to let
			    ;; `fill-context-prefix' handle
			    ;; `adaptive-fill-regexp' variable, but
                            ;; ignore Org markup, like "*" at bol.
			    (let ((adaptive-fill-function #'org-adaptive-fill-paragraph-function))
			      (fill-context-prefix
			       post-affiliated
			       (org-element-end element)))))
		      ((looking-at "[ \t]+") (match-string 0))
		      (t  "")))))
	   (comment-block
	    ;; Only fill contents if P is within block boundaries.
	    (let* ((cbeg (save-excursion (goto-char post-affiliated)
					 (forward-line)
					 (point)))
		   (cend (save-excursion
			   (goto-char (org-element-end element))
			   (skip-chars-backward " \r\t\n")
			   (line-beginning-position))))
	      (when (and (>= p cbeg) (< p cend))
		(if (save-excursion (forward-line 0) (looking-at "[ \t]+"))
		    (match-string 0)
		  ""))))))))))

(eval-when-compile (require 'ob-core)) ; `org-babel-do-in-edit-buffer'

(defun org-fill-element (&optional justify)
  "Fill element at point, when applicable.

This function only applies to comment blocks, comments, example
blocks and paragraphs.  Also, as a special case, re-align table
when point is at one.

If JUSTIFY is non-nil (interactively, with prefix argument),
justify as well.  If `sentence-end-double-space' is non-nil, then
period followed by one space does not end a sentence, so don't
break a line there.  The variable `fill-column' controls the
width for filling.

For convenience, when point is at a plain list, an item or
a footnote definition, try to fill the first paragraph within."
  (declare-function org-table-align "org-table" ())
  (declare-function org-edit-src-exit "org-src" ())
  (declare-function org-edit-src-code "org-src" (&optional code edit-buffer-name))
  (declare-function org-babel-where-is-src-block-head "ob-core" (&optional src-block))
  (defvar org-src-window-setup)
  (org-with-syntax-table org-mode-transpose-word-syntax-table
    ;; Move to end of line in order to get the first paragraph within
    ;; a plain list or a footnote definition.
    (let ((element (save-excursion (end-of-line) (org-element-at-point))))
      ;; First check if point is in a blank line at the beginning of
      ;; the buffer.  In that case, ignore filling.
      (cl-case (org-element-type element)
	;; Use major mode filling function is source blocks.
        (src-block
         (let ((regionp (region-active-p)))
           (require 'org-fill)
           (org-babel-do-in-edit-buffer
            ;; `org-babel-do-in-edit-buffer' will preserve region if it
            ;; is within src block contents.  Otherwise, the region
            ;; crosses src block boundaries.  We re-fill the whole src
            ;; block in such scenario.
            (when (and regionp (not (region-active-p)))
              (push-mark (point-min))
              (goto-char (point-max))
              (setq mark-active t))
            (funcall-interactively #'fill-paragraph justify 'region))))
	;; Align Org tables, leave table.el tables as-is.
	(table-row
         (require 'org-table)
         (org-table-align)
         t)
	(table
	 (when (eq (org-element-property :type element) 'org)
	   (save-excursion
	     (goto-char (org-element-post-affiliated element))
             (require 'org-table)
	     (org-table-align)))
	 t)
	(paragraph
	 ;; Paragraphs may contain `line-break' type objects.
	 (let ((beg (max (point-min)
			 (org-element-contents-begin element)))
	       (end (min (point-max)
			 (org-element-contents-end element))))
	   ;; Do nothing if point is at an affiliated keyword.
	   (if (< (line-end-position) beg) t
	     ;; Fill paragraph, taking line breaks into account.
	     (save-excursion
	       (goto-char beg)
	       (let ((cuts (list beg)))
		 (while (re-search-forward "\\\\\\\\[ \t]*\n" end t)
		   (when (org-element-type-p
			  (save-excursion (backward-char)
					  (org-element-context))
                          'line-break)
		     (push (point) cuts)))
		 (dolist (c (delq end cuts))
		   (fill-region-as-paragraph c end justify)
		   (setq end c))))
	     t)))
	;; Contents of `comment-block' type elements should be
	;; filled as plain text, but only if point is within block
	;; markers.
	(comment-block
	 (let* ((case-fold-search t)
		(beg (save-excursion
		       (goto-char (org-element-begin element))
		       (re-search-forward "^[ \t]*#\\+begin_comment" nil t)
		       (forward-line)
		       (point)))
		(end (save-excursion
		       (goto-char (org-element-end element))
		       (re-search-backward "^[ \t]*#\\+end_comment" nil t)
		       (line-beginning-position))))
	   (if (or (< (point) beg) (> (point) end)) t
	     (fill-region-as-paragraph
	      (save-excursion (end-of-line)
			      (re-search-backward "^[ \t]*$" beg 'move)
			      (line-beginning-position))
	      (save-excursion (forward-line 0)
			      (re-search-forward "^[ \t]*$" end 'move)
			      (line-beginning-position))
	      justify))))
	;; Fill comments.
	(comment
	 (let ((begin (org-element-post-affiliated element))
	       (end (org-element-end element)))
	   (when (and (>= (point) begin) (<= (point) end))
	     (let ((begin (save-excursion
			    (end-of-line)
			    (if (re-search-backward "^[ \t]*#[ \t]*$" begin t)
				(progn (forward-line) (point))
			      begin)))
		   (end (save-excursion
			  (end-of-line)
			  (if (re-search-forward "^[ \t]*#[ \t]*$" end 'move)
			      (1- (line-beginning-position))
			    (skip-chars-backward " \r\t\n")
			    (line-end-position)))))
	       ;; Do not fill comments when at a blank line.
	       (when (> end begin)
		 (let ((fill-prefix
			(save-excursion
			  (forward-line 0)
			  (looking-at "[ \t]*#")
			  (let ((comment-prefix (match-string 0)))
			    (goto-char (match-end 0))
			    (if (looking-at adaptive-fill-regexp)
				(concat comment-prefix (match-string 0))
			      (concat comment-prefix " "))))))
		   (save-excursion
		     (fill-region-as-paragraph begin end justify))))))
	   t))
	;; Ignore every other element.
	(otherwise t)))))

(defun org-fill-paragraph (&optional justify region)
  "Fill element at point, when applicable.

This function only applies to comment blocks, comments, example
blocks and paragraphs.  Also, as a special case, re-align table
when point is at one.

For convenience, when point is at a plain list, an item or
a footnote definition, try to fill the first paragraph within.

If JUSTIFY is non-nil (interactively, with prefix argument),
justify as well.  If `sentence-end-double-space' is non-nil, then
period followed by one space does not end a sentence, so don't
break a line there.  The variable `fill-column' controls the
width for filling.

The REGION argument is non-nil if called interactively; in that
case, if Transient Mark mode is enabled and the mark is active,
fill each of the elements in the active region, instead of just
filling the current element."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (when current-prefix-arg 'full) t))
               org-mode)
  (let ((hash (and (not (buffer-modified-p))
		   (buffer-hash))))
    (cond
     ((and region transient-mark-mode mark-active
	   (not (eq (region-beginning) (region-end))))
      (let ((origin (point-marker))
	    (start (region-beginning)))
	(unwind-protect
	    (progn
	      (goto-char (region-end))
	      (skip-chars-backward " \t\n")
	      (let ((org--single-lines-list-is-paragraph nil))
                (while (> (point) start)
		  (org-fill-element justify)
		  (org-backward-paragraph)
                  (skip-chars-backward " \t\n"))))
	  (goto-char origin)
	  (set-marker origin nil))))
     (t
      (save-excursion
	(when (org-match-line "[ \t]*$")
	  (skip-chars-forward " \t\n"))
	(org-fill-element justify))))
    ;; If we didn't change anything in the buffer (and the buffer was
    ;; previously unmodified), then flip the modification status back
    ;; to "unchanged".
    (when (and hash (equal hash (buffer-hash)))
      (set-buffer-modified-p nil))
    ;; Return non-nil.
    t))

(defun org-auto-fill-function ()
  "Auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let* ((fill-prefix (org-adaptive-fill-function))
	     ;; Enforce empty fill prefix, if required.  Otherwise, it
	     ;; will be computed again.
	     (adaptive-fill-mode (not (equal fill-prefix ""))))
	(when fill-prefix (do-auto-fill))))))

(defun org-comment-line-break-function (&optional soft)
  "Break line at point and indent, continuing comment if within one.
The inserted newline is marked hard if variable
`use-hard-newlines' is true, unless optional argument SOFT is
non-nil.

This function is a simplified version of `comment-indent-new-line'
that bypasses the complex Emacs machinery dealing with comments.
We instead rely on Org parser, utilizing `org-adaptive-fill-function'"
  (let ((fill-prefix (org-adaptive-fill-function)))
    (if soft (insert-and-inherit ?\n) (newline 1))
    (save-excursion (forward-char -1) (delete-horizontal-space))
    (delete-horizontal-space)
    (indent-to-left-margin)
    (when fill-prefix
      (insert-before-markers-and-inherit fill-prefix))))

(provide 'org-fill)

;;; org-fill.el ends here
