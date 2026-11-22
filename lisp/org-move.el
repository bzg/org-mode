;;; org-move.el --- Org move commands                      -*- lexical-binding: t; -*-

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

;; This library provides commands to move point in Org buffers.

;;; Code:

(require 'org-element)
(require 'org-fold)
(require 'outline)

(defvar org-complex-heading-regexp)
(declare-function org-current-level "org" ())
(defvar org-planning-line-re)
(defvar org-logbook-drawer-re)
(defvar org-drawer-regexp)
(defvar org-keyword-regexp)

(defvaralias 'org-special-ctrl-a 'org-special-ctrl-a/e)
(defcustom org-special-ctrl-a/e nil
  "Non-nil means `C-a' and `C-e' behave specially in headlines and items.

When t, `C-a' will bring back the cursor to the beginning of the
headline text, i.e. after the stars and after a possible TODO
keyword.  In an item, this will be the position after bullet and
check-box, if any.  When the cursor is already at that position,
another `C-a' will bring it to the beginning of the line.

`C-e' will jump to the end of the headline, ignoring the presence
of tags in the headline.  A second `C-e' will then jump to the
true end of the line, after any tags.  This also means that, when
this variable is non-nil, `C-e' also will never jump beyond the
end of the heading of a folded section, i.e. not after the
ellipses.

When set to the symbol `reversed', the first `C-a' or `C-e' works
normally, going to the true line boundary first.  Only a directly
following, identical keypress will bring the cursor to the
special positions.

This may also be a cons cell where the behavior for `C-a' and
`C-e' is set separately."
  :group 'org-edit-structure
  :type '(choice
	  (const :tag "off" nil)
	  (const :tag "on: after stars/bullet and before tags first" t)
	  (const :tag "reversed: true line boundary first" reversed)
	  (cons :tag "Set C-a and C-e separately"
		(choice :tag "Special C-a"
			(const :tag "off" nil)
			(const :tag "on: after  stars/bullet first" t)
			(const :tag "reversed: before stars/bullet first" reversed))
		(choice :tag "Special C-e"
			(const :tag "off" nil)
			(const :tag "on: before tags first" t)
			(const :tag "reversed: after tags first" reversed)))))

(defun org-beginning-of-line (&optional n)
  "Go to the beginning of the current visible line.

If this is a headline, and `org-special-ctrl-a/e' is not nil or
symbol `reversed', on the first attempt move to where the
headline text starts, and only move to beginning of line when the
cursor is already before the start of the text of the headline.

If `org-special-ctrl-a/e' is symbol `reversed' then go to the
start of the text on the second attempt.

With argument N not nil or 1, move forward N - 1 lines first."
  (interactive "^p" org-mode)
  (let ((origin (point))
	(special (pcase org-special-ctrl-a/e
		   (`(,C-a . ,_) C-a) (_ org-special-ctrl-a/e)))
	deactivate-mark)
    ;; First move to a visible line.
    (if (bound-and-true-p visual-line-mode)
	(beginning-of-visual-line n)
      (move-beginning-of-line n)
      ;; `move-beginning-of-line' may leave point after invisible
      ;; characters if line starts with such of these (e.g., with
      ;; a link at column 0).  Really move to the beginning of the
      ;; current visible line.
      (forward-line 0))
    (cond
     ;; No special behavior.  Point is already at the beginning of
     ;; a line, logical or visual.
     ((not special))
     ;; `beginning-of-visual-line' left point before logical beginning
     ;; of line: point is at the beginning of a visual line.  Bail
     ;; out.
     ((and (bound-and-true-p visual-line-mode) (not (bolp))))
     ((let ((case-fold-search nil)) (looking-at org-complex-heading-regexp))
      ;; At a headline, special position is before the title, but
      ;; after any TODO keyword or priority cookie.
      (let ((refpos (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
			 (line-end-position)))
	    (bol (point)))
	(if (eq special 'reversed)
	    (when (and (= origin bol) (eq last-command this-command))
	      (goto-char refpos))
	  (when (or (> origin refpos) (<= origin bol))
	    (goto-char refpos)))))
     ((and (looking-at org-list-full-item-re)
	   (org-element-type-p
            (save-match-data (org-element-at-point))
	    '(item plain-list)))
      ;; Set special position at first white space character after
      ;; bullet, and check-box, if any.
      (let ((after-bullet
	     (let ((box (match-end 3)))
	       (cond ((not box) (match-end 1))
		     ((eq (char-after box) ?\s) (1+ box))
		     (t box)))))
	(if (eq special 'reversed)
	    (when (and (= (point) origin) (eq last-command this-command))
	      (goto-char after-bullet))
	  (when (or (> origin after-bullet) (>= (point) origin))
	    (goto-char after-bullet)))))
     ;; No special context.  Point is already at beginning of line.
     (t nil))))

(defun org-end-of-line (&optional n)
  "Go to the end of the line, but before ellipsis, if any.

If this is a headline, and `org-special-ctrl-a/e' is not nil or
symbol `reversed', ignore tags on the first attempt, and only
move to after the tags when the cursor is already beyond the end
of the headline.

If `org-special-ctrl-a/e' is symbol `reversed' then ignore tags
on the second attempt.

With argument N not nil or 1, move forward N - 1 lines first."
  (interactive "^p" org-mode)
  (let ((origin (point))
	(special (pcase org-special-ctrl-a/e
		   (`(,_ . ,C-e) C-e) (_ org-special-ctrl-a/e)))
	deactivate-mark)
    ;; First move to a visible line.
    (if (bound-and-true-p visual-line-mode)
	(beginning-of-visual-line n)
      (move-beginning-of-line n))
    (cond
     ;; At a headline, with tags.
     ((and special
	   (save-excursion
	     (forward-line 0)
	     (let ((case-fold-search nil))
	       (looking-at org-complex-heading-regexp)))
	   (match-end 5))
      (let ((tags (save-excursion
		    (goto-char (match-beginning 5))
		    (skip-chars-backward " \t")
		    (point)))
	    (visual-end (and (bound-and-true-p visual-line-mode)
			     (save-excursion
			       (end-of-visual-line)
			       (point)))))
	;; If `end-of-visual-line' brings us before end of line or
	;; even tags, i.e., the headline spans over multiple visual
	;; lines, move there.
	(cond ((and visual-end
		    (< visual-end tags)
		    (<= origin visual-end))
	       (goto-char visual-end))
	      ((eq special 'reversed)
	       (if (and (= origin (line-end-position))
			(eq this-command last-command))
		   (goto-char tags)
		 (end-of-line)))
	      (t
	       (if (or (< origin tags) (>= origin (line-end-position)))
		   (goto-char tags)
		 (end-of-line))))))
     ((bound-and-true-p visual-line-mode)
      (let ((bol (line-beginning-position)))
	(end-of-visual-line)
	;; If `end-of-visual-line' gets us past the ellipsis at the
	;; end of a line, backtrack and use `end-of-line' instead.
	(when (/= bol (line-beginning-position))
	  (goto-char bol)
	  (end-of-line))))
     (t (end-of-line)))))

(defun org-backward-sentence (&optional _arg)
  "Go to beginning of sentence, or beginning of table field.
This will call `backward-sentence' or `org-table-beginning-of-field',
depending on context."
  (interactive nil org-mode)
  (declare-function org-table-beginning-of-field "org-table" (&optional n))
  (let* ((element (org-element-at-point))
	 (contents-begin (org-element-contents-begin element))
	 (table (org-element-lineage element 'table t)))
    (if (and table
	     (> (point) contents-begin)
	     (<= (point) (org-element-contents-end table)))
        (progn
          (require 'org-table)
          (call-interactively #'org-table-beginning-of-field))
      (save-restriction
	(when (and contents-begin
		   (< (point-min) contents-begin)
		   (> (point) contents-begin))
	  (narrow-to-region contents-begin
			    (org-element-contents-end element)))
	(call-interactively #'backward-sentence)))))

(defun org-forward-sentence (&optional _arg)
  "Go to end of sentence, or end of table field.
This will call `forward-sentence' or `org-table-end-of-field',
depending on context."
  (interactive nil org-mode)
  (declare-function org-table-end-of-field "org-table" (&optional n))
  (if (and (org-at-heading-p)
	   (save-restriction (skip-chars-forward " \t") (not (eolp))))
      (save-restriction
	(narrow-to-region (line-beginning-position) (line-end-position))
	(call-interactively #'forward-sentence))
    (let* ((element (org-element-at-point))
	   (contents-end (org-element-contents-end element))
	   (table (org-element-lineage element 'table t)))
      (if (and table
	       (>= (point) (org-element-contents-begin table))
	       (< (point) contents-end))
          (progn
            (require 'org-table)
	    (call-interactively #'org-table-end-of-field))
	(save-restriction
	  (when (and contents-end
		     (> (point-max) contents-end)
		     ;; Skip blank lines between elements.
		     (< (org-element-end element)
			(save-excursion (goto-char contents-end)
					(skip-chars-forward " \r\t\n"))))
	    (narrow-to-region (org-element-contents-begin element)
			      contents-end))
	  ;; End of heading is considered as the end of a sentence.
	  (let ((sentence-end (concat (sentence-end) "\\|^\\*+ .*$")))
	    (call-interactively #'forward-sentence)))))))

(defun org-back-to-heading (&optional invisible-ok)
  "Go back to beginning of heading or inlinetask."
  (forward-line 0)
  (or (and (org-at-heading-p (not invisible-ok))
           (not (and (featurep 'org-inlinetask)
                   (fboundp 'org-inlinetask-end-p)
                   (org-inlinetask-end-p))))
      (unless
          (org-element-lineage-map
              (org-element-at-point)
              (lambda (el)
                (goto-char (org-element-begin el))
                (or invisible-ok (not (org-invisible-p))))
            '(headline inlinetask)
            'with-self 'first-match)
        (user-error "Before first headline at position %d in buffer %s"
		    (point) (current-buffer))))
  (point))

(defun org-back-to-heading-or-point-min (&optional invisible-ok)
  "Go back to heading or first point in buffer.
If point is before first heading go to first point in buffer
instead of back to heading."
  (if (org-before-first-heading-p)
      (goto-char (point-min))
    (org-back-to-heading invisible-ok)))

(defun org-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (outline-up-heading arg t))

(defun org-up-heading-safe ()
  "Move to the heading line of which the present line is a subheading.
Return the true heading level, as number or nil when there is no such
heading.

When point is not at heading, go to the parent of the current heading.
When point is at or inside an inlinetask, go to the containing
heading.

This version will not throw an error.  It will return the true level
of the headline found, or nil if no higher level is found.

When no higher level is found, the still move point to the containing
heading, if there is any in the accessible portion of the buffer.

When narrowing is in effect, ignore headings starting before the
available portion of the buffer."
  (let* ((current-heading (org-element-lineage
                           (org-element-at-point)
                           '(headline inlinetask)
                           'with-self))
         (parent (org-element-lineage current-heading 'headline)))
    (if (and parent
             (<= (point-min) (org-element-begin parent)))
        (progn
          (goto-char (org-element-begin parent))
          (org-element-property :true-level parent))
      (when (and current-heading
                 (<= (point-min) (org-element-begin current-heading)))
        (goto-char (org-element-begin current-heading))
        nil))))

(defun org-up-heading-or-point-min ()
  "Move to the heading line of which the present is a subheading, or point-min.
This version is needed to make point-min behave like a virtual
heading of level 0 for property-inheritance.  It will return the
level of the headline found (down to 0) or nil if already at a
point before the first headline or at point-min."
  (when (ignore-errors (org-back-to-heading t))
    (if (< 1 (funcall outline-level))
	(or (org-up-heading-safe)
            ;; The first heading may not be level 1 heading.
            (goto-char (point-min)))
      (unless (= (point) (point-min)) (goto-char (point-min))))))

(defun org-goto-sibling (&optional previous)
  "Goto the next sibling heading, even if it is invisible.
When PREVIOUS is set, go to the previous sibling instead.  Returns t
when a sibling was found.  When none is found, return nil and don't
move point."
  (let ((fun (if previous 're-search-backward 're-search-forward))
	(pos (point))
	(re org-outline-regexp-bol)
	level l)
    (when (ignore-errors (org-back-to-heading t))
      (when (org-element-type-p (org-element-at-point) 'inlinetask)
        (org-up-heading-safe))
      (setq level (funcall outline-level))
      (catch 'exit
	(or previous (forward-char 1))
	(while (funcall fun re nil t)
	  (setq l (funcall outline-level))
	  (when (< l level) (goto-char pos) (throw 'exit nil))
	  (when (= l level) (goto-char (match-beginning 0)) (throw 'exit t)))
	(goto-char pos)
	nil))))

(defun org-goto-first-child (&optional element)
  "Goto the first child, even if it is invisible.
Return t when a child was found.  Otherwise don't move point and
return nil."
  (let ((heading (org-element-lineage
                  (or element (org-element-at-point))
                  '(headline inlinetask org-data)
                  'with-self)))
    (when heading
      (unless (or (org-element-type-p heading 'inlinetask)
                  (not (org-element-contents-begin heading)))
        (let ((pos (point)))
          (goto-char (org-element-contents-begin heading))
          (if (re-search-forward
               org-outline-regexp-bol
               (org-element-end heading)
               t)
              (progn (goto-char (match-beginning 0)) t)
            (goto-char pos) nil))))))

(defun org-get-next-sibling ()
  "Move to next heading of the same level, and return point.
If there is no such heading, return nil.
This is like outline-next-sibling, but invisible headings are ok."
  (let ((level (funcall outline-level)))
    (outline-next-heading)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-heading))
    (unless (or (eobp) (< (funcall outline-level) level))
      (point))))

(defun org-get-previous-sibling ()
  "Move to previous heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((opoint (point))
	(level (funcall outline-level)))
    (outline-previous-heading)
    (when (and (/= (point) opoint) (outline-on-heading-p t))
      (while (and (> (funcall outline-level) level)
		  (not (bobp)))
	(outline-previous-heading))
      (unless (< (funcall outline-level) level)
        (point)))))

(defun org-end-of-subtree (&optional invisible-ok to-heading element)
  "Goto to the end of a visible subtree at point or ELEMENT and return point.
The subtree is considered at first heading parent containing point or
ELEMENT.

When end of the subtree has blank lines, move point before these blank
lines.

When INVISIBLE-OK is non-nil, ignore visibility.

When before first heading, goto `point-max' minus blank lines.
When TO-HEADING is non-nil, go to the next heading or `point-max'."
  (when element
    (setq element (org-element-lineage
                   element
                   '(headline)
                   'include-self))
    (goto-char (org-element-begin element)))
  (unless (and invisible-ok element)
    (org-back-to-heading-or-point-min invisible-ok)
    (setq element
          (org-element-lineage
           (org-element-at-point)
           '(headline)
           'include-self)))
  (if (org-element-type-p element 'headline)
      (goto-char (org-element-end element))
    (goto-char (point-max)))
  (unless to-heading
    (when (memq (preceding-char) '(?\n ?\^M))
      ;; Go to end of line before heading
      (forward-char -1)
      ;; Skip blank lines
      (skip-chars-backward "\n\r\t ")))
  (point))

(defun org-end-of-meta-data (&optional full)
  "Skip planning line and properties drawer in current entry.

When optional argument FULL is t, also skip planning information,
clocking lines, any kind of drawer, and blank lines.

When FULL is non-nil but not t, skip planning information,
properties, clocking lines, logbook drawers, and blank lines."
  (org-back-to-heading t)
  (forward-line)
  ;; Skip planning information.
  (when (looking-at-p org-planning-line-re) (forward-line))
  ;; Skip property drawer.
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0))
    (forward-line))
  ;; When FULL is not nil, skip more.
  (when (and full (not (org-at-heading-p)))
    (catch 'exit
      (let ((end (save-excursion (outline-next-heading) (point)))
	    (re (concat "[ \t]*$" "\\|" org-clock-line-re)))
	(while (not (eobp))
	  (cond ;; Skip clock lines and blank lines.
	   ((looking-at-p re) (forward-line))
	   ;; Skip logbook drawer.
	   ((looking-at-p org-logbook-drawer-re)
	    (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
		(forward-line)
	      (throw 'exit t)))
	   ;; When FULL is t, skip regular drawer too.
	   ((and (eq full t) (looking-at-p org-drawer-regexp))
	    (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
		(forward-line)
	      (throw 'exit t)))
	   (t (throw 'exit t))))))))

(defun org--line-fully-invisible-p ()
  "Return non-nil if the current line is fully invisible."
  (let ((line-beg (line-beginning-position))
	(line-pos (1- (line-end-position)))
	(is-invisible t))
    (while (and (< line-beg line-pos) is-invisible)
      (setq is-invisible (org-invisible-p line-pos))
      (setq line-pos (1- line-pos)))
    is-invisible))

(defun org-forward-heading-same-level (arg &optional invisible-ok)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading.
Normally this only looks at visible headings, but when INVISIBLE-OK is
non-nil it will also look at invisible ones."
  (interactive "p" org-mode)
  (let ((backward? (and arg (< arg 0))))
    (if (org-before-first-heading-p)
	(if backward? (goto-char (point-min)) (outline-next-heading))
      (org-back-to-heading invisible-ok)
      (unless backward? (end-of-line))	;do not match current headline
      (let ((level (org-current-level))
	    (f (if backward? #'re-search-backward #'re-search-forward))
	    (count (if arg (abs arg) 1))
	    (result (point)))
	(while (and (> count 0)
		    (funcall f org-outline-regexp-bol nil 'move))
	  (let ((l (- (match-end 0) (match-beginning 0) 1)))
	    (cond ((< l level) (setq count 0))
		  ((and (= l level)
			(or invisible-ok
			    ;; FIXME: See commit a700fadd72 and the
			    ;; related discussion on why using
			    ;; `org--line-fully-invisible-p' is needed
			    ;; here, which is to serve the needs of an
			    ;; external package.  If the change is
			    ;; wrong regarding Org itself, it should
			    ;; be removed.
			    (not (org--line-fully-invisible-p))))
		   (cl-decf count)
		   (when (= l level) (setq result (point)))))))
	(goto-char result))
      (forward-line 0))))

(defun org-backward-heading-same-level (arg &optional invisible-ok)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p" org-mode)
  (org-forward-heading-same-level (if arg (- arg) -1) invisible-ok))

(defun org-next-visible-heading (arg)
  "Move to the next visible heading line.
With ARG, repeats or can move backward if negative."
  (interactive "p" org-mode)
  (let ((regexp (concat "^" (org-get-limited-outline-regexp))))
    (if (< arg 0)
	(forward-line 0)
      (end-of-line))
    (while (and (< arg 0) (re-search-backward regexp nil :move))
      (unless (bobp)
	(when (org-invisible-p nil t)
	  (goto-char (org-fold-previous-visibility-change))
          (unless (looking-at-p regexp)
            (re-search-backward regexp nil :mode))))
      (cl-incf arg))
    (while (and (> arg 0) (re-search-forward regexp nil :move))
      (when (org-invisible-p nil t)
	(goto-char (org-fold-next-visibility-change))
        (skip-chars-forward " \t\n")
	(end-of-line))
      (cl-decf arg))
    (if (> arg 0) (goto-char (point-max)) (forward-line 0))))

(defun org-previous-visible-heading (arg)
  "Move to the previous visible heading.
With ARG, repeats or can move forward if negative."
  (interactive "p" org-mode)
  (org-next-visible-heading (- arg)))

(defun org-forward-paragraph (&optional arg)
  "Move forward by a paragraph, or equivalent, unit.

With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N paragraphs.

The function moves point between two structural
elements (paragraphs, tables, lists, etc.).

It also provides the following special moves for convenience:

  - on a table or a property drawer, move to its beginning;
  - on comment, example, export, source and verse blocks, stop
    at blank lines;
  - skip consecutive clocks, diary S-exps, and keywords."
  (interactive "^p" org-mode)
  (unless arg (setq arg 1))
  (if (< arg 0) (org-backward-paragraph (- arg))
    (while (and (> arg 0) (not (eobp)))
      (org--forward-paragraph-once)
      (cl-decf arg))
    ;; Return moves left.
    arg))

(defun org-backward-paragraph (&optional arg)
  "Move backward by a paragraph, or equivalent, unit.

With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N paragraphs.

The function moves point between two structural
elements (paragraphs, tables, lists, etc.).

It also provides the following special moves for convenience:

  - on a table or a property drawer, move to its beginning;
  - on comment, example, export, source and verse blocks, stop
    at blank lines;
  - skip consecutive clocks, diary S-exps, and keywords."
  (interactive "^p" org-mode)
  (unless arg (setq arg 1))
  (if (< arg 0) (org-forward-paragraph (- arg))
    (while (and (> arg 0) (not (bobp)))
      (org--backward-paragraph-once)
      (cl-decf arg))
    ;; Return moves left.
    arg))

(defvar org--single-lines-list-is-paragraph t
  "Treat plain lists with single line items as a whole paragraph.")

(defun org--paragraph-at-point ()
  "Return paragraph, or equivalent, element at point.

Paragraph element at point is the element at point, with the
following special cases:

- treat table rows (resp. node properties) as the table
  \(resp. property drawer) containing them.

- treat plain lists with an item every line as a whole.

- treat consecutive keywords, clocks, and diary-sexps as a single
  block.

Function may return a real element, or a pseudo-element with type
`pseudo-paragraph'."
  (let* ((e (org-element-at-point))
	 (type (org-element-type e))
	 ;; If we need to fake a new pseudo-element, triplet is
	 ;;
	 ;;   (BEG END PARENT)
	 ;;
	 ;; where BEG and END are element boundaries, and PARENT the
	 ;; element containing it, or nil.
	 (triplet
	  (cond
	   ((memq type '(table property-drawer))
	    (list (org-element-begin e)
		  (org-element-end e)
		  (org-element-parent e)))
	   ((memq type '(node-property table-row))
	    (let ((e (org-element-parent e)))
	      (list (org-element-begin e)
		    (org-element-end e)
		    (org-element-parent e))))
	   ((memq type '(clock diary-sexp keyword))
	    (let* ((regexp (pcase type
			     (`clock org-clock-line-re)
			     (`diary-sexp "%%(")
			     (_ org-keyword-regexp)))
		   (end (if (< 0 (org-element-post-blank e))
			    (org-element-end e)
			  (org-with-wide-buffer
			   (forward-line)
			   (while (looking-at regexp) (forward-line))
			   (skip-chars-forward " \t\n")
			   (line-beginning-position))))
		   (begin (org-with-point-at (org-element-begin e)
			    (while (and (not (bobp)) (looking-at regexp))
			      (forward-line -1))
			    ;; We may have gotten one line too far.
			    (if (looking-at regexp)
				(point)
			      (line-beginning-position 2)))))
	      (list begin end (org-element-parent e))))
	   ;; Find the full plain list containing point, the check it
	   ;; contains exactly one line per item.
	   ((let ((l (org-element-lineage e 'plain-list t)))
	      (while (org-element-type-p
                      (org-element-parent l)
                      '(item plain-list))
		(setq l (org-element-parent l)))
	      (and l org--single-lines-list-is-paragraph
		   (org-with-point-at (org-element-post-affiliated l)
		     (forward-line (length (org-element-property :structure l)))
		     (= (point) (org-element-contents-end l)))
		   ;; Return value.
		   (list (org-element-begin l)
			 (org-element-end l)
			 (org-element-parent l)))))
	   (t nil))))			;no triplet: return element
    (pcase triplet
      (`(,b ,e ,p)
       (org-element-create
	'pseudo-paragraph
	(list :begin b :end e :parent p :post-blank 0 :post-affiliated b)))
      (_ e))))

(defun org--forward-paragraph-once ()
  "Move forward to end of paragraph or equivalent, once.
See `org-forward-paragraph'."
  (interactive nil org-mode)
  (save-restriction
    (widen)
    (when (org-match-line "^[ \t]*$")
      ;; We should not skip when point is at the end of non-empty line.
      (skip-chars-forward " \t\n"))
    (cond
     ((eobp) nil)
     ;; When inside a folded part, move out of it.
     ((when (org-invisible-p nil t)
        (goto-char (cdr (org-fold-get-region-at-point)))
        (forward-line)
        t))
     (t
      (let* ((element (org--paragraph-at-point))
	     (type (org-element-type element))
	     (contents-begin (org-element-contents-begin element))
	     (end (org-element-end element))
	     (post-affiliated (org-element-post-affiliated element)))
	(cond
	 ((eq type 'plain-list)
	  (forward-char)
	  (org--forward-paragraph-once))
	 ;; If the element is folded, skip it altogether.
         ((when (org-with-point-at post-affiliated (org-invisible-p (line-end-position) t))
            (goto-char (cdr (org-fold-get-region-at-point
			     nil
			     (org-with-point-at post-affiliated
			       (line-end-position)))))
	    (forward-line)
	    t))
	 ;; At a greater element, move inside.
	 ((and contents-begin
	       (> contents-begin (point))
	       (not (eq type 'paragraph)))
	  (goto-char contents-begin)
	  ;; Items and footnote definitions contents may not start at
	  ;; the beginning of the line.  In this case, skip until the
	  ;; next paragraph.
	  (cond
	   ((not (bolp)) (org--forward-paragraph-once))
	   ((org-previous-line-empty-p) (forward-line -1))
	   (t nil)))
	 ;; Move between empty lines in some blocks.
	 ((memq type '(comment-block example-block export-block src-block
				     verse-block))
	  (let ((contents-start
		 (org-with-point-at post-affiliated
		   (line-beginning-position 2))))
	    (if (< (point) contents-start)
		(goto-char contents-start)
	      (let ((contents-end
		     (org-with-point-at end
		       (skip-chars-backward " \t\n")
		       (line-beginning-position))))
		(cond
		 ((>= (point) contents-end)
		  (goto-char end)
		  (skip-chars-backward " \t\n")
		  (forward-line))
		 ((re-search-forward "^[ \t]*\n" contents-end :move)
		  (forward-line -1))
		 (t nil))))))
	 (t
	  ;; Move to element's end.
	  (goto-char end)
	  (skip-chars-backward " \t\n")
	  (forward-line))))))))

(defun org--backward-paragraph-once ()
  "Move backward to start of paragraph or equivalent, once.
See `org-backward-paragraph'."
  (interactive nil org-mode)
  (save-restriction
    (widen)
    (cond
     ((bobp) nil)
     ;; Blank lines at the beginning of the buffer.
     ((and (org-match-line "^[ \t]*$")
	   (save-excursion (skip-chars-backward " \t\n") (bobp)))
      (goto-char (point-min)))
     ;; When inside a folded part, move out of it.
     ((when (org-invisible-p (1- (point)) t)
        (goto-char (1- (car (org-fold-get-region-at-point nil (1- (point))))))
	(org--backward-paragraph-once)
	t))
     (t
      (let* ((element (org--paragraph-at-point))
	     (type (org-element-type element))
	     (begin (org-element-begin element))
	     (post-affiliated (org-element-post-affiliated element))
	     (contents-end (org-element-contents-end element))
	     (end (org-element-end element))
	     (parent (org-element-parent element))
	     (reach
	      ;; Move to the visible empty line above position P, or
	      ;; to position P.  Return t.
	      (lambda (p)
		(goto-char p)
		(when (and (org-previous-line-empty-p)
			   (let ((end (line-end-position 0)))
			     (or (= end (point-min))
				 (not (org-invisible-p (1- end))))))
		  (forward-line -1))
		t)))
	(cond
	 ;; Already at the beginning of an element.
	 ((= begin (point))
	  (cond
	   ;; There is a blank line above.  Move there.
	   ((and (org-previous-line-empty-p)
		 (not (org-invisible-p (1- (line-end-position 0)))))
	    (forward-line -1))
	   ;; At the beginning of the first element within a greater
	   ;; element.  Move to the beginning of the greater element.
	   ((and parent
                 (not (org-element-type-p parent 'section))
                 (= begin (org-element-contents-begin parent)))
	    (funcall reach (org-element-begin parent)))
	   ;; Since we have to move anyway, find the beginning
	   ;; position of the element above.
	   (t
	    (forward-char -1)
	    (org--backward-paragraph-once))))
	 ;; Skip paragraphs at the very beginning of footnote
	 ;; definitions or items.
	 ((and (eq type 'paragraph)
	       (org-with-point-at begin (not (bolp))))
	  (funcall reach (progn (goto-char begin) (line-beginning-position))))
	 ;; If the element is folded, skip it altogether.
	 ((org-with-point-at post-affiliated (org-invisible-p (line-end-position) t))
	  (funcall reach begin))
	 ;; At the end of a greater element, move inside.
	 ((and contents-end
	       (<= contents-end (point))
	       (not (eq type 'paragraph)))
	  (cond
	   ((memq type '(footnote-definition plain-list))
	    (skip-chars-backward " \t\n")
	    (org--backward-paragraph-once))
	   ((= contents-end (point))
	    (forward-char -1)
	    (org--backward-paragraph-once))
	   (t
	    (goto-char contents-end))))
	 ;; Move between empty lines in some blocks.
	 ((and (memq type '(comment-block example-block export-block src-block
					  verse-block))
	       (let ((contents-start
		      (org-with-point-at post-affiliated
			(line-beginning-position 2))))
		 (when (> (point) contents-start)
		   (let ((contents-end
			  (org-with-point-at end
			    (skip-chars-backward " \t\n")
			    (line-beginning-position))))
		     (if (> (point) contents-end)
			 (progn (goto-char contents-end) t)
		       (skip-chars-backward " \t\n" begin)
		       (re-search-backward "^[ \t]*\n" contents-start :move)
		       t))))))
	 ;; Move to element's start.
	 (t
	  (funcall reach begin))))))))

(defun org-forward-element ()
  "Move forward by one element.
Move to the next element at the same level, when possible."
  (interactive nil org-mode)
  (cond ((eobp) (user-error "Cannot move further down"))
	((org-with-limited-levels (org-at-heading-p))
	 (let ((origin (point)))
	   (goto-char (org-end-of-subtree nil t))
	   (unless (org-with-limited-levels (org-at-heading-p))
	     (goto-char origin)
	     (user-error "Cannot move further down"))))
	(t
	 (let* ((elem (org-element-at-point))
		(end (org-element-end elem))
		(parent (org-element-parent elem)))
	   (cond ((and parent (= (org-element-contents-end parent) end))
		  (goto-char (org-element-end parent)))
		 ((integer-or-marker-p end) (goto-char end))
		 (t (message "No element at point")))))))

(defun org-backward-element ()
  "Move backward by one element.
Move to the previous element at the same level, when possible."
  (interactive nil org-mode)
  (cond ((bobp) (user-error "Cannot move further up"))
	((org-with-limited-levels (org-at-heading-p))
	 ;; At a headline, move to the previous one, if any, or stay
	 ;; here.
	 (let ((origin (point)))
	   (org-with-limited-levels (org-backward-heading-same-level 1))
	   ;; When current headline has no sibling above, move to its
	   ;; parent.
	   (when (= (point) origin)
	     (or (org-with-limited-levels (org-up-heading-safe))
		 (progn (goto-char origin)
			(user-error "Cannot move further up"))))))
	(t
	 (let* ((elem (org-element-at-point))
		(beg (org-element-begin elem)))
	   (cond
	    ;; Move to beginning of current element if point isn't
	    ;; there already.
	    ((null beg) (message "No element at point"))
	    ((/= (point) beg) (goto-char beg))
	    (t (goto-char beg)
	       (skip-chars-backward " \r\t\n")
	       (unless (bobp)
		 (let ((prev (org-element-at-point)))
		   (goto-char (org-element-begin prev))
		   (while (and (setq prev (org-element-parent prev))
			       (<= (org-element-end prev) beg))
		     (goto-char (org-element-begin prev)))))))))))

(defun org-up-element ()
  "Move to upper element."
  (interactive nil org-mode)
  (if (org-with-limited-levels (org-at-heading-p))
      (unless (org-up-heading-safe) (user-error "No surrounding element"))
    (let* ((elem (org-element-at-point))
	   (parent (org-element-parent elem)))
      ;; Skip sections
      (when (org-element-type-p parent 'section)
        (setq parent (org-element-parent parent)))
      (if (and parent
               (not (org-element-type-p parent 'org-data)))
          (goto-char (org-element-begin parent))
	(if (org-with-limited-levels (org-before-first-heading-p))
	    (user-error "No surrounding element")
	  (org-with-limited-levels (org-back-to-heading)))))))

(defun org-down-element ()
  "Move to inner element."
  (interactive nil org-mode)
  (let ((element (org-element-at-point)))
    (cond
     ((org-element-type-p element '(plain-list table))
      (goto-char (org-element-contents-begin element))
      (forward-char))
     ((org-element-type-p element org-element-greater-elements)
      ;; If contents are hidden, first disclose them.
      (when (org-invisible-p (line-end-position)) (org-cycle))
      (goto-char (or (org-element-contents-begin element)
		     (user-error "No content for this element"))))
     (t (user-error "No inner element")))))

(defun org-next-block (arg &optional backward block-regexp)
  "Jump to the next block.

With a prefix argument ARG, jump forward ARG many blocks.

When BACKWARD is non-nil, jump to the previous block.

When BLOCK-REGEXP is non-nil, use this regexp to find blocks.
Match data is set according to this regexp when the function
returns.

Return point at beginning of the opening line of found block.
Throw an error if no block is found."
  (interactive "p" org-mode)
  (let ((re (or block-regexp "^[ \t]*#\\+BEGIN"))
	(case-fold-search t)
	(search-fn (if backward #'re-search-backward #'re-search-forward))
	(count (or arg 1))
	(origin (point))
	last-element)
    (if backward (forward-line 0)
      (let ((inhibit-field-text-motion t)) (end-of-line)))
    (while (and (> count 0) (funcall search-fn re nil t))
      (let ((element (save-excursion
		       (goto-char (match-beginning 0))
		       (save-match-data (org-element-at-point)))))
	(when (and (org-element-type-p
                    element
		    '(center-block comment-block dynamic-block
				   example-block export-block quote-block
				   special-block src-block verse-block))
		   (<= (match-beginning 0)
		      (org-element-post-affiliated element)))
	  (setq last-element element)
	  (cl-decf count))))
    (if (= count 0)
	(prog1 (goto-char (org-element-post-affiliated last-element))
	  (save-match-data (org-fold-show-context)))
      (goto-char origin)
      (user-error "No %s code blocks" (if backward "previous" "further")))))

(defun org-previous-block (arg &optional block-regexp)
  "Jump to the previous block.
With a prefix argument ARG, jump backward ARG many source blocks.
When BLOCK-REGEXP is non-nil, use this regexp to find blocks."
  (interactive "p" org-mode)
  (org-next-block arg t block-regexp))

(provide 'org-move)

;;; org-move.el ends here
