;;; org-indent-static.el --- Indentation support for Org    -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.
;;

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

;; URL: https://orgmode.org
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements Org mode integration with built-in Emacs
;; indentation API.
;;
;;; Code:

(require 'org-element)
(require 'org-src)
(declare-function org-current-level "org" ())
(declare-function org-log-into-drawer "org" ())

(defcustom org-adapt-indentation nil
  "Non-nil means adapt indentation to outline node level.

When set to t, Org assumes that you write outlines by indenting
text in each node to align with the headline, after the stars.

When this variable is set to `headline-data', Org only adapts the
indentation of the data lines right below the headline, such as
planning/clock lines and property/logbook drawers.

The following issues are influenced by this variable:

- The indentation is increased by one space in a demotion
  command, and decreased by one in a promotion command.  However,
  in the latter case, if shifting some line in the entry body
  would alter document structure (e.g., insert a new headline),
  indentation is not changed at all.

- Property drawers and planning information is inserted indented
  when this variable is set.  When nil, they will not be indented.

- TAB indents a line relative to current level.  The lines below
  a headline will be indented when this variable is set to t.

Note that this is all about true indentation, by adding and
removing space characters.  See also \"org-indent.el\" which does
level-dependent indentation in a virtual way, i.e. at display
time in Emacs."
  :group 'org-edit-structure
  :type '(choice
	  (const :tag "Adapt indentation for all lines" t)
	  (const :tag "Adapt indentation for headline data lines"
		 headline-data)
	  (const :tag "Do not adapt indentation at all" nil))
  :safe (lambda (x) (memq x '(t nil headline-data))))

(defun org--at-headline-data-p (&optional beg element)
  "Return non-nil when `point' or BEG is inside headline metadata.

Metadata is planning line, properties drawer, logbook drawer right
after property drawer, or clock log line immediately following
properties drawer/planning line/ heading.

Optional argument ELEMENT contains element at BEG."
  (org-with-wide-buffer
   (when beg (goto-char beg))
   (setq element (or element (org-element-at-point)))
   (if (or (org-element-type-p element 'headline)
           (not (org-element-lineage element '(headline inlinetask))))
       nil ; Not inside heading.
     ;; Skip to top-level parent in section.
     (while (not (org-element-type-p (org-element-parent element) 'section))
       (setq element (org-element-parent element)))
     (pcase (org-element-type element)
       ((or `planning `property-drawer)
        t)
       (`drawer
        ;; LOGBOOK drawer with appropriate name.
        (equal
         (org-log-into-drawer)
         (org-element-property :drawer-name element)))
       (`clock
        ;; Previous element must be headline metadata or headline.
        (goto-char (1- (org-element-begin element)))
        (or (org-at-heading-p)
            (org--at-headline-data-p)))))))

(defvar org-element-greater-elements)
(defun org--get-expected-indentation (element contentsp)
  "Expected indentation column for current line, according to ELEMENT.
ELEMENT is an element containing point.  CONTENTSP is non-nil
when indentation is to be computed according to contents of
ELEMENT."
  (let ((type (org-element-type element))
	(start (org-element-begin element))
	(post-affiliated (org-element-post-affiliated element)))
    (org-with-wide-buffer
     (cond
      (contentsp
       (cl-case type
	 ((diary-sexp footnote-definition) 0)
         (section
          (org--get-expected-indentation
           (org-element-parent element)
           t))
	 ((headline inlinetask nil)
	  (if (not org-adapt-indentation) 0
	    (let ((level (org-current-level)))
	      (if level (1+ level) 0))))
	 ((item plain-list) (org-list-item-body-column post-affiliated))
	 (t
	  (when start (goto-char start))
	  (current-indentation))))
      ((memq type '(headline inlinetask nil))
       (if (org-match-line "[ \t]*$")
	   (org--get-expected-indentation element t)
	 0))
      ((memq type '(diary-sexp footnote-definition)) 0)
      ;; First paragraph of a footnote definition or an item.
      ;; Indent like parent.
      ((and start (< (line-beginning-position) start))
       (org--get-expected-indentation
	(org-element-parent element) t))
      ;; At first line: indent according to previous sibling, if any,
      ;; ignoring footnote definitions and inline tasks, or parent's
      ;; contents.  If `org-adapt-indentation' is `headline-data', ignore
      ;; previous headline data siblings.
      ((and start (= (line-beginning-position) start))
       (catch 'exit
	 (while t
	   (if (= (point-min) start) (throw 'exit 0)
	     (goto-char (1- start))
	     (let* ((previous (org-element-at-point))
		    (parent previous))
	       (while (and parent (<= (org-element-end parent) start))
		 (setq previous parent
		       parent (org-element-parent parent)))
	       (cond
		((not previous) (throw 'exit 0))
		((> (org-element-end previous) start)
		 (throw 'exit (org--get-expected-indentation previous t)))
		((org-element-type-p
                  previous '(footnote-definition inlinetask))
		 (setq start (org-element-begin previous)))
                ;; Do not indent like previous when the previous
                ;; element is headline data and `org-adapt-indentation'
                ;; is set to `headline-data'.
                ((and (eq 'headline-data org-adapt-indentation)
                      (not (org--at-headline-data-p start element))
                      (or (org-at-heading-p)
                          (org--at-headline-data-p (1- start) previous)))
                 (throw 'exit 0))
		(t (goto-char (org-element-begin previous))
		   (throw 'exit
			  (if (bolp) (current-indentation)
			    ;; At first paragraph in an item or
			    ;; a footnote definition.
			    (org--get-expected-indentation
			     (org-element-parent previous) t))))))))))
      ;; Otherwise, move to the first non-blank line above.
      (t
       (forward-line 0)
       (let ((pos (point)))
	 (skip-chars-backward " \r\t\n")
	 (cond
	  ;; Two blank lines end a footnote definition or a plain
	  ;; list.  When we indent an empty line after them, the
	  ;; containing list or footnote definition is over, so it
	  ;; qualifies as a previous sibling.  Therefore, we indent
	  ;; like its first line.
	  ((and (memq type '(footnote-definition plain-list))
		(> (count-lines (point) pos) 2))
	   (goto-char start)
	   (current-indentation))
	  ;; Line above is the first one of a paragraph at the
	  ;; beginning of an item or a footnote definition.  Indent
	  ;; like parent.
	  ((and start (< (line-beginning-position) start))
	   (org--get-expected-indentation
	    (org-element-parent element) t))
	  ;; Line above is the beginning of an element, i.e., point
	  ;; was originally on the blank lines between element's start
	  ;; and contents.
	  ((and post-affiliated (= (line-beginning-position) post-affiliated))
	   (org--get-expected-indentation element t))
	  ;; POS is after contents in a greater element or other block.
	  ;; Indent like the beginning of the element.
	  ((and (or (memq type org-element-greater-elements)
                    (memq type '(comment-block example-block export-block
                                               src-block verse-block)))
		(let ((cend (or (org-element-contents-end element)
                                (org-with-wide-buffer
			         (goto-char (org-element-end element))
			         (skip-chars-backward " \r\t\n")
			         (line-beginning-position)))))
		  (and cend (<= cend pos))))
	   ;; As a special case, if point is at the end of a footnote
	   ;; definition or an item, indent like the very last element
	   ;; within.  If that last element is an item, indent like
	   ;; its contents.
	   (if (memq type '(footnote-definition item plain-list))
	       (let ((last (org-element-at-point)))
		 (goto-char pos)
		 (org--get-expected-indentation
		  last (org-element-type-p last 'item)))
	     (goto-char start)
	     (current-indentation)))
	  ;; In any other case, indent like the current line.
	  (t (current-indentation)))))))))

(defun org--align-node-property ()
  "Align node property at point.
Alignment is done according to `org-property-format', which see."
  (when (save-excursion
	  (forward-line 0)
	  (looking-at org-property-re))
    (org-combine-change-calls (match-beginning 0) (match-end 0)
      (let ((newtext (concat (match-string 4)
	                     (org-trim
	                      (format org-property-format (match-string 1) (match-string 3))))))
        ;; Do not use `replace-match' here as we want to inherit folding
        ;; properties if inside fold.
        (delete-region (match-beginning 0) (match-end 0))
        (insert-and-inherit newtext)))))

(defvar org-src-content-indentation) ; org-element.el
(defun org-indent-line ()
  "Indent line depending on context.

Indentation is done according to the following rules:

  - Footnote definitions, diary sexps, headlines and inline tasks
    have to start at column 0.

  - On the very first line of an element, consider, in order, the
    next rules until one matches:

    1. If there's a sibling element before, ignoring footnote
       definitions and inline tasks, indent like its first line.

    2. If element has a parent, indent like its contents.  More
       precisely, if parent is an item, indent after the bullet.
       Else, indent like parent's first line.

    3. Otherwise, indent relatively to current level, if
       `org-adapt-indentation' is t, or to left margin.

  - On a blank line at the end of an element, indent according to
    the type of the element.  More precisely

    1. If element is a plain list, an item, or a footnote
       definition, indent like the very last element within.

    2. If element is a paragraph, indent like its last non blank
       line.

    3. Otherwise, indent like its very first line.

  - In the code part of a source block, use language major mode
    to indent current line if `org-src-tab-acts-natively' is
    non-nil.

  - Otherwise, indent like the first non-blank line above.

The function doesn't indent an item as it could break the whole
list structure.  Instead, use \\<org-mode-map>`\\[org-shiftmetaleft]' or \
`\\[org-shiftmetaright]'.

Also align node properties according to `org-property-format'."
  (interactive nil org-mode)
  (let* ((element (save-excursion (forward-line 0) (org-element-at-point-no-context)))
	 (type (org-element-type element)))
    (unless (or (org-at-heading-p) ; headline has no indent ever.
                ;; Do not indent first element after headline data.
                (and (eq org-adapt-indentation 'headline-data)
                     (not (org--at-headline-data-p nil element))
                     ;; Not at headline data and previous is headline data/headline.
                     (or (memq type '(headline inlinetask)) ; blank lines after heading
                         (and element
                              (save-excursion
                                (goto-char (1- (org-element-begin element)))
                                (or (org-at-heading-p)
                                    (org--at-headline-data-p)))))))
      (cond ((and (memq type '(plain-list item))
		  (= (line-beginning-position)
		     (org-element-post-affiliated element)))
	     nil)
	    ((and (eq type 'latex-environment)
		  (>= (point) (org-element-post-affiliated element))
		  (< (point)
		     (org-with-point-at (org-element-end element)
		       (skip-chars-backward " \t\n")
		       (line-beginning-position 2))))
	     nil)
	    ((and (eq type 'src-block)
		  org-src-tab-acts-natively
		  (> (line-beginning-position)
		     (org-element-post-affiliated element))
		  (< (line-beginning-position)
		     (org-with-point-at (org-element-end element)
		       (skip-chars-backward " \t\n")
		       (line-beginning-position))))
             (let ((block-content-ind
                    (when (not (org-src-preserve-indentation-p element))
                      (org-with-point-at (org-element-property :begin element)
                        (+ (org-current-text-indentation)
                           org-src-content-indentation)))))
               ;; Avoid over-indenting when beginning of a new line is not empty.
               ;; https://list.orgmode.org/OMCpuwZ--J-9@phdk.org/
               (org-with-undo-amalgamate
                 (when block-content-ind
                   (save-excursion (indent-line-to block-content-ind)))
                 (ignore-errors ; do not err when there is no proper major mode
                   ;; It is important to call `indent-according-to-mode'
                   ;; rather than `indent-line-function' here or we may
                   ;; sometimes break `electric-indent-mode'
                   ;; https://orgmode.org/list/5O9VMGb6WRaqeHR5_NXTb832Z2Lek_5L40YPDA52-S3kPwGYJspI8kLWaGtuq3DXyhtHpj1J7jTIXb39RX9BtCa2ecrWHjijZqI8QAD742U=@proton.me
                   (org-babel-do-in-edit-buffer (indent-according-to-mode)))
                 (when (and block-content-ind (looking-at-p "^$"))
                   (indent-line-to block-content-ind)))))
	    (t
	     (let ((column (org--get-expected-indentation element nil)))
	       ;; Preserve current column.
	       (if (<= (current-column) (current-indentation))
		   (indent-line-to column)
		 (save-excursion (indent-line-to column))))
	     ;; Align node property.  Also preserve current column.
	     (when (eq type 'node-property)
	       (let ((column (current-column)))
		 (org--align-node-property)
		 (org-move-to-column column))))))))

(defun org-indent-region (start end)
  "Indent each non-blank line in the region.
Called from a program, START and END specify the region to
indent.  The function will not indent contents of example blocks,
verse blocks and export blocks as leading white spaces are
assumed to be significant there."
  (interactive "r" org-mode)
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \r\t\n")
    (unless (eobp) (forward-line 0))
    (let ((indent-to
	   (lambda (ind pos)
	     ;; Set IND as indentation for all lines between point and
	     ;; POS.  Blank lines are ignored.  Leave point after POS
	     ;; once done.
	     (let ((limit (copy-marker pos)))
	       (while (< (point) limit)
		 (unless (looking-at-p "[ \t]*$") (indent-line-to ind))
		 (forward-line))
	       (set-marker limit nil))))
	  (end (copy-marker end)))
      (while (< (point) end)
	(if (or (looking-at-p " \r\t\n") (org-at-heading-p)) (forward-line)
	  (let* ((element (org-element-at-point))
		 (type (org-element-type element))
		 (element-end (copy-marker (org-element-end element)))
		 (ind (org--get-expected-indentation element nil)))
	    (cond
	     ;; Element indented as a single block.  Example blocks
	     ;; preserving indentation are a special case since the
	     ;; "contents" must not be indented whereas the block
	     ;; boundaries can.
	     ((or (memq type '(export-block latex-environment))
		  (and (eq type 'example-block)
		       (not (org-src-preserve-indentation-p element))))
	      (let ((offset (- ind (current-indentation))))
		(unless (zerop offset)
		  (indent-rigidly (org-element-begin element)
				  (org-element-end element)
				  offset)))
	      (goto-char element-end))
	     ;; Elements indented line wise.  Be sure to exclude
	     ;; example blocks (preserving indentation) and source
	     ;; blocks from this category as they are treated
	     ;; specially later.
	     ((or (memq type '(paragraph table table-row))
		  (not (or (org-element-contents-begin element)
			 (memq type '(example-block src-block)))))
	      (when (eq type 'node-property)
		(org--align-node-property)
		(forward-line 0))
	      (funcall indent-to ind (min element-end end)))
	     ;; Elements consisting of three parts: before the
	     ;; contents, the contents, and after the contents.  The
	     ;; contents are treated specially, according to the
	     ;; element type, or not indented at all.  Other parts are
	     ;; indented as a single block.
	     (t
	      (let* ((post (copy-marker
			    (org-element-post-affiliated element)))
		     (cbeg
		      (copy-marker
		       (cond
			((not (org-element-contents-begin element))
			 ;; Fake contents for source blocks.
			 (org-with-wide-buffer
			  (goto-char post)
			  (line-beginning-position 2)))
			((memq type '(footnote-definition item plain-list))
			 ;; Contents in these elements could start on
			 ;; the same line as the beginning of the
			 ;; element.  Make sure we start indenting
			 ;; from the second line.
			 (org-with-wide-buffer
			  (goto-char post)
			  (end-of-line)
			  (skip-chars-forward " \r\t\n")
			  (if (eobp) (point) (line-beginning-position))))
			(t (org-element-contents-begin element)))))
		     (cend (copy-marker
			    (or (org-element-contents-end element)
				;; Fake contents for source blocks.
				(org-with-wide-buffer
				 (goto-char element-end)
				 (skip-chars-backward " \r\t\n")
				 (line-beginning-position)))
			    t)))
		;; Do not change items indentation individually as it
		;; might break the list as a whole.  On the other
		;; hand, when at a plain list, indent it as a whole.
		(cond ((eq type 'plain-list)
		       (let ((offset (- ind (org-current-text-indentation))))
			 (unless (zerop offset)
			   (indent-rigidly (org-element-begin element)
					   (org-element-end element)
					   offset))
			 (goto-char cbeg)))
		      ((eq type 'item) (goto-char cbeg))
		      (t (funcall indent-to ind (min cbeg end))))
		(when (< (point) end)
		  (cl-case type
		    ((example-block verse-block))
		    (src-block
		     ;; In a source block, indent source code
		     ;; according to language major mode, but only if
		     ;; `org-src-tab-acts-natively' is non-nil.
		     (when (and (< (point) end) org-src-tab-acts-natively)
		       (ignore-errors
			 (org-babel-do-in-edit-buffer
			  (indent-region (point-min) (point-max))))))
		    (t (org-indent-region (point) (min cend end))))
		  (goto-char (min cend end))
		  (when (< (point) end)
		    (funcall indent-to ind (min element-end end))))
		(set-marker post nil)
		(set-marker cbeg nil)
		(set-marker cend nil))))
	    (set-marker element-end nil))))
      (set-marker end nil))))

(defun org-indent-drawer ()
  "Indent the drawer at point.
Signal an error when not at a drawer."
  (interactive nil org-mode)
  (let ((element (org-element-at-point)))
    (unless (org-element-type-p element '(drawer property-drawer))
      (user-error "Not at a drawer"))
    (org-with-wide-buffer
     (org-indent-region (org-element-begin element)
			(org-element-end element))))
  (message "Drawer at point indented"))

(defun org-indent-block ()
  "Indent the block at point.
Signal an error when not at a block."
  (interactive nil org-mode)
  (let ((element (org-element-at-point)))
    (unless (org-element-type-p
             element
	     '(comment-block center-block dynamic-block example-block
			     export-block quote-block special-block
			     src-block verse-block))
      (user-error "Not at a block"))
    (org-with-wide-buffer
     (org-indent-region (org-element-begin element)
			(org-element-end element))))
  (message "Block at point indented"))

(defun org-unindent-buffer ()
  "Un-indent the visible part of the buffer.
Relative indentation (between items, inside blocks, etc.) isn't
modified."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Cannot un-indent a buffer not in Org mode"))
  (letrec ((parse-tree (org-element-parse-buffer 'greater-element nil 'defer))
	   (unindent-tree
	    (lambda (contents)
	      (dolist (element (reverse contents))
		(if (org-element-type-p element '(headline section))
		    (funcall unindent-tree (org-element-contents element))
		  (save-excursion
		    (save-restriction
		      (narrow-to-region
		       (org-element-begin element)
		       (org-element-end element))
		      (org-do-remove-indentation))))))))
    (funcall unindent-tree (org-element-contents parse-tree))))

(provide 'org-indent-static)

;;; org-indent-static.el ends here
