;;; org-colview.el --- Column View in Org            -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2016 Free Software Foundation, Inc.

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

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the column view for Org.

;;; Code:

(require 'cl-lib)
(require 'org)

(declare-function org-agenda-redo "org-agenda" ())
(declare-function org-agenda-do-context-action "org-agenda" ())
(declare-function org-clock-sum-today "org-clock" (&optional headline-filter))

(defvar org-agenda-columns-add-appointments-to-effort-sum)
(defvar org-agenda-columns-compute-summary-properties)
(defvar org-agenda-columns-show-summaries)
(defvar org-agenda-view-columns-initially)

;;; Configuration

(defcustom org-columns-modify-value-for-display-function nil
  "Function that modifies values for display in column view.
For example, it can be used to cut out a certain part from a time stamp.
The function must take 2 arguments:

column-title    The title of the column (*not* the property name)
value           The value that should be modified.

The function should return the value that should be displayed,
or nil if the normal value should be used."
  :group 'org-properties
  :type '(choice (const nil) (function)))

;;; Column View

(defvar org-columns-overlays nil
  "Holds the list of current column overlays.")

(defvar org-columns--time 0.0
  "Number of seconds since the epoch, as a floating point number.")

(defvar-local org-columns-current-fmt nil
  "Local variable, holds the currently active column format.")
(defvar-local org-columns-current-fmt-compiled nil
  "Local variable, holds the currently active column format.
This is the compiled version of the format.")
(defvar-local org-columns-current-maxwidths nil
  "Loval variable, holds the currently active maximum column widths.")
(defvar org-columns-begin-marker (make-marker)
  "Points to the position where last a column creation command was called.")
(defvar org-columns-top-level-marker (make-marker)
  "Points to the position where current columns region starts.")

(defconst org-columns--fractional-duration-re
  (concat "[0-9.]+ *" (regexp-opt (mapcar #'car org-effort-durations)))
  "Regexp matching a duration.")

(defvar org-columns-map (make-sparse-keymap)
  "The keymap valid in column display.")

(defun org-columns-content ()
  "Switch to contents view while in columns view."
  (interactive)
  (org-overview)
  (org-content))

(org-defkey org-columns-map "c" 'org-columns-content)
(org-defkey org-columns-map "o" 'org-overview)
(org-defkey org-columns-map "e" 'org-columns-edit-value)
(org-defkey org-columns-map "\C-c\C-t" 'org-columns-todo)
(org-defkey org-columns-map "\C-c\C-c" 'org-columns-set-tags-or-toggle)
(org-defkey org-columns-map "\C-c\C-o" 'org-columns-open-link)
(org-defkey org-columns-map "v" 'org-columns-show-value)
(org-defkey org-columns-map "q" 'org-columns-quit)
(org-defkey org-columns-map "r" 'org-columns-redo)
(org-defkey org-columns-map "g" 'org-columns-redo)
(org-defkey org-columns-map [left] 'backward-char)
(org-defkey org-columns-map "\M-b" 'backward-char)
(org-defkey org-columns-map "a" 'org-columns-edit-allowed)
(org-defkey org-columns-map "s" 'org-columns-edit-attributes)
(org-defkey org-columns-map "\M-f"
	    (lambda () (interactive) (goto-char (1+ (point)))))
(org-defkey org-columns-map [right]
	    (lambda () (interactive) (goto-char (1+ (point)))))
(org-defkey org-columns-map [down]
	    (lambda () (interactive)
	      (let ((col (current-column)))
		(beginning-of-line 2)
		(while (and (org-invisible-p2) (not (eobp)))
		  (beginning-of-line 2))
		(move-to-column col)
		(if (eq major-mode 'org-agenda-mode)
		    (org-agenda-do-context-action)))))
(org-defkey org-columns-map [up]
	    (lambda () (interactive)
	      (let ((col (current-column)))
		(beginning-of-line 0)
		(while (and (org-invisible-p2) (not (bobp)))
		  (beginning-of-line 0))
		(move-to-column col)
		(if (eq major-mode 'org-agenda-mode)
		    (org-agenda-do-context-action)))))
(org-defkey org-columns-map [(shift right)] 'org-columns-next-allowed-value)
(org-defkey org-columns-map "n" 'org-columns-next-allowed-value)
(org-defkey org-columns-map [(shift left)] 'org-columns-previous-allowed-value)
(org-defkey org-columns-map "p" 'org-columns-previous-allowed-value)
(org-defkey org-columns-map "<" 'org-columns-narrow)
(org-defkey org-columns-map ">" 'org-columns-widen)
(org-defkey org-columns-map [(meta right)] 'org-columns-move-right)
(org-defkey org-columns-map [(meta left)] 'org-columns-move-left)
(org-defkey org-columns-map [(shift meta right)] 'org-columns-new)
(org-defkey org-columns-map [(shift meta left)] 'org-columns-delete)
(dotimes (i 10)
  (org-defkey org-columns-map (number-to-string i)
	      `(lambda () (interactive)
		 (org-columns-next-allowed-value nil ,i))))

(easy-menu-define org-columns-menu org-columns-map "Org Column Menu"
  '("Column"
    ["Edit property" org-columns-edit-value t]
    ["Next allowed value" org-columns-next-allowed-value t]
    ["Previous allowed value" org-columns-previous-allowed-value t]
    ["Show full value" org-columns-show-value t]
    ["Edit allowed values" org-columns-edit-allowed t]
    "--"
    ["Edit column attributes" org-columns-edit-attributes t]
    ["Increase column width" org-columns-widen t]
    ["Decrease column width" org-columns-narrow t]
    "--"
    ["Move column right" org-columns-move-right t]
    ["Move column left" org-columns-move-left t]
    ["Add column" org-columns-new t]
    ["Delete column" org-columns-delete t]
    "--"
    ["CONTENTS" org-columns-content t]
    ["OVERVIEW" org-overview t]
    ["Refresh columns display" org-columns-redo t]
    "--"
    ["Open link" org-columns-open-link t]
    "--"
    ["Quit" org-columns-quit t]))

(defun org-columns--displayed-value (property value)
  "Return displayed value for PROPERTY in current entry.

VALUE is the real value of the property, as a string.

This function assumes `org-columns-current-fmt-compiled' is
initialized."
  (pcase (assoc-string property org-columns-current-fmt-compiled t)
    (`(,_ ,_ ,_ ,_ ,fmt ,printf ,_)
     (cond
      ((and (functionp org-columns-modify-value-for-display-function)
	    (funcall
	     org-columns-modify-value-for-display-function
	     (nth 1 (assoc-string property org-columns-current-fmt-compiled t))
	     value)))
      ((equal (upcase property) "ITEM")
       (concat (make-string (1- (org-current-level))
			    (if org-hide-leading-stars ?\s ?*))
	       "* "
	       (org-columns-compact-links value)))
      (printf (org-columns-number-to-string
	       (org-columns-string-to-number value fmt) fmt printf))
      (value)))))

(defun org-columns--collect-values (&optional agenda)
  "Collect values for columns on the current line.

When optional argument AGENDA is non-nil, assume the value is
meant for the agenda, i.e., caller is `org-agenda-columns'.

Return a list of triplets (PROPERTY VALUE DISPLAYED) suitable for
`org-columns--display-here'.

This function assumes `org-columns-current-fmt-compiled' is
initialized."
  (mapcar
   (lambda (spec)
     (let* ((p (car spec))
	    (v (or (cdr (assoc-string
			 p (get-text-property (point) 'org-summaries) t))
		   (org-entry-get (point) p 'selective t)
		   (and agenda
			;; Effort property is not defined.  Try to use
			;; appointment duration.
			org-agenda-columns-add-appointments-to-effort-sum
			(string= (upcase p) (upcase org-effort-property))
			(get-text-property (point) 'duration)
			(org-propertize
			 (org-minutes-to-clocksum-string
			  (get-text-property (point) 'duration))
			 'face 'org-warning))
		   "")))
       (list p v (org-columns--displayed-value p v))))
   org-columns-current-fmt-compiled))

(defun org-columns--autowidth-alist (cache)
  "Derive the maximum column widths from the format and the cache.
Return an alist (PROPERTY . WIDTH), with PROPERTY as a string and
WIDTH as an integer greater than 0."
  (mapcar
   (lambda (spec)
     (pcase spec
       (`(,property ,name ,width . ,_)
	(if width (cons property width)
	  ;; No width is specified in the columns format.  Compute it
	  ;; by checking all possible values for PROPERTY.
	  (let ((width (length name)))
	    (dolist (entry cache (cons property width))
	      (let ((value (nth 2 (assoc-string property (cdr entry) t))))
		(setq width (max (length value) width)))))))))
   org-columns-current-fmt-compiled))

(defun org-columns-new-overlay (beg end &optional string face)
  "Create a new column overlay and add it to the list."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face (or face 'secondary-selection))
    (org-overlay-display ov string face)
    (push ov org-columns-overlays)
    ov))

(defun org-columns--display-here (columns &optional dateline)
  "Overlay the current line with column display.
COLUMNS is an alist (PROPERTY VALUE DISPLAYED).  Optional
argument DATELINE is non-nil when the face used should be
`org-agenda-column-dateline'."
  (save-excursion
    (beginning-of-line)
    (let* ((level-face (and (looking-at "\\(\\**\\)\\(\\* \\)")
			    (org-get-level-face 2)))
	   (ref-face (or level-face
			 (and (eq major-mode 'org-agenda-mode)
			      (org-get-at-bol 'face))
			 'default))
	   (color (list :foreground (face-attribute ref-face :foreground)))
	   (font (list :height (face-attribute 'default :height)
		       :family (face-attribute 'default :family)))
	   (face (list color font 'org-column ref-face))
	   (face1 (list color font 'org-agenda-column-dateline ref-face)))
      ;; Each column is an overlay on top of a character.  So there has
      ;; to be at least as many characters available on the line as
      ;; columns to display.
      (let ((columns (length org-columns-current-fmt-compiled))
	    (chars (- (line-end-position) (line-beginning-position))))
	(when (> columns chars)
	  (save-excursion
	    (end-of-line)
	    (let ((inhibit-read-only t))
	      (insert (make-string (- columns chars) ?\s))))))
      ;; Display columns.  Create and install the overlay for the
      ;; current column on the next character.
      (let ((limit (+ (- (length columns) 1) (line-beginning-position))))
	(dolist (column columns)
	  (pcase column
	    (`(,property ,original ,value)
	     (let* ((width
		     (cdr
		      (assoc-string property org-columns-current-maxwidths t)))
		    (fmt (format (if (= (point) limit) "%%-%d.%ds |"
				   "%%-%d.%ds | ")
				 width width))
		    (text
		     (format
		      fmt
		      (let ((v (org-columns-add-ellipses value width)))
			(pcase (upcase property)
			  ("PRIORITY"
			   (propertize v 'face (org-get-priority-face original)))
			  ("TAGS"
			   (if (not org-tags-special-faces-re)
			       (propertize v 'face 'org-tag)
			     (replace-regexp-in-string
			      org-tags-special-faces-re
			      (lambda (m)
				(propertize m 'face (org-get-tag-face m)))
			      v nil nil 1)))
			  ("TODO"
			   (propertize v 'face (org-get-todo-face original)))
			  (_ v)))))
		    (ov (org-columns-new-overlay
			 (point) (1+ (point)) text (if dateline face1 face))))
	       (overlay-put ov 'keymap org-columns-map)
	       (overlay-put ov 'org-columns-key property)
	       (overlay-put ov 'org-columns-value original)
	       (overlay-put ov 'org-columns-value-modified value)
	       (overlay-put ov 'org-columns-format fmt)
	       (overlay-put ov 'line-prefix "")
	       (overlay-put ov 'wrap-prefix "")
	       (forward-char))))))
      ;; Make the rest of the line disappear.
      (let ((ov (org-columns-new-overlay (point) (line-end-position))))
	(overlay-put ov 'invisible t)
	(overlay-put ov 'keymap org-columns-map)
	(overlay-put ov 'line-prefix "")
	(overlay-put ov 'wrap-prefix ""))
      (let ((ov (make-overlay (1- (line-end-position))
			      (line-beginning-position 2))))
	(overlay-put ov 'keymap org-columns-map)
	(push ov org-columns-overlays))
      (org-with-silent-modifications
       (let ((inhibit-read-only t))
	 (put-text-property
	  (line-end-position 0)
	  (line-beginning-position 2)
	  'read-only
	  (substitute-command-keys
	   "Type \\<org-columns-map>\\[org-columns-edit-value] \
to edit property")))))))

(defun org-columns-add-ellipses (string width)
  "Truncate STRING with WIDTH characters, with ellipses."
  (cond
   ((<= (length string) width) string)
   ((<= width (length org-columns-ellipses))
    (substring org-columns-ellipses 0 width))
   (t (concat (substring string 0 (- width (length org-columns-ellipses)))
	      org-columns-ellipses))))

(defvar org-columns-full-header-line-format nil
  "The full header line format, will be shifted by horizontal scrolling." )
(defvar org-previous-header-line-format nil
  "The header line format before column view was turned on.")
(defvar org-columns-inhibit-recalculation nil
  "Inhibit recomputing of columns on column view startup.")
(defvar org-columns-flyspell-was-active nil
  "Remember the state of `flyspell-mode' before column view.
Flyspell-mode can cause problems in columns view, so it is turned off
for the duration of the command.")

(defvar header-line-format)
(defvar org-columns-previous-hscroll 0)

(defun org-columns--display-here-title ()
  "Overlay the newline before the current line with the table title."
  (interactive)
  (let ((title ""))
    (dolist (column org-columns-current-fmt-compiled)
      (pcase column
	(`(,property ,name . ,_)
	 (let* ((width
		 (cdr (assoc-string property org-columns-current-maxwidths t)))
		(fmt (format "%%-%d.%ds | " width width)))
	   (setq title (concat title (format fmt (or name property))))))))
    (setq-local org-previous-header-line-format header-line-format)
    (setq org-columns-full-header-line-format
	  (concat
	   (org-add-props " " nil 'display '(space :align-to 0))
	   (org-add-props (substring title 0 -1) nil 'face 'org-column-title)))
    (setq org-columns-previous-hscroll -1)
    (org-add-hook 'post-command-hook 'org-columns-hscoll-title nil 'local)))

(defun org-columns-hscoll-title ()
  "Set the `header-line-format' so that it scrolls along with the table."
  (sit-for .0001) ; need to force a redisplay to update window-hscroll
  (when (not (= (window-hscroll) org-columns-previous-hscroll))
    (setq header-line-format
	  (concat (substring org-columns-full-header-line-format 0 1)
		  (substring org-columns-full-header-line-format
			     (1+ (window-hscroll))))
	  org-columns-previous-hscroll (window-hscroll))
    (force-mode-line-update)))

(defvar org-colview-initial-truncate-line-value nil
  "Remember the value of `truncate-lines' across colview.")

;;;###autoload
(defun org-columns-remove-overlays ()
  "Remove all currently active column overlays."
  (interactive)
  (when (marker-buffer org-columns-begin-marker)
    (with-current-buffer (marker-buffer org-columns-begin-marker)
      (when (local-variable-p 'org-previous-header-line-format)
	(setq header-line-format org-previous-header-line-format)
	(kill-local-variable 'org-previous-header-line-format)
	(remove-hook 'post-command-hook 'org-columns-hscoll-title 'local))
      (move-marker org-columns-begin-marker nil)
      (move-marker org-columns-top-level-marker nil)
      (org-with-silent-modifications
       (mapc 'delete-overlay org-columns-overlays)
       (setq org-columns-overlays nil)
       (let ((inhibit-read-only t))
	 (remove-text-properties (point-min) (point-max) '(read-only t))))
      (when org-columns-flyspell-was-active
	(flyspell-mode 1))
      (when (local-variable-p 'org-colview-initial-truncate-line-value)
	(setq truncate-lines org-colview-initial-truncate-line-value)))))

(defun org-columns-compact-links (s)
  "Replace [[link][desc]] with [desc] or [link]."
  (while (string-match org-bracket-link-regexp s)
    (setq s (replace-match
	     (concat "[" (match-string (if (match-end 3) 3 1) s) "]")
	     t t s)))
  s)

(defun org-columns-show-value ()
  "Show the full value of the property."
  (interactive)
  (let ((value (get-char-property (point) 'org-columns-value)))
    (message "Value is: %s" (or value ""))))

(defvar org-agenda-columns-active) ;; defined in org-agenda.el

(defun org-columns-quit ()
  "Remove the column overlays and in this way exit column editing."
  (interactive)
  (org-with-silent-modifications
   (org-columns-remove-overlays)
   (let ((inhibit-read-only t))
     (remove-text-properties (point-min) (point-max) '(read-only t))))
  (when (eq major-mode 'org-agenda-mode)
    (setq org-agenda-columns-active nil)
    (message
     "Modification not yet reflected in Agenda buffer, use `r' to refresh")))

(defun org-columns-check-computed ()
  "Check if this column value is computed.
If yes, throw an error indicating that changing it does not make sense."
  (let ((val (get-char-property (point) 'org-columns-value)))
    (when (and (stringp val)
	       (get-char-property 0 'org-computed val))
      (error "This value is computed from the entry's children"))))

(defun org-columns-todo (&optional _arg)
  "Change the TODO state during column view."
  (interactive "P")
  (org-columns-edit-value "TODO"))

(defun org-columns-set-tags-or-toggle (&optional _arg)
  "Toggle checkbox at point, or set tags for current headline."
  (interactive "P")
  (if (string-match "\\`\\[[ xX-]\\]\\'"
		    (get-char-property (point) 'org-columns-value))
      (org-columns-next-allowed-value)
    (org-columns-edit-value "TAGS")))

(defvar org-agenda-overriding-columns-format nil
  "When set, overrides any other format definition for the agenda.
Don't set this, this is meant for dynamic scoping.")

(defun org-columns-edit-value (&optional key)
  "Edit the value of the property at point in column view.
Where possible, use the standard interface for changing this line."
  (interactive)
  (org-columns-check-computed)
  (let* ((col (current-column))
	 (key (or key (get-char-property (point) 'org-columns-key)))
	 (value (get-char-property (point) 'org-columns-value))
	 (bol (point-at-bol)) (eol (point-at-eol))
	 (pom (or (get-text-property bol 'org-hd-marker)
		  (point)))	     ; keep despite of compiler waring
	 (org-columns--time (float-time (current-time)))
	 nval eval allowed)
    (cond
     ((equal key "CLOCKSUM")
      (error "This special column cannot be edited"))
     ((equal key "ITEM")
      (setq eval `(org-with-point-at ,pom
		    (org-edit-headline))))
     ((equal key "TODO")
      (setq eval `(org-with-point-at ,pom
		    (call-interactively 'org-todo))))
     ((equal key "PRIORITY")
      (setq eval `(org-with-point-at ,pom
		    (call-interactively 'org-priority))))
     ((equal key "TAGS")
      (setq eval `(org-with-point-at ,pom
		    (let ((org-fast-tag-selection-single-key
			   (if (eq org-fast-tag-selection-single-key 'expert)
			       t org-fast-tag-selection-single-key)))
		      (call-interactively 'org-set-tags)))))
     ((equal key "DEADLINE")
      (setq eval `(org-with-point-at ,pom
		    (call-interactively 'org-deadline))))
     ((equal key "SCHEDULED")
      (setq eval `(org-with-point-at ,pom
		    (call-interactively 'org-schedule))))
     ((equal key "BEAMER_env")
      (setq eval `(org-with-point-at ,pom
		    (call-interactively 'org-beamer-select-environment))))
     (t
      (setq allowed (org-property-get-allowed-values pom key 'table))
      (if allowed
	  (setq nval (completing-read
		      "Value: " allowed nil
		      (not (get-text-property 0 'org-unrestricted
					      (caar allowed)))))
	(setq nval (read-string "Edit: " value)))
      (setq nval (org-trim nval))
      (when (not (equal nval value))
	(setq eval `(org-entry-put ,pom ,key ,nval)))))
    (when eval
      (cond
       ((equal major-mode 'org-agenda-mode)
	(org-columns-eval eval)
	;; The following let preserves the current format, and makes sure
	;; that in only a single file things need to be updated.
	(let* ((org-agenda-overriding-columns-format org-columns-current-fmt)
	       (buffer (marker-buffer pom))
	       (org-agenda-contributing-files
		(list (with-current-buffer buffer
			(buffer-file-name (buffer-base-buffer))))))
	  (org-agenda-columns)))
       (t
	(let ((inhibit-read-only t))
	  (org-with-silent-modifications
	   (remove-text-properties
	    (max (point-min) (1- bol)) eol '(read-only t)))
	  (org-columns-eval eval))
	(org-move-to-column col)
	(org-columns-update key))))))

(defun org-edit-headline () ; FIXME: this is not columns specific.  Make interactive?????  Use from agenda????
  "Edit the current headline, the part without TODO keyword, TAGS."
  (org-back-to-heading)
  (when (looking-at org-todo-line-regexp)
    (let ((pos (point))
	  (pre (buffer-substring (match-beginning 0) (match-beginning 3)))
	  (txt (match-string 3))
	  (post "")
	  txt2)
      (if (string-match (org-re "[ \t]+:[[:alnum:]:_@#%]+:[ \t]*$") txt)
	  (setq post (match-string 0 txt)
		txt (substring txt 0 (match-beginning 0))))
      (setq txt2 (read-string "Edit: " txt))
      (when (not (equal txt txt2))
	(goto-char pos)
	(insert pre txt2 post)
	(delete-region (point) (point-at-eol))
	(org-set-tags nil t)))))

(defun org-columns-edit-allowed ()
  "Edit the list of allowed values for the current property."
  (interactive)
  (let* ((pom (or (org-get-at-bol 'org-marker)
		  (org-get-at-bol 'org-hd-marker)
		  (point)))
	 (key (get-char-property (point) 'org-columns-key))
	 (key1 (concat key "_ALL"))
	 (allowed (org-entry-get pom key1 t))
	 nval)
    ;; FIXME: Cover editing TODO, TAGS etc in-buffer settings.????
    ;; FIXME: Write back to #+PROPERTY setting if that is needed.
    (setq nval (read-string "Allowed: " allowed))
    (org-entry-put
     (cond ((marker-position org-entry-property-inherited-from)
	    org-entry-property-inherited-from)
	   ((marker-position org-columns-top-level-marker)
	    org-columns-top-level-marker)
	   (t pom))
     key1 nval)))

(defun org-columns-eval (form)
  (let (hidep)
    (save-excursion
      (beginning-of-line 1)
      ;; `next-line' is needed here, because it skips invisible line.
      (condition-case nil (org-no-warnings (next-line 1)) (error nil))
      (setq hidep (org-at-heading-p 1)))
    (eval form)
    (and hidep (outline-hide-entry))))

(defun org-columns-previous-allowed-value ()
  "Switch to the previous allowed value for this column."
  (interactive)
  (org-columns-next-allowed-value t))

(defun org-columns-next-allowed-value (&optional previous nth)
  "Switch to the next allowed value for this column.
When PREVIOUS is set, go to the previous value.  When NTH is
an integer, select that value."
  (interactive)
  (org-columns-check-computed)
  (let* ((col (current-column))
	 (key (get-char-property (point) 'org-columns-key))
	 (value (get-char-property (point) 'org-columns-value))
	 (bol (point-at-bol)) (eol (point-at-eol))
	 (pom (or (get-text-property bol 'org-hd-marker)
		  (point)))	     ; keep despite of compiler waring
	 (allowed (or (org-property-get-allowed-values pom key)
		      (and (memq
			    (nth 4 (assoc-string key
						 org-columns-current-fmt-compiled
						 t))
			    '(checkbox checkbox-n-of-m checkbox-percent))
			   '("[ ]" "[X]"))
		      (org-colview-construct-allowed-dates value)))
	 nval)
    (when (integerp nth)
      (setq nth (1- nth))
      (if (= nth -1) (setq nth 9)))
    (when (equal key "ITEM")
      (error "Cannot edit item headline from here"))
    (unless (or allowed (member key '("SCHEDULED" "DEADLINE" "CLOCKSUM")))
      (error "Allowed values for this property have not been defined"))
    (if (member key '("SCHEDULED" "DEADLINE" "CLOCKSUM"))
	(setq nval (if previous 'earlier 'later))
      (if previous (setq allowed (reverse allowed)))
      (cond
       (nth
	(setq nval (nth nth allowed))
	(if (not nval)
	    (error "There are only %d allowed values for property `%s'"
		   (length allowed) key)))
       ((member value allowed)
	(setq nval (or (car (cdr (member value allowed)))
		       (car allowed)))
	(if (equal nval value)
	    (error "Only one allowed value for this property")))
       (t (setq nval (car allowed)))))
    (cond
     ((equal major-mode 'org-agenda-mode)
      (org-columns-eval `(org-entry-put ,pom ,key ,nval))
      ;; The following let preserves the current format, and makes sure
      ;; that in only a single file things need to be updated.
      (let* ((org-agenda-overriding-columns-format org-columns-current-fmt)
	     (buffer (marker-buffer pom))
	     (org-agenda-contributing-files
	      (list (with-current-buffer buffer
		      (buffer-file-name (buffer-base-buffer))))))
	(org-agenda-columns)))
     (t
      (let ((inhibit-read-only t))
	(remove-text-properties (max (1- bol) (point-min)) eol '(read-only t))
	(org-columns-eval `(org-entry-put ,pom ,key ,nval)))
      (org-move-to-column col)
      (org-columns-update key)))))

(defun org-colview-construct-allowed-dates (s)
  "Construct a list of three dates around the date in S.
This respects the format of the time stamp in S, active or non-active,
and also including time or not.  S must be just a time stamp, no text
around it."
  (when (and s (string-match (concat "^" org-ts-regexp3 "$") s))
    (let* ((time (org-parse-time-string s 'nodefaults))
	   (active (equal (string-to-char s) ?<))
	   (fmt (funcall (if (nth 1 time) 'cdr 'car) org-time-stamp-formats))
	   time-before time-after)
      (unless active (setq fmt (concat "[" (substring fmt 1 -1) "]")))
      (setf (car time) (or (car time) 0))
      (setf (nth 1 time) (or (nth 1 time) 0))
      (setf (nth 2 time) (or (nth 2 time) 0))
      (setq time-before (copy-sequence time))
      (setq time-after (copy-sequence time))
      (setf (nth 3 time-before) (1- (nth 3 time)))
      (setf (nth 3 time-after) (1+ (nth 3 time)))
      (mapcar (lambda (x) (format-time-string fmt (apply 'encode-time x)))
	      (list time-before time time-after)))))

(defun org-columns-open-link (&optional arg)
  (interactive "P")
  (let ((value (get-char-property (point) 'org-columns-value)))
    (org-open-link-from-string value arg)))

;;;###autoload
(defun org-columns-get-format-and-top-level ()
  (let ((fmt (org-columns-get-format)))
    (org-columns-goto-top-level)
    fmt))

(defun org-columns-get-format (&optional fmt-string)
  (interactive)
  (let (fmt-as-property fmt)
    (when (condition-case nil (org-back-to-heading) (error nil))
      (setq fmt-as-property (org-entry-get nil "COLUMNS" t)))
    (setq fmt (or fmt-string fmt-as-property org-columns-default-format))
    (setq-local org-columns-current-fmt fmt)
    (org-columns-compile-format fmt)
    fmt))

(defun org-columns-goto-top-level ()
  "Move to the beginning of the column view area.
Also sets `org-columns-top-level-marker' to the new position."
  (goto-char
   (move-marker
    org-columns-top-level-marker
    (cond ((org-before-first-heading-p) (point-min))
	  ((org-entry-get nil "COLUMNS" t) org-entry-property-inherited-from)
	  (t (org-back-to-heading) (point))))))

;;;###autoload
(defun org-columns (&optional global columns-fmt-string)
  "Turn on column view on an Org mode file.

Column view applies to the whole buffer if point is before the
first headline.  Otherwise, it applies to the first ancestor
setting \"COLUMNS\" property.  If there is none, it defaults to
the current headline.  With a \\[universal-argument] prefix \
argument, turn on column
view for the whole buffer unconditionally.

When COLUMNS-FMT-STRING is non-nil, use it as the column format."
  (interactive "P")
  (org-columns-remove-overlays)
  (move-marker org-columns-begin-marker (point))
  (org-columns-goto-top-level)
  ;; Initialize `org-columns-current-fmt' and
  ;; `org-columns-current-fmt-compiled'.
  (let ((org-columns--time (float-time (current-time))))
    (org-columns-get-format columns-fmt-string)
    (unless org-columns-inhibit-recalculation (org-columns-compute-all))
    (save-excursion
      (save-restriction
	(when (and (not global) (org-at-heading-p))
	  (narrow-to-region (point) (org-end-of-subtree t t)))
	(when (assoc-string "CLOCKSUM" org-columns-current-fmt-compiled t)
	  (org-clock-sum))
	(when (assoc-string "CLOCKSUM_T" org-columns-current-fmt-compiled t)
	  (org-clock-sum-today))
	(let ((cache
	       ;; Collect contents of columns ahead of time so as to
	       ;; compute their maximum width.
	       (org-map-entries
		(lambda () (cons (point) (org-columns--collect-values)))
		nil nil (and org-columns-skip-archived-trees 'archive))))
	  (when cache
	    (setq-local org-columns-current-maxwidths
			(org-columns--autowidth-alist cache))
	    (org-columns--display-here-title)
	    (when (setq-local org-columns-flyspell-was-active
			      (org-bound-and-true-p flyspell-mode))
	      (flyspell-mode 0))
	    (unless (local-variable-p 'org-colview-initial-truncate-line-value)
	      (setq-local org-colview-initial-truncate-line-value
			  truncate-lines))
	    (setq truncate-lines t)
	    (dolist (entry cache)
	      (goto-char (car entry))
	      (org-columns--display-here (cdr entry)))))))))

(defconst org-columns-compile-map
  '(("none" none +)
    (":" add_times +)
    ("+" add_numbers +)
    ("$" currency +)
    ("X" checkbox +)
    ("X/" checkbox-n-of-m +)
    ("X%" checkbox-percent +)
    ("max" max_numbers max)
    ("min" min_numbers min)
    ("mean" mean_numbers (lambda (&rest x) (/ (apply '+ x) (float (length x)))))
    (":max" max_times max)
    (":min" min_times min)
    (":mean" mean_times (lambda (&rest x) (/ (apply '+ x) (float (length x)))))
    ("@min" min_age min)
    ("@max" max_age max)
    ("@mean" mean_age (lambda (&rest x) (/ (apply '+ x) (float (length x)))))
    ("est+" estimate org-estimate-combine))
  "Operator <-> format,function map.
Used to compile/uncompile columns format and completing read in
interactive function `org-columns-new'.

operator    string used in #+COLUMNS definition describing the
	    summary type
format      symbol describing summary type selected interactively in
	    `org-columns-new' and internally in
	    `org-columns-number-to-string' and
	    `org-columns-string-to-number'
function    called with a list of values as argument to calculate
	    the summary value")

(defun org-columns-new (&optional prop title width _op fmt fun &rest _rest)
  "Insert a new column, to the left of the current column."
  (interactive)
  (let ((editp (and prop
		    (assoc-string prop org-columns-current-fmt-compiled t)))
	cell)
    (setq prop (completing-read
		"Property: " (mapcar #'list (org-buffer-property-keys t nil t))
		nil nil prop))
    (setq title (read-string (concat "Column title [" prop "]: ") (or title prop)))
    (setq width (read-string "Column width: " (if width (number-to-string width))))
    (if (string-match "\\S-" width)
	(setq width (string-to-number width))
      (setq width nil))
    (setq fmt (completing-read
	       "Summary [none]: "
	       (mapcar (lambda (x) (list (symbol-name (cadr x))))
		       org-columns-compile-map)
	       nil t))
    (setq fmt (intern fmt)
	  fun (cdr (assoc fmt (mapcar 'cdr org-columns-compile-map))))
    (if (eq fmt 'none) (setq fmt nil))
    (if editp
	(progn
	  (setcar editp prop)
	  (setcdr editp (list title width nil fmt nil fun)))
      (setq cell (nthcdr (1- (current-column))
			 org-columns-current-fmt-compiled))
      (setcdr cell (cons (list prop title width nil fmt nil
			       (car fun) (cadr fun))
			 (cdr cell))))
    (org-columns-store-format)
    (org-columns-redo)))

(defun org-columns-delete ()
  "Delete the column at point from columns view."
  (interactive)
  (let* ((n (current-column))
	 (title (nth 1 (nth n org-columns-current-fmt-compiled))))
    (when (y-or-n-p
	   (format "Are you sure you want to remove column \"%s\"? " title))
      (setq org-columns-current-fmt-compiled
	    (delq (nth n org-columns-current-fmt-compiled)
		  org-columns-current-fmt-compiled))
      (org-columns-store-format)
      (org-columns-redo)
      (if (>= (current-column) (length org-columns-current-fmt-compiled))
	  (backward-char 1)))))

(defun org-columns-edit-attributes ()
  "Edit the attributes of the current column."
  (interactive)
  (let* ((n (current-column))
	 (info (nth n org-columns-current-fmt-compiled)))
    (apply 'org-columns-new info)))

(defun org-columns-widen (arg)
  "Make the column wider by ARG characters."
  (interactive "p")
  (let* ((n (current-column))
	 (entry (nth n org-columns-current-fmt-compiled))
	 (width (or (nth 2 entry)
		    (cdr (assoc-string (car entry)
				       org-columns-current-maxwidths
				       t)))))
    (setq width (max 1 (+ width arg)))
    (setcar (nthcdr 2 entry) width)
    (org-columns-store-format)
    (org-columns-redo)))

(defun org-columns-narrow (arg)
  "Make the column narrower by ARG characters."
  (interactive "p")
  (org-columns-widen (- arg)))

(defun org-columns-move-right ()
  "Swap this column with the one to the right."
  (interactive)
  (let* ((n (current-column))
	 (cell (nthcdr n org-columns-current-fmt-compiled))
	 e)
    (when (>= n (1- (length org-columns-current-fmt-compiled)))
      (error "Cannot shift this column further to the right"))
    (setq e (car cell))
    (setcar cell (car (cdr cell)))
    (setcdr cell (cons e (cdr (cdr cell))))
    (org-columns-store-format)
    (org-columns-redo)
    (forward-char 1)))

(defun org-columns-move-left ()
  "Swap this column with the one to the left."
  (interactive)
  (let* ((n (current-column)))
    (when (= n 0)
      (error "Cannot shift this column further to the left"))
    (backward-char 1)
    (org-columns-move-right)
    (backward-char 1)))

(defun org-columns-store-format ()
  "Store the text version of the current columns format in appropriate place.
This is either in the COLUMNS property of the node starting the current column
display, or in the #+COLUMNS line of the current buffer."
  (let (fmt (cnt 0))
    (setq fmt (org-columns-uncompile-format org-columns-current-fmt-compiled))
    (setq-local org-columns-current-fmt fmt)
    (if (marker-position org-columns-top-level-marker)
	(save-excursion
	  (goto-char org-columns-top-level-marker)
	  (if (and (org-at-heading-p)
		   (org-entry-get nil "COLUMNS"))
	      (org-entry-put nil "COLUMNS" fmt)
	    (goto-char (point-min))
	    ;; Overwrite all #+COLUMNS lines....
	    (while (re-search-forward "^[ \t]*#\\+COLUMNS:.*" nil t)
	      (setq cnt (1+ cnt))
	      (replace-match (concat "#+COLUMNS: " fmt) t t))
	    (unless (> cnt 0)
	      (goto-char (point-min))
	      (or (org-at-heading-p t) (outline-next-heading))
	      (let ((inhibit-read-only t))
		(insert-before-markers "#+COLUMNS: " fmt "\n")))
	    (setq-local org-columns-default-format fmt))))))

(defun org-columns-compute-all ()
  "Compute all columns that have operators defined."
  (org-with-silent-modifications
   (remove-text-properties (point-min) (point-max) '(org-summaries t)))
  (let ((org-columns--time (float-time (current-time))))
    (dolist (spec org-columns-current-fmt-compiled)
      (pcase spec
	(`(,property ,_ ,_ ,operator . ,_)
	 (when operator (save-excursion (org-columns-compute property))))))))

(defun org-columns-update (property)
  "Recompute PROPERTY, and update the columns display for it."
  (org-columns-compute property)
  (let (fmt val pos)
    (save-excursion
      (mapc (lambda (ov)
	      (when (equal (overlay-get ov 'org-columns-key) property)
		(setq pos (overlay-start ov))
		(goto-char pos)
		(when (setq val (cdr (assoc-string
				      property
				      (get-text-property
				       (point-at-bol) 'org-summaries)
				      t)))
		  (setq fmt (overlay-get ov 'org-columns-format))
		  (overlay-put ov 'org-columns-value val)
		  (overlay-put ov 'display (format fmt val)))))
	    org-columns-overlays))))

(defvar org-inlinetask-min-level
  (if (featurep 'org-inlinetask) org-inlinetask-min-level 15))

;;;###autoload
(defun org-columns-compute (property)
  "Sum the values of property PROPERTY hierarchically, for the entire buffer."
  (interactive)
  (let* ((re org-outline-regexp-bol)
	 (lmax 30)		    ; Does anyone use deeper levels???
	 (lvals (make-vector lmax nil))
	 (lflag (make-vector lmax nil))
	 (level 0)
	 (ass (assoc-string property org-columns-current-fmt-compiled t))
	 (format (nth 4 ass))
	 (printf (nth 5 ass))
	 (fun (nth 6 ass))
	 (beg org-columns-top-level-marker)
	 (inminlevel org-inlinetask-min-level)
	 (last-level org-inlinetask-min-level)
	 val valflag flag end sumpos sum-alist sum str str1 useval)
    (save-excursion
      ;; Find the region to compute
      (goto-char beg)
      (setq end (condition-case nil (org-end-of-subtree t) (error (point-max))))
      (goto-char end)
      ;; Walk the tree from the back and do the computations
      (while (re-search-backward re beg t)
	(setq sumpos (match-beginning 0)
	      last-level (if (not (or (zerop level) (eq level inminlevel)))
			     level last-level)
	      level (org-outline-level)
	      val (org-entry-get nil property)
	      valflag (and val (string-match "\\S-" val)))
	(cond
	 ((< level last-level)
	  ;; Put the sum of lower levels here as a property.  If
	  ;; values are estimate, use an appropriate sum function.
	  (setq sum (funcall
		     (if (eq fun 'org-estimate-combine) #'org-estimate-combine
		       #'+)
		     (if (and (/= last-level inminlevel)
			      (aref lvals last-level))
			 (apply fun (aref lvals last-level)) 0)
		     (if (aref lvals inminlevel)
			 (apply fun (aref lvals inminlevel)) 0))
		flag (or (aref lflag last-level) ; any valid entries from children?
			 (aref lflag inminlevel)) ; or inline tasks?
		str (org-columns-number-to-string sum format printf)
		str1 (org-add-props (copy-sequence str) nil 'org-computed t 'face 'bold)
		useval (if flag str1 (if valflag val ""))
		sum-alist (get-text-property sumpos 'org-summaries))
	  (let ((old (assoc-string property sum-alist t)))
	    (if old (setcdr old useval)
	      (push (cons property useval) sum-alist)
	      (org-with-silent-modifications
	       (add-text-properties sumpos (1+ sumpos)
				    (list 'org-summaries sum-alist)))))
	  (when (and val (not (equal val (if flag str val))))
	    (org-entry-put nil property (if flag str val)))
	  ;; add current to current level accumulator
	  (when (or flag valflag)
	    (push (if flag sum (org-columns-string-to-number val format))
		  (aref lvals level))
	    (aset lflag level t))
	  ;; clear accumulators for deeper levels
	  (loop for l from (1+ level) to (1- lmax) do
		(aset lvals l nil)
		(aset lflag l nil)))
	 ((>= level last-level)
	  ;; add what we have here to the accumulator for this level
	  (when valflag
	    (push (org-columns-string-to-number val format) (aref lvals level))
	    (aset lflag level t)))
	 (t (error "This should not happen")))))))

(defun org-columns-redo ()
  "Construct the column display again."
  (interactive)
  (message "Recomputing columns...")
  (let ((line (org-current-line))
	(col (current-column)))
    (save-excursion
      (if (marker-position org-columns-begin-marker)
	  (goto-char org-columns-begin-marker))
      (org-columns-remove-overlays)
      (if (derived-mode-p 'org-mode)
	  (call-interactively 'org-columns)
	(org-agenda-redo)
	(call-interactively 'org-agenda-columns)))
    (org-goto-line line)
    (move-to-column col))
  (message "Recomputing columns...done"))

;;;###autoload
(defun org-columns-number-to-string (n fmt &optional printf)
  "Convert a computed column number N to a string value.
FMT is a symbol describing the summary type.  Optional argument
PRINTF, when non-nil, is a format string used to print N."
  (cond
   ((eq fmt 'estimate) (org-estimate-print n printf))
   ((not (numberp n)) "")
   ((memq fmt '(add_times max_times min_times mean_times))
    (org-hours-to-clocksum-string n))
   ((eq fmt 'checkbox)
    (cond ((= n (floor n)) "[X]")
	  ((> n 1.) "[-]")
	  (t "[ ]")))
   ((memq fmt '(checkbox-n-of-m checkbox-percent))
    (let* ((n1 (floor n))
	   (n2 (+ (floor (+ .5 (* 1000000 (- n n1)))) n1)))
      (cond ((not (eq fmt 'checkbox-percent)) (format "[%d/%d]" n1 n2))
	    ((or (= n1 0) (= n2 0)) "[0%]")
	    (t (format "[%d%%]" (round (* 100.0 n1) n2))))))
   (printf (format printf n))
   ((eq fmt 'currency) (format "%.2f" n))
   ((memq fmt '(min_age max_age mean_age))
    (format-seconds "%dd %.2hh %mm %ss" n))
   (t (number-to-string n))))

(defun org-columns-string-to-number (s fmt)
  "Convert a column value S to a number.
FMT is a symbol describing the summary type."
  (cond
   ((not s) nil)
   ((memq fmt '(min_age max_age mean_age))
    (cond
     ((string= s "") org-columns--time)
     ((string-match "\\`\\(?: *\\([0-9]+\\)d\\)?\\(?: *\\([0-9]+\\)h\\)?\
\\(?: *\\([0-9]+\\)m\\)?\\(?: *\\([0-9]+\\)s\\)?\\'" s)
      (let ((d (if (match-end 1) (string-to-number (match-string 1 s)) 0))
	    (h (if (match-end 2) (string-to-number (match-string 2 s)) 0))
	    (m (if (match-end 3) (string-to-number (match-string 3 s)) 0))
	    (s (if (match-end 4) (string-to-number (match-string 4 s)) 0)))
	(+ (* 60 (+ (* 60 (+ (* 24 d) h)) m)) s)))
     (t
      (- org-columns--time
	 (float-time (apply #'encode-time (org-parse-time-string s)))))))
   ((string-match-p ":" s)		;Interpret HH:MM:SS.
    (let ((sum 0.0))
      (dolist (n (nreverse (split-string s ":")) sum)
	(setq sum (+ (string-to-number n) (/ sum 60))))))
   ((memq fmt '(checkbox checkbox-n-of-m checkbox-percent))
    (if (equal s "[X]") 1. 0.000001))
   ((eq fmt 'estimate) (org-string-to-estimate s))
   ((string-match-p org-columns--fractional-duration-re s)
    (let ((s (concat "0:" (org-duration-string-to-minutes s t)))
	  (sum 0.0))
      (dolist (n (nreverse (split-string s ":")) sum)
	(setq sum (+ (string-to-number n) (/ sum 60))))))
   (t (string-to-number s))))

(defun org-columns-uncompile-format (cfmt)
  "Turn the compiled columns format back into a string representation."
  (let ((rtn "") e s prop title op width fmt printf ee map)
    (while (setq e (pop cfmt))
      (setq prop (car e)
	    title (nth 1 e)
	    width (nth 2 e)
	    op (nth 3 e)
	    fmt (nth 4 e)
	    printf (nth 5 e))
      (setq map (copy-sequence org-columns-compile-map))
      (while (setq ee (pop map))
	(if (equal fmt (nth 1 ee))
	    (setq op (car ee) map nil)))
      (if (and op printf) (setq op (concat op ";" printf)))
      (if (equal title prop) (setq title nil))
      (setq s (concat "%" (if width (number-to-string width))
		      prop
		      (if title (concat "(" title ")"))
		      (if op (concat "{" op "}"))))
      (setq rtn (concat rtn " " s)))
    (org-trim rtn)))

(defun org-columns-compile-format (fmt)
  "Turn a column format string FMT into an alist of specifications.

The alist has one entry for each column in the format.  The elements of
that list are:
property     the property
title        the title field for the columns
width        the column width in characters, can be nil for automatic
operator     the operator if any
format       the output format for computed results, derived from operator
printf       a printf format for computed values
fun          the lisp function to compute summary values, derived from operator

This function updates `org-columns-current-fmt-compiled'."
  (setq org-columns-current-fmt-compiled nil)
  (let ((start 0))
    (while (string-match
	    "%\\([0-9]+\\)?\\([[:alnum:]_-]+\\)\\(?:(\\([^)]+\\))\\)?\
\\(?:{\\([^}]+\\)}\\)?\\s-*"
	    fmt start)
      (setq start (match-end 0))
      (let* ((width (and (match-end 1) (string-to-number (match-string 1 fmt))))
	     (prop (match-string 2 fmt))
	     (title (or (match-string 3 fmt) prop))
	     (op (match-string 4 fmt))
	     (f nil)
	     (printf nil)
	     (fun '+))
	(when (and op (string-match ";" op))
	  (setq printf (substring op (match-end 0)))
	  (setq op (substring op 0 (match-beginning 0))))
	(let ((op-match (assoc op org-columns-compile-map)))
	  (when op-match
	    (setq f (nth 1 op-match))
	    (setq fun (nth 2 op-match))))
	(push (list prop title width op f printf fun)
	      org-columns-current-fmt-compiled)))
    (setq org-columns-current-fmt-compiled
	  (nreverse org-columns-current-fmt-compiled))))



;;; Dynamic block for Column view

(defun org-columns--capture-view (maxlevel skip-empty format local)
  "Get the column view of the current buffer.

MAXLEVEL sets the level limit.  SKIP-EMPTY tells whether to skip
empty rows, an empty row being one where all the column view
specifiers but ITEM are empty.  FORMAT is a format string for
columns, or nil.  When LOCAL is non-nil, only capture headings in
current subtree.

This function returns a list containing the title row and all
other rows.  Each row is a list of fields, as strings, or
`hline'."
  (org-columns (not local) format)
  (goto-char org-columns-top-level-marker)
  (let ((columns (length org-columns-current-fmt-compiled))
	(has-item (assoc-string "ITEM" org-columns-current-fmt-compiled t))
	table)
    (org-map-entries
     (lambda ()
       (when (get-char-property (point) 'org-columns-key)
	 (let (row)
	   (dotimes (i columns)
	     (let* ((col (+ (line-beginning-position) i))
		    (p (get-char-property col 'org-columns-key)))
	       (push (org-quote-vert
		      (get-char-property col
					 (if (string= (upcase p) "ITEM")
					     'org-columns-value
					   'org-columns-value-modified)))
		     row)))
	   (unless (and skip-empty
			(let ((r (delete-dups (remove "" row))))
			  (or (null r) (and has-item (= (length r) 1)))))
	     (push (cons (org-reduced-level (org-current-level)) (nreverse row))
		   table)))))
     (and maxlevel (format "LEVEL<=%d" maxlevel))
     (and local 'tree)
     'archive 'comment)
    (org-columns-quit)
    ;; Add column titles and a horizontal rule in front of the table.
    (cons (mapcar #'cadr org-columns-current-fmt-compiled)
	  (cons 'hline (nreverse table)))))

(defun org-columns--clean-item (item)
  "Remove sensitive contents from string ITEM.
This includes objects that may not be duplicated within
a document, e.g., a target, or those forbidden in tables, e.g.,
an inline src-block."
  (let ((data (org-element-parse-secondary-string
	       item (org-element-restriction 'headline))))
    (org-element-map data
	'(footnote-reference inline-babel-call inline-src-block target
			     radio-target statistics-cookie)
      #'org-element-extract-element)
    (org-no-properties (org-element-interpret-data data))))

;;;###autoload
(defun org-dblock-write:columnview (params)
  "Write the column view table.
PARAMS is a property list of parameters:

:id       the :ID: property of the entry where the columns view
	  should be built.  When the symbol `local', call locally.
	  When `global' call column view with the cursor at the beginning
	  of the buffer (usually this means that the whole buffer switches
	  to column view).  When \"file:path/to/file.org\", invoke column
	  view at the start of that file.  Otherwise, the ID is located
	  using `org-id-find'.
:hlines   When t, insert a hline before each item.  When a number, insert
	  a hline before each level <= that number.
:indent   When non-nil, indent each ITEM field according to its level.
:vlines   When t, make each column a colgroup to enforce vertical lines.
:maxlevel When set to a number, don't capture headlines below this level.
:skip-empty-rows
	  When t, skip rows where all specifiers other than ITEM are empty.
:width    apply widths specified in columns format using <N> specifiers.
:format   When non-nil, specify the column view format to use."
  (let ((table
	 (let ((id (plist-get params :id))
	       view-file view-pos)
	   (pcase id
	     (`global nil)
	     ((or `local `nil) (setq view-pos (point)))
	     ((and (let id-string (format "%s" id))
		   (guard (string-match "^file:\\(.*\\)" id-string)))
	      (setq view-file (match-string-no-properties 1 id-string))
	      (unless (file-exists-p view-file)
		(user-error "No such file: %S" id-string)))
	     ((and (let idpos (org-find-entry-with-id id)) idpos)
	      (setq view-pos idpos))
	     ((let `(,filename . ,position) (org-id-find id))
	      (setq view-file filename)
	      (setq view-pos position))
	     (_ (user-error "Cannot find entry with :ID: %s" id)))
	   (with-current-buffer (if view-file (get-file-buffer view-file)
				  (current-buffer))
	     (org-with-wide-buffer
	      (when view-pos (goto-char view-pos))
	      (org-columns--capture-view (plist-get params :maxlevel)
					 (plist-get params :skip-empty-rows)
					 (plist-get params :format)
					 view-pos))))))
    (when table
      ;; Prune level information from the table.  Also normalize
      ;; headings: remove stars, add indentation entities, if
      ;; required, and possibly precede some of them with a horizontal
      ;; rule.
      (let ((item-index
	     (let ((p (assoc-string "ITEM" org-columns-current-fmt-compiled t)))
	       (and p (cl-position p
				   org-columns-current-fmt-compiled
				   :test #'equal))))
	    (hlines (plist-get params :hlines))
	    (indent (plist-get params :indent))
	    new-table)
	;; Copy header and first rule.
	(push (pop table) new-table)
	(push (pop table) new-table)
	(dolist (row table (setq table (nreverse new-table)))
	  (let ((level (car row)))
	    (when (and (not (eq (car new-table) 'hline))
		       (or (eq hlines t)
			   (and (numberp hlines) (<= level hlines))))
	      (push 'hline new-table))
	    (when item-index
	      (let ((item (org-columns--clean-item (nth item-index (cdr row)))))
		(setf (nth item-index (cdr row))
		      (if (and indent (> level 1))
			  (concat "\\_" (make-string (* 2 (1- level)) ?\s) item)
			item))))
	    (push (cdr row) new-table))))
      (when (plist-get params :width)
	(setq table
	      (append table
		      (list
		       (mapcar (lambda (spec)
				 (let ((w (nth 2 spec)))
				   (if w (format "<%d>" (max 3 w)) "")))
			       org-columns-current-fmt-compiled)))))
      (when (plist-get params :vlines)
	(setq table
	      (let ((size (length org-columns-current-fmt-compiled)))
		(append (mapcar (lambda (x) (if (eq 'hline x) x (cons "" x)))
				table)
			(list (cons "/" (make-list size "<>")))))))
      (let ((content-lines (org-split-string (plist-get params :content) "\n"))
	    recalc)
	;; Insert affiliated keywords before the table.
	(when content-lines
	  (while (string-match-p "\\`[ \t]*#\\+" (car content-lines))
	    (insert (pop content-lines) "\n")))
	(save-excursion
	  ;; Insert table at point.
	  (insert
	   (mapconcat (lambda (row)
			(if (eq row 'hline) "|-|"
			  (format "|%s|" (mapconcat #'identity row "|"))))
		      table
		      "\n"))
	  ;; Insert TBLFM lines following table.
	  (let ((case-fold-search t))
	    (dolist (line content-lines)
	      (when (string-match-p "\\`[ \t]*#\\+TBLFM:" line)
		(insert "\n" line)
		(unless recalc (setq recalc t))))))
	(when recalc (org-table-recalculate 'all t))
	(org-table-align)))))

;;;###autoload
(defun org-columns-insert-dblock ()
  "Create a dynamic block capturing a column view table."
  (interactive)
  (let ((id (completing-read
	     "Capture columns (local, global, entry with :ID: property) [local]: "
	     (append '(("global") ("local"))
		     (mapcar #'list (org-property-values "ID"))))))
    (org-create-dblock
     (list :name "columnview"
	   :hlines 1
	   :id (cond ((string= id "global") 'global)
		     ((member id '("" "local")) 'local)
		     (id)))))
  (org-update-dblock))

(define-obsolete-function-alias 'org-insert-columns-dblock
  'org-columns-insert-dblock "Org 9.0")



;;; Column view in the agenda

;;;###autoload
(defun org-agenda-columns ()
  "Turn on or update column view in the agenda."
  (interactive)
  (org-columns-remove-overlays)
  (move-marker org-columns-begin-marker (point))
  (let ((org-columns--time (float-time (current-time)))
	(fmt
	 (cond
	  ((org-bound-and-true-p org-agenda-overriding-columns-format))
	  ((let ((m (org-get-at-bol 'org-hd-marker)))
	     (and m
		  (or (org-entry-get m "COLUMNS" t)
		      (with-current-buffer (marker-buffer m)
			org-columns-default-format)))))
	  ((and (local-variable-p 'org-columns-current-fmt)
		org-columns-current-fmt))
	  ((let ((m (next-single-property-change (point-min) 'org-hd-marker)))
	     (and m
		  (let ((m (get-text-property m 'org-hd-marker)))
		    (or (org-entry-get m "COLUMNS" t)
			(with-current-buffer (marker-buffer m)
			  org-columns-default-format))))))
	  (t org-columns-default-format))))
    (setq-local org-columns-current-fmt fmt)
    (org-columns-compile-format fmt)
    (when org-agenda-columns-compute-summary-properties
      (org-agenda-colview-compute org-columns-current-fmt-compiled))
    (save-excursion
      ;; Collect properties for each headline in current view.
      (goto-char (point-min))
      (let (cache)
	(while (not (eobp))
	  (let ((m (or (org-get-at-bol 'org-hd-marker)
		       (org-get-at-bol 'org-marker))))
	    (when m
	      (push (cons (line-beginning-position)
			  (org-with-point-at m
			    (org-columns--collect-values 'agenda)))
		    cache)))
	  (forward-line))
	(when cache
	  (setq-local org-columns-current-maxwidths
		      (org-columns--autowidth-alist cache))
	  (org-columns--display-here-title)
	  (when (setq-local org-columns-flyspell-was-active
			    (org-bound-and-true-p flyspell-mode))
	    (flyspell-mode 0))
	  (dolist (entry cache)
	    (goto-char (car entry))
	    (org-columns--display-here (cdr entry)))
	  (when org-agenda-columns-show-summaries
	    (org-agenda-colview-summarize cache)))))))

(defun org-agenda-colview-summarize (cache)
  "Summarize the summarizable columns in column view in the agenda.
This will add overlays to the date lines, to show the summary for each day."
  (let ((fmt (mapcar
	      (lambda (spec)
		(pcase spec
		  (`(,property ,title ,width . ,_)
		   (if (member-ignore-case property '("CLOCKSUM" "CLOCKSUM_T"))
		       (list property title width ":" 'add_times nil '+ nil)
		     spec))))
	      org-columns-current-fmt-compiled))
	entries)
    ;; Ensure there's at least one summation column.
    (when (cl-some (lambda (spec) (nth 4 spec)) fmt)
      (goto-char (point-max))
      (while (not (bobp))
	(when (or (get-text-property (point) 'org-date-line)
		  (eq (get-text-property (point) 'face)
		      'org-agenda-structure))
	  ;; OK, this is a date line that should be used.
	  (let (rest)
	    (dolist (c cache (setq cache rest))
	      (if (> (car c) (point))
		  (push c entries)
		(push c rest))))
	  ;; Now ENTRIES contains entries below the current one.
	  ;; CACHE is the rest.  Compute the summaries for the
	  ;; properties we want, set nil properties for the rest.
	  (when (setq entries (mapcar 'cdr entries))
	    (org-columns--display-here
	     (mapcar
	      (lambda (spec)
		(pcase spec
		  (`(,(and prop (guard (equal (upcase prop) "ITEM"))) . ,_)
		   ;; Replace ITEM with current date.  Preserve
		   ;; properties for fontification.
		   (let ((date (buffer-substring
				(line-beginning-position)
				(line-end-position))))
		     (list prop date date)))
		  (`(,prop ,_ ,_ ,_ nil . ,_)
		   (list prop "" ""))
		  (`(,prop ,_ ,_ ,_ ,stype ,_ ,sumfunc)
		   (let (lsum)
		     (dolist (entry entries (setq lsum (delq nil lsum)))
		       ;; Use real values for summary, not those
		       ;; prepared for display.
		       (let ((v (nth 1 (assoc-string prop entry t))))
			 (when v
			   (push (org-columns-string-to-number v stype) lsum))))
		     (setq lsum
			   (let ((l (length lsum)))
			     (cond ((> l 1)
				    (org-columns-number-to-string
				     (apply sumfunc lsum) stype))
				   ((= l 1)
				    (org-columns-number-to-string
				     (car lsum) stype))
				   (t ""))))
		     (put-text-property 0 (length lsum) 'face 'bold lsum)
		     (list prop lsum lsum)))))
	      fmt)
	     'dateline)
	    (setq-local org-agenda-columns-active t)))
	(forward-line -1)))))

(defun org-agenda-colview-compute (fmt)
  "Compute the relevant columns in the contributing source buffers."
  (let ((files org-agenda-contributing-files)
	(org-columns-begin-marker (make-marker))
	(org-columns-top-level-marker (make-marker))
	f fm a b)
    (while (setq f (pop files))
      (setq b (find-buffer-visiting f))
      (with-current-buffer (or (buffer-base-buffer b) b)
	(save-excursion
	  (save-restriction
	    (widen)
	    (org-with-silent-modifications
	     (remove-text-properties (point-min) (point-max) '(org-summaries t)))
	    (goto-char (point-min))
	    (org-columns-get-format-and-top-level)
	    (while (setq fm (pop fmt))
	      (cond ((equal (car fm) "CLOCKSUM")
		     (org-clock-sum))
		    ((equal (car fm) "CLOCKSUM_T")
		     (org-clock-sum-today))
		    ((and (nth 4 fm)
			  (setq a (assoc-string (car fm)
						org-columns-current-fmt-compiled
						t))
			  (equal (nth 4 a) (nth 4 fm)))
		     (org-columns-compute (car fm)))))))))))

(defun org-estimate-mean-and-var (v)
  "Return the mean and variance of an estimate."
  (let* ((v (cond ((consp v) v)
		  ((numberp v) (list v v))
		  (t (error "Invalid estimate type"))))
	 (low (float (car v)))
         (high (float (cadr v)))
         (mean (/ (+ low high) 2.0))
         (var (/ (+ (expt (- mean low) 2.0) (expt (- high mean) 2.0)) 2.0)))
    (list  mean var)))

(defun org-estimate-combine (&rest el)
  "Combine a list of estimates, using mean and variance.
The mean and variance of the result will be the sum of the means
and variances (respectively) of the individual estimates."
  (let ((mean 0)
        (var 0))
    (mapc (lambda (e)
	    (let ((stats (org-estimate-mean-and-var e)))
	      (setq mean (+ mean (car stats)))
	      (setq var (+ var (cadr stats)))))
	  el)
    (let ((stdev (sqrt var)))
      (list (- mean stdev) (+ mean stdev)))))

(defun org-estimate-print (e &optional fmt)
  "Prepare a string representation of an estimate.
This formats these numbers as two numbers with a \"-\" between them."
  (let ((fmt (or fmt "%.0f"))
	(e (cond ((consp e) e)
		 ((numberp e) (list e e))
		 (t (error "Invalid estimate type")))))
    (format "%s" (mapconcat (lambda (n) (format fmt n)) e "-"))))

(defun org-string-to-estimate (s)
  "Convert a string to an estimate.
The string should be two numbers joined with a \"-\"."
  (if (string-match "\\(.*\\)-\\(.*\\)" s)
      (list (string-to-number (match-string 1 s))
	    (string-to-number(match-string 2 s)))
    (list (string-to-number s) (string-to-number s))))

(provide 'org-colview)

;;; org-colview.el ends here
