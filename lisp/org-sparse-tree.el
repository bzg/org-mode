;;; org-sparse-tree.el --- Selective display in Org buffers                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

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

;; This library provides commands to filter displayed entries in Org
;; buffers as a sparse tree.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-map)

(defvar org-not-done-regexp)
(defvar org-todo-keywords-1)
(defvar org-deadline-warning-days)
(declare-function org-deadline-close-p "org")
(declare-function org-agenda-prepare-buffers "org")



(defvar org-inhibit-highlight-removal nil) ; dynamically scoped param
(defvar org-ts-type nil)

(defgroup org-sparse-trees nil
  "Options concerning sparse trees in Org mode."
  :tag "Org Sparse Trees"
  :group 'org-structure)

(defcustom org-highlight-sparse-tree-matches t
  "Non-nil means highlight all matches that define a sparse tree.
The highlights will automatically disappear the next time the buffer is
changed by an edit command.
Must be non-nil to traverse the sparse tree using `next-error'."
  :group 'org-sparse-trees
  :type 'boolean)

(defcustom org-remove-highlights-with-change t
  "Non-nil means any change to the buffer will remove temporary highlights.
\\<org-mode-map>\
Such highlights are created by `org-occur' and `org-clock-display'.
When nil, `\\[org-ctrl-c-ctrl-c]' needs to be used \
to get rid of the highlights.
The highlights created by `org-latex-preview' always need
`\\[org-latex-preview]' to be removed."
  :group 'org-sparse-trees
  :group 'org-time
  :type 'boolean)

(defcustom org-occur-case-fold-search t
  "Non-nil means `org-occur' should be case-insensitive.
If set to `smart' the search will be case-insensitive only if it
doesn't specify any upper case character."
  :group 'org-sparse-trees
  :version "26.1"
  :type '(choice
	  (const :tag "Case-sensitive" nil)
	  (const :tag "Case-insensitive" t)
	  (const :tag "Case-insensitive for lower case searches only" smart)))

(defcustom org-occur-hook '(org-first-headline-recenter)
  "Hook that is run after `org-occur' has constructed a sparse tree.
This can be used to recenter the window to show as much of the structure
as possible."
  :group 'org-sparse-trees
  :type 'hook)

(defcustom org-sparse-tree-open-archived-trees nil
  "Non-nil means sparse tree construction shows matches in archived trees.
When nil, matches in these trees are highlighted, but the trees are kept in
collapsed state."
  :group 'org-archive
  :group 'org-sparse-trees
  :type 'boolean)

(defcustom org-sparse-tree-default-date-type nil
  "The default date type when building a sparse tree.
When this is nil, a date is a scheduled or a deadline timestamp.
Otherwise, these types are allowed:

        all: all timestamps
     active: only active timestamps (<...>)
   inactive: only inactive timestamps ([...])
  scheduled: only scheduled timestamps
   deadline: only deadline timestamps"
  :type '(choice (const :tag "Scheduled or deadline" nil)
		 (const :tag "All timestamps" all)
		 (const :tag "Only active timestamps" active)
		 (const :tag "Only inactive timestamps" inactive)
		 (const :tag "Only scheduled timestamps" scheduled)
		 (const :tag "Only deadline timestamps" deadline)
		 (const :tag "Only closed timestamps" closed))
  :version "26.1"
  :package-version '(Org . "8.3")
  :group 'org-sparse-trees)

(defsubst org-re-timestamp (type)
  "Return a regexp for timestamp TYPE.
Allowed values for TYPE are:

        all: all timestamps
     active: only active timestamps (<...>)
   inactive: only inactive timestamps ([...])
  scheduled: only scheduled timestamps
   deadline: only deadline timestamps
     closed: only closed timestamps

When TYPE is nil, fall back on returning a regexp that matches
both scheduled and deadline timestamps."
  (cl-case type
    (all org-ts-regexp-both)
    (active org-ts-regexp)
    (inactive org-ts-regexp-inactive)
    (scheduled org-scheduled-time-regexp)
    (deadline org-deadline-time-regexp)
    (closed org-closed-time-regexp)
    (otherwise
     (concat "\\<"
	     (regexp-opt (list org-deadline-string org-scheduled-string))
	     " *<\\([^>]+\\)>"))))

(defun org-show-todo-tree (arg)
  "Make a compact tree which shows all headlines marked with TODO.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.
With a `\\[universal-argument]' prefix, prompt for a regexp to match.
With a numeric prefix N, construct a sparse tree for the Nth element
of `org-todo-keywords-1'."
  (interactive "P" org-mode)
  (let ((case-fold-search nil)
	(kwd-re
	 (cond ((null arg) (concat org-not-done-regexp "\\s-"))
	       ((equal arg '(4))
		(let ((kwd
		       (completing-read "Keyword (or KWD1|KWD2|...): "
					(mapcar #'list org-todo-keywords-1))))
		  (concat "\\("
			  (mapconcat #'regexp-quote (org-split-string kwd "|") "\\|")
			  "\\)\\(?:[ \t]\\|$\\)")))
	       ((<= (prefix-numeric-value arg) (length org-todo-keywords-1))
		(regexp-quote (nth (1- (prefix-numeric-value arg))
				   org-todo-keywords-1)))
	       (t (user-error "Invalid prefix argument: %s" arg)))))
    (message "%d TODO entries found"
	     (org-occur (concat "^" org-outline-regexp " *" kwd-re )))))

(defun org-check-deadlines (ndays)
  "Check if there are any deadlines due or past due.
A deadline is considered due if it happens within `org-deadline-warning-days'
days from today's date.  If the deadline appears in an entry marked DONE,
it is not shown.  A numeric prefix argument NDAYS can be used to test that
many days.  If the prefix is a raw `\\[universal-argument]', all deadlines \
are shown."
  (interactive "P" org-mode)
  (let* ((org-warn-days
	  (cond
	   ((equal ndays '(4)) 100000)
	   (ndays (prefix-numeric-value ndays))
	   (t (abs org-deadline-warning-days))))
	 (case-fold-search nil)
	 (regexp (concat "\\<" org-deadline-string " *<\\([^>]+\\)>"))
	 (callback
	  (lambda () (org-deadline-close-p (match-string 1) org-warn-days))))
    (message "%d deadlines past-due or due within %d days"
	     (org-occur regexp nil callback)
	     org-warn-days)))

(defun org-match-sparse-tree (&optional todo-only match)
  "Create a sparse tree according to tags string MATCH.

MATCH is a string with match syntax.  It can contain a selection
of tags (\"+work+urgent-boss\"), properties (\"LEVEL>3\"), and
TODO keywords (\"TODO=\\\"WAITING\\\"\") or a combination of
those.  See the manual for details.

If optional argument TODO-ONLY is non-nil, only select lines that
are also TODO tasks."
  (interactive "P" org-mode)
  (org-agenda-prepare-buffers (list (current-buffer)))
  (let ((org--matcher-tags-todo-only todo-only))
    (org-scan-tags 'sparse-tree (cdr (org-make-tags-matcher match t))
		   org--matcher-tags-todo-only)))

(defun org-check-before-date (d)
  "Check if there are deadlines or scheduled entries before date D."
  (interactive (list (org-read-date)) org-mode)
  (let* ((case-fold-search nil)
	 (regexp (org-re-timestamp org-ts-type))
	 (ts-type org-ts-type)
	 (callback
	  (lambda ()
	    (let ((match (match-string 1)))
	      (and (if (memq ts-type '(active inactive all))
		       (org-element-type-p
                        (save-excursion
			  (backward-char)
			  (org-element-context))
			'timestamp)
		     (org-at-planning-p))
		   (time-less-p
		    (org-time-string-to-time match)
		    (org-time-string-to-time d)))))))
    (message "%d entries before %s"
	     (org-occur regexp nil callback)
	     d)))

(defun org-check-after-date (d)
  "Check if there are deadlines or scheduled entries after date D."
  (interactive (list (org-read-date)) org-mode)
  (let* ((case-fold-search nil)
	 (regexp (org-re-timestamp org-ts-type))
	 (ts-type org-ts-type)
	 (callback
	  (lambda ()
	    (let ((match (match-string 1)))
	      (and (if (memq ts-type '(active inactive all))
		       (org-element-type-p
                        (save-excursion
			  (backward-char)
			  (org-element-context))
			'timestamp)
		     (org-at-planning-p))
		   (not (time-less-p
		       (org-time-string-to-time match)
		       (org-time-string-to-time d))))))))
    (message "%d entries after %s"
	     (org-occur regexp nil callback)
	     d)))

(defun org-check-dates-range (start-date end-date)
  "Check for deadlines/scheduled entries between START-DATE and END-DATE."
  (interactive (list (org-read-date nil nil nil "Range starts")
		     (org-read-date nil nil nil "Range end"))
               org-mode)
  (let ((case-fold-search nil)
	(regexp (org-re-timestamp org-ts-type))
	(callback
	 (let ((type org-ts-type))
	   (lambda ()
	     (let ((match (match-string 1)))
	       (and
		(if (memq type '(active inactive all))
		    (org-element-type-p
                     (save-excursion
		       (backward-char)
		       (org-element-context))
		     'timestamp)
		  (org-at-planning-p))
		(not (time-less-p
		    (org-time-string-to-time match)
		    (org-time-string-to-time start-date)))
		(time-less-p
		 (org-time-string-to-time match)
		 (org-time-string-to-time end-date))))))))
    (message "%d entries between %s and %s"
	     (org-occur regexp nil callback) start-date end-date)))

(defun org-sparse-tree (&optional arg type)
  "Create a sparse tree, prompt for the details.
This command can create sparse trees.  You first need to select the type
of match used to create the tree:

t      Show all TODO entries.
T      Show entries with a specific TODO keyword.
m      Show entries selected by a tags/property match.
p      Enter a property name and its value (both with completion on existing
       names/values) and show entries with that property.
r      Show entries matching a regular expression (`/' can be used as well).
b      Show deadlines and scheduled items before a date.
a      Show deadlines and scheduled items after a date.
d      Show deadlines due within `org-deadline-warning-days'.
D      Show deadlines and scheduled items between a date range."
  (interactive "P" org-mode)
  (setq type (or type org-sparse-tree-default-date-type))
  (setq org-ts-type type)
  (message "Sparse tree: [r]egexp [t]odo [T]odo-kwd [m]atch [p]roperty
             [d]eadlines [b]efore-date [a]fter-date [D]ates range
             [c]ycle through date types: %s"
	   (cl-case type
	     (all "all timestamps")
	     (scheduled "only scheduled")
	     (deadline "only deadline")
	     (active "only active timestamps")
	     (inactive "only inactive timestamps")
	     (closed "with a closed timestamp")
	     (otherwise "scheduled/deadline")))
  (let ((answer (read-char-exclusive)))
    (cl-case answer
      (?c
       (org-sparse-tree
	arg
	(cadr
	 (memq type '(nil all scheduled deadline active inactive closed)))))
      (?d (call-interactively #'org-check-deadlines))
      (?b (call-interactively #'org-check-before-date))
      (?a (call-interactively #'org-check-after-date))
      (?D (call-interactively #'org-check-dates-range))
      (?t (call-interactively #'org-show-todo-tree))
      (?T (org-show-todo-tree '(4)))
      (?m (call-interactively #'org-match-sparse-tree))
      ((?p ?P)
       (let* ((kwd (completing-read
		    "Property: " (mapcar #'list (org-buffer-property-keys))))
              (kwd
               ;; Escape "-" in property names.
               (replace-regexp-in-string "-" "\\\\-" kwd))
	      (value (completing-read
		      "Value: " (mapcar #'list (org-property-values kwd)))))
	 (unless (string-match "\\`{.*}\\'" value)
	   (setq value (concat "\"" value "\"")))
	 (org-match-sparse-tree arg (concat kwd "=" value))))
      ((?r ?R ?/) (call-interactively #'org-occur))
      (otherwise (user-error "No such sparse tree command \"%c\"" answer)))))

(defvar-local org-occur-highlights nil
  "List of overlays used for occur matches.")
(put 'org-occur-highlights 'permanent-local t)
(defvar-local org-occur-parameters nil
  "Parameters of the active org-occur calls.
This is a list, each call to org-occur pushes as cons cell,
containing the regular expression and the callback, onto the list.
The list can contain several entries if `org-occur' has been called
several time with the KEEP-PREVIOUS argument.  Otherwise, this list
will only contain one set of parameters.  When the highlights are
removed (for example with \\`C-c C-c', or with the next edit (depending
on `org-remove-highlights-with-change'), this variable is emptied
as well.")

(defun org-occur (regexp &optional keep-previous callback)
  "Make a compact tree showing all matches of REGEXP.

The tree will show the lines where the regexp matches, and any other context
defined in `org-fold-show-context-detail', which see.

When optional argument KEEP-PREVIOUS is non-nil, highlighting and exposing
done by a previous call to `org-occur' will be kept, to allow stacking of
calls to this command.

Optional argument CALLBACK can be a function of no argument.  In this case,
it is called with point at the end of the match, match data being set
accordingly.  Current match is shown only if the return value is non-nil.
The function must neither move point nor alter narrowing."
  (interactive "sRegexp: \nP" org-mode)
  (when (equal regexp "")
    (user-error "Regexp cannot be empty"))
  (unless keep-previous
    (org-remove-occur-highlights nil nil t))
  (push (cons regexp callback) org-occur-parameters)
  (let ((cnt 0))
    (save-excursion
      (goto-char (point-min))
      (when (or (not keep-previous)	    ; do not want to keep
		(not org-occur-highlights)) ; no previous matches
	;; hide everything
	(org-cycle-overview))
      (let ((case-fold-search (if (eq org-occur-case-fold-search 'smart)
				  (isearch-no-upper-case-p regexp t)
				org-occur-case-fold-search)))
	(while (re-search-forward regexp nil t)
	  (when (or (not callback)
		    (save-match-data (funcall callback)))
	    (setq cnt (1+ cnt))
	    (when org-highlight-sparse-tree-matches
	      (org-highlight-new-match (match-beginning 0) (match-end 0)))
	    (org-fold-show-context 'occur-tree)))))
    (when org-remove-highlights-with-change
      (add-hook 'before-change-functions 'org-remove-occur-highlights
		nil 'local))
    (unless org-sparse-tree-open-archived-trees
      (org-fold-hide-archived-subtrees (point-min) (point-max)))
    (when org-highlight-sparse-tree-matches
      (setq next-error-last-buffer (current-buffer)))
    (run-hooks 'org-occur-hook)
    (when (called-interactively-p 'interactive)
      (message "%d match(es) for regexp %s" cnt regexp))
    cnt))

(defun org-occur-next-match (&optional n _reset)
  "Function for `next-error-function' to find sparse tree matches.
N is the number of matches to move, when negative move backwards.
This function always goes back to the starting point when no
match is found."
  (let* ((limit (if (< n 0) (point-min) (point-max)))
	 (search-func (if (< n 0)
			  'previous-single-char-property-change
			'next-single-char-property-change))
	 (n (abs n))
	 (pos (point))
	 p1)
    (catch 'exit
      (while (setq p1 (funcall search-func (point) 'org-type))
	(when (equal p1 limit)
	  (goto-char pos)
	  (user-error "No more matches"))
	(when (equal (get-char-property p1 'org-type) 'org-occur)
	  (setq n (1- n))
	  (when (= n 0)
	    (goto-char p1)
	    (throw 'exit (point))))
	(goto-char p1))
      (goto-char p1)
      (user-error "No more matches"))))

(defun org-highlight-new-match (beg end)
  "Highlight from BEG to END and mark the highlight is an occur headline."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'secondary-selection)
    (overlay-put ov 'org-type 'org-occur)
    (push ov org-occur-highlights)))

(defun org-remove-occur-highlights (&optional _beg _end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive nil org-mode)
  (unless org-inhibit-highlight-removal
    (mapc #'delete-overlay org-occur-highlights)
    (setq org-occur-highlights nil)
    (setq org-occur-parameters nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   'org-remove-occur-highlights 'local))))

(provide 'org-sparse-tree)

;;; org-sparse-tree.el ends here
