;;; org-list.el --- Plain lists for Org-mode
;;
;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Bastien Guerry <bzg AT altern DOT org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 7.01trans
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with plain lists in Org-mode.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org-macs)
(require 'org-compat)

(defvar org-blank-before-new-entry)
(defvar org-M-RET-may-split-line)
(defvar org-complex-heading-regexp)
(defvar org-odd-levels-only)

(declare-function org-invisible-p "org" ())
(declare-function org-on-heading-p "org" (&optional invisible-ok))
(declare-function outline-next-heading "outline" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-back-over-empty-lines "org" ())
(declare-function org-skip-whitespace "org" ())
(declare-function org-trim "org" (s))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-timer-item "org-timer" (&optional arg))
(declare-function org-combine-plists "org" (&rest plists))
(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-show-subtree "org" ())

(defgroup org-plain-lists nil
  "Options concerning plain lists in Org-mode."
  :tag "Org Plain lists"
  :group 'org-structure)

(defcustom org-cycle-include-plain-lists t
  "When t, make TAB cycle visibility on plain list items.

Cycling plain lists works only when the cursor is on a plain list
item.  When the cursor is on an outline heading, plain lists are
treated as text.  This is the most stable way of handling this,
which is why it is the default.

When this is the symbol `integrate', then during cycling, plain
list items will *temporarily* be interpreted as outline headlines
with a level given by 1000+i where i is the indentation of the
bullet.  This setting can lead to strange effects when switching
visibility to `children', because the first \"child\" in a
subtree decides what children should be listed.  If that first
\"child\" is a plain list item with an implied large level
number, all true children and grand children of the outline
heading will be exposed in a children' view."
  :group 'org-plain-lists
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "With cursor in plain list (recommended)" t)
	  (const :tag "As children of outline headings" integrate)))

(defcustom org-list-demote-modify-bullet nil
  "Default bullet type installed when demoting an item.
This is an association list, for each bullet type, this alist will point
to the bullet that should be used when this item is demoted.
For example,

 (setq org-list-demote-modify-bullet
       '((\"+\" . \"-\") (\"-\" . \"+\") (\"*\" . \"+\")))

will make

  + Movies
    + Silence of the Lambs
    + My Cousin Vinny
  + Books
    + The Hunt for Red October
    + The Road to Omaha

into

  + Movies
    - Silence of the Lambs
    - My Cousin Vinny
  + Books
    - The Hunt for Red October
    - The Road to Omaha"
  :group 'org-plain-lists
  :type '(repeat
	  (cons
	   (choice :tag "If the current bullet is  "
		   (const "-")
		   (const "+")
		   (const "*")
		   (const "1.")
		   (const "1)"))
	   (choice :tag "demotion will change it to"
		   (const "-")
		   (const "+")
		   (const "*")
		   (const "1.")
		   (const "1)")))))

(defcustom org-plain-list-ordered-item-terminator t
  "The character that makes a line with leading number an ordered list item.
Valid values are ?. and ?\).  To get both terminators, use t.  While
?. may look nicer, it creates the danger that a line with leading
number may be incorrectly interpreted as an item.  ?\) therefore is
the safe choice."
  :group 'org-plain-lists
  :type '(choice (const :tag "dot like in \"2.\"" ?.)
		 (const :tag "paren like in \"2)\"" ?\))
		 (const :tab "both" t)))

(defcustom org-list-two-spaces-after-bullet-regexp nil
  "A regular expression matching bullets that should have 2 spaces after them.
When nil, no bullet will have two spaces after them.
When a string, it will be used as a regular expression. When the
bullet type of a list is changed, the new bullet type will be
matched against this regexp. If it matches, there will be two
spaces instead of one after the bullet in each item of he list."
  :group 'org-plain-lists
  :type '(choice
	  (const :tag "never" nil)
	  (regexp)))

(defcustom org-empty-line-terminates-plain-lists nil
  "Non-nil means an empty line ends all plain list levels.
Otherwise, look for `org-list-end-regexp'."

  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-list-end-regexp "^[ \t]*\n[ \t]*\n"
  "Regexp matching the end of all plain list levels.
It must start with \"^\" and end with \"\\n\". It defaults to 2
blank lines. `org-empty-line-terminates-plain-lists' has
precedence over it."
  :group 'org-plain-lists
  :type 'string)

(defcustom org-auto-renumber-ordered-lists t
  "Non-nil means automatically renumber ordered plain lists.
Renumbering happens when the sequence have been changed with
\\[org-shiftmetaup] or \\[org-shiftmetadown].  After other editing commands,
use \\[org-ctrl-c-ctrl-c] to trigger renumbering."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-provide-checkbox-statistics t
  "Non-nil means update checkbox statistics after insert and toggle.
When this is set, checkbox statistics is updated each time you
either insert a new checkbox with \\[org-insert-todo-heading] or
toggle a checkbox with \\[org-ctrl-c-ctrl-c]."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-hierarchical-checkbox-statistics t
  "Non-nil means checkbox statistics counts only the state of direct children.
When nil, all boxes below the cookie are counted.
This can be set to nil on a per-node basis using a COOKIE_DATA property
with the word \"recursive\" in the value."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-description-max-indent 20
  "Maximum indentation for the second line of a description list.
When the indentation would be larger than this, it will become
5 characters instead."
  :group 'org-plain-lists
  :type 'integer)

(defcustom org-list-radio-list-templates
  '((latex-mode "% BEGIN RECEIVE ORGLST %n
% END RECEIVE ORGLST %n
\\begin{comment}
#+ORGLST: SEND %n org-list-to-latex
-
\\end{comment}\n")
    (texinfo-mode "@c BEGIN RECEIVE ORGLST %n
@c END RECEIVE ORGLST %n
@ignore
#+ORGLST: SEND %n org-list-to-texinfo
-
@end ignore\n")
    (html-mode "<!-- BEGIN RECEIVE ORGLST %n -->
<!-- END RECEIVE ORGLST %n -->
<!--
#+ORGLST: SEND %n org-list-to-html
-
-->\n"))
  "Templates for radio lists in different major modes.
All occurrences of %n in a template will be replaced with the name of the
list, obtained by prompting the user."
  :group 'org-plain-lists
  :type '(repeat
	  (list (symbol :tag "Major mode")
		(string :tag "Format"))))

;;; Internal functions

(defun org-list-end-re ()
  "Return the regex corresponding to the end of a list.
It depends on `org-empty-line-terminates-plain-lists'."
  (if org-empty-line-terminates-plain-lists
      "^[ \t]*\n"
    org-list-end-regexp))

(defun org-item-re (&optional general)
  "Return the correct regular expression for plain lists.
If GENERAL is non-nil, return the general regexp independent of the value
of `org-plain-list-ordered-item-terminator'."
  (cond
   ((or general (eq org-plain-list-ordered-item-terminator t))
    "\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
   ((= org-plain-list-ordered-item-terminator ?.)
    "\\([ \t]*\\([-+]\\|\\([0-9]+\\.\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
   ((= org-plain-list-ordered-item-terminator ?\))
    "\\([ \t]*\\([-+]\\|\\([0-9]+)\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
   (t (error "Invalid value of `org-plain-list-ordered-item-terminator'"))))

(defun org-list-terminator-between (min max &optional firstp)
  "Find the position of a list ender between MIN and MAX, or nil.
This function looks for `org-list-end-re' not matching a block.

If FIRSTP in non-nil, return the point at the beginning of the
nearest valid terminator from min. Otherwise, return the point at
the end of the nearest terminator from max."
  (save-excursion
    (let* ((start (if firstp min max))
	   (end   (if firstp max min))
	   (search-fun (if firstp
			   #'org-search-forward-unenclosed
			 #'org-search-backward-unenclosed))
	   (list-end-p (progn
			 (goto-char start)
			 (funcall search-fun (org-list-end-re) end))))
      ;; Is there a valid list terminator somewhere ?
      (and list-end-p
	   ;; we want to be on the first line of the list ender
	   (match-beginning 0)))))

(defun org-search-backward-unenclosed (regexp &optional bound noerror count)
  "Like `re-search-backward' but don't stop inside blocks or at protected places.
This function does not throw errors.

Optional fourth argument COUNT searches for that many
occurrences, valid or not, then makes sure the last one is
valid."
  (let ((origin (point)))
    (cond
     ;; nothing found: return nil
     ((not (re-search-backward regexp bound (or noerror t) count)) nil)
     ;; match is enclosed or protected: start again, searching one
     ;; more occurrence away.
     ((or (save-match-data
	    (org-in-regexps-block-p "^[ \t]*#\\+\\(begin\\|BEGIN\\)_\\([a-zA-Z0-9_]+\\)"
				    '(concat "^[ \t]*#\\+\\(end\\|END\\)_" (match-string 2))))
	  (get-text-property (match-beginning 0) 'org-protected))
      (goto-char origin)
      (org-search-backward-unenclosed regexp bound noerror (1+ (or count 1))))
     ;; else return point.
     (t
      (point)))))

(defun org-search-forward-unenclosed (regexp &optional bound noerror count)
  "Like `re-search-forward' but don't stop inside blocks or at protected places.
This function does not throw errors.

Optional fourth argument COUNT searches for that many occurrences,
valid or not, then makes sure the last one is valid."
  (let ((origin (point)))
    (cond
     ;; nothing found: return nil
     ((not (re-search-forward regexp bound (or noerror t) count)) nil)
     ;; match is enclosed or protected: start again, searching one
     ;; more occurrence away.
     ((or (save-match-data
	    (org-in-regexps-block-p "^[ \t]*#\\+\\(begin\\|BEGIN\\)_\\([a-zA-Z0-9_]+\\)"
				    '(concat "^[ \t]*#\\+\\(end\\|END\\)_" (match-string 2))))
	  (get-text-property (match-beginning 0) 'org-protected))
      (goto-char origin)
      (org-search-forward-unenclosed regexp bound noerror (1+ (or count 1))))
     ;; else return point.
     (t
      (point)))))

(defun org-get-item-same-level-internal (search-fun pos limit pre-move)
  "Return point at the beginning of next item at the same level.
Search items using function SEARCH-FUN, from POS to LIMIT. It
uses PRE-MOVE before searches. Return nil if no item was found.

Internal use only. Prefer `org-get-next-item' and
`org-get-previous-item' for cleaner code."
  (save-excursion
    (when pos (goto-char pos))
    (let ((begin (point))
	  (ind (progn
		 (org-beginning-of-item)
		 (org-get-indentation)))
	  (start (point-at-bol)))
      ;; we don't want to match the current line.
      (funcall pre-move)
      ;; we skip any sublist on the way
      (while (and (funcall search-fun (org-item-re) limit)
		  (> (org-get-indentation) ind))
	(funcall pre-move))
      (when (and (/= (point-at-bol) start) ; Have we moved ?
		 (= (org-get-indentation) ind))
	(point-at-bol)))))

;;; Predicates

(defun org-in-item-p ()
  "Is the cursor inside a plain list ?"
  (save-excursion
    ;; we move to eol so that the current line can be matched by
    ;; `org-item-re'.
    (let* ((limit (or (save-excursion (outline-previous-heading)) (point-min)))
	   (actual-pos (goto-char (point-at-eol)))
	   (last-item-start (save-excursion
			      (org-search-backward-unenclosed (org-item-re) limit)))
	   (list-ender (org-list-terminator-between last-item-start actual-pos)))
      ;; We are in a list when we are on an item line or we can find
      ;; an item before and there is no valid list ender between us
      ;; and the item found.
      (and last-item-start
	   (not list-ender)))))

(defun org-first-list-item-p ()
  "Is this heading the first item in a plain list?"
  (unless (org-at-item-p)
    (error "Not at a plain list item"))
  (save-excursion
    (= (save-excursion (org-beginning-of-item)) (org-beginning-of-item-list))))

(defun org-at-item-p ()
  "Is point in a line starting a hand-formatted item?"
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at (org-item-re))))

(defun org-at-item-bullet-p ()
  "Is point at the bullet of a plain list item?"
  (and (org-at-item-p)
       (not (member (char-after) '(?\  ?\t)))
       (< (point) (match-end 0))))

(defun org-at-item-checkbox-p ()
  "Is point at a line starting a plain-list item with a checklet?"
  (and (org-at-item-p)
       (save-excursion
	 (goto-char (match-end 0))
	 (skip-chars-forward " \t")
	 (looking-at "\\[[- X]\\]"))))

(defun org-checkbox-blocked-p ()
  "Is the current checkbox blocked from for being checked now?
A checkbox is blocked if all of the following conditions are fulfilled:

1. The checkbox is not checked already.
2. The current entry has the ORDERED property set.
3. There is an unchecked checkbox in this entry before the current line."
  (catch 'exit
    (save-match-data
      (save-excursion
	(unless (org-at-item-checkbox-p) (throw 'exit nil))
	(when (equal (match-string 0) "[X]")
	  ;; the box is already checked!
	  (throw 'exit nil))
	(let ((end (point-at-bol)))
	  (condition-case nil (org-back-to-heading t)
	    (error (throw 'exit nil)))
	  (unless (org-entry-get nil "ORDERED") (throw 'exit nil))
	  (if (re-search-forward "^[ \t]*[-+*0-9.)] \\[[- ]\\]" end t)
	      (org-current-line)
	    nil))))))

;;; Navigate

(defun org-list-top-point ()
  "Return point at the top level item in a list, or nil if not in a list."
  (save-excursion
    (and (org-in-item-p)
	 (let ((pos (point-at-eol))
	       (bound (or (outline-previous-heading) (point-min))))
	   ;; Is there some list above this one ? If so, go to its ending.
	   ;; Otherwise, go back to the heading above or bob.
	   (goto-char (or (org-list-terminator-between bound pos) bound))
	   ;; From there, search down our list.
	   (org-search-forward-unenclosed (org-item-re) pos)
	   (point-at-bol)))))

(defun org-list-bottom-point ()
  "Return point just before list ending or nil if not in a list."
  (save-excursion
    (and (org-in-item-p)
	 (let ((pos (org-beginning-of-item))
	       (bound (or (and (outline-next-heading)
			       (skip-chars-backward " \t\r\n")
			       (1+ (point-at-eol)))
			  (point-max))))
	   ;; The list ending is either first point matching
	   ;; org-list-end-re, point at first white-line before next
	   ;; heading, or eob.
	   (or (org-list-terminator-between pos bound t) bound)))))

(defun org-beginning-of-item ()
  "Go to the beginning of the current hand-formatted item.
If the cursor is not in an item, throw an error. Return point."
  (interactive)
  (if (org-in-item-p)
      (if (org-at-item-p)
	  (progn (beginning-of-line 1)
		 (point))
	(org-search-backward-unenclosed (org-item-re))
	(goto-char (point-at-bol)))
    (error "Not in an item")))

(defun org-end-of-item ()
  "Go to the end of the current hand-formatted item.
If the cursor is not in an item, throw an error."
  (interactive)
  (let ((next-p (org-get-next-item (point) (org-list-bottom-point))))
    (cond ((not (org-in-item-p))
	   (error "Not in an item"))
	  (next-p
	   (goto-char next-p))
	  (t
	   (org-end-of-item-list)))))

(defun org-end-of-item-text-before-children ()
  "Move to the end of the item text, stops before the first child if any.
Assumes that the cursor is in the first line of an item."
  (let ((limit (org-list-bottom-point)))
    (end-of-line)
    (goto-char
     (if (org-search-forward-unenclosed (org-item-re) limit)
	 (point-at-bol)
       limit))))

(defun org-end-of-item-before-blank ()
  "Return point at end of item, before any blank line.
Point returned is at eol."
  (save-excursion
    (org-end-of-item)
    (skip-chars-backward " \r\t\n")
    (point-at-eol)))

(defun org-get-next-item (pos limit)
  "Get the point of the next item at the same level as POS.
 Stop searching at LIMIT. Return nil if no item is found. This
 function does not move point."
  (org-get-item-same-level-internal
   #'org-search-forward-unenclosed
   pos
   limit
   #'end-of-line))

(defun org-get-previous-item (pos limit)
  "Get the point of the previous item at the same level as POS.
 Stop searching at LIMIT. Return nil if no item is found. This
 function does not move point."
  (org-get-item-same-level-internal
   #'org-search-backward-unenclosed
   pos
   limit
   #'beginning-of-line))

(defun org-next-item ()
  "Move to the beginning of the next item.
Item is at the same level in the current plain list. Error if not
in a plain list, or if this is the last item in the list."
  (interactive)
  (let ((next-p (org-get-next-item (point) (org-list-bottom-point))))
    (if next-p
	(goto-char next-p)
      (error "On last item"))))

(defun org-previous-item ()
  "Move to the beginning of the previous item.
Item is at the same level in the current plain list. Error if not
in a plain list, or if this is the first item in the list."
  (interactive)
  (let ((prev-p (org-get-previous-item (point) (org-list-top-point))))
    (if prev-p
	(goto-char prev-p)
      (error "On first item"))))

(defun org-beginning-of-item-list ()
  "Go to the beginning item of the current list or sublist.
Return point."
  (interactive)
  (let ((limit (org-list-top-point))
	(move-up (lambda (pos bound)
		   ;; prev-p: any item of same level before ?
		   (let ((prev-p (org-get-previous-item pos bound)))
		     ;; recurse until no more item of the same level
		     ;; can be found.
		     (if prev-p
			 (funcall move-up prev-p bound)
		       pos)))))
    ;; Go to the last item found and at bol in case we didn't move
    (goto-char (funcall move-up (point) limit))
    (goto-char (point-at-bol))))

(defun org-end-of-item-list ()
  "Go to the end of the current list or sublist.
 Return point."
  (interactive)
  (org-beginning-of-item)
  (let ((limit (org-list-bottom-point))
	(ind (org-get-indentation))
	(get-last-item (lambda (pos bound)
			 ;; next-p: any item of same level after ?
			 (let ((next-p (org-get-next-item pos bound)))
			   ;; recurse until no more item of the same level
			   ;; can be found.
			   (if next-p
			       (funcall get-last-item next-p bound)
			     pos)))))
    ;; Move to the last item of every list or sublist encountered, and
    ;; down to bol of a higher-level item, or limit.
    (while (and (/= (point) limit)
		(>= (org-get-indentation) ind))
      (goto-char (funcall get-last-item (point) limit))
      (end-of-line)
      (when (org-search-forward-unenclosed (org-item-re) limit 'move)
	(beginning-of-line)))
    (point)))

;;; Manipulate

(defun org-list-exchange-items (beg-A beg-B)
  "Swap item starting at BEG-A with item starting at BEG-B.
  Blank lines at the end of items are left in place. Assumes
  BEG-A is lesser than BEG-B."
  (save-excursion
    (let* ((end-of-item-no-blank (lambda (pos)
				   (goto-char pos)
				   (goto-char (org-end-of-item-before-blank))))
	   (end-A-no-blank (funcall end-of-item-no-blank beg-A))
	   (end-B-no-blank (funcall end-of-item-no-blank beg-B))
	   (body-A (buffer-substring beg-A end-A-no-blank))
	   (body-B (buffer-substring beg-B end-B-no-blank))
	   (between-A-no-blank-and-B (buffer-substring end-A-no-blank beg-B)))
      (goto-char beg-A)
      (delete-region beg-A end-B-no-blank)
      (insert (concat body-B between-A-no-blank-and-B body-A)))))

(defun org-move-item-down ()
  "Move the plain list item at point down, i.e. swap with following item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive)
  (let ((pos (point))
	(col (current-column))
	(actual-item (org-beginning-of-item))
	(next-item (org-get-next-item (point) (save-excursion (org-end-of-item-list)))))
    (if (not next-item)
	(progn
	  (goto-char pos)
	  (error "Cannot move this item further down"))
      (org-list-exchange-items actual-item next-item)
      (org-maybe-renumber-ordered-list)
      (org-next-item)
      (move-to-column col))))

(defun org-move-item-up ()
  "Move the plain list item at point up, i.e. swap with previous item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive)
  (let ((pos (point))
	(col (current-column))
	(actual-item (org-beginning-of-item))
	(prev-item (org-get-previous-item (point) (save-excursion (org-beginning-of-item-list)))))
    (if (not prev-item)
	(progn
	  (goto-char pos)
	  (error "Cannot move this item further up"))
      (org-list-exchange-items prev-item actual-item)
      (org-maybe-renumber-ordered-list)
      (move-to-column col))))

(defun org-insert-item (&optional checkbox)
  "Insert a new item at the current level.

If cursor is before first character after bullet of the item, the
new item will be created before the current one. Return t when
things worked, nil when we are not in an item, or item is
invisible."
  (unless (or (not (org-in-item-p))
	      (org-invisible-p))
    ;; Timer list: delegate to `org-timer-item'.
    (if (save-excursion
	  (org-beginning-of-item)
	  (looking-at "[ \t]*[-+*][ \t]+[0-9]+:[0-9]+:[0-9]+ ::"))
	(progn
	  (org-timer-item) t)
      ;; else check if we're in a special block. If so, move before it
      ;; prior to add a new item.
      (when (org-in-regexps-block-p
	     "^[ \t]*#\\+\\(begin\\|BEGIN\\)_\\([a-zA-Z0-9_]+\\)"
	     '(concat "^[ \t]*#\\+\\(end\\|END\\)_" (match-string 2)))
	;; in case we're on the #+begin line
	(end-of-line)
	(re-search-backward "^[ \t]*#\\+\\(begin\\|BEGIN\\)" nil t)
	(end-of-line 0))
      (let ((pos (point))
	    (before-p (and (org-at-item-p)
			   (<= (point) (match-end 0))))
	    (item-start (org-beginning-of-item))
	    (bullet-init (and (looking-at (org-item-re))
			      (match-string 0)))
	    (description-p (and (looking-at "[ \t]*\\(.*?\\) ::")
				(match-string 1)))
	    ;; Guess number of blank lines used to separate items.
	    (blank-lines-nb
	     (let ((insert-blank-p
		    (cdr (assq 'plain-list-item org-blank-before-new-entry))))
	       (cond
		((or
		  org-empty-line-terminates-plain-lists
		  (not insert-blank-p))
		 0)
		((eq insert-blank-p t) 1)
		;; plain-list-item is 'auto. Count blank
		;; lines separating items in list.
		(t
		 (save-excursion
		   (if (progn
			 (org-end-of-item-list)
			 (skip-chars-backward " \r\t\n")
			 (org-search-backward-unenclosed
			  "^[ \t]*$" (save-excursion (org-beginning-of-item-list)) t))
		       (1+ (org-back-over-empty-lines))
		     0))))))
	    (insert-fun
	     (lambda (&optional string-after-bullet)
	       ;; insert bullet above item in order to avoid
	       ;; bothering with possible blank lines ending
	       ;; last item
	       (org-beginning-of-item)
	       (insert (concat bullet-init
			       (when checkbox "[ ] ")
			       (when description-p
				 (concat (read-string "Term: ") " :: "))))
	       (save-excursion
		 (insert (concat string-after-bullet
				 (make-string (1+ blank-lines-nb) ?\n))))
	       (unless before-p (org-move-item-down)))))
	(goto-char pos)
	(cond
	 (before-p
	  (funcall insert-fun)
	  ;; Renumber in this case, as we're not moving down.
	  (org-maybe-renumber-ordered-list) t)
	 ;; if we can't split item, just insert bullet at the end of
	 ;; item.
	 ((not (org-get-alist-option org-M-RET-may-split-line 'item))
	  (funcall insert-fun) t)
	 ;; else, insert a new bullet along with everything from point
	 ;; down to last non-blank line of item
	 (t
	  (delete-horizontal-space)
	  ;; get pos again in case previous command changed line.
	  (let* ((pos (point))
		 (end-before-blank (org-end-of-item-before-blank))
		 (after-bullet (when (< pos end-before-blank)
				 (prog1
				     (buffer-substring pos end-before-blank)
				   (delete-region pos end-before-blank)))))
	    (funcall insert-fun after-bullet) t)))))))

;;; Indentation

(defun org-get-string-indentation (s)
  "What indentation has S due to SPACE and TAB at the beginning of the string?"
  (let ((n -1) (i 0) (w tab-width) c)
    (catch 'exit
      (while (< (setq n (1+ n)) (length s))
	(setq c (aref s n))
	(cond ((= c ?\ ) (setq i (1+ i)))
	      ((= c ?\t) (setq i (* (/ (+ w i) w) w)))
	      (t (throw 'exit t)))))
    i))

(defvar org-suppress-item-indentation) ; dynamically scoped parameter

(defun org-shift-item-indentation (delta)
  "Shift the indentation in current item by DELTA."
  (unless (org-bound-and-true-p org-suppress-item-indentation)
    (save-excursion
      (let ((beg (point-at-bol))
	    (end (progn (org-end-of-item) (point)))
	    i)
	(goto-char end)
	(beginning-of-line 0)
	(while (> (point) beg)
	  (when (looking-at "[ \t]*\\S-")
	    ;; this is not an empty line
	    (setq i (org-get-indentation))
	    (if (and (> i 0) (> (setq i (+ i delta)) 0))
		(indent-line-to i)))
	  (beginning-of-line 0))))))


(defvar org-last-indent-begin-marker (make-marker))
(defvar org-last-indent-end-marker (make-marker))

(defun org-outdent-item (arg)
  "Outdent a local list item, but not its children."
  (interactive "p")
  (org-indent-item-tree (- arg) 'no-subtree))

(defun org-indent-item (arg)
  "Indent a local list item, but not its children."
  (interactive "p")
  (org-indent-item-tree arg 'no-subtree))

(defun org-outdent-item-tree (arg &optional no-subtree)
  "Outdent a local list item including its children.
If NO-SUBTREE is set, only outdent the item itself, not its children."
  (interactive "p")
  (org-indent-item-tree (- arg) no-subtree))

(defun org-indent-item-tree (arg &optional no-subtree)
  "Indent a local list item including its children.
If NO-SUBTREE is set, only indent the item itself, not its children."
  (interactive "p")
  (and (org-region-active-p) (org-cursor-to-region-beginning))
  (unless (org-at-item-p)
    (error "Not on an item"))
  (let ((origin-ind (save-excursion
		      (goto-char (org-list-top-point))
		      (org-get-indentation)))
	beg end ind ind1 ind-bul delta ind-down ind-up firstp)
    (setq firstp (org-first-list-item-p))
    (save-excursion
      (setq end (and (org-region-active-p) (region-end)))
      (if (and (memq last-command '(org-shiftmetaright org-shiftmetaleft))
	       (memq this-command '(org-shiftmetaright org-shiftmetaleft)))
	  (setq beg org-last-indent-begin-marker
		end org-last-indent-end-marker)
	(org-beginning-of-item)
	(setq beg (move-marker org-last-indent-begin-marker (point)))
	(if no-subtree
	    (org-end-of-item-text-before-children)
	  (org-end-of-item))
	(setq end (move-marker org-last-indent-end-marker (or end (point)))))
      (goto-char beg)
      (setq ind-bul (org-item-indent-positions)
	    ind (caar ind-bul)
	    ind-down (car (nth 2 ind-bul))
	    ind-up (car (nth 1 ind-bul))
	    delta (if (> arg 0)
		      (if ind-down (- ind-down ind) 2)
		    (if ind-up (- ind-up ind) -2)))
      (if (and (< (+ delta ind) origin-ind)
	       ;; verify we're not at the top level item
	       (/= (point-at-bol) (org-list-top-point)))
	  (error "Cannot outdent beyond top level item"))
      (while (< (point) end)
	(beginning-of-line 1)
	(skip-chars-forward " \t") (setq ind1 (current-column))
	(delete-region (point-at-bol) (point))
	(or (eolp) (org-indent-to-column (+ ind1 delta)))
	(beginning-of-line 2)))
    (org-fix-bullet-type
     (and (> arg 0)
	  (not firstp)
	  (cdr (assoc (cdr (nth 0 ind-bul)) org-list-demote-modify-bullet))))
    (org-maybe-renumber-ordered-list-safe)
    (save-excursion
      (beginning-of-line 0)
      (ignore-errors (org-beginning-of-item))
      (org-maybe-renumber-ordered-list-safe))))

(defun org-item-indent-positions ()
  "Return indentation for plain list items.
This returns a list with three values:	The current indentation, the
parent indentation and the indentation a child should have.
Assumes cursor in item line."
  (let* ((bolpos (point-at-bol))
	 (ind (org-get-indentation))
	 (bullet (org-get-bullet))
	 ind-down ind-up bullet-up bullet-down pos)
    (save-excursion
      (org-beginning-of-item-list)
      (skip-chars-backward "\n\r \t")
      (when (org-in-item-p)
	(org-beginning-of-item)
	(let ((prev-indent (org-get-indentation)))
	  (when (< prev-indent ind)
	    (setq ind-up prev-indent)
	    (setq bullet-up (org-get-bullet))))))
    (setq pos (point))
    (save-excursion
      (cond
       ((and (ignore-errors (progn (org-previous-item) t))
	     (or (end-of-line) t)
	     (org-search-forward-unenclosed (org-item-re) bolpos t))
	(setq ind-down (org-get-indentation)
	      bullet-down (org-get-bullet)))
       ((and (goto-char pos)
	     (org-at-item-p))
	(goto-char (match-end 0))
	(skip-chars-forward " \t")
	(setq ind-down (current-column)
	      bullet-down (org-get-bullet)))))
    (if (and bullet-down (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet-down))
	(setq bullet-down (concat "1" (match-string 1 bullet-down))))
    (if (and bullet-up (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet-up))
	(setq bullet-up (concat "1" (match-string 1 bullet-up))))
    (if (and bullet (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet))
	(setq bullet (concat "1" (match-string 1 bullet))))
    (list (cons ind bullet)
	  (cons ind-up bullet-up)
	  (cons ind-down bullet-down))))

(defvar org-tab-ind-state) ; defined in org.el
(defun org-cycle-item-indentation ()
  (let ((org-suppress-item-indentation t)
	(org-adapt-indentation nil))
    (cond
     ((and (looking-at "[ \t]*$")
	   (org-looking-back "^\\([ \t]*\\)\\([-+*]\\|[0-9]+[).]\\)[ \t]+"))
      (setq this-command 'org-cycle-item-indentation)
      (if (eq last-command 'org-cycle-item-indentation)
	  (condition-case nil
	      (progn (org-outdent-item 1)
		     (if (equal org-tab-ind-state (org-get-indentation))
			 (org-outdent-item 1))
		     (end-of-line 1))
	    (error
	     (progn
	       (while (< (org-get-indentation) org-tab-ind-state)
		 (progn (org-indent-item 1) (end-of-line 1)))
	       (setq this-command 'org-cycle))))
	(setq org-tab-ind-state (org-get-indentation))
	(org-indent-item 1))
      t))))

;;; Bullets

(defun org-get-bullet ()
  (save-excursion
    (goto-char (point-at-bol))
    (and (looking-at
	  "^\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\(\\*\\)\\)\\( \\|$\\)")
	 (or (match-string 2) (match-string 4)))))

(defun org-fix-bullet-type (&optional force-bullet)
  "Make sure all items in this list have the same bullet as the first item.
Also, fix the indentation."
  (interactive)
  (unless (org-at-item-p) (error "This is not a list"))
  (org-preserve-lc
   (let* ((bullet
	   (progn
	     (org-beginning-of-item-list)
	     (looking-at "[ \t]*\\(\\S-+\\)")
	     (concat (or force-bullet (match-string 1)) " "
		     ;; do we need to concat another white space ?
		     (when (and org-list-two-spaces-after-bullet-regexp
				(string-match org-list-two-spaces-after-bullet-regexp bullet))
		       " "))))
	  (replace-bullet
	   (lambda (result bullet)
	     (let* ((old (progn
			   (skip-chars-forward " \t")
			   (looking-at "\\S-+ *")
			   (match-string 0))))
	       (unless (equal bullet old)
		 (replace-match bullet)
		 ;; when bullet lengths are differents, move the whole
		 ;; sublist accordingly
		 (org-shift-item-indentation (- (length bullet) (length old))))))))
     (org-apply-on-list replace-bullet nil bullet)
     ;; fix item numbers if necessary
     (when (string-match "[0-9]" bullet) (org-renumber-ordered-list)))))

(defun org-renumber-ordered-list (&optional arg)
  "Renumber an ordered plain list.
Cursor needs to be in the first line of an item, the line that starts
with something like \"1.\" or \"2)\". Start to count at ARG or 1."
  (interactive "p")
  (unless (and (org-at-item-p)
	       (match-beginning 3))
    (error "This is not an ordered list"))
  (org-preserve-lc
   (let* ((offset (progn
		    (org-beginning-of-item)
		    (or (and (looking-at "[ \t]*\\[@start:\\([0-9]+\\)")
			     (string-to-number (match-string 1)))
			arg
			1)))
	  (item-fmt (progn
		      (looking-at "[ \t]*[0-9]+\\([.)]\\)")
		      (concat "%d" (or (match-string 1) "."))))
	  ;; Here is the function applied at each item of the list.
	  (renumber-item (lambda (counter off fmt)
			   (let* ((new (format fmt (+ counter off)))
				  (old (progn
					 (looking-at (org-item-re))
					 (match-string 2)))
				  (begin (match-beginning 2))
				  (end (match-end 2)))
			     (delete-region begin end)
			     (goto-char begin)
			     (insert new)
			     ;; In case item number went from 9. to 10.
			     ;; or the other way.
			     (org-shift-item-indentation (- (length new) (length old)))
			     (1+ counter)))))
     (org-apply-on-list renumber-item 0 offset item-fmt))))

(defun org-maybe-renumber-ordered-list ()
  "Renumber the ordered list at point if setup allows it.
This tests the user option `org-auto-renumber-ordered-lists' before
doing the renumbering."
  (interactive)
  (when (and org-auto-renumber-ordered-lists
	     (org-at-item-p))
    (if (match-beginning 3)
	(org-renumber-ordered-list 1)
      (org-fix-bullet-type))))

(defun org-maybe-renumber-ordered-list-safe ()
  (ignore-errors
    (save-excursion
      (org-maybe-renumber-ordered-list))))

(defun org-cycle-list-bullet (&optional which)
  "Cycle through the different itemize/enumerate bullets.
This cycle the entire list level through the sequence:

   `-'	->  `+'  ->  `*'  ->  `1.'  ->	`1)'

If WHICH is a string, use that as the new bullet.  If WHICH is an integer,
0 means `-', 1 means `+' etc."
  (interactive "P")
  (org-preserve-lc
   (let* ((current (progn
		     (org-beginning-of-item-list)
		     (org-at-item-p)
		     (match-string 0)))
	  (prevp (eq which 'previous))
	  (new (cond
		((and (numberp which)
		      (nth (1- which) '("-" "+" "*" "1." "1)"))))
		((string-match "-" current) (if prevp "1)" "+"))
		((string-match "\\+" current)
		 (if prevp "-" (if (looking-at "\\S-") "1." "*")))
		((string-match "\\*" current) (if prevp "+" "1."))
		((string-match "\\." current)
		 (if prevp (if (looking-at "\\S-") "+" "*") "1)"))
		((string-match ")" current) (if prevp "1." "-"))
		(t (error "This should not happen"))))
	  (old (and (looking-at "\\([ \t]*\\)\\(\\S-+\\)")
		    (match-string 2))))
     (replace-match (concat "\\1" new))
     (org-shift-item-indentation (- (length new) (length old)))
     (org-fix-bullet-type)
     (org-maybe-renumber-ordered-list))))

;;; Checkboxes

(defun org-toggle-checkbox (&optional toggle-presence)
  "Toggle the checkbox in the current line.
With prefix arg TOGGLE-PRESENCE, add or remove checkboxes.
With double prefix, set checkbox to [-].
When there is an active region, toggle status or presence of the checkbox
in the first line, and make every item in the region have the same
status or presence, respectively.
If the cursor is in a headline, apply this to all checkbox items in the
text below the heading."
  (interactive "P")
  (catch 'exit
    (let (beg end status first-present first-status blocked)
      (cond
       ((org-region-active-p)
	(setq beg (region-beginning) end (region-end)))
       ((org-on-heading-p)
	(setq beg (point) end (save-excursion (outline-next-heading) (point))))
       ((org-at-item-checkbox-p)
	(save-excursion
	  (if (equal toggle-presence '(4))
	      (progn
		(replace-match "")
		(goto-char (match-beginning 0))
		(just-one-space))
	    (when (setq blocked (org-checkbox-blocked-p))
	      (error "Checkbox blocked because of unchecked box in line %d"
		     blocked))
	    (replace-match
	     (cond ((equal toggle-presence '(16)) "[-]")
		   ((member (match-string 0) '("[ ]" "[-]")) "[X]")
		   (t "[ ]"))
	     t t)))
	(throw 'exit t))
       ((org-at-item-p)
	;; add a checkbox
	(save-excursion
	  (goto-char (match-end 0))
	  (insert "[ ] "))
	(throw 'exit t))
       (t (error "Not at a checkbox or heading, and no active region")))
      (setq end (move-marker (make-marker) end))
      (save-excursion
	(goto-char beg)
	(setq first-present (org-at-item-checkbox-p)
	      first-status
	      (save-excursion
		(and (re-search-forward "[ \t]\\(\\[[ X]\\]\\)" end t)
		     (equal (match-string 1) "[X]"))))
	(while (< (point) end)
	  (if toggle-presence
	      (cond
	       ((and first-present (org-at-item-checkbox-p))
		(save-excursion
		  (replace-match "")
		  (goto-char (match-beginning 0))
		  (just-one-space)))
	       ((and (not first-present) (not (org-at-item-checkbox-p))
		     (org-at-item-p))
		(save-excursion
		  (goto-char (match-end 0))
		  (insert "[ ] "))))
	    (when (org-at-item-checkbox-p)
	      (setq status (equal (match-string 0) "[X]"))
	      (replace-match
	       (if first-status "[ ]" "[X]") t t)))
	  (beginning-of-line 2)))))
  (org-update-checkbox-count-maybe))

(defun org-reset-checkbox-state-subtree ()
  "Reset all checkboxes in an entry subtree."
  (interactive "*")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-show-subtree)
      (goto-char (point-min))
      (let ((end (point-max)))
	(while (< (point) end)
	  (when (org-at-item-checkbox-p)
	    (replace-match "[ ]" t t))
	  (beginning-of-line 2))))
    (org-update-checkbox-count-maybe)))

(defvar org-checkbox-statistics-hook nil
  "Hook that is run whenever Org thinks checkbox statistics should be updated.
This hook runs even if `org-provide-checkbox-statistics' is nil, to it can
be used to implement alternative ways of collecting statistics information.")

(defun org-update-checkbox-count-maybe ()
  "Update checkbox statistics unless turned off by user."
  (when org-provide-checkbox-statistics
    (org-update-checkbox-count))
  (run-hooks 'org-checkbox-statistics-hook))

(defun org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and update them
with the current numbers.  With optional prefix argument ALL, do this for
the whole buffer."
  (interactive "P")
  (save-excursion
    (let* ((buffer-invisibility-spec (org-inhibit-invisibility)) ; Emacs 21
	   (beg (condition-case nil
		    (progn (org-back-to-heading) (point))
		  (error (point-min))))
	   (end (move-marker (make-marker)
			     (progn (outline-next-heading) (point))))
	   (re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	   (re-box "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
	   (re-find (concat re "\\|" re-box))
	   beg-cookie end-cookie is-percent c-on c-off lim new
	   eline curr-ind next-ind continue-from startsearch
	   (recursive
	    (or (not org-hierarchical-checkbox-statistics)
		(string-match "\\<recursive\\>"
			      (or (ignore-errors
				    (org-entry-get nil "COOKIE_DATA"))
				  ""))))
	   (cstat 0)
	   )
      (when all
	(goto-char (point-min))
	(outline-next-heading)
	(setq beg (point) end (point-max)))
      (goto-char end)
      ;; find each statistics cookie
      (while (and (re-search-backward re-find beg t)
		  (not (save-match-data
			 (and (org-on-heading-p)
			      (string-match "\\<todo\\>"
					    (downcase
					     (or (org-entry-get
						  nil "COOKIE_DATA")
						 "")))))))
	(setq beg-cookie (match-beginning 1)
	      end-cookie (match-end 1)
	      cstat (+ cstat (if end-cookie 1 0))
	      startsearch (point-at-eol)
	      continue-from (match-beginning 0)
	      is-percent (match-beginning 2)
	      lim (cond
		   ((org-on-heading-p) (outline-next-heading) (point))
		   ((org-at-item-p) (org-end-of-item) (point))
		   (t nil))
	      c-on 0
	      c-off 0)
	(when lim
	  ;; find first checkbox for this cookie and gather
	  ;; statistics from all that are at this indentation level
	  (goto-char startsearch)
	  (if (re-search-forward re-box lim t)
	      (progn
		(org-beginning-of-item)
		(setq curr-ind (org-get-indentation))
		(setq next-ind curr-ind)
		(while (and (bolp) (org-at-item-p)
			    (if recursive
				(<= curr-ind next-ind)
			      (= curr-ind next-ind)))
		  (save-excursion (end-of-line) (setq eline (point)))
		  (if (re-search-forward re-box eline t)
		      (if (member (match-string 2) '("[ ]" "[-]"))
			  (setq c-off (1+ c-off))
			(setq c-on (1+ c-on))))
		  (if (not recursive)
		      ;; org-get-next-item goes through list-enders
		      ;; with proper limit.
		      (goto-char (or (org-get-next-item (point) lim) lim))
		    (end-of-line)
		    (when (org-search-forward-unenclosed (org-item-re) lim t)
		      (beginning-of-line)))
		  (setq next-ind (org-get-indentation)))))
	  (goto-char continue-from)
	  ;; update cookie
	  (when end-cookie
	    (setq new (if is-percent
			  (format "[%d%%]" (/ (* 100 c-on) (max 1 (+ c-on c-off))))
			(format "[%d/%d]" c-on (+ c-on c-off))))
	    (goto-char beg-cookie)
	    (insert new)
	    (delete-region (point) (+ (point) (- end-cookie beg-cookie))))
	  ;; update items checkbox if it has one
	  (when (org-at-item-p)
	    (org-beginning-of-item)
	    (when (and (> (+ c-on c-off) 0)
		       (re-search-forward re-box (point-at-eol) t))
	      (setq beg-cookie (match-beginning 2)
		    end-cookie (match-end       2))
	      (delete-region beg-cookie end-cookie)
	      (goto-char beg-cookie)
	      (cond ((= c-off 0) (insert "[X]"))
		    ((= c-on  0) (insert "[ ]"))
		    (t		(insert "[-]")))
	      )))
	(goto-char continue-from))
      (when (interactive-p)
	(message "Checkbox statistics updated %s (%d places)"
		 (if all "in entire file" "in current outline entry") cstat)))))

(defun org-get-checkbox-statistics-face ()
  "Select the face for checkbox statistics.
The face will be `org-done' when all relevant boxes are checked.
Otherwise it will be `org-todo'."
  (if (match-end 1)
      (if (equal (match-string 1) "100%")
	  'org-checkbox-statistics-done
	'org-checkbox-statistics-todo)
    (if (and (> (match-end 2) (match-beginning 2))
	     (equal (match-string 2) (match-string 3)))
	'org-checkbox-statistics-done
      'org-checkbox-statistics-todo)))

;;; Misc Tools

(defun org-apply-on-list (function init-value &rest args)
  "Call FUNCTION for each item of a the list under point.

FUNCTION must be called with at least one argument : a return
value that will contain the value returned by the function at
the previous item, plus ARGS extra arguments. INIT-VALUE will be
the value passed to the function at the first item of the list.

As an example, (org-apply-on-list (lambda (result) (1+ result)) 0)
will return the number of items in the current list.

Sublists of the list are skipped. Cursor is always at the
beginning of the item."
  (save-excursion
    (let ((move-down-action
	   (lambda (pos value &rest args)
	     (goto-char pos)
	     (let ((return-value (apply function value args))
		   ;; we need to recompute each time end of list in case
		   ;; function modified list.
		   (next-p (org-get-next-item pos (org-end-of-item-list))))
	       (if next-p
		   (apply move-down-action next-p return-value args)
		 return-value)))))
      (apply move-down-action (org-beginning-of-item-list) init-value args))))

(defun org-sort-list (&optional with-case sorting-type getkey-func compare-func)
  "Sort plain list items.
The cursor may be at any item of the list that should be sorted.
Sublists are not sorted.

Sorting can be alphabetically, numerically, by date/time as given by
a time stamp, by a property or by priority.

The command prompts for the sorting type unless it has been given to the
function through the SORTING-TYPE argument, which needs to be a character,
\(?n ?N ?a ?A ?t ?T ?s ?S ?d ?D ?p ?P ?r ?R ?f ?F).  Here is the
precise meaning of each character:

n   Numerically, by converting the beginning of the entry/item to a number.
a   Alphabetically, ignoring the TODO keyword and the priority, if any.
t   By date/time, either the first active time stamp in the entry, or, if
    none exist, by the first inactive one.
    In items, only the first line will be checked.

Capital letters will reverse the sort order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies a
function to be called with point at the beginning of the record.
It must return either a string or a number that should serve as
the sorting key for that record.

Comparing entries ignores case by default. However, with an
optional argument WITH-CASE, the sorting considers case as well."
  (interactive "P")
  (let* ((case-func (if with-case 'identity 'downcase))
	 (start (org-beginning-of-item-list))
	 (end (save-excursion (org-end-of-item-list)))
	 (sorting-type
	  (progn
	    (message
	     "Sort plain list: [a]lpha  [n]umeric  [t]ime  [f]unc   A/N/T/F means reversed:")
	    (read-char-exclusive)))
	 (getkey-func (and (= (downcase sorting-type) ?f)
			   (org-icompleting-read "Sort using function: "
						 obarray 'fboundp t nil nil)
			   (intern getkey-func))))
    (message "Sorting items...")
    (save-restriction
      (narrow-to-region start end)
      (let* ((dcst (downcase sorting-type))
	     (case-fold-search nil)
	     (now (current-time))
	     (sort-func (cond
			 ((= dcst ?a) 'string<)
			 ((= dcst ?f) compare-func)
			 ((member dcst '(?p ?t ?s ?d ?c)) '<)
			 (t nil)))
	     (begin-record (lambda ()
			     (skip-chars-forward " \r\t\n")
			     (beginning-of-line)))
	     (end-record (lambda ()
			   (goto-char (org-end-of-item-before-blank))))
	     (value-to-sort (lambda nil
			      (when (looking-at "[ \t]*[-+*0-9.)]+\\([ \t]+\\[[- X]\\]\\)?[ \t]+")
				(cond
				 ((= dcst ?n)
				  (string-to-number (buffer-substring (match-end 0)
								      (point-at-eol))))
				 ((= dcst ?a)
				  (buffer-substring (match-end 0) (point-at-eol)))
				 ((= dcst ?t)
				  (if (or (re-search-forward org-ts-regexp (point-at-eol) t)
					  (re-search-forward org-ts-regexp-both
							     (point-at-eol) t))
				      (org-time-string-to-seconds (match-string 0))
				    (org-float-time now)))
				 ((= dcst ?f)
				  (if getkey-func
				      (let ((value (funcall getkey-func)))
					(if (stringp value)
					    (funcall case-func value)
					  value))
				    (error "Invalid key function `%s'" getkey-func)))
				 (t (error "Invalid sorting type `%c'" sorting-type)))))))
	(sort-subr (/= dcst sorting-type) begin-record end-record value-to-sort nil sort-func)
	(org-maybe-renumber-ordered-list)
	(run-hooks 'org-after-sorting-entries-or-items-hook)
	(message "Sorting items...done")))))

;;; Send and receive lists

(defun org-list-parse-list (&optional delete)
  "Parse the list at point and maybe DELETE it.
Return a list containing first level items as strings and
sublevels as a list of strings."
  (let* ((start (goto-char (org-list-top-point)))
	 (end (org-list-bottom-point))
	 output itemsep ltype)
    (while (org-search-forward-unenclosed (org-item-re) end t)
      (save-excursion
	(beginning-of-line)
	(cond ((looking-at-p "^[ \t]*[0-9]")
	       (setq itemsep "[0-9]+\\(?:\\.\\|)\\)"
		     ltype 'ordered))
	      ((looking-at-p "^.*::")
	       (setq itemsep "[-+*]" ltype 'descriptive))
	      (t (setq itemsep "[-+*]" ltype 'unordered))))
      (let* ((indent1 (org-get-indentation))
	     (nextitem (or (org-get-next-item (point) end) end))
	     (item (org-trim (buffer-substring (point) (org-end-of-item-text-before-children))))
	     (nextindent (if (= (point) end) 0 (org-get-indentation)))
	     (item (if (string-match "^\\[\\([xX ]\\)\\]" item)
		       (replace-match (if (equal (match-string 1 item) " ")
					  "[CBOFF]"
					"[CBON]")
				      t nil item)
		     item)))
	(push item output)
	(when (> nextindent indent1)
	  (save-restriction
	    (narrow-to-region (point) nextitem)
	    (push (org-list-parse-list) output)))))
    (when delete
      (delete-region start end)
      (save-match-data
	(when (looking-at (org-list-end-re))
	  (replace-match "\n"))))
    (setq output (nreverse output))
    (push ltype output)))

(defun org-list-make-subtree ()
  "Convert the plain list at point into a subtree."
  (interactive)
  (goto-char (org-list-top-point))
  (let ((list (org-list-parse-list t)) nstars)
    (save-excursion
      (if (ignore-errors
	    (org-back-to-heading))
	  (progn (re-search-forward org-complex-heading-regexp nil t)
		 (setq nstars (length (match-string 1))))
	(setq nstars 0)))
    (org-list-make-subtrees list (1+ nstars))))

(defun org-list-make-subtrees (list level)
  "Convert LIST into subtrees starting at LEVEL."
  (if (symbolp (car list))
      (org-list-make-subtrees (cdr list) level)
    (mapcar (lambda (item)
	      (if (stringp item)
		  (insert (make-string
			   (if org-odd-levels-only
			       (1- (* 2 level)) level) ?*) " " item "\n")
		(org-list-make-subtrees item (1+ level))))
	    list)))

(defun org-list-insert-radio-list ()
  "Insert a radio list template appropriate for this major mode."
  (interactive)
  (let* ((e (assq major-mode org-list-radio-list-templates))
	 (txt (nth 1 e))
	 name pos)
    (unless e (error "No radio list setup defined for %s" major-mode))
    (setq name (read-string "List name: "))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

(defun org-list-send-list (&optional maybe)
  "Send a transformed version of this list to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined for
this list."
  (interactive)
  (catch 'exit
    (unless (org-at-item-p) (error "Not at a list"))
    (save-excursion
      (re-search-backward "#\\+ORGLST" nil t)
      (unless (looking-at "[ \t]*#\\+ORGLST[: \t][ \t]*SEND[ \t]+\\([^ \t\r\n]+\\)[ \t]+\\([^ \t\r\n]+\\)\\([ \t]+.*\\)?")
	(if maybe
	    (throw 'exit nil)
	  (error "Don't know how to transform this list"))))
    (let* ((name (match-string 1))
	   (transform (intern (match-string 2)))
	   (bottom-point
	    (save-excursion
	      (re-search-forward "\\(\\\\end{comment}\\|@end ignore\\|-->\\)" nil t)
	      (match-beginning 0)))
	   (top-point
	    (progn
	      (re-search-backward "#\\+ORGLST" nil t)
	      (re-search-forward (org-item-re) bottom-point t)
	      (match-beginning 0)))
	   (list (save-restriction
		   (narrow-to-region top-point bottom-point)
		   (org-list-parse-list)))
	   beg txt)
      (unless (fboundp transform)
	(error "No such transformation function %s" transform))
      (let ((txt (funcall transform list)))
	;; Find the insertion place
	(save-excursion
	  (goto-char (point-min))
	  (unless (re-search-forward
		   (concat "BEGIN RECEIVE ORGLST +" name "\\([ \t]\\|$\\)") nil t)
	    (error "Don't know where to insert translated list"))
	  (goto-char (match-beginning 0))
	  (beginning-of-line 2)
	  (setq beg (point))
	  (unless (re-search-forward (concat "END RECEIVE ORGLST +" name) nil t)
	    (error "Cannot find end of insertion region"))
	  (beginning-of-line 1)
	  (delete-region beg (point))
	  (goto-char beg)
	  (insert txt "\n")))
      (message "List converted and installed at receiver location"))))

(defun org-list-to-generic (list params)
  "Convert a LIST parsed through `org-list-parse-list' to other formats.

Valid parameters PARAMS are

:ustart	    String to start an unordered list
:uend	    String to end an unordered list

:ostart	    String to start an ordered list
:oend	    String to end an ordered list

:dstart	    String to start a descriptive list
:dend	    String to end a descriptive list
:dtstart    String to start a descriptive term
:dtend	    String to end a descriptive term
:ddstart    String to start a description
:ddend	    String to end a description

:splice	    When set to t, return only list body lines, don't wrap
	    them into :[u/o]start and :[u/o]end.  Default is nil.

:istart	    String to start a list item
:iend	    String to end a list item
:isep	    String to separate items
:lsep	    String to separate sublists

:cboff      String to insert for an unchecked checkbox
:cbon       String to insert for a checked checkbox"
  (interactive)
  (let* ((p params) sublist
	 (splicep (plist-get p :splice))
	 (ostart (plist-get p :ostart))
	 (oend (plist-get p :oend))
	 (ustart (plist-get p :ustart))
	 (uend (plist-get p :uend))
	 (dstart (plist-get p :dstart))
	 (dend (plist-get p :dend))
	 (dtstart (plist-get p :dtstart))
	 (dtend (plist-get p :dtend))
	 (ddstart (plist-get p :ddstart))
	 (ddend (plist-get p :ddend))
	 (istart (plist-get p :istart))
	 (iend (plist-get p :iend))
	 (isep (plist-get p :isep))
	 (lsep (plist-get p :lsep))
	 (cbon (plist-get p :cbon))
	 (cboff (plist-get p :cboff)))
    (let ((wrapper
	   (cond ((eq (car list) 'ordered)
		  (concat ostart "\n%s" oend "\n"))
		 ((eq (car list) 'unordered)
		  (concat ustart "\n%s" uend "\n"))
		 ((eq (car list) 'descriptive)
		  (concat dstart "\n%s" dend "\n"))))
	  rtn term defstart defend)
      (while (setq sublist (pop list))
	(cond ((symbolp sublist) nil)
	      ((stringp sublist)
	       (when (string-match "^\\(.*\\) ::" sublist)
		 (setq term (org-trim (format (concat dtstart "%s" dtend)
					      (match-string 1 sublist))))
		 (setq sublist (concat ddstart
				       (org-trim (substring sublist (match-end 0)))
				       ddend)))
	       (if (string-match "\\[CBON\\]" sublist)
		   (setq sublist (replace-match cbon t t sublist)))
	       (if (string-match "\\[CBOFF\\]" sublist)
		   (setq sublist (replace-match cboff t t sublist)))
	       (if (string-match "\\[-\\]" sublist)
		   (setq sublist (replace-match "$\\boxminus$" t t sublist)))
	       (setq rtn (concat rtn istart term sublist iend isep)))
	      (t (setq rtn (concat rtn	;; previous list
				   lsep	;; list separator
				   (org-list-to-generic sublist p)
				   lsep	;; list separator
				   )))))
      (format wrapper rtn))))

(defun org-list-to-latex (list &optional params)
  "Convert LIST into a LaTeX list.
LIST is as returned by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splicep nil :ostart "\\begin{enumerate}" :oend "\\end{enumerate}"
	       :ustart "\\begin{itemize}" :uend "\\end{itemize}"
	       :dstart "\\begin{description}" :dend "\\end{description}"
	       :dtstart "[" :dtend "]"
	       :ddstart "" :ddend ""
	       :istart "\\item " :iend ""
	       :isep "\n" :lsep "\n"
	       :cbon "\\texttt{[X]}" :cboff "\\texttt{[ ]}")
    params)))

(defun org-list-to-html (list &optional params)
  "Convert LIST into a HTML list.
LIST is as returned by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splicep nil :ostart "<ol>" :oend "</ol>"
	       :ustart "<ul>" :uend "</ul>"
	       :dstart "<dl>" :dend "</dl>"
	       :dtstart "<dt>" :dtend "</dt>"
	       :ddstart "<dd>" :ddend "</dd>"
	       :istart "<li>" :iend "</li>"
	       :isep "\n" :lsep "\n"
	       :cbon "<code>[X]</code>" :cboff "<code>[ ]</code>")
    params)))

(defun org-list-to-texinfo (list &optional params)
  "Convert LIST into a Texinfo list.
LIST is as returned by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splicep nil :ostart "@itemize @minus" :oend "@end itemize"
	       :ustart "@enumerate" :uend "@end enumerate"
	       :dstart "@table" :dend "@end table"
	       :dtstart "@item " :dtend "\n"
	       :ddstart "" :ddend ""
	       :istart "@item\n" :iend ""
	       :isep "\n" :lsep "\n"
	       :cbon "@code{[X]}" :cboff "@code{[ ]}")
    params)))

(provide 'org-list)

;; arch-tag: 73cf50c1-200f-4d1d-8a53-4e842a5b11c8
;;; org-list.el ends here
