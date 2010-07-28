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
spaces instead of one after the bullet in each item of the list."
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

(defcustom org-list-automatic-rules '((bullet . t)
				      (checkbox . t)
				      (indent . t)
				      (insert . t)
				      (renumber . t))
  "Non-nil means apply set of rules when acting on lists.

By default, automatic actions are taken when using
\\[org-shiftmetaup], \\[org-shiftmetadown], \\[org-meta-return],
\\[org-metaright], \\[org-metaleft], \\[org-shiftmetaright],
\\[org-shiftmetaleft], \\[org-ctrl-c-minus] or
\\[org-insert-todo-heading].  You can disable individually these
rules by setting sets to nil. Valid sets are:

bullet    when non-nil, cycling bullet do not allow lists at
          column 0 to have * as a bullet and descriptions lists
          to be numbered.
checkbox  when non-nil, checkbox statistics is updated each time
          you either insert a new checkbox or toggle a checkbox.
indent    when non-nil indenting or outdenting list top-item will
          move the whole list, indenting the first item of a
          sub-list will be forbidden and outdenting a list whose
          bullet is * to column 0 will change that bullet to -.
insert    when non-nil, trying to insert an item inside a block
          will insert it right before the block instead of
          throwing an error.
renumber  when non-nil, renumber ordered plain lists whenever it
          is modified.  You can always use \\[org-ctrl-c-ctrl-c]
          to trigger renumbering."
   :group 'org-plain-lists
   :type '(alist :tag "Sets of rules"
		 :key-type
		 (choice
		  (const :tag "Bullet" bullet)
		  (const :tag "Checkbox" checkbox)
		  (const :tag "Indent" indent)
		  (const :tag "Insert" insert)
		  (const :tag "Renumber" renumber))
		 :value-type
		 (boolean :tag "Activate" :value t)))

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
    "\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\*\\)\\([ \t]+\\|$\\)")
   ((= org-plain-list-ordered-item-terminator ?.)
    "\\([ \t]*\\([-+]\\|\\([0-9]+\\.\\)\\)\\|[ \t]+\\*\\)\\([ \t]+\\|$\\)")
   ((= org-plain-list-ordered-item-terminator ?\))
    "\\([ \t]*\\([-+]\\|\\([0-9]+)\\)\\)\\|[ \t]+\\*\\)\\([ \t]+\\|$\\)")
   (t (error "Invalid value of `org-plain-list-ordered-item-terminator'"))))

(defconst org-item-beginning-re (concat "^" (org-item-re))
  "Regexp matching the beginning of a plain list item.")

(defun org-list-terminator-between (min max &optional firstp)
  "Find the position of a list ender between MIN and MAX, or nil.
This function looks for `org-list-end-re' outside a block.

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
			 (funcall search-fun (org-list-end-re) end t))))
      ;; Is there a valid list terminator somewhere ?
      (and list-end-p
	   ;; we want to be on the first line of the list ender
	   (match-beginning 0)))))

(defun org-list-search-unenclosed-generic (search-fun regexp bound noerror count)
  "Search for REGEXP with SEARCH-FUN but don't stop inside blocks or at protected places."
  (let ((origin (point)))
    (cond
     ;; nothing found: return nil
     ((not (funcall search-fun regexp bound noerror count)) nil)
     ((or (save-match-data
	    (org-in-regexps-block-p "^[ \t]*#\\+\\(begin\\|BEGIN\\)_\\([a-zA-Z0-9_]+\\)"
				    '(concat "^[ \t]*#\\+\\(end\\|END\\)_" (match-string 2))))
	  (get-text-property (match-beginning 0) 'org-protected))
      ;; match is enclosed or protected: start again, searching one
      ;; occurrence away.
      (goto-char origin)
      (org-list-search-unenclosed-generic search-fun regexp bound noerror (1+ count)))
     ;; else return point.
     (t (point)))))

(defun org-search-backward-unenclosed (regexp &optional bound noerror)
  "Like `re-search-backward' but don't stop inside blocks or at protected places."
  (org-list-search-unenclosed-generic #'re-search-backward regexp bound noerror 1))

(defun org-search-forward-unenclosed (regexp &optional bound noerror)
  "Like `re-search-forward' but don't stop inside blocks or at protected places."
  (org-list-search-unenclosed-generic #'re-search-forward regexp bound noerror 1))

(defun org-list-at-regexp-after-bullet-p (regexp)
  "Is point at a list item with REGEXP after bullet?"
  (and (org-at-item-p)
       (save-excursion
	 (goto-char (match-end 0))
         ;; Ignore counter if any
         (when (looking-at "\\(?:\\[@start:[0-9]+\\][ \t]*\\)?")
           (goto-char (match-end 0)))
	 (looking-at regexp))))

(defun org-list-get-item-same-level (search-fun pos limit pre-move)
  "Return point at the beginning of next item at the same level.
Search items using function SEARCH-FUN, from POS to LIMIT. It
uses PRE-MOVE before search. Return nil if no item was found."
  (save-excursion
    (goto-char pos)
    (let ((ind (progn
		 (org-beginning-of-item)
		 (org-get-indentation)))
	  (start (point-at-bol)))
      ;; We don't want to match the current line.
      (funcall pre-move)
      ;; Skip any sublist on the way
      (while (and (funcall search-fun org-item-beginning-re limit t)
		  (> (org-get-indentation) ind)))
      (when (and (/= (point-at-bol) start) ; Have we moved ?
		 (= (org-get-indentation) ind))
	(point-at-bol)))))

(defun org-list-insert-item-generic (pos &optional checkbox after-bullet)
  "Insert a new list item at POS.

If POS is before first character after bullet of the item, the
new item will be created before the current one.

Insert a checkbox if CHECKBOX is non-nil, and string AFTER-BULLET
after the bullet. Cursor will be after this text once the
function ends."
  (goto-char pos)
  ;; Is point in a special block?
  (when (org-in-regexps-block-p
	 "^[ \t]*#\\+\\(begin\\|BEGIN\\)_\\([a-zA-Z0-9_]+\\)"
	 '(concat "^[ \t]*#\\+\\(end\\|END\\)_" (match-string 2)))
    (if (not (cdr (assq 'insert org-list-automatic-rules)))
	;; Rule in `org-list-automatic-rules' forbids insertion.
	(error "Cannot insert item inside a block.")
      ;; Else, move before it prior to add a new item.
      (end-of-line)
      (re-search-backward "^[ \t]*#\\+\\(begin\\|BEGIN\\)_" nil t)
      (end-of-line 0)))
  (let* ((true-pos (point))
	 (bullet (and (org-beginning-of-item)
		      (org-list-bullet-string (org-get-bullet))))
         (ind (org-get-indentation))
	 (before-p (progn
		     ;; Description item: text starts after colons.
		     (or (org-at-item-description-p)
			 ;; At a checkbox: text starts after it.
			 (org-at-item-checkbox-p)
			 ;; Otherwise, text starts after bullet.
			 (org-at-item-p))
		     (<= true-pos (match-end 0))))
	 ;; Guess number of blank lines used to separate items.
	 (blank-lines-nb
	  (let ((insert-blank-p
		 (cdr (assq 'plain-list-item org-blank-before-new-entry))))
	    (cond
	     ;; Trivial cases where there should be none.
	     ((or org-empty-line-terminates-plain-lists
		  (not insert-blank-p)) 0)
	     ;; When `org-blank-before-new-entry' says so, it is 1.
	     ((eq insert-blank-p t) 1)
	     ;; plain-list-item is 'auto. Count blank lines separating
	     ;; neighbours items in list.
	     (t (let ((next-p (org-get-next-item (point) (org-list-bottom-point))))
		  (cond
		   ;; Is there a next item?
		   (next-p (goto-char next-p)
			   (org-back-over-empty-lines))
		   ;; Is there a previous item?
		   ((not (org-list-first-item-p)) (org-back-over-empty-lines))
		   ;; Item alone: count lines separating it from parent, if any
		   ((/= (org-list-top-point) (point-at-bol))
		    (org-back-over-empty-lines))
		   ;; No parent: no blank line.
		   (t 0)))))))
	 (insert-fun
	  (lambda (text)
	    ;; insert bullet above item in order to avoid bothering
	    ;; with possible blank lines ending last item.
	    (org-beginning-of-item)
            (indent-to-column ind)
	    (insert (concat bullet (when checkbox "[ ] ") after-bullet))
	    ;; Stay between after-bullet and before text.
	    (save-excursion
	      (insert (concat text (make-string (1+ blank-lines-nb) ?\n))))
	    (unless before-p (org-move-item-down))
	    (when checkbox (org-update-checkbox-count-maybe)))))
    (goto-char true-pos)
    (cond
     (before-p (funcall insert-fun nil)
	       ;; Not taking advantage of renumbering while moving
	       ;; down. Need to call it directly.
	       (org-maybe-renumber-ordered-list) t)
     ;; Can't split item: insert bullet at the end of item.
     ((not (org-get-alist-option org-M-RET-may-split-line 'item))
      (funcall insert-fun nil) t)
     ;; else, insert a new bullet along with everything from point
     ;; down to last non-blank line of item.
     (t
      (delete-horizontal-space)
      ;; Get pos again in case previous command modified line.
      (let* ((pos (point))
	     (end-before-blank (org-end-of-item-before-blank))
	     (after-text
	      (when (< pos end-before-blank)
		(prog1
		    (delete-and-extract-region pos end-before-blank)
		  ;; delete any blank line at and before point.
		  (beginning-of-line)
		  (while (looking-at "^[ \t]*$")
		    (delete-region (point-at-bol) (1+ (point-at-eol)))
		    (beginning-of-line 0))))))
	(funcall insert-fun after-text) t)))))

;;; Predicates

(defun org-in-item-p ()
  "Is the cursor inside a plain list ?"
  (unless (let ((outline-regexp org-outline-regexp)) (org-at-heading-p))
    (save-excursion
      (let* ((limit (save-excursion (outline-previous-heading)))
	     ;; Move to eol so current line can be matched by `org-item-re'.
	     (actual-pos (goto-char (point-at-eol)))
	     (last-item-start (save-excursion
				(org-search-backward-unenclosed org-item-beginning-re limit t)))
	     (list-ender (org-list-terminator-between last-item-start actual-pos)))
	;; We are in a list when we are on an item line or when we can
	;; find an item before point and there is no valid list ender
	;; between it and the point.
	(and last-item-start
	     (not list-ender))))))

(defun org-list-first-item-p ()
  "Is this item the first item in a plain list?"
  (save-excursion
    (= (org-beginning-of-item) (org-beginning-of-item-list))))

(defun org-at-item-p ()
  "Is point in a line starting a hand-formatted item?"
  (save-excursion
    (beginning-of-line) (looking-at org-item-beginning-re)))

(defun org-at-item-bullet-p ()
  "Is point at the bullet of a plain list item?"
  (and (org-at-item-p)
       (not (member (char-after) '(?\  ?\t)))
       (< (point) (match-end 0))))

(defun org-at-item-timer-p ()
  "Is point at a line starting a plain list item with a timer?"
  (org-list-at-regexp-after-bullet-p "\\([0-9]+:[0-9]+:[0-9]+\\)[ \t]+::[ \t]+"))

(defun org-at-item-description-p ()
  "Is point at a description list item?"
  (org-list-at-regexp-after-bullet-p "\\(\\S-.+\\)[ \t]+::[ \t]+"))

(defun org-at-item-checkbox-p ()
  "Is point at a line starting a plain-list item with a checklet?"
  (org-list-at-regexp-after-bullet-p "\\(\\[[- X]\\]\\)[ \t]+"))

(defun org-item-has-child-p ()
  "Does the current item have subitems?"
  (save-excursion
    (org-beginning-of-item)
    (let ((ind (org-get-indentation)))
      (org-end-of-item-or-at-child)
      (and (org-at-item-p)
	   (> (org-get-indentation) ind)))))

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
	(when (equal (match-string 1) "[X]")
	  ;; the box is already checked!
	  (throw 'exit nil))
	(let ((end (point-at-bol)))
	  (condition-case nil (org-back-to-heading t)
	    (error (throw 'exit nil)))
	  (unless (org-entry-get nil "ORDERED") (throw 'exit nil))
	  (when (org-search-forward-unenclosed
                 "^[ \t]*[-+*0-9.)]+[ \t]+\\(\\[@start:[0-9]+\\][ \t]+\\)?\\[[- ]\\]" end t)
	    (org-current-line)))))))

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
	   (org-search-forward-unenclosed org-item-beginning-re pos t)
	   (point-at-bol)))))

(defun org-list-bottom-point ()
  "Return point just before list ending or nil if not in a list."
  (save-excursion
    (and (org-in-item-p)
	 (let ((pos (org-beginning-of-item))
	       (bound (or (and (let ((outline-regexp org-outline-regexp))
				 ;; Use default regexp because folding
				 ;; changes OUTLINE-REGEXP.
				 (outline-next-heading))
			       (skip-chars-backward " \t\r\n")
			       (1+ (point-at-eol)))
			  (point-max))))
	   ;; The list ending is either first point matching
	   ;; `org-list-end-re', point at first white-line before next
	   ;; heading, or eob.
	   (or (org-list-terminator-between pos bound t) bound)))))

(defun org-beginning-of-item ()
  "Go to the beginning of the current hand-formatted item.
If the cursor is not in an item, throw an error. Return point."
  (interactive)
  (if (not (org-in-item-p))
      (error "Not in an item")
    ;; Possibly match the current line.
    (end-of-line)
    (org-search-backward-unenclosed org-item-beginning-re nil t)
    (goto-char (point-at-bol))))

(defun org-end-of-item ()
  "Go to the end of the current hand-formatted item.
If the cursor is not in an item, throw an error."
  (interactive)
  (let ((next-p (org-get-next-item (point) (org-list-bottom-point))))
    (cond ((not (org-in-item-p)) (error "Not in an item"))
	  (next-p (goto-char next-p))
	  (t (org-end-of-item-list)))))

(defun org-end-of-item-or-at-child ()
  "Move to the end of the item text, stops before the first child if any."
  (let ((limit (org-list-bottom-point)))
    (end-of-line)
    (goto-char
     (if (org-search-forward-unenclosed org-item-beginning-re limit t)
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
  (org-list-get-item-same-level
   #'org-search-forward-unenclosed pos limit #'end-of-line))

(defun org-get-previous-item (pos limit)
  "Get the point of the previous item at the same level as POS.
 Stop searching at LIMIT. Return nil if no item is found. This
 function does not move point."
  (org-list-get-item-same-level
   #'org-search-backward-unenclosed pos limit #'beginning-of-line))

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
  (org-beginning-of-item)
  (let ((limit (org-list-top-point))
	(move-up (lambda (pos bound)
		   ;; prev-p: any item of same level before ?
		   (let ((prev-p (org-get-previous-item pos bound)))
		     ;; recurse until no more item of the same level
		     ;; can be found.
		     (if prev-p (funcall move-up prev-p bound) pos)))))
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
			   (if next-p (funcall get-last-item next-p bound) pos)))))
    ;; Move to the last item of every list or sublist encountered, and
    ;; down to bol of a higher-level item, or limit.
    (while (and (/= (point) limit)
		(>= (org-get-indentation) ind))
      (goto-char (funcall get-last-item (point) limit))
      (end-of-line)
      (when (org-search-forward-unenclosed org-item-beginning-re limit 'move)
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
    (if (save-excursion
	  (org-beginning-of-item)
	  (org-at-item-timer-p))
	;; Timer list: delegate to `org-timer-item'.
	(progn (org-timer-item) t)
      ;; if we're in a description list, ask for the new term.
      (let ((desc-text (when (save-excursion
			       (and (org-beginning-of-item)
				    (org-at-item-description-p)))
			 (concat (read-string "Term: ") " :: "))))
	(org-list-insert-item-generic
	 (point) (and checkbox (not desc-text)) desc-text)))))

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
	    (end (org-end-of-item)))
	(beginning-of-line 0)
	(while (> (point) beg)
	  (when (looking-at "[ \t]*\\S-")
	    ;; this is not an empty line
	    (let ((i (org-get-indentation)))
	      (when (and (> i 0) (> (+ i delta) 0))
		(indent-line-to (+ i delta)))))
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
If NO-SUBTREE is set, only indent the item itself, not its
children. Return t if sucessful."
  (interactive "p")
  (and (org-region-active-p) (org-cursor-to-region-beginning))
  (unless (org-at-item-p)
    (error "Not on an item"))
  (let ((line (org-current-line))
	(col (current-column))
	(pos (point))
	(origin-ind (save-excursion
		      (goto-char (org-list-top-point))
		      (org-get-indentation)))
	beg end ind ind1 ind-pos bullet delta ind-down ind-up)
    (setq end (and (org-region-active-p) (region-end)))
    ;; If moving a subtree, don't drag additional items on subsequent
    ;; moves.
    (if (and (memq last-command '(org-shiftmetaright org-shiftmetaleft))
	     (memq this-command '(org-shiftmetaright org-shiftmetaleft)))
	(setq beg org-last-indent-begin-marker
	      end org-last-indent-end-marker)
      (org-beginning-of-item)
      (setq beg (move-marker org-last-indent-begin-marker (point)))
      ;; Determine end point of indentation
      (if no-subtree (org-end-of-item-or-at-child) (org-end-of-item))
      (setq end (move-marker org-last-indent-end-marker (or end (point)))))
    ;; Get some information
    (goto-char beg)
    (setq ind-pos (org-item-indent-positions)
	  bullet (cdr (car ind-pos))
          bul-up (cdr (nth 1 ind-pos))
	  ind (caar ind-pos)
	  ind-down (car (nth 2 ind-pos))
	  ind-up (car (nth 1 ind-pos))
	  delta (if (> arg 0)
		    (if ind-down (- ind-down ind) 2)
		  (if ind-up (- ind-up ind) -2)))
    ;; Make some checks before indenting.
    (cond
     ((< (+ delta ind) 0)
      (goto-char pos)
      (error "Cannot outdent beyond margin"))
     ;; Apply indent rules if activated.
     ((cdr (assq 'indent org-list-automatic-rules))
      (cond
       ;; 1. If at top-point move the whole list. Moreover, if
       ;; *-list is going to column 0, change bullet to "-".
       ((= (point-at-bol) (org-list-top-point))
	(when (and (= (+ delta ind) 0) (equal bullet "*"))
	  (org-fix-bullet-type (setq bullet "-")))
	(setq end (set-marker org-last-indent-end-marker (org-list-bottom-point))))
       ;; 2. Do not indent before top-item.
       ((< (+ delta ind) origin-ind)
	(goto-char pos)
	(error "Cannot outdent beyond top level item"))
       ;; 3. Do not indent the first item of a list.
       ((and (org-list-first-item-p) (> delta 0))
	(goto-char pos)
	(error "Cannot indent the beginning of a sublist"))
       ;; 4. Do not outdent item that has children without moving.
       ;; In the case of a subtree, make sure the check applies to
       ;; its last item.
       ((and (< delta 0)
	     (save-excursion (goto-char (1- end)) (org-item-has-child-p)))
	(goto-char pos)
	(error "Cannot outdent an item having children")))))
    ;; Replace bullet of current item with the bullet it is going to
    ;; have if we're outdenting. This is needed to prevent indentation
    ;; problems of subtrees when outdenting changes bullet size.
    (when (< delta 0)
      (let ((new-bul (org-list-bullet-string (or bul-up bullet))))
        (org-list-replace-bullet new-bul)))
    ;; Proceed to reindentation.
    (while (< (point) end)
      (beginning-of-line)
      (skip-chars-forward " \t") (setq ind1 (current-column))
      (delete-region (point-at-bol) (point))
      (or (eolp) (org-indent-to-column (+ ind1 delta)))
      (beginning-of-line 2))
    ;; Get back to original position, shifted by delta
    (goto-line line)
    (move-to-column (max (+ delta col) 0))
    ;; Fix and reorder all lists and sublists from parent of the list
    ;; at point, or from list at point if it hasn't got any parent or
    ;; if we're outdenting.
    (save-excursion
      ;; Take care of parent list, if it makes sense.
      (org-beginning-of-item-list)
      (unless (or (< arg 0) (= (org-list-top-point) (point)))
        (beginning-of-line 0)
        (org-beginning-of-item)
        (org-fix-bullet-type)))
    ;; Take care of list at point. If demoting, look at
    ;; `org-list-demote-modify-bullet'.
    (org-fix-bullet-type
     (and (> arg 0)
	  (cdr (assoc bullet org-list-demote-modify-bullet))))
    (save-excursion
      (when (org-item-has-child-p)
        ;; Take care of child, or of every sublist if we're moving a
        ;; subtree.
        (org-end-of-item-or-at-child)
        (if no-subtree
            (org-fix-bullet-type)
          (let ((fix-list (lambda (i)
                            (when (org-list-first-item-p)
                              (org-fix-bullet-type
                               (and (> arg 0)
                                    (cdr (assoc (org-get-bullet) org-list-demote-modify-bullet)))))
                            (when (org-item-has-child-p)
                              (org-end-of-item-or-at-child)
                              (org-apply-on-list fix-list nil)))))
            (org-apply-on-list fix-list nil))))))
  t)

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
	     (org-search-forward-unenclosed org-item-beginning-re bolpos t))
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

(defvar org-tab-ind-state)
(defun org-cycle-item-indentation ()
  (let ((org-suppress-item-indentation t)
	(org-adapt-indentation nil))
    (when (and (or (org-at-item-description-p) (org-at-item-checkbox-p) (org-at-item-p))
	       (>= (match-end 0) (save-excursion
                                   (org-end-of-item-or-at-child)
                                   (skip-chars-backward " \r\t\n")
                                   (point))))
      (setq this-command 'org-cycle-item-indentation)
      ;; When in the middle of the cycle, try to outdent first. If it
      ;; fails, and point is still at initial position, indent. Else,
      ;; go back to original position.
      (if (eq last-command 'org-cycle-item-indentation)
	  (cond
	   ((ignore-errors (org-indent-item -1)))
	   ((and (= (org-get-indentation) (car org-tab-ind-state))
		 (ignore-errors (org-indent-item 1))))
	   (t (back-to-indentation)
	      (indent-to-column (car org-tab-ind-state))
	      (end-of-line)
	      (org-fix-bullet-type (nth 1 org-tab-ind-state))
	      ;; Break cycle
	      (setq this-command 'identity)))
	;; If a cycle is starting, remember indentation and bullet,
        ;; then try to indent. If it fails, try to outdent.
	(setq org-tab-ind-state
              (list (org-get-indentation) (org-get-bullet)))
	(cond
	 ((ignore-errors (org-indent-item 1)))
	 ((ignore-errors (org-indent-item -1)))
	 (t (error "Cannot move item"))))
      t)))

;;; Bullets

(defun org-get-bullet ()
  (and (org-at-item-p)
       (org-trim (match-string 1))))

(defun org-list-bullet-string (bullet)
  "Concatenate BULLET with an appropriate number of whitespaces.
It determines the number of whitespaces to append by looking at
`org-list-two-spaces-after-bullet-regexp'."
  (concat
   bullet " "
   ;; Do we need to concat another white space ?
   (when (string-match org-list-two-spaces-after-bullet-regexp bullet) " ")))

(defun org-list-replace-bullet (new-bullet)
  "Replace current item's bullet with NEW-BULLET.
Assume point is at item. Indent body if needed."
  (save-excursion
    (let ((old (progn
                 (looking-at "[ \t]*\\(\\S-+[ \t]*\\)")
                 (match-string 1))))
      (unless (equal new-bullet old)
        (replace-match new-bullet nil nil nil 1)
        ;; When bullet lengths are differents, move the whole
        ;; sublist accordingly
        (org-shift-item-indentation (- (length new-bullet) (length old)))))))

(defun org-fix-bullet-type (&optional force-bullet)
  "Make sure all items in this list have the same bullet as the first item.
Also, fix the indentation."
  (interactive)
  (unless (org-at-item-p) (error "This is not a list"))
  (org-preserve-lc
   (let* ((ini-bul (progn (org-beginning-of-item-list) (org-get-bullet)))
	  (bullet (org-list-bullet-string (or force-bullet ini-bul)))
	  (replace-bullet
	   (lambda (result bullet)
	     (org-list-replace-bullet bullet))))
     (org-apply-on-list replace-bullet nil bullet)
     (org-maybe-renumber-ordered-list))))

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
					 (looking-at org-item-beginning-re)
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
This tests the if 'renumber rule is set in
`org-list-automatic-rules' before doing the renumbering.
Do not throw error on failure."
  (interactive)
  (when (cdr (assq 'renumber org-list-automatic-rules))
    (ignore-errors (org-renumber-ordered-list))))

(defun org-cycle-list-bullet (&optional which)
  "Cycle through the different itemize/enumerate bullets.
This cycle the entire list level through the sequence:

   `-'  ->  `+'  ->  `*'  ->  `1.'  ->  `1)'

If WHICH is a valid string, use that as the new bullet. If WHICH
is an integer, 0 means `-', 1 means `+' etc. If WHICH is
'previous, cycle backwards."
  (interactive "P")
  (org-preserve-lc
   (let* ((bullet (progn (org-beginning-of-item-list)
			 (org-get-bullet)))
	  (current (cond
		    ((string-match "\\." bullet) "1.")
		    ((string-match ")" bullet) "1)")
		    (t bullet)))
	  (bullet-rule-p (cdr (assq 'bullet org-list-automatic-rules)))
	  (bullet-list (append '("-" "+" )
			       ;; *-bullets are not allowed at column 0
			       (unless (and bullet-rule-p
					    (looking-at "\\S-")) '("*"))
			       ;; Description items cannot be numbered
			       (unless (and bullet-rule-p
					    (or (eq org-plain-list-ordered-item-terminator ?.)
						(org-at-item-description-p))) '("1)"))
			       (unless (and bullet-rule-p
					    (or (eq  org-plain-list-ordered-item-terminator ?\))
						(org-at-item-description-p))) '("1."))))
	  (len (length bullet-list))
	  (item-index (- len (length (member current bullet-list))))
	  (get-value (lambda (index) (nth (mod index len) bullet-list)))
	  (new (cond
		((member which bullet-list) which)
		((numberp which) (funcall get-value which))
		((eq 'previous which) (funcall get-value (1- item-index)))
		(t (funcall get-value (1+ item-index))))))
     (org-fix-bullet-type new))))

;;; Checkboxes

(defun org-toggle-checkbox (&optional toggle-presence)
  "Toggle the checkbox in the current line.

With prefix arg TOGGLE-PRESENCE, add or remove checkboxes.  With
double prefix, set checkbox to [-].

When there is an active region, toggle status or presence of the
checkbox in the first line, and make every item in the region
have the same status or presence, respectively.

If the cursor is in a headline, apply this to all checkbox items
in the text below the heading, taking as reference the first item
in subtree."
  (interactive "P")
  ;; Bounds is a list of type (beg end single-p) where single-p is t
  ;; when `org-toggle-checkbox' is applied to a single item. Only
  ;; toggles on single items will return errors.
  (let* ((bounds
          (cond
           ((org-region-active-p)
            (list (region-beginning) (region-end) nil))
           ((org-on-heading-p)
            ;; In this case, reference line is the first item in subtree
            (let ((limit (save-excursion (outline-next-heading) (point))))
              (save-excursion
                (org-search-forward-unenclosed org-item-beginning-re limit 'move)
                (list (point) limit nil))))
           ((org-at-item-p)
            (list (point-at-bol) (point-at-eol) t))
           (t (error "Not at an item or heading, and no active region"))))
         ;; marker is needed because deleting checkboxes will change END
         (end (copy-marker (nth 1 bounds)))
         (single-p (nth 2 bounds))
         (ref-presence (save-excursion (goto-char (car bounds)) (org-at-item-checkbox-p)))
         (ref-status (equal (match-string 1) "[X]"))
         (act-on-item
          (lambda (ref-pres ref-stat)
            (if (equal toggle-presence '(4))
                (cond
                 ((and ref-pres (org-at-item-checkbox-p))
                  (replace-match ""))
                 ((and (not ref-pres)
                       (not (org-at-item-checkbox-p))
                       (org-at-item-p))
                  (goto-char (match-end 0))
                  ;; Ignore counter, if any
                  (when (looking-at "\\(?:\\[@start:[0-9]+\\][ \t]*\\)?")
                    (goto-char (match-end 0)))
                  (let ((desc-p (and (org-at-item-description-p)
                                     (cdr (assq 'checkbox org-list-automatic-rules)))))
                    (cond
                     ((and single-p desc-p)
                      (error "Cannot add a checkbox in a description list"))
                     ((not desc-p) (insert "[ ] "))))))
              (let ((blocked (org-checkbox-blocked-p)))
                (cond
                 ((and blocked single-p)
                  (error "Checkbox blocked because of unchecked box in line %d" blocked))
                 (blocked nil)
                 ((org-at-item-checkbox-p)
                  (replace-match
                   (cond ((equal toggle-presence '(16)) "[-]")
                         (ref-stat "[ ]")
                         (t "[X]"))
                   t t nil 1))))))))
    (save-excursion
      (while (< (point) end)
        (funcall act-on-item ref-presence ref-status)
        (org-search-forward-unenclosed org-item-beginning-re end 'move)))
    (org-update-checkbox-count-maybe)))

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
	    (replace-match "[ ]" t t nil 1))
	  (beginning-of-line 2))))
    (org-update-checkbox-count-maybe)))

(defvar org-checkbox-statistics-hook nil
  "Hook that is run whenever Org thinks checkbox statistics should be updated.
This hook runs even if 'checkbox rules in
`org-list-automatic-rules' do not apply, so it can be used to
implement alternative ways of collecting statistics
information.")

(defun org-update-checkbox-count-maybe ()
  "Update checkbox statistics unless turned off by user."
  (when (cdr (assq 'checkbox org-list-automatic-rules))
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
      (while (and (org-search-backward-unenclosed re-find beg t)
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
	  (if (org-search-forward-unenclosed re-box lim t)
	      (progn
		(org-beginning-of-item)
		(setq curr-ind (org-get-indentation))
		(setq next-ind curr-ind)
		(while (and (bolp) (org-at-item-p)
			    (if recursive
				(<= curr-ind next-ind)
			      (= curr-ind next-ind)))
		  (setq eline (point-at-eol))
		  (if (org-search-forward-unenclosed re-box eline t)
		      (if (member (match-string 2) '("[ ]" "[-]"))
			  (setq c-off (1+ c-off))
			(setq c-on (1+ c-on))))
		  (if (not recursive)
		      ;; org-get-next-item goes through list-enders
		      ;; with proper limit.
		      (goto-char (or (org-get-next-item (point) lim) lim))
		    (end-of-line)
		    (when (org-search-forward-unenclosed org-item-beginning-re lim t)
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
		       (org-search-forward-unenclosed re-box (point-at-eol) t))
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

FUNCTION must be called with at least one argument: a return
value that will contain the value returned by the function at the
previous item, plus ARGS extra arguments.  INIT-VALUE will be the
value passed to the function at the first item of the list.

As an example, (org-apply-on-list (lambda (result) (1+ result)) 0)
will return the number of items in the current list.

Sublists of the list are skipped. Cursor is always at the
beginning of the item."
  (save-excursion
    (let ((end (copy-marker (save-excursion (org-end-of-item-list))))
	  (next-p (make-marker))
	  (move-down-action
	   (lambda (pos value &rest args)
	     (goto-char pos)
	     (set-marker next-p (org-get-next-item pos end))
	     (let ((return-value (apply function value args)))
	       (if (marker-position next-p)
		   (apply move-down-action next-p return-value args)
		 return-value)))))
      (apply move-down-action (org-beginning-of-item-list) init-value args))))

(defun org-sort-list (&optional with-case sorting-type getkey-func compare-func)
  "Sort plain list items.
The cursor may be at any item of the list that should be sorted.
Sublists are not sorted. Checkboxes, if any, are ignored.

Sorting can be alphabetically, numerically, by date/time as given by
a time stamp, by a property or by priority.

The command prompts for the sorting type unless it has been given
to the function through the SORTING-TYPE argument, which needs to
be a character, \(?n ?N ?a ?A ?t ?T ?f ?F).  Here is the precise
meaning of each character:

n   Numerically, by converting the beginning of the item to a number.
a   Alphabetically.  Only the first line of item is checked.
t   By date/time, either the first active time stamp in the entry, if
    any, or by the first inactive one.  In a timer list, sort the timers.

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
			 ((= dcst ?t) '<)
			 (t nil)))
	     (begin-record (lambda ()
			     (skip-chars-forward " \r\t\n")
			     (beginning-of-line)))
	     (end-record (lambda ()
			   (goto-char (org-end-of-item-before-blank))))
	     (value-to-sort
	      (lambda ()
		(when (looking-at "[ \t]*[-+*0-9.)]+\\([ \t]+\\[[- X]\\]\\)?[ \t]+")
		  (cond
		   ((= dcst ?n)
		    (string-to-number (buffer-substring (match-end 0)
							(point-at-eol))))
		   ((= dcst ?a)
		    (buffer-substring (match-end 0) (point-at-eol)))
		   ((= dcst ?t)
		    (cond
		     ;; If it is a timer list, convert timer to seconds
		     ((org-at-item-timer-p)
		      (org-timer-hms-to-secs (match-string 1)))
		     ((or (org-search-forward-unenclosed org-ts-regexp
							 (point-at-eol) t)
			  (org-search-forward-unenclosed org-ts-regexp-both
							 (point-at-eol) t))
		      (org-time-string-to-seconds (match-string 0)))
		     (t (org-float-time now))))
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
    (while (org-search-forward-unenclosed org-item-beginning-re end t)
      (save-excursion
	(beginning-of-line)
	(setq ltype (cond ((looking-at-p "^[ \t]*[0-9]") 'ordered)
			  ((org-at-item-description-p) 'descriptive)
			  (t 'unordered))))
      (let* ((indent1 (org-get-indentation))
	     (nextitem (or (org-get-next-item (point) end) end))
	     (item (org-trim (buffer-substring (point) (org-end-of-item-or-at-child))))
	     (nextindent (if (= (point) end) 0 (org-get-indentation)))
	     (item (if (string-match "^\\(?:\\[@start:[0-9]+\\][ \t]+\\)?\\[\\([xX ]\\)\\]" item)
		       (replace-match (if (equal (match-string 1 item) " ")
					  "CBOFF"
					"CBON")
				      t nil item 1)
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
  (if (not (org-in-item-p))
      (error "Not in a list.")
    (goto-char (org-list-top-point))
    (let ((list (org-list-parse-list t)) nstars)
      (save-excursion
	(if (ignore-errors
	      (org-back-to-heading))
	    (progn (org-search-forward-unenclosed org-complex-heading-regexp nil t)
		   (setq nstars (length (match-string 1))))
	  (setq nstars 0)))
      (org-list-make-subtrees list (1+ nstars)))))

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
	      (re-search-forward org-item-beginning-re bottom-point t)
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
	  (delete-region beg (point-at-bol))
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
	       (when (string-match "^\\(\\S-+\\)[ \t]+::" sublist)
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
