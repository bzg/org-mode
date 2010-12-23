;;; org-list.el --- Plain lists for Org-mode
;;
;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Bastien Guerry <bzg AT altern DOT org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 7.4
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
(defvar org-outline-regexp)
(defvar org-ts-regexp)
(defvar org-ts-regexp-both)

(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-back-over-empty-lines "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-combine-plists "org" (&rest plists))
(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-icompleting-read "org" (&rest args))
(declare-function org-in-regexps-block-p "org"
		  (start-re end-re &optional bound))
(declare-function org-inlinetask-goto-beginning "org-inlinetask" ())
(declare-function org-inlinetask-goto-end "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function org-invisible-p "org" ())
(declare-function org-level-increment "org" ())
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-on-heading-p "org" (&optional invisible-ok))
(declare-function org-remove-if "org" (predicate seq))
(declare-function org-show-subtree "org" ())
(declare-function org-time-string-to-seconds "org" (s))
(declare-function org-timer-hms-to-secs "org-timer" (hms))
(declare-function org-timer-item "org-timer" (&optional arg))
(declare-function org-trim "org" (s))
(declare-function org-uniquify "org" (list))
(declare-function outline-next-heading "outline" ())
(declare-function outline-previous-heading "outline" ())

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
When a string, it will be used as a regular expression.  When the
bullet type of a list is changed, the new bullet type will be
matched against this regexp. If it matches, there will be two
spaces instead of one after the bullet in each item of the list."
  :group 'org-plain-lists
  :type '(choice
	  (const :tag "never" nil)
	  (regexp)))

(defcustom org-list-ending-method 'both
  "Determine where plain lists should end.
Valid values are: `regexp', `indent' or `both'.

When set to `regexp', Org will look into two variables,
`org-empty-line-terminates-plain-lists' and the more general
`org-list-end-regexp', to determine what will end lists. This is
the fastest method.

When set to `indent', a list will end whenever a line following
an item, but not starting one, is less or equally indented than
it.

When set to `both', each of the preceding methods is applied to
determine lists endings. This is the default method."
  :group 'org-plain-lists
  :type '(choice
	  (const :tag "With a regexp defining ending" regexp)
	  (const :tag "With indentation of regular (no bullet) text" indent)
	  (const :tag "With both methods" both)))

(defcustom org-empty-line-terminates-plain-lists nil
  "Non-nil means an empty line ends all plain list levels.
This variable only makes sense if `org-list-ending-method' is set
to `regexp' or `both'. This is then equivalent to set
`org-list-end-regexp' to \"^[ \\t]*$\"."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-list-end-regexp "^[ \t]*\n[ \t]*\n"
  "Regexp matching the end of all plain list levels.
It must start with \"^\" and end with \"\\n\".  It defaults to 2
blank lines. `org-empty-line-terminates-plain-lists' has
precedence over it."
  :group 'org-plain-lists
  :type 'string)

(defcustom org-list-automatic-rules '((bullet . t)
				      (checkbox . t)
				      (indent . t)
				      (insert . t))
  "Non-nil means apply set of rules when acting on lists.
By default, automatic actions are taken when using
 \\[org-meta-return], \\[org-metaright], \\[org-metaleft],
 \\[org-shiftmetaright], \\[org-shiftmetaleft],
 \\[org-ctrl-c-minus], \\[org-toggle-checkbox] or
 \\[org-insert-todo-heading]. You can disable individually these
 rules by setting them to nil. Valid rules are:

bullet    when non-nil, cycling bullet do not allow lists at
          column 0 to have * as a bullet and descriptions lists
          to be numbered.
checkbox  when non-nil, checkbox statistics is updated each time
          you either insert a new checkbox or toggle a checkbox.
          It also prevents from inserting a checkbox in a
          description item.
indent    when non-nil, indenting or outdenting list top-item
          with its subtree will move the whole list and
          outdenting a list whose bullet is * to column 0 will
          change that bullet to -
insert    when non-nil, trying to insert an item inside a block
          will insert it right before the block instead of
          throwing an error."
   :group 'org-plain-lists
   :type '(alist :tag "Sets of rules"
		 :key-type
		 (choice
		  (const :tag "Bullet" bullet)
		  (const :tag "Checkbox" checkbox)
		  (const :tag "Indent" indent)
		  (const :tag "Insert" insert))
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

(defconst org-list-blocks '("EXAMPLE" "VERSE" "SRC")
  "Names of blocks where lists are not allowed.")

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

(defconst org-list-full-item-re
  (concat "[ \t]*\\(\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\)"
	  "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\)\\]\\)?"
	  "\\(?:\\(\\[[ X-]\\]\\)[ \t]+\\)?")
  "Matches a list item and puts everything into groups:
group 1: the bullet
group 2: the counter
group 3: the checkbox")

(defun org-list-context ()
  "Determine context, and its boundaries, around point.

Context is determined by reading `org-context' text property if
applicable, or looking at Org syntax around.

Context will be an alist like (MIN MAX CONTEXT) where MIN and MAX
are boundaries and CONTEXT is a symbol among `nil', `drawer',
`block', `invalid' and `inlinetask'.

Symbols `block' and `invalid' refer to `org-list-blocks'."
  (save-match-data
    (let* ((origin (point))
	   (context-prop (get-text-property origin 'org-context)))
      (if context-prop
	  (list
	   (or (previous-single-property-change
		(min (1+ (point)) (point-max)) 'org-context) (point-min))
	   (or (next-single-property-change origin 'org-context) (point-max))
	   (cond
	    ((equal (downcase context-prop) "inlinetask") 'inlinetask)
	    ((member (upcase context-prop) org-list-blocks) 'invalid)
	    (t 'block)))
	(save-excursion
	  (beginning-of-line)
	  (let* ((outline-regexp (org-get-limited-outline-regexp))
		 ;; can't use org-drawers-regexp as this function might be
		 ;; called in buffers not in Org mode
		 (drawers-re (concat "^[ \t]*:\\("
				     (mapconcat 'regexp-quote org-drawers "\\|")
				     "\\):[ \t]*$"))
		 (case-fold-search t)
		 ;; compute position of surrounding headings. this is the
		 ;; default context.
		 (heading
		  (save-excursion
		    (list
		     (or (and (org-at-heading-p) (point-at-bol))
			 (outline-previous-heading)
			 (point-min))
		     (or (outline-next-heading)
			 (point-max))
		     nil)))
		 (prev-head (car heading))
		 (next-head (nth 1 heading))
		 ;; Are we strictly inside a drawer?
		 (drawerp
		  (when (and (org-in-regexps-block-p
			      drawers-re "^[ \t]*:END:" prev-head)
			     (save-excursion
			       (beginning-of-line)
			       (and (not (looking-at drawers-re))
				    (not (looking-at "^[ \t]*:END:")))))
		    (save-excursion
		      (list
		       (progn
			 (re-search-backward drawers-re prev-head t)
			 (1+ (point-at-eol)))
		       (if (re-search-forward "^[ \t]*:END:" next-head t)
			   (1- (point-at-bol))
			 next-head)
		       'drawer))))
		 ;; Are we strictly in a block, and of which type?
		 (blockp
		  (save-excursion
		    (when (and (org-in-regexps-block-p
				"^[ \t]*#\\+begin_" "^[ \t]*#\\+end_" prev-head)
			       (save-excursion
				 (beginning-of-line)
				 (not (looking-at
				       "^[ \t]*#\\+\\(begin\\|end\\)_"))))
		      (list
		       (progn
			 (re-search-backward
			  "^[ \t]*#\\+begin_\\(\\S-+\\)" prev-head t)
			 (1+ (point-at-eol)))
		       (save-match-data
			 (if (re-search-forward "^[ \t]*#\\+end_" next-head t)
			     (1- (point-at-bol))
			   next-head))
		       (if (member (upcase (match-string 1)) org-list-blocks)
			   'invalid
			 'block)))))
		 ;; Are we in an inlinetask?
		 (inlinetaskp
		  (when (and (featurep 'org-inlinetask)
			     (org-inlinetask-in-task-p)
			     (not (looking-at "^\\*+")))
		    (save-excursion
		      (list
		       (progn (org-inlinetask-goto-beginning)
			      (1+ (point-at-eol)))
		       (progn
			 (org-inlinetask-goto-end)
			 (forward-line -1)
			 (1- (point-at-bol)))
		       'inlinetask))))
		 ;; list actual candidates
		 (context-list
		  (delq nil (list heading drawerp blockp inlinetaskp))))
	    ;; Return the closest context around
	    (assq (apply 'max (mapcar 'car context-list)) context-list)))))))

(defun org-list-search-unenclosed-generic (search re bound noerr)
  "Search a string outside blocks and protected places.
Arguments SEARCH, RE, BOUND and NOERR are similar to those in
`search-forward', `search-backward', `re-search-forward' and
`re-search-backward'."
  (catch 'exit
    (let ((origin (point)))
      (while t
	;; 1. No match: return to origin or bound, depending on NOERR.
	(unless (funcall search re bound noerr)
	  (throw 'exit (and (goto-char (if (memq noerr '(t nil)) origin bound))
			    nil)))
	;; 2. Match not in block or protected: return point. Else
	;; skip the block and carry on.
	(unless (or (get-text-property (match-beginning 0) 'org-protected)
		    (org-list-maybe-skip-block search bound))
	  (throw 'exit (point)))))))

(defun org-search-backward-unenclosed (regexp &optional bound noerror)
  "Like `re-search-backward' but don't stop inside blocks or protected places.
Arguments REGEXP, BOUND and NOERROR are similar to those used in
`re-search-backward'."
  (org-list-search-unenclosed-generic
   #'re-search-backward regexp (or bound (point-min)) noerror))

(defun org-search-forward-unenclosed (regexp &optional bound noerror)
  "Like `re-search-forward' but don't stop inside blocks or protected places.
Arguments REGEXP, BOUND and NOERROR are similar to those used in
`re-search-forward'."
  (org-list-search-unenclosed-generic
   #'re-search-forward regexp (or bound (point-max)) noerror))

(defun org-list-at-regexp-after-bullet-p (regexp)
  "Is point at a list item with REGEXP after bullet?"
  (and (org-at-item-p)
       (save-excursion
	 (goto-char (match-end 0))
         ;; Ignore counter if any
         (when (looking-at "\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?")
           (goto-char (match-end 0)))
	 (looking-at regexp))))

(defun org-list-separating-blank-lines-number (pos top bottom)
  "Return number of blank lines that should separate items in list.
POS is the position of point to be considered.

TOP and BOTTOM are respectively position of list beginning and
list ending.

Assume point is at item's beginning. If the item is alone, apply
some heuristics to guess the result."
  (save-excursion
    (let ((insert-blank-p
	   (cdr (assq 'plain-list-item org-blank-before-new-entry)))
	  usr-blank)
      (cond
       ;; Trivial cases where there should be none.
       ((or (and (not (eq org-list-ending-method 'indent))
		 org-empty-line-terminates-plain-lists)
	    (not insert-blank-p)) 0)
       ;; When `org-blank-before-new-entry' says so, it is 1.
       ((eq insert-blank-p t) 1)
       ;; plain-list-item is 'auto. Count blank lines separating
       ;; neighbours items in list.
       (t (let ((next-p (org-get-next-item (point) bottom)))
	    (cond
	     ;; Is there a next item?
	     (next-p (goto-char next-p)
		     (org-back-over-empty-lines))
	     ;; Is there a previous item?
	     ((org-get-previous-item (point) top)
	      (org-back-over-empty-lines))
	     ;; User inserted blank lines, trust him
	     ((and (> pos (org-end-of-item-before-blank bottom))
		   (> (save-excursion
			(goto-char pos)
			(skip-chars-backward " \t")
			(setq usr-blank (org-back-over-empty-lines))) 0))
	      usr-blank)
	     ;; Are there blank lines inside the item ?
	     ((save-excursion
		(org-search-forward-unenclosed
		 "^[ \t]*$" (org-end-of-item-before-blank bottom) t)) 1)
	     ;; No parent: no blank line.
	     (t 0))))))))

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
	(error "Cannot insert item inside a block")
      ;; Else, move before it prior to add a new item.
      (end-of-line)
      (re-search-backward "^[ \t]*#\\+\\(begin\\|BEGIN\\)_" nil t)
      (end-of-line 0)))
  (let* ((true-pos (point))
	 (top (org-list-top-point))
	 (bottom (copy-marker (org-list-bottom-point)))
	 (bullet (and (goto-char (org-list-get-item-begin))
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
	 (blank-lines-nb (org-list-separating-blank-lines-number
			  true-pos top bottom))
	 (insert-fun
	  (lambda (text)
	    ;; insert bullet above item in order to avoid bothering
	    ;; with possible blank lines ending last item.
	    (goto-char (org-list-get-item-begin))
            (org-indent-to-column ind)
	    (insert (concat bullet (when checkbox "[ ] ") after-bullet))
	    ;; Stay between after-bullet and before text.
	    (save-excursion
	      (insert (concat text (make-string (1+ blank-lines-nb) ?\n))))
	    (unless before-p
	      ;; store bottom: exchanging items doesn't change list
	      ;; bottom point but will modify marker anyway
	      (setq bottom (marker-position bottom))
	      (let ((col (current-column)))
		(org-list-exchange-items
		 (org-list-get-item-begin) (org-get-next-item (point) bottom)
		 bottom)
	      ;; recompute next-item: last sexp modified list
	      (goto-char (org-get-next-item (point) bottom))
	      (org-move-to-column col)))
	    ;; checkbox update might modify bottom point, so use a
	    ;; marker here
	    (setq bottom (copy-marker bottom))
	    (when checkbox (org-update-checkbox-count-maybe))
	    (org-list-repair nil))))
    (goto-char true-pos)
    (cond
     (before-p (funcall insert-fun nil) t)
     ;; Can't split item: insert bullet at the end of item.
     ((not (org-get-alist-option org-M-RET-may-split-line 'item))
      (funcall insert-fun nil) t)
     ;; else, insert a new bullet along with everything from point
     ;; down to last non-blank line of item.
     (t
      (delete-horizontal-space)
      ;; Get pos again in case previous command modified line.
      (let* ((pos (point))
	     (end-before-blank (org-end-of-item-before-blank bottom))
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

(defvar org-last-indent-begin-marker (make-marker))
(defvar org-last-indent-end-marker (make-marker))

(defun org-list-indent-item-generic (arg no-subtree struct)
  "Indent a local list item including its children.
When number ARG is a negative, item will be outdented, otherwise
it will be indented.

If a region is active, all items inside will be moved.

If NO-SUBTREE is non-nil, only indent the item itself, not its
children.

Return t if successful."
  (save-excursion
    (beginning-of-line)
    (let* ((regionp (org-region-active-p))
	   (rbeg (and regionp (region-beginning)))
	   (rend (and regionp (region-end))))
      (cond
       ((and regionp
	     (goto-char rbeg)
	     (not (org-search-forward-unenclosed org-item-beginning-re rend t)))
	(error "No item in region"))
       ((not (org-at-item-p))
	(error "Not at an item"))
       (t
	(let* ((top (org-list-get-top-point struct))
	       (parents (org-list-struct-parent-alist struct))
	       (prevs (org-list-struct-prev-alist struct))
	       ;; Are we going to move the whole list?
	       (specialp (and (cdr (assq 'indent org-list-automatic-rules))
			      (not no-subtree)
			      (= top (point)))))
	  ;; Determine begin and end points of zone to indent. If moving
	  ;; more than one item, save them for subsequent moves.
	  (unless (and (memq last-command '(org-shiftmetaright org-shiftmetaleft))
		       (memq this-command '(org-shiftmetaright org-shiftmetaleft)))
	    (if regionp
		(progn
		  (set-marker org-last-indent-begin-marker rbeg)
		  (set-marker org-last-indent-end-marker rend))
	      (set-marker org-last-indent-begin-marker (point))
	      (set-marker org-last-indent-end-marker
			  (cond
			   (specialp (org-list-get-bottom-point struct))
			   (no-subtree (1+ (point)))
			   (t (org-list-get-item-end (point) struct))))))
	  (let* ((beg (marker-position org-last-indent-begin-marker))
		 (end (marker-position org-last-indent-end-marker)))
	    (cond
	     ;; Special case: moving top-item with indent rule
	     (specialp
	      (let* ((level-skip (org-level-increment))
		     (offset (if (< arg 0) (- level-skip) level-skip))
		     (top-ind (org-list-get-ind beg struct))
		     (old-struct (mapcar (lambda (e) (copy-alist e)) struct)))
		(if (< (+ top-ind offset) 0)
		    (error "Cannot outdent beyond margin")
		  ;; Change bullet if necessary
		  (when (and (= (+ top-ind offset) 0)
			     (string-match "*"
					   (org-list-get-bullet beg struct)))
		    (org-list-set-bullet beg struct
					 (org-list-bullet-string "-")))
		  ;; Shift every item by OFFSET and fix bullets. Then
		  ;; apply changes to buffer.
		  (mapc (lambda (e)
			  (let ((ind (org-list-get-ind (car e) struct)))
			    (org-list-set-ind (car e) struct (+ ind offset))))
			struct)
		  (org-list-struct-fix-bul struct prevs)
		  (org-list-struct-apply-struct struct old-struct))))
	     ;; Forbidden move:
	     ((and (< arg 0)
		   ;; If only one item is moved, it mustn't have a child
		   (or (and no-subtree
			    (not regionp)
			    (org-list-get-child beg struct))
		       ;; If a subtree or region is moved, the last item
		       ;; of the subtree mustn't have a child
		       (let ((last-item (caar
					 (reverse
					  (org-remove-if
					   (lambda (e) (>= (car e) end))
					   struct)))))
			 (org-list-get-child last-item struct))))
	      (error "Cannot outdent an item without its children"))
	     ;; Normal shifting
	     (t
	      (let* ((new-parents
		      (if (< arg 0)
			  (org-list-struct-outdent beg end struct parents)
			(org-list-struct-indent beg end struct parents prevs))))
		(org-list-struct-fix-struct struct new-parents))
	      (org-update-checkbox-count-maybe)))))))))
  t)

;;; Predicates

(defun org-in-item-p ()
  "Is the cursor inside a plain list?
This checks `org-list-ending-method'."
  (save-excursion
    (beginning-of-line)
    (unless (or (let ((outline-regexp org-outline-regexp)) (org-at-heading-p))
		(and (not (eq org-list-ending-method 'indent))
		     (looking-at (org-list-end-re))
		     (progn (forward-line -1) (looking-at (org-list-end-re)))))
      (or (and (org-at-item-p) (point-at-bol))
	  (let* ((case-fold-search t)
		 (context (org-list-context))
		 (lim-up (car context))
		 (inlinetask-re (and (featurep 'org-inlinetask)
				     (org-inlinetask-outline-regexp)))
		 (ind-ref (if (looking-at "^[ \t]*$")
			      10000
			    (org-get-indentation))))
	    (catch 'exit
	      (while t
		(let ((ind (org-get-indentation)))
		  (cond
		   ((<= (point) lim-up)
		    (throw 'exit (and (org-at-item-p) (< ind ind-ref))))
		   ((and (not (eq org-list-ending-method 'indent))
			 (looking-at (org-list-end-re)))
		    (throw 'exit nil))
		   ;; Skip blocks, drawers, inline-tasks, blank lines
		   ((looking-at "^[ \t]*#\\+end_")
		    (re-search-backward "^[ \t]*#\\+begin_" nil t))
		   ((looking-at "^[ \t]*:END:")
		    (re-search-backward org-drawer-regexp nil t)
		    (beginning-of-line))
		   ((and inlinetask-re (looking-at inlinetask-re))
		    (org-inlinetask-goto-beginning)
		    (forward-line -1))
		   ((looking-at "^[ \t]*$")
		    (forward-line -1))
		   ((< ind ind-ref)
		    (if (org-at-item-p)
			(throw 'exit (point))
		      (setq ind-ref ind)
		      (forward-line -1)))
		   (t (if (and (eq org-list-ending-method 'regexp)
			       (org-at-item-p))
			  (throw 'exit (point))
			(forward-line -1))))))))))))

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
  (org-list-at-regexp-after-bullet-p
   "\\([0-9]+:[0-9]+:[0-9]+\\)[ \t]+::[ \t]+"))

(defun org-at-item-description-p ()
  "Is point at a description list item?"
  (org-list-at-regexp-after-bullet-p "\\(\\S-.+\\)[ \t]+::[ \t]+"))

(defun org-at-item-checkbox-p ()
  "Is point at a line starting a plain-list item with a checklet?"
  (org-list-at-regexp-after-bullet-p "\\(\\[[- X]\\]\\)[ \t]+"))

;;; Navigate

(defalias 'org-list-get-item-begin 'org-in-item-p)

(defun org-beginning-of-item ()
  "Go to the beginning of the current hand-formatted item.
If the cursor is not in an item, throw an error."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if begin (goto-char begin) (error "Not in an item"))))

(defun org-beginning-of-item-list ()
  "Go to the beginning item of the current list or sublist.
Return an error if not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let ((struct (org-list-struct)))
	(goto-char (org-list-get-list-begin begin (org-list-struct)))))))

(defun org-end-of-item-list ()
  "Go to the end of the current list or sublist.
If the cursor in not in an item, throw an error."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let ((struct (org-list-struct)))
	(goto-char (org-list-get-list-end begin (org-list-struct)))))))

(defun org-end-of-item ()
  "Go to the end of the current hand-formatted item.
If the cursor is not in an item, throw an error."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let ((struct (org-list-struct)))
	(goto-char (org-list-get-item-end begin struct))))))

(defun org-previous-item ()
  "Move to the beginning of the previous item.
Item is at the same level in the current plain list. Error if not
in a plain list, or if this is the first item in the list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-struct-prev-alist struct))
	     (prevp (org-list-get-prev-item begin struct prevs)))
	(if prevp (goto-char prevp) (error "On first item"))))))

(defun org-next-item ()
  "Move to the beginning of the next item.
Item is at the same level in the current plain list. Error if not
in a plain list, or if this is the last item in the list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-struct-prev-alist struct))
	     (prevp (org-list-get-next-item begin struct prevs)))
	(if prevp (goto-char prevp) (error "On last item"))))))

;;; Manipulate

(defun org-list-exchange-items (beg-A beg-B struct)
  "Swap item starting at BEG-A with item starting at BEG-B in STRUCT.
Blank lines at the end of items are left in place.

Assume BEG-A is lesser than BEG-B and that BEG-A and BEG-B
belong to the same sub-list.

This function modifies STRUCT."
  (save-excursion
    (let* ((end-of-item-no-blank
	    (lambda (pos)
	      (goto-char (org-list-get-item-end-before-blank pos struct))))
	   (end-A-no-blank (funcall end-of-item-no-blank beg-A))
	   (end-B-no-blank (funcall end-of-item-no-blank beg-B))
	   (body-A (buffer-substring beg-A end-A-no-blank))
	   (body-B (buffer-substring beg-B end-B-no-blank))
	   (between-A-no-blank-and-B (buffer-substring end-A-no-blank beg-B)))
      (goto-char beg-A)
      (delete-region beg-A end-B-no-blank)
      (insert (concat body-B between-A-no-blank-and-B body-A))
      ;; Now modify struct. No need to re-read the list, the
      ;; transformation is just a shift of positions
      (let* ((sub-A (cons beg-A (org-list-get-subtree beg-A struct)))
	     (sub-B (cons beg-B (org-list-get-subtree beg-B struct)))
	     (end-A (org-list-get-item-end beg-A struct))
	     (end-B (org-list-get-item-end beg-B struct))
	     (inter-A-B (- beg-B end-A))
	     (size-A (- end-A beg-A))
	     (size-B (- end-B beg-B)))
	(mapc (lambda (e) (org-list-set-pos e struct (+ e size-B inter-A-B)))
	      sub-A)
	(mapc (lambda (e) (org-list-set-pos e struct (- e size-A inter-A-B)))
	      sub-B)
	(sort struct (lambda (e1 e2) (< (car e1) (car e2))))))))

(defun org-move-item-down ()
  "Move the plain list item at point down, i.e. swap with following item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive)
  (unless (org-at-item-p) (error "Not at an item"))
  (let* ((pos (point))
	 (col (current-column))
	 (actual-item (point-at-bol))
	 (struct (org-list-struct))
	 (prevs (org-list-struct-prev-alist struct))
	 (next-item (org-list-get-next-item (point-at-bol) struct prevs)))
    (if (not next-item)
	(progn
	  (goto-char pos)
	  (error "Cannot move this item further down"))
      (org-list-exchange-items actual-item next-item struct)
      ;; Use a short variation of `org-list-struct-fix-struct' as
      ;; there's no need to go through all the steps.
      (let ((old-struct (mapcar (lambda (e) (copy-alist e)) struct))
	    (prevs (org-list-struct-prev-alist struct))
	    (parents (org-list-struct-parent-alist struct)))
        (org-list-struct-fix-bul struct prevs)
        (org-list-struct-fix-ind struct parents)
        (org-list-struct-apply-struct struct old-struct)
	(goto-char (org-list-get-next-item (point-at-bol) struct prevs)))
      (org-move-to-column col))))

(defun org-move-item-up ()
  "Move the plain list item at point up, i.e. swap with previous item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive)
  (unless (org-at-item-p) (error "Not at an item"))
  (let* ((pos (point))
	 (col (current-column))
	 (actual-item (point-at-bol))
	 (struct (org-list-struct))
	 (prevs (org-list-struct-prev-alist struct))
	 (prev-item (org-list-get-prev-item (point-at-bol) struct prevs)))
    (if (not prev-item)
	(progn
	  (goto-char pos)
	  (error "Cannot move this item further up"))
      (org-list-exchange-items prev-item actual-item struct)
      ;; Use a short variation of `org-list-struct-fix-struct' as
      ;; there's no need to go through all the steps.
      (let ((old-struct (mapcar (lambda (e) (copy-alist e)) struct))
	    (prevs (org-list-struct-prev-alist struct))
	    (parents (org-list-struct-parent-alist struct)))
        (org-list-struct-fix-bul struct prevs)
        (org-list-struct-fix-ind struct parents)
        (org-list-struct-apply-struct struct old-struct))
      (org-move-to-column col))))

(defun org-insert-item (&optional checkbox)
  "Insert a new item at the current level.
If cursor is before first character after bullet of the item, the
new item will be created before the current one.

If CHECKBOX is non-nil, add a checkbox next to the bullet.

Return t when things worked, nil when we are not in an item, or
item is invisible."
  (unless (or (not (org-in-item-p))
	      (save-excursion
		(goto-char (org-get-item-beginning))
		(outline-invisible-p)))
    (if (save-excursion
	  (goto-char (org-list-get-item-begin))
	  (org-at-item-timer-p))
	;; Timer list: delegate to `org-timer-item'.
	(progn (org-timer-item) t)
      ;; if we're in a description list, ask for the new term.
      (let ((desc-text (when (save-excursion
			       (and (goto-char (org-list-get-item-begin))
				    (org-at-item-description-p)))
			 (concat (read-string "Term: ") " :: "))))
        ;; Don't insert a checkbox if checkbox rule is applied and it
        ;; is a description item.
	(org-list-insert-item-generic
	 (point) (and checkbox
                      (or (not desc-text)
                          (not (cdr (assq 'checkbox org-list-automatic-rules)))))
         desc-text)))))

;;; Structures

;; The idea behind structures is to avoid moving back and forth in the
;; buffer on costly operations like indenting or fixing bullets.

;; It achieves this by taking a snapshot of an interesting part of the
;; list, in the shape of an alist, using `org-list-struct'.

;; It then proceeds to changes directly on the alist, with the help of
;; and `org-list-struct-origins'. When those are done,
;; `org-list-struct-apply-struct' applies the changes to the buffer.

(defun org-list-struct ()
  "Return structure of list at point.

A list structure is an alist where keys is point at item, and
values are:
1. indentation,
2. bullet with trailing whitespace,
3. bullet counter, if any,
4. checkbox, if any,
5. position at item end.

Assume point is at an item."
  (save-excursion
    (beginning-of-line)
    (let* ((case-fold-search t)
	   (context (org-list-context))
	   (lim-up (car context))
	   (lim-down (nth 1 context))
	   (text-min-ind 10000)
	   (drawers-re (concat "^[ \t]*:\\("
				     (mapconcat 'regexp-quote org-drawers "\\|")
				     "\\):[ \t]*$"))
	   (inlinetask-re (and (featurep 'org-inlinetask)
			       (org-inlinetask-outline-regexp)))
	   (beg-cell (cons (point) (org-get-indentation)))
	   ind itm-lst itm-lst-2 end-lst end-lst-2 struct
	   (assoc-at-point
	    ;; Return an association whose key is point and values are
	    ;; indentation, bullet string, bullet counter, and
	    ;; checkbox.
	    (function
	     (lambda (ind)
	       (looking-at org-list-full-item-re)
	       (list (point)
		     ind
		     (match-string-no-properties 1)   ; bullet
		     (match-string-no-properties 2)   ; counter
		     (match-string-no-properties 3)))))	; checkbox
	   (end-before-blank
	    ;; Ensure list ends at the first blank line.
	    (function
	     (lambda ()
	       (skip-chars-backward " \r\t\n")
	       (min (1+ (point-at-eol)) lim-down)))))
      ;; 1. Read list from starting item to its beginning, and save
      ;;    top item position and indentation in BEG-CELL. Also store
      ;;    ending position of items in END-LST.
      (save-excursion
	(catch 'exit
	  (while t
	    (let ((ind (org-get-indentation)))
	      (cond
	       ((<= (point) lim-up)
		;; At upward limit: if we ended at an item, store it,
		;; else dimiss useless data recorded above BEG-CELL.
		;; Jump to part 2.
		(throw 'exit
		       (setq itm-lst
			     (if (not (org-at-item-p))
				 (memq (assq (car beg-cell) itm-lst) itm-lst)
			       (setq beg-cell (cons (point) ind))
			       (cons (funcall assoc-at-point ind) itm-lst)))))
	       ((and (not (eq org-list-ending-method 'indent))
		     (looking-at (org-list-end-re)))
		;; Looking at a list ending regexp. Dismiss useless
		;; data recorded above BEG-CELL. Jump to part 2.
		(throw 'exit
		       (setq itm-lst
			     (memq (assq (car beg-cell) itm-lst) itm-lst))))
	       ;; Skip blocks, drawers, inline tasks, blank lines
	       ;; along the way.
	       ((looking-at "^[ \t]*#\\+end_")
		(re-search-backward "^[ \t]*#\\+begin_" nil t))
	       ((looking-at "^[ \t]*:END:")
		(re-search-backward drawers-re nil t)
		(beginning-of-line))
	       ((and inlinetask-re (looking-at inlinetask-re))
		(org-inlinetask-goto-beginning)
		(forward-line -1))
	       ((looking-at "^[ \t]*$")
		(forward-line -1))
	       ((org-at-item-p)
		;; Point is at an item. Add data to ITM-LST. It may
		;; also end a previous item: save it in END-LST. If
		;; ind is less or equal than BEG-CELL and there is no
		;; end at this ind or lesser, this item becomes the
		;; new BEG-CELL.
		(setq itm-lst (cons (funcall assoc-at-point ind) itm-lst)
		      end-lst (cons (cons ind (point-at-bol)) end-lst))
		(when (or (and (eq org-list-ending-method 'regexp)
			       (<= ind (cdr beg-cell)))
			  (< ind text-min-ind))
		  (setq beg-cell (cons (point-at-bol) ind)))
		(forward-line -1))
	       (t
		;; Point is not at an item. Unless ending method is
		;; `regexp', interpret line's indentation:
		;;
		;; - text at column 0 is necessarily out of any list.
		;;   Dismiss data recorded above BEG-CELL. Jump to
		;;   part 2.
		;;
		;; - any other case, it can possibly be an ending
		;;   position for an item above. Save it and proceed.
		(cond
		 ((eq org-list-ending-method 'regexp))
		 ((= ind 0)
		  (throw 'exit
			 (setq itm-lst
			       (memq (assq (car beg-cell) itm-lst) itm-lst))))
		 (t
		  (when (< ind text-min-ind) (setq text-min-ind ind))
		  (setq end-lst (cons (cons ind (point-at-bol)) end-lst))))
		(forward-line -1)))))))
      ;; 2. Read list from starting point to its end, that is until we
      ;;    get out of context, or a non-item line is less or equally
      ;;    indented that BEG-CELL's cdr. Also store ending position
      ;;    of items in END-LST-2.
      (catch 'exit
      	(while t
      	  (let ((ind (org-get-indentation)))
      	    (cond
      	     ((>= (point) lim-down)
	      ;; At downward limit: this is de facto the end of the
	      ;; list. Save point as an ending position, and jump to
	      ;; part 3.
      	      (throw 'exit
		     (setq end-lst-2
			   (cons
			    (cons 0 (funcall end-before-blank)) end-lst-2))))
	     ((and (not (eq org-list-ending-method 'regexp))
		   (looking-at (org-list-end-re)))
	      ;; Looking at a list ending regexp. Save point as an
	      ;; ending position and jump to part 3.
	      (throw 'exit
		     (setq end-lst-2
			   (cons (cons ind (point-at-bol)) end-lst-2))))
	     ;; Skip blocks, drawers, inline tasks and blank lines
	     ;; along the way
	     ((looking-at "^[ \t]*#\\+begin_")
	      (re-search-forward "^[ \t]*#\\+end_")
	      (forward-line 1))
	     ((looking-at drawers-re)
	      (re-search-forward "^[ \t]*:END:" nil t)
	      (forward-line 1))
	     ((and inlinetask-re (looking-at inlinetask-re))
	      (org-inlinetask-goto-end)
	      (forward-line 1))
	     ((looking-at "^[ \t]*$")
	      (forward-line 1))
	     ((org-at-item-p)
	      ;; Point is at an item. Add data to ITM-LST-2. It may also
	      ;; end a previous item, so save it in END-LST-2.
	      (setq itm-lst-2 (cons (funcall assoc-at-point ind) itm-lst-2)
		    end-lst-2 (cons (cons ind (point-at-bol)) end-lst-2))
	      (forward-line 1))
	     (t
	      ;; Point is not at an item. If ending method is not
	      ;; `regexp', two situations are of interest:
	      ;;
	      ;; - ind is lesser or equal than BEG-CELL's. The list is
	      ;;   over. Store point as an ending position and jump to
	      ;;   part 3.
	      ;;
	      ;; - ind is lesser or equal than previous item's. This
	      ;;    is an ending position. Store it and proceed.
	       (cond
		((eq org-list-ending-method 'regexp))
		((<= ind (cdr beg-cell))
		 (setq end-lst-2
		       (cons (cons ind (funcall end-before-blank)) end-lst-2))
		 (throw 'exit nil))
		((<= ind (nth 1 (car itm-lst-2)))
		 (setq end-lst-2 (cons (cons ind (point-at-bol)) end-lst-2))))
	       (forward-line 1))))))
      (setq struct (append itm-lst (cdr (nreverse itm-lst-2))))
      (setq end-lst (append end-lst (cdr (nreverse end-lst-2))))
      ;; 3. Correct ill-formed lists by making sure top item has the
      ;;    least indentation of the list
      (let ((min-ind (nth 1 (car struct))))
	(mapc (lambda (item)
		(let ((ind (nth 1 item)))
		  (when (< ind min-ind) (setcar (cdr item) min-ind))))
	      struct))
      ;; 4. Associate each item to its end pos.
      (org-list-struct-assoc-end struct end-lst)
      ;; 5. Return STRUCT
      struct)))

(defun org-list-struct-assoc-end (struct end-list)
  "Associate proper ending point to items in STRUCT.

END-LIST is a pseudo-alist where car is indentation and cdr is
ending position.

This function modifies STRUCT."
  (let ((endings end-list))
    (mapc
     (lambda (elt)
       (let ((pos (car elt))
	     (ind (nth 1 elt)))
	 ;; remove end candidates behind current item
	 (while (or (<= (cdar endings) pos))
	   (pop endings))
	 ;; add end position to item assoc
	 (let ((old-end (nthcdr 5 elt))
	       (new-end (assoc-default ind endings '<=)))
	   (if old-end
	       (setcar old-end new-end)
	     (setcdr elt (append (cdr elt) (list new-end)))))))
     struct)))

(defun org-list-struct-prev-alist (struct)
  "Return alist between item and previous item in STRUCT."
  (let ((item-end-alist (mapcar (lambda (e) (cons (car e) (nth 5 e)))
				struct)))
    (mapcar (lambda (e)
	      (let ((prev (car (rassq (car e) item-end-alist))))
		(cons (car e) prev)))
	    struct)))

(defun org-list-struct-parent-alist (struct)
  "Return alist between item and parent in STRUCT."
  (let ((ind-to-ori (list (list (nth 1 (car struct)))))
	(prev-pos (list (caar struct))))
    (cons prev-pos
	  (mapcar (lambda (item)
		    (let ((pos (car item))
			  (ind (nth 1 item))
			  (prev-ind (caar ind-to-ori)))
		      (setq prev-pos (cons pos prev-pos))
		      (cond
		       ((> prev-ind ind)
			(setq ind-to-ori
			      (member (assq ind ind-to-ori) ind-to-ori))
			(cons pos (cdar ind-to-ori)))
		       ((< prev-ind ind)
			(let ((origin (nth 1 prev-pos)))
			  (setq ind-to-ori (cons (cons ind origin) ind-to-ori))
			  (cons pos origin)))
		       (t (cons pos (cdar ind-to-ori))))))
		  (cdr struct)))))

(defun org-list-get-parent (item struct &optional parents)
  "Return parent of ITEM in STRUCT, or nil.
PARENTS, when provided, is the alist of items' parent. See
`org-list-struct-parent-alist'."
  (let ((parents (or parents (org-list-struct-parent-alist struct))))
    (cdr (assq item parents))))

(defun org-list-get-child (item struct)
  "Return child of ITEM in STRUCT, or nil."
  (let ((ind (org-list-get-ind item struct))
	(child-maybe (car (nth 1 (member (assq item struct) struct)))))
    (when (and child-maybe
	       (< ind (org-list-get-ind child-maybe struct)))
      child-maybe)))

(defun org-list-get-next-item (item struct prevs)
  "Return next item in same sub-list as ITEM in STRUCT, or nil.
PREVS is the alist of previous items. See
`org-list-struct-prev-alist'."
  (car (rassq item prevs)))

(defun org-list-get-prev-item (item struct prevs)
  "Return previous item in same sub-list as ITEM in STRUCT, or nil.
PREVS is the alist of previous items. See
`org-list-struct-prev-alist'."
  (cdr (assq item prevs)))

(defun org-list-get-subtree (item struct)
  "Return all items with ITEM as a common ancestor or nil.
PREVS is the alist of previous items. See
`org-list-struct-prev-alist'."
  (let* ((item-end (org-list-get-item-end item struct))
	 (sub-struct (cdr (member (assq item struct) struct)))
	 subtree)
    (catch 'exit
      (mapc (lambda (e) (let ((pos (car e)))
		     (if (< pos item-end)
			 (setq subtree (cons pos subtree))
		       (throw 'exit nil))))
	    sub-struct))
    (nreverse subtree)))

(defun org-list-get-all-items (item struct prevs)
  "List of items in the same sub-list as ITEM in STRUCT.
PREVS, when provided, is the alist of previous items. See
`org-list-struct-prev-alist'."
  (let ((prev-item item)
	(next-item item)
	before-item after-item)
    (while (setq prev-item (org-list-get-prev-item prev-item struct prevs))
      (setq before-item (cons prev-item before-item)))
    (while (setq next-item (org-list-get-next-item next-item struct prevs))
      (setq after-item (cons next-item after-item)))
    (append before-item (list item) (nreverse after-item))))

(defun org-list-get-all-children (item struct prevs)
  "List all children of ITEM in STRUCT, or nil.
PREVS is the alist of previous items. See
`org-list-struct-prev-alist'."
  (let ((child (org-list-get-child item struct)))
    (and child (org-list-get-all-items child struct prevs))))

(defun org-list-get-top-point (struct)
  "Return point at beginning of list.
STRUCT is the structure of the list."
  (caar struct))

(defun org-list-get-bottom-point (struct)
  "Return point at bottom of list.
STRUCT is the structure of the list."
  (apply 'max
	 (mapcar (lambda (e) (org-list-get-item-end (car e) struct)) struct)))

(defun org-list-get-list-begin (item struct prevs)
  "Return point at beginning of sub-list ITEM belongs.
STRUCT is the structure of the list. PREVS is the alist of
previous items. See `org-list-struct-prev-alist'."
  (let ((prev-item item) first-item)
    (while (setq prev-item (org-list-get-prev-item prev-item struct prevs))
      (setq first-item prev-item))
    first-item))

(defun org-list-get-list-end (item struct prevs)
  "Return point at end of sub-list ITEM belongs.
STRUCT is the structure of the list. PREVS is the alist of
previous items. See `org-list-struct-prev-alist'."
  (let ((next-item item) last-item)
    (while (setq next-item (org-list-get-next-item next-item struct prevs))
      (setq last-item next-item))
    (org-list-get-item-end last-item struct)))

(defun org-list-get-nth (n key struct)
  "Return the Nth value of KEY in STRUCT."
  (nth n (assq key struct)))

(defun org-list-set-nth (n key struct new)
  "Set the Nth value of KEY in STRUCT to NEW.
\nThis function modifies STRUCT."
  (setcar (nthcdr n (assq key struct)) new))

(defun org-list-get-ind (item struct)
  "Return indentation of ITEM in STRUCT."
  (org-list-get-nth 1 item struct))

(defun org-list-set-ind (item struct ind)
  "Set indentation of ITEM in STRUCT to IND.
\nThis function modifies STRUCT."
  (org-list-set-nth 1 item struct ind))

(defun org-list-get-bullet (item struct)
  "Return bullet of ITEM in STRUCT."
  (org-list-get-nth 2 item struct))

(defun org-list-set-bullet (item struct bullet)
  "Set bullet of ITEM in STRUCT to BULLET.
\nThis function modifies STRUCT."
  (org-list-set-nth 2 item struct bullet))

(defun org-list-get-counter (item struct)
  "Return counter of ITEM in STRUCT."
  (org-list-get-nth 3 item struct))

(defun org-list-get-checkbox (item struct)
  "Return checkbox of ITEM in STRUCT or nil."
  (org-list-get-nth 4 item struct))

(defun org-list-set-checkbox (item struct checkbox)
  "Set checkbox of ITEM in STRUCT to CHECKBOX.
\nThis function modifies STRUCT."
  (org-list-set-nth 4 item struct checkbox))

(defun org-list-get-item-end (item struct)
  "Return end position of ITEM in STRUCT."
  (org-list-get-nth 5 item struct))

(defun org-list-get-item-end-before-blank (item struct)
  "Return point at end of item, before any blank line.
Point returned is at end of line."
  (save-excursion
    (goto-char (org-list-get-item-end item struct))
    (skip-chars-backward " \r\t\n")
    (point-at-eol)))

(defun org-list-struct-fix-bul (struct prevs)
  "Verify and correct bullets for every association in STRUCT.
\nThis function modifies STRUCT."
  (let ((fix-bul
	 (function
	  (lambda (item)
	    (let* ((prev (org-list-get-prev-item item struct prevs))
		   (prev-bul (and prev (org-list-get-bullet prev struct)))
		   (counter (org-list-get-counter item struct))
		   (bullet (org-list-get-bullet item struct)))
	      (org-list-set-bullet
	       item struct
	       (org-list-bullet-string
		(cond
		 ((and prev (string-match "[0-9]+" prev-bul) counter)
		  (replace-match counter nil nil prev-bul))
		 (prev
		  (org-list-inc-bullet-maybe (org-list-get-bullet prev struct)))
		 ((and (string-match "[0-9]+" bullet) counter)
		  (replace-match counter nil nil bullet))
		 ((string-match "[0-9]+" bullet)
		  (replace-match "1" nil nil bullet))
		 (t bullet)))))))))
    (mapc fix-bul (mapcar 'car struct))))

(defun org-list-struct-fix-ind (struct parents &optional bullet-size)
  "Verify and correct indentation for every association in STRUCT.

If numeric optional argument BULLET-SIZE is set, assume all
bullets in list have this length to determine new indentation.

This function modifies STRUCT."
  (let* ((ancestor (org-list-get-top-point struct))
         (top-ind (org-list-get-ind ancestor struct))
         (new-ind
          (lambda (item)
            (let ((parent (org-list-get-parent item struct parents)))
              (if parent
                  ;; Indent like parent + length of parent's bullet
                  (org-list-set-ind
		   item struct (+ (or bullet-size
				      (length
				       (org-list-get-bullet parent struct)))
				  (org-list-get-ind parent struct)))
                ;; If no parent, indent like top-point
		(org-list-set-ind item struct top-ind))))))
    (mapc new-ind (mapcar 'car (cdr struct)))))

(defun org-list-struct-fix-box (struct parents prevs &optional ordered)
  "Verify and correct checkboxes for every association in STRUCT.

If ORDERED is non-nil, a checkbox can only be checked when every
checkbox before it is checked too.  If there was an attempt to
break this rule, the function will return the blocking item.  In
all others cases, the return value will be `nil'.

This function modifies STRUCT."
  (let ((all-items (mapcar 'car struct))
	(set-parent-box
	 (function
	  (lambda (item)
	    (let* ((box-list
		    (mapcar (lambda (child)
			      (org-list-get-checkbox child struct))
			    (org-list-get-all-children item struct prevs))))
	      (org-list-set-checkbox
	       item struct
	       (cond
		((and (member "[ ]" box-list) (member "[X]" box-list)) "[-]")
		((member "[-]" box-list) "[-]")
		((member "[X]" box-list) "[X]")
		((member "[ ]" box-list) "[ ]")
		;; parent has no boxed child: leave box as-is
		(t (org-list-get-checkbox item struct))))))))
	parent-list)
    ;; 1. List all parents with a checkbox
    (mapc
     (lambda (e)
       (let* ((parent (org-list-get-parent e struct parents))
	      (parent-box-p (org-list-get-checkbox parent struct)))
	 (when (and parent-box-p (not (memq parent parent-list)))
	   (setq parent-list (cons parent parent-list)))))
     all-items)
    ;; 2. Sort those parents by decreasing indentation
    (setq parent-list (sort parent-list
			    (lambda (e1 e2)
			      (> (org-list-get-ind e1 struct)
				 (org-list-get-ind e2 struct)))))
    ;; 3. For each parent, get all children's checkboxes to determine
    ;;    and set its checkbox accordingly
    (mapc set-parent-box parent-list)
    ;; 4. If ORDERED is set, see if we need to uncheck some boxes
    (when ordered
      (let* ((box-list
	      (mapcar (lambda (e) (org-list-get-checkbox e struct)) all-items))
	     (after-unchecked (member "[ ]" box-list)))
	;; there are boxes checked after an unchecked one: fix that
	(when (member "[X]" after-unchecked)
	  (let ((index (- (length struct) (length after-unchecked))))
	    (mapc (lambda (e) (org-list-set-checkbox e struct "[ ]"))
		  (nthcdr index all-items))
	    ;; Verify once again the structure, without ORDERED
	    (org-list-struct-fix-box struct parents prevs nil)
	    ;; return blocking item
	    (nth index all-items)))))))

(defun org-list-struct-fix-struct (struct parents)
  "Return STRUCT with correct bullets and indentation."
  ;; Order of functions matters here: checkboxes and endings need
  ;; correct indentation to be set, and indentation needs correct
  ;; bullets.
  ;;
  ;; 0. Save a copy of structure before modifications
  (let ((old-struct (mapcar (lambda (e) (copy-alist e)) struct)))
    ;; 1. Set a temporary, but coherent with PARENTS, indentation in
    ;;    order to get items endings and bullets properly
    (org-list-struct-fix-ind struct parents 2)
    ;; 2. Get pseudo-alist of ending positions and sort it by position.
    ;;    Then associate them to the structure.
    (let (end-list acc-end)
    (mapc (lambda (e)
	    (let* ((pos (car e))
		   (ind-pos (org-list-get-ind pos struct))
		   (end-pos (org-list-get-item-end pos struct)))
	      (unless (assq end-pos struct)
		;; to determine real ind of an ending position that is
		;; not at an item, we have to find the item it belongs
		;; to: it is the last item (ITEM-UP), whose ending is
		;; further than the position we're interested in.
		(let ((item-up (assoc-default end-pos acc-end '>)))
		  (setq end-list
			(append
			 (list (cons
				(if item-up
				    (+ (org-list-get-ind item-up struct) 2)
				  0) ; this case is for the bottom point
				end-pos))
			 end-list))))
	      (setq end-list (append (list (cons ind-pos pos)) end-list))
	      (setq acc-end (cons (cons end-pos pos) acc-end))))
	  struct)
    (setq end-list (sort end-list (lambda (e1 e2) (< (cdr e1) (cdr e2)))))
    (org-list-struct-assoc-end struct end-list))
  ;; 3. Get bullets right
  (let ((prevs (org-list-struct-prev-alist struct)))
    (org-list-struct-fix-bul struct prevs)
    ;; 4. Now get real indentation
    (org-list-struct-fix-ind struct parents)
    ;; 5. Eventually fix checkboxes
    (org-list-struct-fix-box struct parents prevs))
  ;; 6. Apply structure modifications to buffer
  (org-list-struct-apply-struct struct old-struct)))

(defun org-list-struct-outdent (start end struct parents)
  "Outdent items in a structure.
Items are indented when their key is between START, included, and
END, excluded. STRUCT is the concerned structure."
  (let* (acc
	 (out (lambda (cell)
		(let* ((item (car cell))
		       (parent (cdr cell)))
		  (cond
		   ;; Item not yet in zone: keep association
		   ((< item start) cell)
		   ;; Item out of zone: follow associations in acc
		   ((>= item end)
		    (let ((convert (and parent (assq parent acc))))
		      (if convert (cons item (cdr convert)) cell)))
		   ;; Item has no parent: error
		   ((not parent)
		    (error "Cannot outdent top-level items"))
		   ;; Parent is outdented: keep association
		   ((>= parent start)
		    (setq acc (cons (cons parent item) acc)) cell)
		   (t
		    ;; Parent isn't outdented: reparent to grand-parent
		    (let ((grand-parent (org-list-get-parent
					 parent struct parents)))
		      (setq acc (cons (cons parent item) acc))
		      (cons item grand-parent))))))))
    (mapcar out parents)))

(defun org-list-struct-indent (start end struct parents prevs)
  "Indent items in a structure.
Items are indented when their key is between START, included, and
END, excluded.

PARENTS is the alist of parents. See
`org-list-struct-parent-alist'. PREVS is the alist of previous
items. See `org-list-struct-prev-alist'.

STRUCT is the concerned structure. It may be modified if
`org-list-demote-modify-bullet' matches bullets between START and
END."
  (let* (acc
	 (set-assoc (lambda (cell) (setq acc (cons cell acc)) cell))
	 (change-bullet-maybe
	  (function
	   (lambda (item)
	     (let* ((bul (org-trim (org-list-get-bullet item struct)))
		    (new-bul-p (cdr (assoc bul org-list-demote-modify-bullet))))
	       (when new-bul-p (org-list-set-bullet item struct new-bul-p))))))
	 (ind
	  (lambda (cell)
	    (let* ((item (car cell))
		   (parent (cdr cell)))
	      (cond
	       ;; Item not yet in zone: keep association
	       ((< item start) cell)
	       ((>= item end)
		;; Item out of zone: follow associations in acc
		(let ((convert (assq parent acc)))
		  (if convert (cons item (cdr convert)) cell)))
	       (t
		;; Item is in zone...
		(let ((prev (org-list-get-prev-item item struct prevs)))
		  ;; Check if bullet needs to be changed
		  (funcall change-bullet-maybe item)
		  (cond
		   ;; First item indented but not parent: error
		   ((and (not prev) (< parent start))
		    (error "Cannot indent the first item of a list"))
		   ;; First item and parent indented: keep same parent
		   ((not prev) (funcall set-assoc cell))
		   ;; Previous item not indented: reparent to it
		   ((< prev start) (funcall set-assoc (cons item prev)))
		   ;; Previous item indented: reparent like it
		   (t
		    (funcall set-assoc (cons item
					     (cdr (assq prev acc)))))))))))))
    (mapcar ind parents)))

(defun org-list-struct-apply-struct (struct old-struct)
  "Apply modifications to list so it mirrors STRUCT.

OLD-STRUCT is the structure before any modifications. Thus, the
function is smart enough to modify only parts of buffer which
have changed.

Initial position of cursor is restored after the changes."
  (let* ((pos (copy-marker (point)))
	 (shift-body-ind
	  (function
	   ;; Shift the indentation between END and BEG by DELTA.
	   ;; Start from the line before END.
	   (lambda (end beg delta)
	     (unless (= delta 0)
	       (goto-char end)
	       (forward-line -1)
	       (while (or (> (point) beg)
			  (and (= (point) beg) (not (org-at-item-p))))
		 (when (org-looking-at-p "^[ \t]*\\S-")
		   (let ((i (org-get-indentation)))
		     (org-indent-line-to (+ i delta))))
		 (forward-line -1))))))
         (modify-item
          (function
	   ;; Replace item first line elements with new elements from
	   ;; STRUCT, if appropriate.
	   (lambda (item)
	     (goto-char item)
	     (let* ((new-ind (org-list-get-ind item struct))
		    (old-ind (org-list-get-ind item old-struct))
		    (new-bul (org-list-bullet-string
			      (org-list-get-bullet item struct)))
		    (old-bul (org-list-get-bullet item old-struct))
		    (new-box (org-list-get-checkbox item struct)))
	       (looking-at org-list-full-item-re)
	       ;; a. Replace bullet
	       (unless (equal old-bul new-bul)
		 (replace-match new-bul nil nil nil 1))
	       ;; b. Replace checkbox
	       (cond
		((and new-box
		      (save-match-data (org-at-item-description-p))
		      (cdr (assq 'checkbox org-list-automatic-rules)))
		 (message "Cannot add a checkbox to a description list item"))
		((equal (match-string 3) new-box))
		((and (match-string 3) new-box)
		 (replace-match new-box nil nil nil 3))
		((match-string 3)
		 (goto-char (or (match-end 2) (match-end 1)))
		 (looking-at "\\[[ X-]\\][ \t]+")
		 (replace-match ""))
		(t (goto-char (or (match-end 2) (match-end 1)))
		   (insert (concat new-box " "))))
	       ;; c. Indent item to appropriate column
	       (unless (= new-ind old-ind)
		 (delete-region (goto-char (point-at-bol))
				(progn (skip-chars-forward " \t") (point)))
		 (indent-to new-ind)))))))
    ;; 1. First get list of items and position endings. We maintain
    ;;    two alists: ITM-SHIFT, determining indentation shift needed
    ;;    at item, and END-POS, a pseudo-alist where key is ending
    ;;    position and value point
    (let (end-list acc-end itm-shift all-ends sliced-struct)
      (mapc (lambda (e)
	      (let* ((pos (car e))
		     (ind-pos (org-list-get-ind pos struct))
		     (ind-old (org-list-get-ind pos old-struct))
		     (bul-pos (org-list-get-bullet pos struct))
		     (bul-old (org-list-get-bullet pos old-struct))
		     (ind-shift (- (+ ind-pos (length bul-pos))
				   (+ ind-old (length bul-old))))
		     (end-pos (org-list-get-item-end pos old-struct)))
		(setq itm-shift (cons (cons pos ind-shift) itm-shift))
		(unless (assq end-pos old-struct)
		  ;; To determine real ind of an ending position that is
		  ;; not at an item, we have to find the item it belongs
		  ;; to: it is the last item (ITEM-UP), whose ending is
		  ;; further than the position we're interested in.
		  (let ((item-up (assoc-default end-pos acc-end '>)))
		    (setq end-list (append
				    (list (cons end-pos item-up)) end-list))))
		(setq acc-end (cons (cons end-pos pos) acc-end))))
	    old-struct)
      ;; 2. Slice the items into parts that should be shifted by the
      ;;    same amount of indentation. The slices are returned in
      ;;    reverse order so changes modifying buffer do not change
      ;;    positions they refer to.
      (setq all-ends (sort (append (mapcar 'car itm-shift)
				   (org-uniquify (mapcar 'car end-list)))
			   '<))
      (while (cdr all-ends)
	(let* ((up (pop all-ends))
	       (down (car all-ends))
	       (ind (if (assq up struct)
			(cdr (assq up itm-shift))
		      (cdr (assq (cdr (assq up end-list)) itm-shift)))))
	  (setq sliced-struct (cons (list down up ind) sliced-struct))))
      ;; 3. Modify each slice in buffer, from end to beginning, with a
      ;;    special action when beginning is at item start.
      (mapc (lambda (e)
	      (apply shift-body-ind e)
	      (let ((beg (nth 1 e)))
		(when (assq beg struct)
		  (funcall modify-item beg))))
	    sliced-struct))
    ;; 4. Go back to initial position
    (goto-char pos)))

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

(defun org-outdent-item ()
  "Outdent a local list item, but not its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((struct (org-list-struct)))
    (org-list-indent-item-generic -1 t struct)))

(defun org-indent-item ()
  "Indent a local list item, but not its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((struct (org-list-struct)))
    (org-list-indent-item-generic 1 t struct)))

(defun org-outdent-item-tree ()
  "Outdent a local list item including its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((struct (org-list-struct)))
    (org-list-indent-item-generic -1 nil struct)))

(defun org-indent-item-tree ()
  "Indent a local list item including its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((struct (org-list-struct)))
    (org-list-indent-item-generic 1 nil struct)))

(defvar org-tab-ind-state)
(defun org-cycle-item-indentation ()
  "Cycle levels of indentation of an empty item.
The first run indents the item, if applicable.  Subsequents runs
outdent it at meaningful levels in the list.  When done, item is
put back at its original position with its original bullet.

Return t at each successful move."
  (when (org-at-item-p)
    (let* ((org-adapt-indentation nil)
	   (struct (org-list-struct))
	   (ind (org-list-get-ind (point-at-bol) struct)))
      ;; Check that item is really empty
      (when (and (or (org-at-item-description-p)
		     (save-excursion
		       (beginning-of-line)
		       (looking-at org-list-full-item-re)))
		 (>= (match-end 0) (save-excursion
				     (goto-char (org-list-get-item-end
						 (point-at-bol) struct))
				     (skip-chars-backward " \r\t\n")
				     (point))))
	(setq this-command 'org-cycle-item-indentation)
	;; When in the middle of the cycle, try to outdent first. If it
	;; fails, and point is still at initial position, indent. Else,
	;; go back to original position.
	(if (eq last-command 'org-cycle-item-indentation)
	    (cond
	     ((ignore-errors (org-list-indent-item-generic -1 t struct)))
	     ((and (= ind (car org-tab-ind-state))
		   (ignore-errors (org-list-indent-item-generic 1 t struct))))
	     (t (back-to-indentation)
		(org-indent-to-column (car org-tab-ind-state))
		(looking-at "\\S-+")
		(replace-match (cdr org-tab-ind-state))
		(end-of-line)
		;; Break cycle
		(setq this-command 'identity)))
	  ;; If a cycle is starting, remember indentation and bullet,
	  ;; then try to indent. If it fails, try to outdent.
	  (setq org-tab-ind-state (cons ind (org-get-bullet)))
	  (cond
	   ((ignore-errors (org-list-indent-item-generic 1 t struct)))
	   ((ignore-errors (org-list-indent-item-generic -1 t struct)))
	   (t (error "Cannot move item"))))
	t))))

;;; Bullets

(defun org-get-bullet ()
  "Return the bullet of the item at point.
Assume cursor is at an item."
  (save-excursion
    (beginning-of-line)
    (and (looking-at "[ \t]*\\(\\S-+\\)") (match-string 1))))

(defun org-list-bullet-string (bullet)
  "Return BULLET with the correct number of whitespaces.
It determines the number of whitespaces to append by looking at
`org-list-two-spaces-after-bullet-regexp'."
  (save-match-data
    (string-match "\\S-+\\([ \t]*\\)" bullet)
    (replace-match
     (save-match-data
       (concat
        " "
        ;; Do we need to concat another white space ?
        (when (and org-list-two-spaces-after-bullet-regexp
                   (string-match org-list-two-spaces-after-bullet-regexp bullet))
          " ")))
     nil nil bullet 1)))

(defun org-list-inc-bullet-maybe (bullet)
  "Increment BULLET if applicable."
  (if (string-match "[0-9]+" bullet)
      (replace-match
       (number-to-string (1+ (string-to-number (match-string 0 bullet))))
       nil nil bullet)
    bullet))

(defun org-list-repair ()
  "Make sure all items are correctly indented, with the right bullet.
This function scans the list at point, along with any sublist."
  (interactive)
  (unless (org-at-item-p) (error "This is not a list"))
  (let* ((struct (org-list-struct))
	 (parents (org-list-struct-parent-alist struct)))
    (org-list-struct-fix-struct struct parents)))

(defun org-cycle-list-bullet (&optional which)
  "Cycle through the different itemize/enumerate bullets.
This cycle the entire list level through the sequence:

   `-'  ->  `+'  ->  `*'  ->  `1.'  ->  `1)'

If WHICH is a valid string, use that as the new bullet. If WHICH
is an integer, 0 means `-', 1 means `+' etc. If WHICH is
`previous', cycle backwards."
  (interactive "P")
  (unless (org-at-item-p) (error "This is not a list"))
  (save-excursion
    (beginning-of-line)
    (let* ((struct (org-list-struct))
           (parents (org-list-struct-parent-alist struct))
           (prevs (org-list-struct-prev-alist struct))
           (list-beg (org-list-get-list-begin (point) struct prevs))
           (bullet (org-list-get-bullet list-beg struct))
	   (current (cond
		     ((string-match "\\." bullet) "1.")
		     ((string-match ")" bullet) "1)")
		     (t (org-trim bullet))))
	   (bullet-rule-p (cdr (assq 'bullet org-list-automatic-rules)))
           ;; Compute list of possible bullets, depending on context
	   (bullet-list (append '("-" "+" )
				;; *-bullets are not allowed at column 0
				(unless (and bullet-rule-p
					     (looking-at "\\S-")) '("*"))
				;; Description items cannot be numbered
				(unless (and bullet-rule-p
					     (or (eq org-plain-list-ordered-item-terminator ?\))
						 (org-at-item-description-p))) '("1."))
				(unless (and bullet-rule-p
					     (or (eq org-plain-list-ordered-item-terminator ?.)
						 (org-at-item-description-p))) '("1)"))))
	   (len (length bullet-list))
	   (item-index (- len (length (member current bullet-list))))
	   (get-value (lambda (index) (nth (mod index len) bullet-list)))
	   (new (cond
		 ((member which bullet-list) which)
		 ((numberp which) (funcall get-value which))
		 ((eq 'previous which) (funcall get-value (1- item-index)))
		 (t (funcall get-value (1+ item-index))))))
      ;; Use a short variation of `org-list-struct-fix-struct' as
      ;; there's no need to go through all the steps.
      (let ((old-struct (mapcar (lambda (e) (copy-alist e)) struct)))
        (org-list-set-bullet list-beg struct (org-list-bullet-string new))
        (org-list-struct-fix-bul struct prevs)
        (org-list-struct-fix-ind struct parents)
        (org-list-struct-apply-struct struct old-struct)))))

;;; Checkboxes

(defun org-toggle-checkbox (&optional toggle-presence)
  "Toggle the checkbox in the current line.
With prefix arg TOGGLE-PRESENCE, add or remove checkboxes.  With
double prefix, set checkbox to [-].

When there is an active region, toggle status or presence of the
first checkbox there, and make every item inside have the
same status or presence, respectively.

If the cursor is in a headline, apply this to all checkbox items
in the text below the heading, taking as reference the first item
in subtree, ignoring drawers."
  (interactive "P")
  (save-excursion
    (let* (singlep
	   block-item
	   lim-up
	   lim-down
	   (orderedp (ignore-errors (org-entry-get nil "ORDERED")))
	   (bounds
	    ;; In a region, start at first item in region
	    (cond
	     ((org-region-active-p)
	      (let ((limit (region-end)))
		(goto-char (region-beginning))
		(if (org-search-forward-unenclosed org-item-beginning-re
						   limit t)
		    (setq lim-up (point-at-bol))
		  (error "No item in region"))
		(setq lim-down (copy-marker limit))))
	     ((org-on-heading-p)
	      ;; On an heading, start at first item after drawers
	      (let ((limit (save-excursion (outline-next-heading) (point))))
		(forward-line 1)
		(when (looking-at org-drawer-regexp)
		  (re-search-forward "^[ \t]*:END:" limit nil))
		(if (org-search-forward-unenclosed org-item-beginning-re
						   limit t)
		    (setq lim-up (point-at-bol))
		  (error "No item in subtree"))
		(setq lim-down (copy-marker limit))))
	     ;; Just one item: set singlep flag
	     ((org-at-item-p)
	      (setq singlep t)
	      (setq lim-up (point-at-bol)
		    lim-down (point-at-eol)))
	     (t (error "Not at an item or heading, and no active region"))))
	   ;; determine the checkbox going to be applied to all items
	   ;; within bounds
	   (ref-checkbox
	    (progn
	      (goto-char lim-up)
	      (let ((cbox (and (org-at-item-checkbox-p) (match-string 1))))
		(cond
		 ((equal toggle-presence '(16)) "[-]")
		 ((equal toggle-presence '(4))
		  (unless cbox "[ ]"))
		 ((equal "[ ]" cbox) "[X]")
		 (t "[ ]"))))))
      ;; When an item is found within bounds, grab the full list at
      ;; point structure, then: 1. set checkbox of all its items
      ;; within bounds to ref-checkbox; 2. fix checkboxes of the whole
      ;; list; 3. move point after the list.
      (goto-char lim-up)
      (while (and (< (point) lim-down)
		  (org-search-forward-unenclosed
		   org-item-beginning-re lim-down 'move))
	(let* ((struct (org-list-struct))
	       (struct-copy (mapcar (lambda (e) (copy-alist e)) struct))
	       (parents (org-list-struct-parent-alist struct))
	       (prevs (org-list-struct-prev-alist struct))
	       (bottom (copy-marker (org-list-get-bottom-point struct)))
	       (items-to-toggle (org-remove-if
				 (lambda (e) (or (< e lim-up) (> e lim-down)))
				 (mapcar 'car (cdr struct)))))
	  (mapc (lambda (e) (org-list-set-checkbox
			e struct
			;; if there is no box at item, leave as-is
			;; unless function was called with C-u prefix
			(let ((cur-box (org-list-get-checkbox e struct)))
			  (if (or cur-box (equal toggle-presence '(4)))
			      ref-checkbox
			    cur-box))))
		items-to-toggle)
	  (setq block-item (org-list-struct-fix-box
			    struct parents prevs orderedp))
	  ;; Report some problems due to ORDERED status of subtree. If
	  ;; only one box was being checked, throw an error, else,
	  ;; only signal problems.
	  (cond
	   ((and singlep block-item (> lim-up block-item))
	    (error
	     "Checkbox blocked because of unchecked box at line %d"
	     (org-current-line block-item)))
	   (block-item
	    (message
	     "Checkboxes were removed due to unchecked box at line %d"
	     (org-current-line block-item))))
	  (goto-char bottom)
	  (org-list-struct-apply-struct struct struct-copy))))
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
This hook runs even if checkbox rule in
`org-list-automatic-rules' does not apply, so it can be used to
implement alternative ways of collecting statistics
information.")

(defun org-update-checkbox-count-maybe ()
  "Update checkbox statistics unless turned off by user."
  (when (cdr (assq 'checkbox org-list-automatic-rules))
    (org-update-checkbox-count))
  (run-hooks 'org-checkbox-statistics-hook))

(defun org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and
update them with the current numbers.

With optional prefix argument ALL, do this for the whole buffer."
  (interactive "P")
  (save-excursion
    (let ((cookie-re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	  (box-re "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)")
	  (recursivep
	   (or (not org-hierarchical-checkbox-statistics)
	       (string-match "\\<recursive\\>"
			     (or (ignore-errors
				   (org-entry-get nil "COOKIE_DATA"))
				 ""))))
	  (bounds (if all
		      (cons (point-min) (point-max))
		    (cons (or (ignore-errors (org-back-to-heading) (point))
			      (point-min))
			  (save-excursion (outline-next-heading) (point)))))
	  (count-boxes
	   (function
            ;; add checked boxes and boxes of all types in all
            ;; structures in STRUCTS to c-on and c-all, respectively.
            ;; This looks at RECURSIVEP value. If ITEM is nil, count
            ;; across the whole structure, else count only across
            ;; subtree whose ancestor is ITEM.
	    (lambda (item structs)
	      (mapc
               (lambda (s)
                 (let* ((pre (org-list-struct-prev-alist s))
                        (items
                         (if recursivep
                             (or (and item (org-list-get-subtree item s))
                                 (mapcar 'car s))
                           (or (and item (org-list-get-all-children item s pre))
                               (org-list-get-all-items
                                (org-list-get-top-point s) s pre))))
                        (cookies (delq nil (mapcar
                                            (lambda (e)
                                              (org-list-get-checkbox e s))
                                            items))))
                   (setq c-all (+ (length cookies) c-all)
                         c-on (+ (org-count "[X]" cookies) c-on))))
               structs))))
	  cookies-list backup-end structs-backup)
      (goto-char (car bounds))
      ;; 1. Build an alist for each cookie found within BOUNDS. The
      ;;    key will be position at beginning of cookie and values
      ;;    ending position, format of cookie, number of checked boxes
      ;;    to report, and total number of boxes.
      (while (re-search-forward cookie-re (cdr bounds) t)
	(save-excursion
	  (let ((c-on 0) (c-all 0))
	    (save-match-data
              ;; There are two types of cookies: those at headings and those
              ;; at list items.
	      (cond
	       ((and (org-on-heading-p)
		     (string-match "\\<todo\\>"
				   (downcase
				    (or (org-entry-get nil "COOKIE_DATA") "")))))
               ;; This cookie is at an heading, but specifically for
               ;; todo, not for checkboxes: skip it.
	       ((org-on-heading-p)
		(setq backup-end (save-excursion
                                   (outline-next-heading) (point)))
                ;; This cookie is at an heading. Grab structure of
		;; every list containing a checkbox between point and
		;; next headline, and save them in STRUCTS-BACKUP
		(while (org-search-forward-unenclosed box-re backup-end 'move)
		  (let* ((struct (org-list-struct))
			 (bottom (org-list-get-bottom-point struct)))
		    (setq structs-backup (cons struct structs-backup))
		    (goto-char bottom)))
		(funcall count-boxes nil structs-backup))
	       ((org-at-item-p)
		;; This cookie is at an item. Look in STRUCTS-BACKUP
                ;; to see if we have the structure of list at point in
                ;; it. Else compute the structure.
		(let ((item (point-at-bol)))
		  (if (and backup-end (< item backup-end))
		      (funcall count-boxes item structs-backup)
		    (let ((struct (org-list-struct)))
                      (setq end-entry (org-list-get-bottom-point struct)
                            structs-backup (list struct)))
		    (funcall count-boxes item structs-backup))))))
	    ;; Build the cookies list, with appropriate information
	    (setq cookies-list (cons (list (match-beginning 1) ; cookie start
					   (match-end 1) ; cookie end
					   (match-beginning 2) ; percent?
					   c-on   ; checked boxes
					   c-all) ; total boxes
				     cookies-list)))))
      ;; 2. Apply alist to buffer, in reverse order so positions stay
      ;;    unchanged after cookie modifications.
      (mapc (lambda (cookie)
	      (let* ((beg (car cookie))
		     (end (nth 1 cookie))
		     (percentp (nth 2 cookie))
		     (checked (nth 3 cookie))
		     (total (nth 4 cookie))
		     (new (if percentp
			      (format "[%d%%]" (/ (* 100 checked)
						  (max 1 total)))
			    (format "[%d/%d]" checked total))))
		(goto-char beg)
		(insert new)
		(delete-region (point) (+ (point) (- end beg)))))
	    cookies-list))))

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
  "Call FUNCTION on each item of the list at point.
FUNCTION must be called with at least one argument: INIT-VALUE,
that will contain the value returned by the function at the
previous item, plus ARGS extra arguments.

As an example, (org-apply-on-list (lambda (result) (1+ result)) 0)
will return the number of items in the current list.

Sublists of the list are skipped.  Cursor is always at the
beginning of the item."
  (let* ((pos (copy-marker (point)))
	 (end (copy-marker (org-list-bottom-point)))
	 (next-p (copy-marker (org-get-beginning-of-list (org-list-top-point))))
	 (value init-value))
    (while (< next-p end)
      (goto-char next-p)
      (set-marker next-p (or (org-get-next-item (point) end) end))
      (setq value (apply function value args)))
    (goto-char pos)
    value))

(defun org-sort-list (&optional with-case sorting-type getkey-func compare-func)
  "Sort plain list items.
The cursor may be at any item of the list that should be sorted.
Sublists are not sorted.  Checkboxes, if any, are ignored.

Sorting can be alphabetically, numerically, by date/time as given by
a time stamp, by a property or by priority.

Comparing entries ignores case by default. However, with an
optional argument WITH-CASE, the sorting considers case as well.

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
the sorting key for that record. It will then use COMPARE-FUNC to
compare entries."
  (interactive "P")
  (let* ((case-func (if with-case 'identity 'downcase))
         (struct (org-list-struct))
         (prevs (org-list-struct-prev-alist struct))
	 (start (org-list-get-list-begin (point-at-bol) struct prevs))
	 (end (org-list-get-list-end (point-at-bol) struct prevs))
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
      (goto-char (point-min))
      (let* ((dcst (downcase sorting-type))
	     (case-fold-search nil)
	     (now (current-time))
	     (sort-func (cond
			 ((= dcst ?a) 'string<)
			 ((= dcst ?f) compare-func)
			 ((= dcst ?t) '<)
			 (t nil)))
	     (next-record (lambda ()
			     (skip-chars-forward " \r\t\n")
			     (beginning-of-line)))
	     (end-record (lambda ()
			   (goto-char (org-list-get-item-end (point) struct))))
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
	(sort-subr (/= dcst sorting-type)
		   next-record
		   end-record
		   value-to-sort
		   nil
		   sort-func)
	(org-list-repair nil)
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
	(setq ltype (cond ((org-looking-at-p "^[ \t]*[0-9]") 'ordered)
			  ((org-at-item-description-p) 'descriptive)
			  (t 'unordered))))
      (let* ((indent1 (org-get-indentation))
	     (nextitem (or (org-get-next-item (point) end) end))
	     (item (org-trim (buffer-substring (point)
					       (org-end-of-item-or-at-child end))))
	     (nextindent (if (= (point) end) 0 (org-get-indentation)))
	     (item (if (string-match
			"^\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\([xX ]\\)\\]"
			item)
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
	(when (and (not (eq org-list-ending-method 'indent))
		   (looking-at (org-list-end-re)))
	  (replace-match "\n"))))
    (setq output (nreverse output))
    (push ltype output)))

(defun org-list-make-subtree ()
  "Convert the plain list at point into a subtree."
  (interactive)
  (if (not (org-in-item-p))
      (error "Not in a list")
    (let ((list (org-list-parse-list t)) nstars)
      (save-excursion
	(if (ignore-errors
	      (org-back-to-heading))
	    (progn (looking-at org-complex-heading-regexp)
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
    (unless (org-at-item-p) (error "Not at a list item"))
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
	      (re-search-forward
	       "\\(\\\\end{comment}\\|@end ignore\\|-->\\)" nil t)
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
		   (concat "BEGIN RECEIVE ORGLST +"
			   name
			   "\\([ \t]\\|$\\)") nil t)
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
	       (when (string-match "^\\(.*\\)[ \t]+::" sublist)
		 (setq term (org-trim (format (concat dtstart "%s" dtend)
					      (match-string 1 sublist))))
		 (setq sublist (concat ddstart
				       (org-trim (substring sublist
							    (match-end 0)))
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
