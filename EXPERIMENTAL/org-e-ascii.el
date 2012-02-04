;;; org-e-ascii.el --- ASCII Back-End For Org Export Engine

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an ASCII back-end for Org generic exporter.

;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-ascii "*Test e-ASCII*") RET
;;
;; in an Org mode buffer then switch to that buffer to see the ASCII
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.

;;; Code:

(eval-when-compile (require 'cl))

(declare-function org-element-get-contents "org-element" (element))
(declare-function org-element-get-property "org-element" (property element))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-map "org-element"
		  (data types fun &optional info first-match))
(declare-function org-element-time-stamp-interpreter
		  "org-element" (time-stamp contents))

(declare-function org-export-clean-table "org-export" (table specialp))
(declare-function org-export-collect-footnote-definitions
		  "org-export" (data info))
(declare-function org-export-collect-headlines "org-export" (info &optional n))
(declare-function org-export-collect-listings "org-export" (info))
(declare-function org-export-collect-tables "org-export" (info))
(declare-function org-export-data "org-export" (data backend info))
(declare-function org-export-expand-macro "org-export" (macro info))
(declare-function org-export-get-coderef-format "org-export" (path desc))
(declare-function org-export-get-footnote-number "org-export" (footnote info))
(declare-function org-export-get-headline-number "org-export" (headline info))
(declare-function org-export-get-ordinal "org-export"
		  (element info &optional types within-section predicate))
(declare-function org-export-get-parent-headline "org-export" (blob info))
(declare-function org-export-get-relative-level "org-export" (headline info))
(declare-function org-export-handle-code
		  "org-export" (element info &optional num-fmt ref-fmt delayed))
(declare-function org-export-included-file "org-export" (keyword backend info))
(declare-function org-export-low-level-p "org-export" (headline info))
(declare-function org-export-output-file-name "org-export"
		  (extension &optional subtreep pub-dir))
(declare-function org-export-resolve-coderef "org-export" (ref info))
(declare-function org-export-resolve-fuzzy-link "org-export" (link info))
(declare-function org-export-resolve-id-link "org-export" (link info))
(declare-function org-export-secondary-string
		  "org-export" (secondary backend info))
(declare-function org-export-table-format-info "org-export" (table))
(declare-function
 org-export-to-file "org-export"
 (backend file &optional subtreep visible-only body-only ext-plist))



;;; Internal Variables

;; The following setting won't allow to modify preferred charset
;; through a buffer keyword or an option item, but, since the property
;; will appear in communication channel nonetheless, it allows to
;; override `org-e-ascii-charset' variable on the fly by the ext-plist
;; mechanism.

;; We also install a developer filter for headlines, in order to
;; control blank lines in output string.

(defconst org-e-ascii-option-alist
  '((:ascii-charset nil nil org-e-ascii-charset)
    (:filter-headline nil nil (cons
			       'org-e-ascii-filter-headline-blank-lines
			       org-export-filter-headline-functions)))
  "Alist between ASCII export properties and ways to set them.
See `org-export-option-alist' for more information on the
structure or the values.")

(defconst org-e-ascii-dictionary
  '(("Footnotes\n"
     ("en"
      :ascii "Footnotes\n"
      :latin1 "Footnotes\n"
      :utf-8 "Footnotes\n")
     ("fr"
      :ascii "Notes de bas de page\n"
      :latin1 "Notes de bas de page\n"
      :utf-8 "Notes de bas de page\n"))
    ("Listing %d: %s"
     ("en"
      :ascii "Listing %d: %s"
      :latin1 "Listing %d: %s"
      :utf-8 "Listing %d: %s")
     ("fr"
      :ascii "Programme %d : %s"
      :latin1 "Programme %d : %s"
      :utf-8 "Programme nº %d : %s"))
    ("List Of Listings\n"
     ("en"
      :ascii "List Of Listings\n"
      :latin1 "List Of Listings\n"
      :utf-8 "List Of Listings\n")
     ("fr"
      :ascii "Liste des programmes\n"
      :latin1 "Liste des programmes\n"
      :utf-8 "Liste des programmes\n"))
    ("List Of Tables\n"
     ("en"
      :ascii "List Of Tables\n"
      :latin1 "List Of Tables\n"
      :utf-8 "List Of Tables\n")
     ("fr"
      :ascii "Liste des tableaux\n"
      :latin1 "Liste des tableaux\n"
      :utf-8 "Liste des tableaux\n"))
    ("Listing %d: "
     ("en"
      :ascii "Listing %d: "
      :latin1 "Listing %d: "
      :utf-8 "Listing %d: ")
     ("fr"
      :ascii "Programme %d : "
      :latin1 "Programme %d : "
      :utf-8 "Programme nº %d : "))
    ("Table Of Contents\n"
     ("en"
      :ascii "Table Of Contents\n"
      :latin1 "Table Of Contents\n"
      :utf-8 "Table Of Contents\n")
     ("fr"
      :ascii "Sommaire\n"
      :latin1 "Table des matières\n"
      :utf-8 "Table des matières\n"))
    ("Table %d: %s"
     ("en"
      :ascii "Table %d: %s"
      :latin1 "Table %d: %s"
      :utf-8 "Table %d: %s")
     ("fr"
      :ascii "Tableau %d : %s"
      :latin1 "Tableau %d : %s"
      :utf-8 "Tableau nº %d : %s"))
    ("See section %s"
     ("en"
      :ascii "See section %s"
      :latin1 "See section %s"
      :utf-8 "See section %s")
     ("fr"
      :ascii "cf. section %s"
      :latin1 "cf. section %s"
      :utf-8 "cf. section %s"))
    ("Table %d: "
     ("en"
      :ascii "Table %d: "
      :latin1 "Table %d: "
      :utf-8 "Table %d: ")
     ("fr"
      :ascii "Tableau %d : "
      :latin1 "Tableau %d : "
      :utf-8 "Tableau nº %d : "))
    ("Unknown reference"
     ("en"
      :ascii "Unknown reference"
      :latin1 "Unknown reference"
      :utf-8 "Unknown reference")
     ("fr"
      :ascii "Destination inconnue"
      :latin1 "Référence inconnue"
      :utf-8 "Référence inconnue")))
  "Dictionary for ASCII back-end.

Alist whose car is the string to translate and cdr is an alist
whose car is the language string and cdr is a plist whose
properties are possible charsets and value the translated term.

It is used as a database for `org-e-ascii--translate'.")



;;; User Configurable Variables

(defgroup org-export-e-ascii nil
  "Options for exporting Org mode files to ASCII."
  :tag "Org Export ASCII"
  :group 'org-export)

(defcustom org-e-ascii-text-width 72
  "Maximum width of exported text.
This number includes margin size, as set in
`org-e-ascii-global-margin'."
  :group 'org-export-e-ascii
  :type 'integer)

(defcustom org-e-ascii-global-margin 0
  "Width of the left margin, in number of characters."
  :group 'org-export-e-ascii
  :type 'integer)

(defcustom org-e-ascii-inner-margin 2
  "Width of the inner margin, in number of characters.
Inner margin is applied between each headline."
  :group 'org-export-e-ascii
  :type 'integer)

(defcustom org-e-ascii-quote-margin 6
  "Width of margin used for quoting text, in characters.
This margin is applied on both sides of the text."
  :group 'org-export-e-ascii
  :type 'integer)

(defcustom org-e-ascii-inlinetask-width 30
  "Width of inline tasks, in number of characters.
This number ignores any margin."
  :group 'org-export-e-ascii
  :type 'integer)

(defcustom org-e-ascii-headline-spacing '(1 . 2)
  "Number of blank lines inserted around headlines.

This variable can be set to a cons cell.  In that case, its car
represents the number of blank lines present before headline
contents whereas its cdr reflects the number of blank lines after
contents.

A nil value replicates the number of blank lines found in the
original Org buffer at the same place."
  :group 'org-export-e-ascii
  :type '(choice
	  (const :tag "Replicate original spacing" nil)
	  (cons :tag "Set an uniform spacing"
		(integer :tag "Number of blank lines before contents")
		(integer :tag "Number of blank lines after contents"))))

(defcustom org-e-ascii-charset 'ascii
  "The charset allowed to represent various elements and objects.
Possible values are:
`ascii'    Only use plain ASCII characters
`latin1'   Include Latin-1 characters
`utf-8'    Use all UTF-8 characters"
  :group 'org-export-e-ascii
  :type '(choice
	  (const :tag "ASCII" ascii)
	  (const :tag "Latin-1" latin1)
	  (const :tag "UTF-8" utf-8)))

(defcustom org-e-ascii-underline '((ascii ?= ?~ ?-)
				   (latin1 ?= ?~ ?-)
				   (utf-8 ?═ ?─ ?╌ ?┄ ?┈))
  "Characters for underlining headings in ASCII export.

Alist whose key is a symbol among `ascii', `latin1' and `utf-8'
and whose value is a list of characters.

For each supported charset, this variable associates a sequence
of underline characters.  In a sequence, the characters will be
used in order for headlines level 1, 2, ...  If no character is
available for a given level, the headline won't be underlined."
  :group 'org-export-e-ascii
  :type '(list
	  (cons :tag "Underline characters sequence"
		(const :tag "ASCII charset" ascii)
		(repeat character))
	  (cons :tag "Underline characters sequence"
		(const :tag "Latin-1 charset" latin1)
		(repeat character))
	  (cons :tag "Underline characters sequence"
		(const :tag "UTF-8 charset" utf-8)
		(repeat character))))

(defcustom org-e-ascii-bullets '((ascii ?* ?+ ?-)
				 (latin1 ?§ ?¶)
				 (utf-8 ?◊))
  "Bullet characters for headlines converted to lists in ASCII export.

Alist whose key is a symbol among `ascii', `latin1' and `utf-8'
and whose value is a list of characters.

The first character is used for the first level considered as low
level, and so on.  If there are more levels than characters given
here, the list will be repeated.

Note that this variable doesn't affect plain lists
representation."
  :group 'org-export-e-ascii
  :type '(list
	  (cons :tag "Bullet characters for low level headlines"
		(const :tag "ASCII charset" ascii)
		(repeat character))
	  (cons :tag "Bullet characters for low level headlines"
		(const :tag "Latin-1 charset" latin1)
		(repeat character))
	  (cons :tag "Bullet characters for low level headlines"
		(const :tag "UTF-8 charset" utf-8)
		(repeat character))))

(defcustom org-e-ascii-links-to-notes t
  "Non-nil means convert links to notes before the next headline.
When nil, the link will be exported in place.  If the line
becomes long in this way, it will be wrapped."
  :group 'org-export-e-ascii
  :type 'boolean)

(defcustom org-e-ascii-table-keep-all-vertical-lines nil
  "Non-nil means keep all vertical lines in ASCII tables.
When nil, vertical lines will be removed except for those needed
for column grouping."
  :group 'org-export-e-ascii
  :type 'boolean)

(defcustom org-e-ascii-table-widen-columns t
  "Non-nil means widen narrowed columns for export.
When nil, narrowed columns will look in ASCII export just like in
Org mode, i.e. with \"=>\" as ellipsis."
  :group 'org-export-e-ascii
  :type 'boolean)

(defcustom org-e-ascii-caption-above nil
  "When non-nil, place caption string before the element.
Otherwise, place it right after it."
  :group 'org-export-e-ascii
  :type 'boolean)

(defcustom org-e-ascii-verbatim-format "`%s'"
  "Format string used for verbatim text and inline code."
  :group 'org-export-e-ascii
  :type 'string)

(defcustom org-e-ascii-format-drawer-function nil
  "Function called to format a drawer in ASCII.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.
  WIDTH     the text width within the drawer.

The function should return either the string to be exported or
nil to ignore the drawer.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-ascii-format-drawer-default \(name contents width\)
  \"Format a drawer element for ASCII export.\"
  contents\)"
  :group 'org-export-e-ascii
  :type 'function)

(defcustom org-e-ascii-format-inlinetask-function nil
  "Function called to format an inlinetask in ASCII.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a string.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return either the string to be exported or
nil to ignore the inline task.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-ascii-format-inlinetask-default
  \(todo type priority name tags contents\)
  \"Format an inline task element for ASCII export.\"
  \(let* \(\(utf8p \(eq \(plist-get info :ascii-charset\) 'utf-8\)\)
           \(width org-e-ascii-inlinetask-width\)
    \(org-e-ascii--indent-string
     \(concat
      ;; Top line, with an additional blank line if not in UTF-8.
      \(make-string width \(if utf8p ?━ ?_\)\)  \"\\n\"
      \(unless utf8p \(concat \(make-string width ? \) \"\\n\"\)\)
      ;; Add title.  Fill it if wider than inlinetask.
      \(let \(\(title \(org-e-ascii--build-title inlinetask info width\)\)\)
	\(if \(<= \(length title\) width\) title
	  \(org-e-ascii--fill-string title width info\)\)\)
      \"\\n\"
      ;; If CONTENTS is not empty, insert it along with
      ;; a separator.
      \(when \(org-string-nw-p contents\)
        \(concat \(make-string width \(if utf8p ?─ ?-\)\) \"\\n\" contents\)\)
      ;; Bottom line.
      \(make-string width \(if utf8p ?━ ?_\)\)\)
     ;; Flush the inlinetask to the right.
     \(- \(plist-get info :ascii-width\)
        \(plist-get info :ascii-margin\)
        \(plist-get info :ascii-inner-margin\)
        \(org-e-ascii--current-text-width inlinetask info\)\)"
  :group 'org-export-e-ascii
  :type 'function)



;;; Internal Functions

;; Internal functions fall into three categories.

;; The first one is about text formatting.  The core function is
;; `org-e-ascii--current-text-width', which determines the current
;; text width allowed to a given element.  In other words, it helps
;; keeping each line width within maximum text width defined in
;; `org-e-ascii-text-width'.  Once this information is known,
;; `org-e-ascii--fill-string', `org-e-ascii--justify-string',
;; `org-e-ascii--box-string' and `org-e-ascii--indent-string' can
;; operate on a given output string.

;; The second category contains functions handling elements listings,
;; triggered by "#+TOC:" keyword.  As such, `org-e-ascii--build-toc'
;; returns a complete table of contents, `org-e-ascii--list-listings'
;; returns a list of referenceable src-block elements, and
;; `org-e-ascii--list-tables' does the same for table elements.

;; The third category includes general helper functions.
;; `org-e-ascii--build-title' creates the title for a given headline
;; or inlinetask element.  `org-e-ascii--build-caption' returns the
;; caption string associated to a table or a src-block.
;; `org-e-ascii--describe-links' creates notes about links for
;; insertion at the end of a section.  It uses
;; `org-e-ascii--unique-links' to get the list of links to describe.
;; Eventually, `org-e-ascii--translate' reads `org-e-ascii-dictionary'
;; to internationalize output.


(defun org-e-ascii--fill-string (s text-width info &optional justify)
  "Fill a string with specified text-width and return it.

S is the string being filled.  TEXT-WIDTH is an integer
specifying maximum length of a line.  INFO is the plist used as
a communication channel.

Optional argument JUSTIFY can specify any type of justification
among `left', `center', `right' or `full'.  A nil value is
equivalent to `left'.  For a justification that doesn't also fill
string, see `org-e-ascii--justify-string'.

Return nil if S isn't a string."
  ;; Don't fill paragraph when break should be preserved.
  (cond ((not (stringp s)) nil)
	((plist-get info :preserve-breaks) s)
	(t (with-temp-buffer
	     (let ((fill-column text-width)
		   (use-hard-newlines t))
	       (insert s)
	       (fill-region (point-min) (point-max) justify))
	     (buffer-string)))))

(defun org-e-ascii--justify-string (s text-width how)
  "Justify string S.
TEXT-WIDTH is an integer specifying maximum length of a line.
HOW determines the type of justification: it can be `left',
`right', `full' or `center'."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (let ((fill-column text-width))
      (while (< (point) (point-max))
	(justify-current-line how)
	(forward-line)))
    (buffer-string)))

(defun org-e-ascii--indent-string (s width)
  "Indent string S by WIDTH white spaces.
Empty lines are not indented."
  (when (stringp s)
    (replace-regexp-in-string
     "\\(^\\)\\(?:.*\\S-\\)" (make-string width ? ) s nil nil 1)))

(defun org-e-ascii--box-string (s info)
  "Return string S with a partial box to its left.
INFO is a plist used as a communicaton channel."
  (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
    (format (if utf8p "╭────\n%s\n╰────" ",----\n%s\n`----")
	    (replace-regexp-in-string
	     "^" (if utf8p "│ " "| ")
	     ;; Remove last newline character.
	     (replace-regexp-in-string "\n[ \t]*\\'" "" s)))))

(defun org-e-ascii--current-text-width (element info)
  "Return maximum text width for ELEMENT's contents.
INFO is a plist used as a communication channel."
  (cond
   ;; Elements with an absolute width: `headline' and `inlinetask'.
   ((eq (car element) 'inlinetask) org-e-ascii-inlinetask-width)
   ((eq (car element) 'headline)
    (- org-e-ascii-text-width
       (let ((low-level-rank (org-export-low-level-p element info)))
	 (if low-level-rank (* low-level-rank 2) org-e-ascii-global-margin))))
   ;; Elements with a relative width: store maximum text width in
   ;; TOTAL-WIDTH.
   (t
    (let* ((genealogy (cons element (plist-get info :genealogy)))
	   ;; Total width is determined by the presence, or not, of an
	   ;; inline task among ELEMENT parents.
	   (total-width
	    (if (loop for parent in genealogy
		      thereis (eq (car parent) 'inlinetask))
		org-e-ascii-inlinetask-width
	      ;; No inlinetask: Remove global margin from text width.
	      (- org-e-ascii-text-width
		 org-e-ascii-global-margin
		 (let ((parent (org-export-get-parent-headline element info)))
		   ;; Inner margin doesn't apply to text before first
		   ;; headline.
		   (if (not parent) 0
		     (let ((low-level-rank
			    (org-export-low-level-p parent info)))
		       ;; Inner margin doesn't apply to contents of
		       ;; low level headlines, since they've got their
		       ;; own indentation mechanism.
		       (if low-level-rank (* low-level-rank 2)
			 org-e-ascii-inner-margin))))))))
      (- total-width
	 ;; Each `quote-block', `quote-section' and `verse-block' above
	 ;; narrows text width by twice the standard margin size.
	 (+ (* (loop for parent in genealogy
		     when (memq (car parent)
				'(quote-block quote-section verse-block))
		     count parent)
	       2 org-e-ascii-quote-margin)
	    ;; Text width within a plain-list is restricted by
	    ;; indentation of current item.  If that's the case,
	    ;; compute it with the help of `:structure' property from
	    ;; parent item, if any.
	    (let ((parent-item
		   (if (eq (car element) 'item) element
		     (loop for parent in genealogy
			   when (eq (car parent) 'item)
			   return parent))))
	      (if (not parent-item) 0
		;; Compute indentation offset of the current item,
		;; that is the sum of the difference between its
		;; indentation and the indentation of the top item in
		;; the list and current item bullet's length.  Also
		;; remove tag length (for description lists) or bullet
		;; length.
		(let ((struct (org-element-get-property :structure parent-item))
		      (beg-item (org-element-get-property :begin parent-item)))
		  (+ (- (org-list-get-ind beg-item struct)
			(org-list-get-ind
			 (org-list-get-top-point struct) struct))
		     (length
		      (or (org-list-get-tag beg-item struct)
			  (org-list-get-bullet beg-item struct)))))))))))))

(defun org-e-ascii--build-title
  (element info text-width &optional underline notags)
  "Format ELEMENT title and return it.

ELEMENT is either an `headline' or `inlinetask' element.  INFO is
a plist used as a communication channel.  TEXT-WIDTH is an
integer representing the maximum length of a line.

When optional argument UNDERLINE is non-nil, underline title,
without the tags, according to `org-e-ascii-underline'
specifications.

if optional argument NOTAGS is nil, no tags will be added to the
title."
  (let* ((headlinep (eq (car element) 'headline))
	 (numbers
	  ;; Numbering is specific to headlines.
	  (and headlinep
	       ;; Section numbering must be active, and headline's
	       ;; level should be above specified limit, if any.
	       (let ((sec-num (plist-get info :section-numbers)))
		 (if (not (wholenump sec-num)) sec-num
		   (<= (org-export-get-relative-level headline info) sec-num)))
	       ;; All tests passed: build numbering string.
	       (concat
		(mapconcat
		 #'number-to-string
		 (org-export-get-headline-number element info) ".")
		" ")))
	 (text (org-export-secondary-string
		(org-element-get-property :title element) 'e-ascii info))
	 (todo
	  (and (plist-get info :with-todo-keywords)
	       (let ((todo (org-element-get-property :todo-keyword element)))
		 (and todo
		      (concat (org-export-secondary-string todo 'e-ascii info)
			      " ")))))
	 (tags (and (not notags)
		    (plist-get info :with-tags)
		    (org-element-get-property :tags element)))
	 (priority
	  (and (plist-get info :with-priority)
	       (concat (org-element-get-property :priority element) " ")))
	 (first-part (concat numbers todo priority text)))
    (concat
     first-part
     ;; Align tags, if any.
     (when tags
       (format
	(format " %%%ds"
		(max (- text-width  (1+ (length first-part))) (length tags)))
	tags))
     ;; Maybe underline text, if ELEMENT type is `headline' and an
     ;; underline character has been defined.
     (when (and underline headlinep)
       (let ((under-char
	      (nth (1- (org-export-get-relative-level element info))
		   (cdr (assq (plist-get info :ascii-charset)
			      org-e-ascii-underline)))))
	 (and under-char
	      (concat "\n"
		      (make-string (length first-part) under-char))))))))

(defun org-e-ascii--build-caption (element info)
  "Return caption string for ELEMENT, if applicable.

INFO is a plist used as a communication channel.

The caption string contains the sequence number of ELEMENT if it
has a name affiliated keyword, along with the real caption, if
any.  Return nil when ELEMENT has no affiliated caption or name
keyword."
  (let ((caption (org-element-get-property :caption element))
	(name (org-element-get-property :name element)))
    (when (or caption name)
      ;; Get sequence number of current src-block among every
      ;; src-block with either a caption or a name.
      (let ((reference
	     (org-export-get-ordinal
	      element info nil nil
	      (lambda (el) (or (org-element-get-property :caption el)
			  (org-element-get-property :name el)))))
	    (title-fmt (org-e-ascii--translate
			(case (car element)
			  (table "Table %d: %s")
			  (src-block "Listing %d: %s")) info)))
	(org-e-ascii--fill-string
	 (format
	  title-fmt reference
	  (if (not caption) name
	    (org-export-secondary-string (car caption) 'e-ascii info)))
	 (org-e-ascii--current-text-width element info) info)))))

(defun org-e-ascii--build-toc (info &optional n keyword)
  "Return a table of contents.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is an integer specifying the
depth of the table.

Optional argument KEYWORD specifies the TOC keyword, if any, from
which the table of contents generation has been initiated."
  (let ((title (org-e-ascii--translate "Table Of Contents\n" info)))
    (concat
     title
     (make-string (1- (length title))
		  (if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))
     "\n\n"
     (let ((text-width
	    (if keyword (org-e-ascii--current-text-width keyword info)
	      (- org-e-ascii-text-width org-e-ascii-global-margin))))
       (mapconcat
	(lambda (headline)
	  (let* ((level (org-export-get-relative-level headline info))
		 (indent (* (1- level) 3)))
	    (concat
	     (unless (zerop indent) (concat (make-string (1- indent) ?.) " "))
	     (org-e-ascii--build-title
	      headline info (- text-width indent) nil
	      (eq (plist-get info :with-tags) 'not-in-toc)))))
	(org-export-collect-headlines info n) "\n")))))

(defun org-e-ascii--list-listings (keyword info)
  "Return a list of listings.

KEYWORD is the keyword that initiated the list of listings
generation.  INFO is a plist used as a communication channel."
  (let ((title (org-e-ascii--translate "List Of Listings\n" info)))
    (concat
     title
     (make-string (1- (length title))
		  (if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))
     "\n\n"
     (let ((text-width
	    (if keyword (org-e-ascii--current-text-width keyword info)
	      (- org-e-ascii-text-width org-e-ascii-global-margin)))
	   ;; Use a counter instead of retreiving ordinal of each
	   ;; src-block.
	   (count 0))
       (mapconcat
	(lambda (src-block)
	  ;; Store initial text so its length can be computed.  This is
	  ;; used to properly align caption right to it in case of
	  ;; filling (like contents of a description list item).
	  (let ((initial-text
		 (format (org-e-ascii--translate "Listing %d: " info)
			 (incf count))))
	    (concat
	     initial-text
	     (org-trim
	      (org-e-ascii--indent-string
	       (org-e-ascii--fill-string
		(let ((caption (org-element-get-property :caption src-block)))
		  (if (not caption) (org-element-get-property :name src-block)
		    (org-export-secondary-string
		     ;; Use short name in priority, if available.
		     (or (cdr caption) (car caption)) 'e-ascii info)))
		(- text-width (length initial-text)) info)
	       (length initial-text))))))
	(org-export-collect-listings info) "\n")))))

(defun org-e-ascii--list-tables (keyword info)
  "Return a list of listings.

KEYWORD is the keyword that initiated the list of listings
generation.  INFO is a plist used as a communication channel."
  (let ((title (org-e-ascii--translate "List Of Tables\n" info)))
    (concat
     title
     (make-string (1- (length title))
		  (if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))
     "\n\n"
     (let ((text-width
	    (if keyword (org-e-ascii--current-text-width keyword info)
	      (- org-e-ascii-text-width org-e-ascii-global-margin)))
	   ;; Use a counter instead of retreiving ordinal of each
	   ;; src-block.
	   (count 0))
       (mapconcat
	(lambda (table)
	  ;; Store initial text so its length can be computed.  This is
	  ;; used to properly align caption right to it in case of
	  ;; filling (like contents of a description list item).
	  (let ((initial-text
		 (format (org-e-ascii--translate "Table %d: " info)
			 (incf count))))
	    (concat
	     initial-text
	     (org-trim
	      (org-e-ascii--indent-string
	       (org-e-ascii--fill-string
		(let ((caption (org-element-get-property :caption table)))
		  (if (not caption) (org-element-get-property :name table)
		    ;; Use short name in priority, if available.
		    (org-export-secondary-string
		     (or (cdr caption) (car caption)) 'e-ascii info)))
		(- text-width (length initial-text)) info)
	       (length initial-text))))))
	(org-export-collect-tables info) "\n")))))

(defun org-e-ascii--unique-links (element info)
  "Return a list of unique link references in ELEMENT.

ELEMENT is either an headline element or a section element.  INFO
is a plist used as a communication channel.

It covers links that may be found current headline's title, in
the following section and in any inlinetask's title there."
  (let* (seen
	 (unique-link-p
	  (function
	   ;; Return LINK if it wasn't referenced so far, or nil.
	   ;; Update SEEN links along the way.
	   (lambda (link)
	     (let ((footprint
		    (cons (org-element-get-property :raw-link link)
			  (org-element-get-contents link))))
	       (unless (member footprint seen)
		 (push footprint seen) link)))))
	 (harvest-links-in-title
	  (function
	   ;; Return a list of all unique links in ELEMENT.  ELEMENT
	   ;; may be an headline or an inlinetask element.
	   (lambda (element)
	     (let (acc)
	       (dolist (obj (org-element-get-property :title element) acc)
		 (when (and (listp obj) (eq (car obj) 'link))
		   (let ((link (funcall unique-link-p obj)))
		     (and link (push link acc)))))))))
	 ;; Retrieve headline's section, if it exists.
	 (section (if (eq (car element) 'section) element
		    (let ((sec (car (org-element-get-contents element))))
		      (and (eq (car sec) 'section) sec))))
	 (headline (if (eq (car element) 'headline) element
		     (org-export-get-parent-headline element info))))
    (append
     ;; Links that may be in HEADLINE's title.
     (funcall harvest-links-in-title headline)
     ;; Get all links in SECTION.
     (org-element-map
      section 'link (lambda (link local) (funcall unique-link-p link)) info)
     ;; Links that may be in inlinetasks titles within SECTION.
     (let (acc)
       (org-element-map
	section 'inlinetask
	(lambda (inlinetask local)
	  (push (funcall harvest-links-in-title inlinetask) acc))
	info)
       (delq nil acc)))))

(defun org-e-ascii--describe-links (links width info)
  "Return a string describing a list of links.

LINKS is a list of link type objects, as returned by
`org-e-ascii--unique-links'.  WIDTH is the text width allowed for
the output string.  INFO is a plist used as a communication
channel."
  (mapconcat
   (lambda (link)
     (let ((type (org-element-get-property :type link))
	   (anchor (let ((desc (org-element-get-contents link)))
		     (if (not desc)
			 (org-element-get-property :raw-link link)
		       (org-export-secondary-string desc 'e-ascii info)))))
       (cond
	;; Coderefs and radio links are ignored.
	((member type '("coderef" "radio")) nil)
	;; Id, custom-id and fuzzy links (with the exception of
	;; targets): Headlines refer to their numbering.
	((member type '("custom-id" "fuzzy" "id"))
	 (let ((destination (if (string= type "fuzzy")
				(org-export-resolve-fuzzy-link link info)
			      (org-export-resolve-id-link link info))))
	   (unless (eq (car destination) 'target)
	     (concat
	      (org-e-ascii--fill-string
	       (format
		"[%s] %s"
		anchor
		(if (not destination)
		    (org-e-ascii--translate "Unknown reference" info)
		  (format
		   (org-e-ascii--translate "See section %s" info)
		   (mapconcat 'number-to-string
			      (org-export-get-headline-number destination info)
			      "."))))
	       width info) "\n\n"))))
	;; Do not add a link that cannot be resolved and doesn't have
	;; any description: destination is already visible in the
	;; paragraph.
	((not (org-element-get-contents link)) nil)
	(t
	 (concat
	  (org-e-ascii--fill-string
	   (format "[%s] %s" anchor (org-element-get-property :raw-link link))
	   width info)
	  "\n\n")))))
   links ""))



;;; Template

(defun org-e-ascii-template--document-title (info)
  "Return document title, as a string.
INFO is a plist used as a communication channel."
  (let ((text-width org-e-ascii-text-width)
	(title (org-export-secondary-string
		(plist-get info :title) 'e-ascii info))
	(author
	 (and (plist-get info :with-author)
	      (let ((auth (plist-get info :author)))
		(and auth (org-export-secondary-string auth 'e-ascii info)))))
	(email
	 (and (plist-get info :with-email)
	      (org-export-secondary-string
	       (plist-get info :email) 'e-ascii info)))
	(date (plist-get info :date)))
    ;; There are two types of title blocks depending on the presence
    ;; of a title to display.
    (if (string= title "")
	;; Title block without a title.  DATE is positioned at the top
	;; right of the document, AUTHOR to the top left and EMAIL
	;; just below.
	(cond
	 ((and (org-string-nw-p date) (org-string-nw-p author))
	  (concat
	   author
	   (make-string (- text-width (length date) (length author)) ? )
	   date
	   (when (org-string-nw-p email) (concat "\n" email))
	   "\n\n\n"))
	 ((and (org-string-nw-p date) (org-string-nw-p email))
	  (concat
	   email
	   (make-string (- text-width (length date) (length email)) ? )
	   date "\n\n\n"))
	 ((org-string-nw-p date)
	  (concat
	   (org-e-ascii--justify-string date text-width 'right)
	   "\n\n\n"))
	 ((and (org-string-nw-p author) (org-string-nw-p email))
	  (concat author "\n" email "\n\n\n"))
	 ((org-string-nw-p author) (concat author "\n\n\n"))
	 ((org-string-nw-p email) (concat email "\n\n\n")))
      ;; Title block with a title.  Document's TITLE, along with the
      ;; AUTHOR and its EMAIL are both overlined and an underlined,
      ;; centered.  Date is just below, also centered.
      (let* ((utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	     ;; Format TITLE.  It may be filled if it is too wide,
	     ;; that is wider than the two thirds of the total width.
	     (title-len (min (length title) (/ (* 2 text-width) 3)))
	     (formatted-title (org-e-ascii--fill-string title title-len info))
	     (line
	      (make-string
	       (min (+ (max title-len (length author) (length email)) 2)
		    text-width) (if utf8p ?━ ?_))))
	(org-e-ascii--justify-string
	 (concat line "\n"
		 (unless utf8p "\n")
		 (upcase formatted-title)
		 (if utf8p "\n\n\n" "\n\n")
		 (cond
		  ((and (org-string-nw-p author) (org-string-nw-p email))
		   (concat author "\n" email))
		  ((org-string-nw-p author) author)
		  ((org-string-nw-p email) email))
		 "\n" line
		 (when (org-string-nw-p date) (concat "\n\n\n" date))
		 "\n\n\n") text-width 'center)))))

(defun org-e-ascii-template (contents info)
  "Return complete document string after ASCII conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (org-element-normalize-string
   (org-e-ascii--indent-string
    (let ((text-width (- org-e-ascii-text-width org-e-ascii-global-margin)))
      ;; 1. Build title block.
      (concat
       (org-e-ascii-template--document-title info)
       ;; 2. Table of contents.
       (let ((depth (plist-get info :with-toc)))
	 (when depth
	   (concat
	    (org-e-ascii--build-toc info (and (wholenump depth) depth))
	    "\n\n\n")))
       ;; 3. Document's body.
       contents
       ;; 4. Footnote definitions.
       (let ((definitions (org-export-collect-footnote-definitions
			   (plist-get info :parse-tree) info))
	     ;; Insert full links right inside the footnote definition
	     ;; as they have no chance to be inserted later.
	     (org-e-ascii-links-to-notes nil))
	 (when definitions
	   (concat
	    "\n\n\n"
	    (let ((title (org-e-ascii--translate "Footnotes\n" info)))
	      (concat
	       title
	       (make-string
		(1- (length title))
		(if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))))
	    "\n\n"
	    (mapconcat
	     (lambda (ref)
	       (let ((id (format "[%s] " (car ref))))
		 ;; Distinguish between inline definitions and
		 ;; full-fledged definitions.
		 (org-trim
		  (let ((def (nth 2 ref)))
		    (if (eq (car def) 'org-data)
			;; Full-fledged definition: footnote ID is
			;; inserted inside the first parsed paragraph
			;; (FIRST), if any, to be sure filling will
			;; take it into consideration.
			(let ((first (car (org-element-get-contents def))))
			  (if (not (eq (car first) 'paragraph))
			      (concat id "\n" (org-export-data def 'e-ascii info))
			    (push id (nthcdr 2 first))
			    (org-export-data def 'e-ascii info)))
		      ;; Fill paragraph once footnote ID is inserted in
		      ;; order to have a correct length for first line.
		      (org-e-ascii--fill-string
		       (concat id (org-export-secondary-string def 'e-ascii info))
		       text-width info))))))
	     definitions "\n\n"))))
       ;; 5. Creator.  Ignore `comment' value as there are no comments in
       ;;    ASCII.  Justify it to the bottom right.
       (let ((creator-info (plist-get info :with-creator)))
	 (unless (or (not creator-info) (eq creator-info 'comment))
	   (concat
	    "\n\n\n"
	    (org-e-ascii--fill-string
	     (plist-get info :creator) text-width info 'right))))))
    org-e-ascii-global-margin)))

(defun org-e-ascii--translate (s info)
  "Translate string S.

INFO is a plist used as a communication channel.

Translation depends on `:language' property and allowed charset.
If no translation in found for a given language and a given
charset, fall-back to S."
  (let* ((charset (intern (format ":%s" (plist-get info :ascii-charset))))
	 (lang (plist-get info :language))
	 (translations (cdr (assoc s org-e-ascii-dictionary))))
    (or (plist-get (cdr (assoc lang translations)) charset) s)))



;;; Transcode Functions

;;;; Babel Call

;; Babel Calls are ignored.


;;;; Center Block

(defun org-e-ascii-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-ascii--justify-string
   contents (org-e-ascii--current-text-width center-block info) 'center))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-ascii-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((name (org-element-get-property :drawer-name drawer))
	(width (org-e-ascii--current-text-width drawer info)))
    (if (functionp org-e-ascii-format-drawer-function)
	(funcall org-e-ascii-format-drawer-function name contents width)
      ;; If there's no user defined function: simply
      ;; display contents of the drawer.
      contents)))


;;;; Dynamic Block

(defun org-e-ascii-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See
`org-export-data'."
  contents)


;;;; Emphasis

(defun org-e-ascii-emphasis (emphasis contents info)
  "Transcode EMPHASIS from Org to ASCII.
CONTENTS is the contents of the emphasized text.  INFO is a plist
holding contextual information.."
  (let ((marker (org-element-get-property :marker emphasis)))
    ;; Leave emphasis markers as-is.
    (concat marker contents marker)))


;;;; Entity

(defun org-e-ascii-entity (entity contents info)
  "Transcode an ENTITY object from Org to ASCII.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-get-property
   (intern (concat ":" (symbol-name (plist-get info :ascii-charset))))
   entity))


;;;; Example Block

(defun org-e-ascii-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-ascii--box-string (org-export-handle-code example-block info) info))


;;;; Export Snippet

(defun org-e-ascii-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-get-property :value export-snippet))


;;;; Export Block

(defun org-e-ascii-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-get-property :type export-block) "ascii")
    (org-remove-indentation (org-element-get-property :value export-block))))


;;;; Fixed Width

(defun org-e-ascii-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-ascii--box-string
   (replace-regexp-in-string
    "^[ \t]*: ?" "" (org-element-get-property :value fixed-width)) info))


;;;; Footnote Definition

;; Footnote Definitions are ignored.  They are compiled at the end of
;; the document, by `org-e-ascii-template'.


;;;; Footnote Reference

(defun org-e-ascii-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "[%s]" (org-export-get-footnote-number footnote-reference info)))


;;;; Headline

(defun org-e-ascii-headline (headline contents info)
  "Transcode an HEADLINE element from Org to ASCII.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Don't export footnote section, which will be handled at the end
  ;; of the template.
  (unless (org-element-get-property :footnote-section-p headline)
    (let* ((low-level-rank (org-export-low-level-p headline info))
	   (width (org-e-ascii--current-text-width headline info))
	   ;; Blank lines between headline and its contents.
	   ;; `org-e-ascii-headline-spacing', when set, overwrites
	   ;; original buffer's spacing.
	   (pre-blanks
	    (make-string
	     (if org-e-ascii-headline-spacing (car org-e-ascii-headline-spacing)
	       (org-element-get-property :pre-blank headline)) ?\n))
	   ;; Even if HEADLINE has no section, there might be some
	   ;; links in its title that we shouldn't forget to describe.
	   (links
	    (unless (eq (caar (org-element-get-contents headline)) 'section)
	      (org-e-ascii--describe-links
	       (org-e-ascii--unique-links headline info) width info))))
      ;; Deep subtree: export it as a list item.
      (if low-level-rank
	  (concat
	   ;; Bullet.
	   (let ((bullets (cdr (assq (plist-get info :ascii-charset)
				     org-e-ascii-bullets))))
	     (char-to-string
	      (nth (mod (1- low-level-rank) (length bullets)) bullets)))
	   " "
	   ;; Title.
	   (org-e-ascii--build-title headline info width) "\n"
	   ;; Contents, indented by length of bullet.
	   pre-blanks
	   (org-e-ascii--indent-string
	    (concat contents
		    (when (org-string-nw-p links) (concat "\n\n" links)))
	    2))
	;; Else: Standard headline.
	(concat
	 (org-e-ascii--build-title headline info width 'underline)
	 "\n" pre-blanks
	 (concat (when (org-string-nw-p links) links) contents))))))


;;;; Horizontal Rule

(defun org-e-ascii-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((attr
	 (read
	  (format
	   "(%s)"
	   (mapconcat
	    #'identity
	    (org-element-get-property :attr_ascii horizontal-rule)
	    " ")))))
    (make-string (or (and (wholenump (plist-get attr :width))
			  (plist-get attr :width))
		     (org-e-ascii--current-text-width horizontal-rule info))
		 (if (eq (plist-get info :ascii-charset) 'utf-8) ?― ?-))))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-ascii-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format org-e-ascii-verbatim-format
	  (org-element-get-property :value inline-src-block)))


;;;; Inlinetask

(defun org-e-ascii-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((width (org-e-ascii--current-text-width inlinetask info))
	(title (org-export-secondary-string
		(org-element-get-property :title inlinetask) 'e-ascii info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-get-property
				:todo-keyword inlinetask)))
		     (and todo
			  (org-export-secondary-string todo 'e-ascii info)))))
	(todo-type (org-element-get-property :todo-type inlinetask))
	(tags (and (plist-get info :with-tags)
		   (org-element-get-property :tags inlinetask)))
	(priority (and (plist-get info :with-priority)
		       (org-element-get-property :priority inlinetask))))
    ;; If `org-e-ascii-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-e-ascii-format-inlinetask-function)
	(funcall org-e-ascii-format-inlinetask-function
		 todo todo-type priority title tags contents width)
      ;; Otherwise, use a default template.
      (let* ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
	(org-e-ascii--indent-string
	 (concat
	  ;; Top line, with an additional blank line if not in UTF-8.
	  (make-string width (if utf8p ?━ ?_))  "\n"
	  (unless utf8p (concat (make-string width ? ) "\n"))
	  ;; Add title.  Fill it if wider than inlinetask.
	  (let ((title (org-e-ascii--build-title inlinetask info width)))
	    (if (<= (length title) width) title
		(org-e-ascii--fill-string title width info)))
	  "\n"
	  ;; If CONTENTS is not empty, insert it along with
	  ;; a separator.
	  (when (org-string-nw-p contents)
	    (concat (make-string width (if utf8p ?─ ?-)) "\n" contents))
	  ;; Bottom line.
	  (make-string width (if utf8p ?━ ?_)))
	 ;; Flush the inlinetask to the right.
	 (- org-e-ascii-text-width org-e-ascii-global-margin
	    (if (not (org-export-get-parent-headline inlinetask info)) 0
	      org-e-ascii-inner-margin)
	    (org-e-ascii--current-text-width inlinetask info)))))))


;;;; Item

(defun org-e-ascii-item (item contents info)
  "Transcode an ITEM element from Org to ASCII.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((bullet
	 ;; First parent of ITEM is always the plain-list.  Get
	 ;; `:type' property from it.
	 (let ((type (org-element-get-property
		      :type (car (plist-get info :genealogy)))))
	   (if (eq type 'descriptive)
	       (concat
		(org-export-secondary-string
		 (org-element-get-property :tag item) 'e-ascii info) ": ")
	     (org-element-get-property :bullet item)))))
    (concat
     ;; Change bullets into more visible form if UTF-8 is active.
     (if (not (eq (plist-get info :ascii-charset) 'utf-8)) bullet
       (replace-regexp-in-string
	"-" "•"
	(replace-regexp-in-string
	 "+" "⁃"
	 (replace-regexp-in-string
	  "*" "‣" bullet))))
     ;; Contents: Pay attention to indentation.  Note: check-boxes are
     ;; already taken care of at the paragraph level so they don't
     ;; interfere with indentation.
     (let ((contents (org-e-ascii--indent-string contents (length bullet))))
       (if (eq (caar (org-element-get-contents item)) 'paragraph)
	   (org-trim contents)
	 (concat "\n" contents))))))


;;;; Keyword

(defun org-e-ascii-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((key (downcase (org-element-get-property :key keyword)))
	(value (org-element-get-property :value keyword)))
    (cond
     ((string= key "ascii") value)
     ((string= key "toc")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (org-e-ascii--build-toc
	     info (and (wholenump depth) depth) keyword)))
	 ((string= "tables" value)
	  (org-e-ascii--list-tables keyword info))
	 ((string= "listings" value)
	  (org-e-ascii--list-listings keyword info))))))))


;;;; Latex Environment

(defun org-e-ascii-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-remove-indentation (org-element-get-property :value latex-environment)))


;;;; Latex Fragment

(defun org-e-ascii-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-element-get-property :value latex-fragment))


;;;; Line Break

(defun org-e-ascii-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
  information."  hard-newline)


;;;; Link

(defun org-e-ascii-link (link desc info)
  "Transcode a LINK object from Org to ASCII.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let ((raw-link (org-element-get-property :raw-link link))
	(type (org-element-get-property :type link)))
    (cond
     ((string= type "coderef")
      (let ((ref (org-element-get-property :path link)))
	(format (org-export-get-coderef-format ref desc)
		(org-export-resolve-coderef ref info))))
     ;; Do not apply a special syntax on radio links.
     ((string= type "radio") desc)
     ;; Do not apply a special syntax on fuzzy links pointing to
     ;; targets.
     ((and (string= type "fuzzy")
	   (let ((path (org-element-get-property :path link)))
	     (loop for target in (plist-get info :target-list)
		   thereis (string=
			    (org-element-get-property :raw-value target)
			    path))))
      (if (org-string-nw-p desc) desc raw-link))
     (t
      (concat (format "[%s]" (if (org-string-nw-p desc) desc raw-link))
	      (unless org-e-ascii-links-to-notes (format " (%s)" raw-link)))))))


;;;; Macro

(defun org-e-ascii-macro (macro contents info)
  "Transcode a MACRO element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-ascii-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to ASCII.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (org-e-ascii--fill-string
   (let ((parent (car (plist-get info :genealogy))))
     ;; If PARAGRAPH is the first one in a list element, be sure to
     ;; add the check-box in front of it, before any filling.  Later,
     ;; it would interfere with line width.
     (if (and (eq (car parent) 'item)
	      (equal (car (org-element-get-contents parent)) paragraph))
	 (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
	   (concat (case (org-element-get-property :checkbox parent)
		     (on (if utf8p "☑ " "[X] "))
		     (off (if utf8p "☐ " "[ ] "))
		     (trans (if utf8p "☒ " "[-] ")))
		   contents))
       contents))
   (org-e-ascii--current-text-width paragraph info) info))


;;;; Plain List

(defun org-e-ascii-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to ASCII.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  contents)


;;;; Plain Text

(defun org-e-ascii-plain-text (text info)
  "Transcode a TEXT string from Org to ASCII.
INFO is a plist used as a communication channel."
  (if (not (and (eq (plist-get info :ascii-charset) 'utf-8)
		(plist-get info :with-special-strings)))
      text
    ;; Usual replacements in utf-8 with proper option set.
    (replace-regexp-in-string
     "\\.\\.\\." "…"
     (replace-regexp-in-string
      "--" "–"
      (replace-regexp-in-string "---" "—" text)))))


;;;; Property Drawer

(defun org-e-ascii-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to ASCII.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")


;;;; Quote Block

(defun org-e-ascii-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((width (org-e-ascii--current-text-width quote-block info)))
    (org-e-ascii--indent-string
     (org-remove-indentation
      (org-e-ascii--fill-string contents width info))
     org-e-ascii-quote-margin)))


;;;; Quote Section

(defun org-e-ascii-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((width (org-e-ascii--current-text-width quote-section info))
	(value
	 (org-export-secondary-string
	  (org-remove-indentation
	   (org-element-get-property :value quote-section)) 'e-ascii info)))
    (org-e-ascii--indent-string
     value
     (+ org-e-ascii-quote-margin
	;; Don't apply inner margin if parent headline is low level.
	(let ((headline (org-export-get-parent-headline quote-section info)))
	  (if (org-export-low-level-p headline info) 0
	    org-e-ascii-inner-margin))))))


;;;; Radio Target

(defun org-e-ascii-radio-target (radio-target contents info)
  "Transcode a RADIO-TARGET object from Org to ASCII.
CONTENTS is the contents of the target.  INFO is a plist holding
contextual information."
  contents)

;;;; Section

(defun org-e-ascii-section (section contents info)
  "Transcode a SECTION element from Org to ASCII.
CONTENTS is the contents of the section.  INFO is a plist holding
contextual information."
  (org-e-ascii--indent-string
   (concat
    contents
    (when org-e-ascii-links-to-notes
      ;; Add list of links at the end of SECTION.
      (let ((links (org-e-ascii--describe-links
		    (org-e-ascii--unique-links section info)
		    (org-e-ascii--current-text-width section info) info)))
	;; Separate list of links and section contents.
	(when (org-string-nw-p links) (concat "\n\n" links)))))
   ;; Do not apply inner margin if parent headline is low level.
   (let ((headline (org-export-get-parent-headline section info)))
     (if (or (not headline) (org-export-low-level-p headline info)) 0
       org-e-ascii-inner-margin))))


;;;; Special Block

(defun org-e-ascii-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)


;;;; Src Block

(defun org-e-ascii-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((caption (org-e-ascii--build-caption src-block info)))
    (concat
     (when (and caption org-e-ascii-caption-above) (concat caption "\n"))
     (org-e-ascii--box-string (org-export-handle-code src-block info) info)
     (when (and caption (not org-e-ascii-caption-above))
       (concat "\n" caption)))))

;;;; Statistics Cookie

(defun org-e-ascii-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-get-property :value statistics-cookie))


;;;; Subscript

(defun org-e-ascii-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to ASCII.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-get-property :use-brackets-p subscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))


;;;; Superscript

(defun org-e-ascii-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to ASCII.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-get-property :use-brackets-p superscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))


;;;; Table

;; While `org-e-ascii-table' is the callback function expected by
;; org-export mechanism, it requires four subroutines to display
;; tables accordingly to chosen charset, alignment and width
;; specifications.

;; Thus, `org-e-ascii-table--column-width' computes the display width
;; for each column in the table,
;; `org-e-ascii-table--vertical-separators' returns a vector
;; containing separators (or lack thereof),
;; `org-e-ascii-table--build-hline' creates various hline strings,
;; depending on charset, separators and position within the tabl and
;; `org-e-ascii-table--format-cell' properly aligns contents within
;; a given cell and width.

(defun org-e-ascii-table (table contents info)
  "Transcode a TABLE element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((raw-table (org-element-get-property :raw-table table))
	(caption (org-e-ascii--build-caption table info)))
    (concat
     ;; Possibly add a caption string above.
     (when (and caption org-e-ascii-caption-above) (concat caption "\n"))
     ;; Insert table.  Note: "table.el" tables are left unmodified.
     (if (eq (org-element-get-property :type table) 'table.el) raw-table
       (let* ((utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	      ;; Extract information out of the raw table (TABLE-INFO)
	      ;; and clean it (CLEAN-TABLE).
	      (table-info (org-export-table-format-info raw-table))
	      (special-col-p (plist-get table-info :special-column-p))
	      (alignment (plist-get table-info :alignment))
	      (clean-table (org-export-clean-table raw-table special-col-p))
	      ;; Change table into lisp, much like `org-table-to-lisp',
	      ;; being more careful about keeping the exact length of
	      ;; cells, for alignment purpose.
	      (lisp-table
	       (mapcar
		(lambda (line)
		  (if (string-match org-table-hline-regexp line) 'hline
		    (org-split-string (org-trim line) "\\s-?|\\s-?")))
		(org-split-string clean-table "[ \t]*\n[ \t]*")))
	      ;; Compute real column widths.
	      (column-widths
	       (org-e-ascii-table--column-width lisp-table table-info))
	      ;; Construct separators according to column groups.
	      (separators (org-e-ascii-table--vertical-separators table-info))
	      ;; Build different `hline' strings, depending on
	      ;;  separators, column widths and position.
	      (hline-standard
	       (org-e-ascii-table--build-hline
		nil separators column-widths info))
	      (hline-top
	       (and utf8p (org-e-ascii-table--build-hline
			   'top separators column-widths info)))
	      (hline-bottom
	       (and utf8p (org-e-ascii-table--build-hline
			   'bottom separators column-widths info))))
	 ;; Now build table back, with correct alignment, considering
	 ;; columns widths and separators.
	 (mapconcat
	  (lambda (line)
	    (cond
	     ((eq line 'hline) hline-standard)
	     ((eq line 'hline-bottom) hline-bottom)
	     ((eq line 'hline-top) hline-top)
	     (t (loop for cell in line
		      for col from 0 to (length line)
		      concat
		      (concat
		       (let ((sep (aref separators col)))
			 (if (and utf8p (not (string= sep ""))) "│" sep))
		       (org-e-ascii-table--format-cell
			cell col column-widths alignment info)) into l
			finally return
			(concat l
				(let ((sep (aref separators col)))
				  (if (and utf8p (not (string= sep ""))) "│"
				    sep)))))))
	  ;; If charset is `utf-8', make sure lisp-table always starts
	  ;; with `hline-top' and ends with `hline-bottom'.
	  (if (not utf8p) lisp-table
	    (setq lisp-table
		  (cons 'hline-top
			(if (eq (car lisp-table) 'hline) (cdr lisp-table)
			  lisp-table)))
	    (setq lisp-table
		  (nconc
		   (if (eq (car (last lisp-table)) 'hline) (butlast lisp-table)
		     lisp-table)
		   '(hline-bottom)))) "\n")))
     ;; Possible add a caption string below.
     (when (and caption (not org-e-ascii-caption-above))
       (concat "\n" caption)))))

(defun org-e-ascii-table--column-width (table table-info)
  "Return vector of TABLE columns width.

TABLE is the Lisp representation of the Org table considered.
TABLE-INFO holds information about the table.  See
`org-export-table-format-info'.

Unlike to `:width' property from `org-export-table-format-info',
the return value contains width of every column, not only those
with a width cookie."
  (let* ((cookies (plist-get table-info :width))
	 (width (make-vector (length cookies) 0)))
    (mapc
     (lambda (line)
       (let ((idx 0))
	 (unless (eq line 'hline)
	   (mapc (lambda (cell)
		   (let ((len (length cell)))
		     (when (> len (aref width idx)) (aset width idx len)))
		   (incf idx))
		 line))))
     table)
    (unless org-e-ascii-table-widen-columns
      ;; When colums are not widened, width cookies have precedence
      ;; over string lengths.  Thus, overwrite the latter with the
      ;; former.
      (loop for w across cookies
	    for idx from 0 to (length cookies)
	    when w do (aset width idx w)))
    ;; Return value.
    width))

(defun org-e-ascii-table--vertical-separators (table-info)
  "Return a vector of strings for vertical separators.

TABLE-INFO holds information about considered table.  See
`org-export-table-format-info'.

Return value is a vector whose length is one more than the number
of columns in the table.  Special column, if any, is ignored."
  (let* ((colgroups (plist-get table-info :column-groups))
	 (separators (make-vector (1+ (length colgroups)) "")))
    (if org-e-ascii-table-keep-all-vertical-lines
	(make-vector (length separators) "|")
      (let ((column 0))
	(mapc (lambda (group)
		(when (memq group '(start start-end))
		  (aset separators column "|"))
		(when (memq group '(end start-end))
		  (aset separators (1+ column) "|"))
		(incf column))
	      colgroups)
	;; Remove unneeded special column.
	(if (not (plist-get table-info :special-column-p)) separators
	  (substring separators 1))))))

(defun org-e-ascii-table--format-cell (cell col width alignment info)
  "Format CELL with column width and alignment constraints.

CELL is the contents of the cell, as a string.

COL is the column containing the cell considered.

WIDTH is a vector holding every column width, as returned by
`org-e-ascii-table--column-width'.

ALIGNMENT is a vector containing alignment strings for every
column.

INFO is a plist used as a communication channel."
  (let ((col-width (if org-e-ascii-table-widen-columns (aref width col)
		     (or (aref width col) (length cell)))))
    ;; When CELL is too large, it has to be truncated.
    (unless (or org-e-ascii-table-widen-columns (<= (length cell) col-width))
      (setq cell (concat (substring cell 0 (- col-width 2)) "=>")))
    (let* ((indent-tabs-mode nil)
	   (align (aref alignment col))
	   (aligned-cell
	    (org-e-ascii--justify-string
	     (org-trim cell) col-width
	     (cond ((string= align "c") 'center)
		   ((string= align "r") 'right)))))
      ;; Return aligned cell, with missing white spaces added and
      ;; space separators between columns.
      (format
       " %s "
       (concat aligned-cell
	       (make-string (- col-width (length aligned-cell)) ? ))))))

(defun org-e-ascii-table--build-hline (position separators column-widths info)
  "Return string used as an horizontal line in tables.

POSITION is a symbol among `top', `bottom' and nil, which
specifies position of the horizontal line within the table.

SEPARATORS is a vector strings specifying separators used in the
table, as returned by `org-e-ascii-table--vertical-separators'.

COLUMN-WIDTHS is a vector of numbers specifying widths of all
columns in the table, as returned by
`org-e-ascii-table--column-width'.

INFO is a plist used as a communication channel."
  (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
    (loop for idx from 0 to (length separators)
	  for width across column-widths
	  concat
	  (concat
	   (cond ((string= (aref separators idx) "") nil)
		 ((and utf8p (zerop idx))
		  (cond ((eq position 'top) "┍")
			((eq position 'bottom) "┕")
			(t "├")))
		 (utf8p
		  (cond ((eq position 'top) "┯")
			((eq position 'bottom) "┷")
			(t "┼")))
		 (t "+"))
	   ;; Hline has to cover all the cell and
	   ;; both white spaces between columns.
	   (make-string (+ width 2)
			(cond ((not utf8p) ?-)
			      ((not position) ?─)
			      (t ?━))))
	  into hline
	  finally return
	  (concat
	   hline
	   (cond
	    ((string= (aref separators idx) "") nil)
	    (utf8p (cond ((eq position 'top) "┑")
			 ((eq position 'bottom) "┙")
			 (t "┤")))
	    (t "+"))))))


;;;; Target

(defun org-e-ascii-target (target contents info)
  "Transcode a TARGET object from Org to ASCII.
CONTENTS is the contents of the target.  INFO is a plist holding
contextual information."
  contents)


;;;; Time-stamp

(defun org-e-ascii-time-stamp (time-stamp contents info)
  "Transcode a TIME-STAMP object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Return time-stamps as-is.
  (org-element-time-stamp-interpreter time-stamp contents))


;;;; Verbatim

(defun org-e-ascii-verbatim (verbatim contents info)
  "Return a VERBATIM object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format org-e-ascii-verbatim-format
	  (org-element-get-property :value verbatim)))


;;;; Verse Block

(defun org-e-ascii-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((verse-width (org-e-ascii--current-text-width verse-block info)))
    (org-e-ascii--indent-string
     (org-e-ascii--justify-string
      (org-export-secondary-string
       (org-element-get-property :value verse-block) 'e-ascii info)
      verse-width 'left)
     org-e-ascii-quote-margin)))


;;; Filter

(defun org-e-ascii-filter-headline-blank-lines (headline back-end info)
  "Filter controlling number of blank lines after an headline.

HEADLINE is a string representing a transcoded headline.
BACK-END is symbol specifying back-end used for export.  INFO is
plist containing the communication channel.

This function only applies to `e-ascii' back-end.  See
`org-e-ascii-headline-spacing' for information.

For any other back-end, HEADLINE is returned as-is."
  (if (not (and (eq back-end 'e-ascii) org-e-ascii-headline-spacing)) headline
    (let ((blanks (make-string (1+ (cdr org-e-ascii-headline-spacing)) ?\n)))
      (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" blanks headline))))



;;; Interactive function

(defun org-e-ascii-export-to-ascii
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a text file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".txt" subtreep pub-dir)))
    (org-export-to-file
     'e-ascii outfile subtreep visible-only body-only ext-plist)))


(provide 'org-e-ascii)
;;; org-e-ascii.el ends here
