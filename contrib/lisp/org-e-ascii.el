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
;;
;; This library implements an ASCII back-end for Org generic exporter.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-e-ascii-export-as-ascii' (temporary buffer) and
;; `org-e-ascii-export-to-ascii' ("txt" file).
;;
;; Output encoding is specified through `org-e-ascii-charset'
;; variable, among `ascii', `latin1' and `utf-8' symbols.
;;
;; By default, horizontal rules span over the full text with, but with
;; a given width attribute (set though #+ATTR_ASCII: :width <num>)
;; they can be shortened and centered.

;;; Code:

(eval-when-compile (require 'cl))
(require 'org-export)

(declare-function aa2u "ext:ascii-art-to-unicode" ())

;;; Define Back-End
;;
;; The following setting won't allow to modify preferred charset
;; through a buffer keyword or an option item, but, since the property
;; will appear in communication channel nonetheless, it allows to
;; override `org-e-ascii-charset' variable on the fly by the ext-plist
;; mechanism.
;;
;; We also install a filter for headlines and sections, in order to
;; control blank lines separating them in output string.

(org-export-define-backend e-ascii
  ((bold . org-e-ascii-bold)
   (center-block . org-e-ascii-center-block)
   (clock . org-e-ascii-clock)
   (code . org-e-ascii-code)
   (drawer . org-e-ascii-drawer)
   (dynamic-block . org-e-ascii-dynamic-block)
   (entity . org-e-ascii-entity)
   (example-block . org-e-ascii-example-block)
   (export-block . org-e-ascii-export-block)
   (export-snippet . org-e-ascii-export-snippet)
   (fixed-width . org-e-ascii-fixed-width)
   (footnote-definition . org-e-ascii-footnote-definition)
   (footnote-reference . org-e-ascii-footnote-reference)
   (headline . org-e-ascii-headline)
   (horizontal-rule . org-e-ascii-horizontal-rule)
   (inline-src-block . org-e-ascii-inline-src-block)
   (inlinetask . org-e-ascii-inlinetask)
   (italic . org-e-ascii-italic)
   (item . org-e-ascii-item)
   (keyword . org-e-ascii-keyword)
   (latex-environment . org-e-ascii-latex-environment)
   (latex-fragment . org-e-ascii-latex-fragment)
   (line-break . org-e-ascii-line-break)
   (link . org-e-ascii-link)
   (macro . org-e-ascii-macro)
   (paragraph . org-e-ascii-paragraph)
   (plain-list . org-e-ascii-plain-list)
   (plain-text . org-e-ascii-plain-text)
   (planning . org-e-ascii-planning)
   (quote-block . org-e-ascii-quote-block)
   (quote-section . org-e-ascii-quote-section)
   (radio-target . org-e-ascii-radio-target)
   (section . org-e-ascii-section)
   (special-block . org-e-ascii-special-block)
   (src-block . org-e-ascii-src-block)
   (statistics-cookie . org-e-ascii-statistics-cookie)
   (strike-through . org-e-ascii-strike-through)
   (subscript . org-e-ascii-subscript)
   (superscript . org-e-ascii-superscript)
   (table . org-e-ascii-table)
   (table-cell . org-e-ascii-table-cell)
   (table-row . org-e-ascii-table-row)
   (target . org-e-ascii-target)
   (template . org-e-ascii-template)
   (timestamp . org-e-ascii-timestamp)
   (underline . org-e-ascii-underline)
   (verbatim . org-e-ascii-verbatim)
   (verse-block . org-e-ascii-verse-block))
  :export-block "ASCII"
  :filters-alist ((:filter-headline . org-e-ascii-filter-headline-blank-lines)
		  (:filter-section . org-e-ascii-filter-headline-blank-lines))
  :options-alist ((:ascii-charset nil nil org-e-ascii-charset)))



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

(defcustom org-e-ascii-table-use-ascii-art nil
  "Non-nil means table.el tables are turned into ascii-art.

It only makes sense when export charset is `utf-8'.  It is nil by
default since it requires ascii-art-to-unicode.el package.  You
can download it here:

  http://gnuvola.org/software/j/aa2u/ascii-art-to-unicode.el.")

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
  TAGS      the inlinetask tags, as a list of strings.
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
;; Eventually, `org-e-ascii--translate' translates a string according
;; to language and charset specification.


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
  (case (org-element-type element)
    ;; Elements with an absolute width: `headline' and `inlinetask'.
    (inlinetask org-e-ascii-inlinetask-width)
    ('headline
     (- org-e-ascii-text-width
	(let ((low-level-rank (org-export-low-level-p element info)))
	  (if low-level-rank (* low-level-rank 2) org-e-ascii-global-margin))))
    ;; Elements with a relative width: store maximum text width in
    ;; TOTAL-WIDTH.
    (otherwise
     (let* ((genealogy (cons element (org-export-get-genealogy element)))
	    ;; Total width is determined by the presence, or not, of an
	    ;; inline task among ELEMENT parents.
	    (total-width
	     (if (loop for parent in genealogy
		       thereis (eq (org-element-type parent) 'inlinetask))
		 org-e-ascii-inlinetask-width
	       ;; No inlinetask: Remove global margin from text width.
	       (- org-e-ascii-text-width
		  org-e-ascii-global-margin
		  (let ((parent (org-export-get-parent-headline element)))
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
		      when (memq (org-element-type parent)
				 '(quote-block quote-section verse-block))
		      count parent)
		2 org-e-ascii-quote-margin)
	     ;; Text width within a plain-list is restricted by
	     ;; indentation of current item.  If that's the case,
	     ;; compute it with the help of `:structure' property from
	     ;; parent item, if any.
	     (let ((parent-item
		    (if (eq (org-element-type element) 'item) element
		      (loop for parent in genealogy
			    when (eq (org-element-type parent) 'item)
			    return parent))))
	       (if (not parent-item) 0
		 ;; Compute indentation offset of the current item,
		 ;; that is the sum of the difference between its
		 ;; indentation and the indentation of the top item in
		 ;; the list and current item bullet's length.  Also
		 ;; remove checkbox length, and tag length (for
		 ;; description lists) or bullet length.
		 (let ((struct (org-element-property :structure parent-item))
		       (beg-item (org-element-property :begin parent-item)))
		   (+ (- (org-list-get-ind beg-item struct)
			 (org-list-get-ind
			  (org-list-get-top-point struct) struct))
		      (length (org-e-ascii--checkbox parent-item info))
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
  (let* ((headlinep (eq (org-element-type element) 'headline))
	 (numbers
	  ;; Numbering is specific to headlines.
	  (and headlinep (org-export-numbered-headline-p element info)
	       ;; All tests passed: build numbering string.
	       (concat
		(mapconcat
		 'number-to-string
		 (org-export-get-headline-number element info) ".")
		" ")))
	 (text (org-export-data (org-element-property :title element) info))
	 (todo
	  (and (plist-get info :with-todo-keywords)
	       (let ((todo (org-element-property :todo-keyword element)))
		 (and todo (concat (org-export-data todo info) " ")))))
	 (tags (and (not notags)
		    (plist-get info :with-tags)
		    (let ((tag-list (org-export-get-tags element info)))
		      (and tag-list
			   (format ":%s:"
				   (mapconcat 'identity tag-list ":"))))))
	 (priority
	  (and (plist-get info :with-priority)
	       (let ((char (org-element-property :priority element)))
		 (and char (format "(#%c) " char)))))
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

(defun org-e-ascii--has-caption-p (element info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal'."
  (org-element-property :caption element))

(defun org-e-ascii--build-caption (element info)
  "Return caption string for ELEMENT, if applicable.

INFO is a plist used as a communication channel.

The caption string contains the sequence number of ELEMENT along
with its real caption.  Return nil when ELEMENT has no affiliated
caption keyword."
  (let ((caption (org-element-property :caption element)))
    (when caption
      ;; Get sequence number of current src-block among every
      ;; src-block with a caption.
      (let ((reference
	     (org-export-get-ordinal
	      element info nil 'org-e-ascii--has-caption-p))
	    (title-fmt (org-e-ascii--translate
			(case (org-element-type element)
			  (table "Table %d: %s")
			  (src-block "Listing %d: %s"))
			info)))
	(org-e-ascii--fill-string
	 (format title-fmt reference (org-export-data (car caption) info))
	 (org-e-ascii--current-text-width element info) info)))))

(defun org-e-ascii--build-toc (info &optional n keyword)
  "Return a table of contents.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is an integer specifying the
depth of the table.

Optional argument KEYWORD specifies the TOC keyword, if any, from
which the table of contents generation has been initiated."
  (let ((title (org-e-ascii--translate "Table of Contents" info)))
    (concat
     title "\n"
     (make-string (length title)
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
  (let ((title (org-e-ascii--translate "List of Listings" info)))
    (concat
     title "\n"
     (make-string (length title)
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
		 (format (org-e-ascii--translate "Listing %d:" info)
			 (incf count))))
	    (concat
	     initial-text " "
	     (org-trim
	      (org-e-ascii--indent-string
	       (org-e-ascii--fill-string
		(let ((caption (org-element-property :caption src-block)))
		  ;; Use short name in priority, if available.
		  (org-export-data (or (cdr caption) (car caption)) info))
		(- text-width (length initial-text)) info)
	       (length initial-text))))))
	(org-export-collect-listings info) "\n")))))

(defun org-e-ascii--list-tables (keyword info)
  "Return a list of listings.

KEYWORD is the keyword that initiated the list of listings
generation.  INFO is a plist used as a communication channel."
  (let ((title (org-e-ascii--translate "List of Tables" info)))
    (concat
     title "\n"
     (make-string (length title)
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
		 (format (org-e-ascii--translate "Table %d:" info)
			 (incf count))))
	    (concat
	     initial-text " "
	     (org-trim
	      (org-e-ascii--indent-string
	       (org-e-ascii--fill-string
		(let ((caption (org-element-property :caption table)))
		  ;; Use short name in priority, if available.
		  (org-export-data (or (cdr caption) (car caption)) info))
		(- text-width (length initial-text)) info)
	       (length initial-text))))))
	(org-export-collect-tables info) "\n")))))

(defun org-e-ascii--unique-links (element info)
  "Return a list of unique link references in ELEMENT.

ELEMENT is either an headline element or a section element.  INFO
is a plist used as a communication channel."
  (let* (seen
	 (unique-link-p
	  (function
	   ;; Return LINK if it wasn't referenced so far, or nil.
	   ;; Update SEEN links along the way.
	   (lambda (link)
	     (let ((footprint
		    (cons (org-element-property :raw-link link)
			  (org-element-contents link))))
	       (unless (member footprint seen)
		 (push footprint seen) link)))))
	 ;; If at a section, find parent headline, if any, in order to
	 ;; count links that might be in the title.
	 (headline
	  (if (eq (org-element-type element) 'headline) element
	    (or (org-export-get-parent-headline element) element))))
    ;; Get all links in HEADLINE.
    (org-element-map
     headline 'link (lambda (link) (funcall unique-link-p link)) info)))

(defun org-e-ascii--describe-links (links width info)
  "Return a string describing a list of links.

LINKS is a list of link type objects, as returned by
`org-e-ascii--unique-links'.  WIDTH is the text width allowed for
the output string.  INFO is a plist used as a communication
channel."
  (mapconcat
   (lambda (link)
     (let ((type (org-element-property :type link))
	   (anchor (let ((desc (org-element-contents link)))
		     (if (not desc) (org-element-property :raw-link link)
		       (org-export-data desc info)))))
       (cond
	;; Coderefs, radio links and fuzzy links are ignored.
	((member type '("coderef" "radio" "fuzzy")) nil)
	;; Id and custom-id links: Headlines refer to their numbering.
	((member type '("custom-id" "id"))
	 (let ((dest (org-export-resolve-id-link link info)))
	   (concat
	    (org-e-ascii--fill-string
	     (format
	      "[%s] %s"
	      anchor
	      (if (not dest) (org-e-ascii--translate "Unknown reference" info)
		(format
		 (org-e-ascii--translate "See section %s" info)
		 (mapconcat 'number-to-string
			    (org-export-get-headline-number dest info) "."))))
	     width info) "\n\n")))
	;; Do not add a link that cannot be resolved and doesn't have
	;; any description: destination is already visible in the
	;; paragraph.
	((not (org-element-contents link)) nil)
	(t
	 (concat
	  (org-e-ascii--fill-string
	   (format "[%s] %s" anchor (org-element-property :raw-link link))
	   width info)
	  "\n\n")))))
   links ""))

(defun org-e-ascii--checkbox (item info)
  "Return checkbox string for ITEM or nil.
INFO is a plist used as a communication channel."
  (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
    (case (org-element-property :checkbox item)
      (on (if utf8p "☑ " "[X] "))
      (off (if utf8p "☐ " "[ ] "))
      (trans (if utf8p "☒ " "[-] ")))))



;;; Template

(defun org-e-ascii-template--document-title (info)
  "Return document title, as a string.
INFO is a plist used as a communication channel."
  (let ((text-width org-e-ascii-text-width)
	(title (org-export-data (plist-get info :title) info))
	(author (and (plist-get info :with-author)
		     (let ((auth (plist-get info :author)))
		       (and auth (org-export-data auth info)))))
	(email (and (plist-get info :with-email)
		    (org-export-data (plist-get info :email) info)))
	(date (org-export-data (plist-get info :date) info)))
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
		 (cond
		  ((and (org-string-nw-p author) (org-string-nw-p email))
		   (concat (if utf8p "\n\n\n" "\n\n") author "\n" email))
		  ((org-string-nw-p author)
		   (concat (if utf8p "\n\n\n" "\n\n") author))
		  ((org-string-nw-p email)
		   (concat (if utf8p "\n\n\n" "\n\n") email)))
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
	    (let ((title (org-e-ascii--translate "Footnotes" info)))
	      (concat
	       title "\n"
	       (make-string
		(length title)
		(if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))))
	    "\n\n"
	    (mapconcat
	     (lambda (ref)
	       (let ((id (format "[%s] " (car ref))))
		 ;; Distinguish between inline definitions and
		 ;; full-fledged definitions.
		 (org-trim
		  (let ((def (nth 2 ref)))
		    (if (eq (org-element-type def) 'org-data)
			;; Full-fledged definition: footnote ID is
			;; inserted inside the first parsed paragraph
			;; (FIRST), if any, to be sure filling will
			;; take it into consideration.
			(let ((first (car (org-element-contents def))))
			  (if (not (eq (org-element-type first) 'paragraph))
			      (concat id "\n" (org-export-data def info))
			    (push id (nthcdr 2 first))
			    (org-export-data def info)))
		      ;; Fill paragraph once footnote ID is inserted in
		      ;; order to have a correct length for first line.
		      (org-e-ascii--fill-string
		       (concat id (org-export-data def info))
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
  "Translate string S according to specified language and charset.
INFO is a plist used as a communication channel."
  (let ((charset (intern (format ":%s" (plist-get info :ascii-charset)))))
    (org-export-translate s charset info)))



;;; Transcode Functions

;;;; Babel Call

;; Babel Calls are ignored.


;;;; Bold

(defun org-e-ascii-bold (bold contents info)
  "Transcode BOLD from Org to ASCII.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "*%s*" contents))


;;;; Center Block

(defun org-e-ascii-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-ascii--justify-string
   contents (org-e-ascii--current-text-width center-block info) 'center))


;;;; Clock

(defun org-e-ascii-clock (clock contents info)
  "Transcode a CLOCK object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat org-clock-string " "
	  (org-translate-time (org-element-property :value clock))
	  (let ((time (org-element-property :time clock)))
	    (and time
		 (concat " => "
			 (apply 'format
				"%2s:%02s"
				(org-split-string time ":")))))))


;;;; Code

(defun org-e-ascii-code (code contents info)
  "Return a CODE object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format org-e-ascii-verbatim-format (org-element-property :value code)))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-ascii-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((name (org-element-property :drawer-name drawer))
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
holding contextual information."
  contents)


;;;; Entity

(defun org-e-ascii-entity (entity contents info)
  "Transcode an ENTITY object from Org to ASCII.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property
   (intern (concat ":" (symbol-name (plist-get info :ascii-charset))))
   entity))


;;;; Example Block

(defun org-e-ascii-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-ascii--box-string
   (org-export-format-code-default example-block info) info))


;;;; Export Snippet

(defun org-e-ascii-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-ascii)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-e-ascii-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "ASCII")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-e-ascii-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-ascii--box-string
   (org-remove-indentation
    (org-element-property :value fixed-width)) info))


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
  (unless (org-element-property :footnote-section-p headline)
    (let* ((low-level-rank (org-export-low-level-p headline info))
	   (width (org-e-ascii--current-text-width headline info))
	   ;; Blank lines between headline and its contents.
	   ;; `org-e-ascii-headline-spacing', when set, overwrites
	   ;; original buffer's spacing.
	   (pre-blanks
	    (make-string
	     (if org-e-ascii-headline-spacing (car org-e-ascii-headline-spacing)
	       (org-element-property :pre-blank headline)) ?\n))
	   ;; Even if HEADLINE has no section, there might be some
	   ;; links in its title that we shouldn't forget to describe.
	   (links
	    (unless (or (eq (caar (org-element-contents headline)) 'section))
	      (let ((title (org-element-property :title headline)))
		(when (consp title)
		  (org-e-ascii--describe-links
		   (org-e-ascii--unique-links title info) width info))))))
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
  "Transcode an HORIZONTAL-RULE object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((text-width (org-e-ascii--current-text-width horizontal-rule info))
	(spec-width
	 (org-export-read-attribute :attr_ascii horizontal-rule :width)))
    (org-e-ascii--justify-string
     (make-string (if (wholenump spec-width) spec-width text-width)
		  (if (eq (plist-get info :ascii-charset) 'utf-8) ?― ?-))
     text-width 'center)))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-ascii-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format org-e-ascii-verbatim-format
	  (org-element-property :value inline-src-block)))


;;;; Inlinetask

(defun org-e-ascii-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((width (org-e-ascii--current-text-width inlinetask info)))
    ;; If `org-e-ascii-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-e-ascii-format-inlinetask-function)
	(funcall org-e-ascii-format-inlinetask-function
		 ;; todo.
		 (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property
				   :todo-keyword inlinetask)))
			(and todo (org-export-data todo info))))
		 ;; todo-type
		 (org-element-property :todo-type inlinetask)
		 ;; priority
		 (and (plist-get info :with-priority)
		      (org-element-property :priority inlinetask))
		 ;; title
		 (org-export-data (org-element-property :title inlinetask) info)
		 ;; tags
		 (and (plist-get info :with-tags)
		      (org-element-property :tags inlinetask))
		 ;; contents and width
		 contents width)
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
	    (if (not (org-export-get-parent-headline inlinetask)) 0
	      org-e-ascii-inner-margin)
	    (org-e-ascii--current-text-width inlinetask info)))))))

;;;; Italic

(defun org-e-ascii-italic (italic contents info)
  "Transcode italic from Org to ASCII.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "/%s/" contents))


;;;; Item

(defun org-e-ascii-item (item contents info)
  "Transcode an ITEM element from Org to ASCII.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	 (checkbox (org-e-ascii--checkbox item info))
	 (list-type (org-element-property :type (org-export-get-parent item)))
	 (bullet
	  ;; First parent of ITEM is always the plain-list.  Get
	  ;; `:type' property from it.
	  (org-list-bullet-string
	   (case list-type
	     (descriptive
	      (concat checkbox
		      (org-export-data (org-element-property :tag item) info)
		      ": "))
	     (ordered
	      ;; Return correct number for ITEM, paying attention to
	      ;; counters.
	      (let* ((struct (org-element-property :structure item))
		     (bul (org-element-property :bullet item))
		     (num (number-to-string
			   (car (last (org-list-get-item-number
				       (org-element-property :begin item)
				       struct
				       (org-list-prevs-alist struct)
				       (org-list-parents-alist struct)))))))
		(replace-regexp-in-string "[0-9]+" num bul)))
	     (t (let ((bul (org-element-property :bullet item)))
		  ;; Change bullets into more visible form if UTF-8 is active.
		  (if (not utf8p) bul
		    (replace-regexp-in-string
		     "-" "•"
		     (replace-regexp-in-string
		      "+" "⁃"
		      (replace-regexp-in-string "*" "‣" bul))))))))))
    (concat
     bullet
     (unless (eq list-type 'descriptive) checkbox)
     ;; Contents: Pay attention to indentation.  Note: check-boxes are
     ;; already taken care of at the paragraph level so they don't
     ;; interfere with indentation.
     (let ((contents (org-e-ascii--indent-string contents (length bullet))))
       (if (eq (org-element-type (car (org-element-contents item))) 'paragraph)
	   (org-trim contents)
	 (concat "\n" contents))))))


;;;; Keyword

(defun org-e-ascii-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "ASCII") value)
     ((string= key "TOC")
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
  (org-remove-indentation (org-element-property :value latex-environment)))


;;;; Latex Fragment

(defun org-e-ascii-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-element-property :value latex-fragment))


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
  (let ((raw-link (org-element-property :raw-link link))
	(type (org-element-property :type link)))
    (cond
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref desc)
		(org-export-resolve-coderef ref info))))
     ;; Do not apply a special syntax on radio links.  Though, use
     ;; transcoded target's contents as output.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (org-export-data (org-element-contents destination) info))))
     ;; Do not apply a special syntax on fuzzy links pointing to
     ;; targets.
     ((string= type "fuzzy")
      (let ((destination (org-export-resolve-fuzzy-link link info)))
	;; Ignore invisible "#+TARGET: path".
	(unless (eq (org-element-type destination) 'keyword)
	  (if (org-string-nw-p desc) desc
	    (when destination
	      (let ((number
		     (org-export-get-ordinal
		      destination info nil 'org-e-ascii--has-caption-p)))
		(when number
		  (if (atom number) (number-to-string number)
		    (mapconcat 'number-to-string number ".")))))))))
     (t
      (if (not (org-string-nw-p desc)) (format "[%s]" raw-link)
	(concat
	 (format "[%s]" desc)
	 (unless org-e-ascii-links-to-notes (format " (%s)" raw-link))))))))


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
   contents
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


;;;; Planning

(defun org-e-ascii-planning (planning contents info)
  "Transcode a PLANNING element from Org to ASCII.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (mapconcat
   'identity
   (delq nil
	 (list (let ((closed (org-element-property :closed planning)))
		 (when closed (concat org-closed-string " "
				      (org-translate-time closed))))
	       (let ((deadline (org-element-property :deadline planning)))
		 (when deadline (concat org-deadline-string " "
					(org-translate-time deadline))))
	       (let ((scheduled (org-element-property :scheduled planning)))
		 (when scheduled (concat org-scheduled-string " "
					 (org-translate-time scheduled))))))
   " "))


;;;; Property Drawer
;;
;; Property drawers are ignored.


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
	 (org-export-data
	  (org-remove-indentation (org-element-property :value quote-section))
	  info)))
    (org-e-ascii--indent-string
     value
     (+ org-e-ascii-quote-margin
	;; Don't apply inner margin if parent headline is low level.
	(let ((headline (org-export-get-parent-headline quote-section)))
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
   (let ((headline (org-export-get-parent-headline section)))
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
     (org-e-ascii--box-string
      (org-export-format-code-default src-block info) info)
     (when (and caption (not org-e-ascii-caption-above))
       (concat "\n" caption)))))

;;;; Statistics Cookie

(defun org-e-ascii-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Subscript

(defun org-e-ascii-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to ASCII.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-property :use-brackets-p subscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))


;;;; Superscript

(defun org-e-ascii-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to ASCII.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-property :use-brackets-p superscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))


;;;; Strike-through

(defun org-e-ascii-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to ASCII.
CONTENTS is text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "+%s+" contents))


;;;; Table

(defun org-e-ascii-table (table contents info)
  "Transcode a TABLE element from Org to ASCII.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let ((caption (org-e-ascii--build-caption table info)))
    (concat
     ;; Possibly add a caption string above.
     (when (and caption org-e-ascii-caption-above) (concat caption "\n"))
     ;; Insert table.  Note: "table.el" tables are left unmodified.
     (cond ((eq (org-element-property :type table) 'org) contents)
	   ((and org-e-ascii-table-use-ascii-art
		 (eq (plist-get info :ascii-charset) 'utf-8)
		 (require 'ascii-art-to-unicode nil t))
	    (with-temp-buffer
	      (insert (org-remove-indentation
		       (org-element-property :value table)))
	      (goto-char (point-min))
	      (aa2u)
	      (goto-char (point-max))
	      (skip-chars-backward " \r\t\n")
	      (buffer-substring (point-min) (point))))
	   (t (org-remove-indentation (org-element-property :value table))))
     ;; Possible add a caption string below.
     (when (and caption (not org-e-ascii-caption-above))
       (concat "\n" caption)))))


;;;; Table Cell

(defun org-e-ascii--table-cell-width (table-cell info)
  "Return width of TABLE-CELL.

INFO is a plist used as a communication channel.

Width of a cell is determined either by a width cookie in the
same column as the cell, or by the maximum cell's length in that
column.

When `org-e-ascii-table-widen-columns' is non-nil, width cookies
are ignored."
  (or (and (not org-e-ascii-table-widen-columns)
	   (org-export-table-cell-width table-cell info))
      (let* ((max-width 0)
	     (table (org-export-get-parent-table table-cell))
	     (specialp (org-export-table-has-special-column-p table))
	     (col (cdr (org-export-table-cell-address table-cell info))))
	(org-element-map
	 table 'table-row
	 (lambda (row)
	   (setq max-width
		 (max (length
		       (org-export-data
			(org-element-contents
			 (elt (if specialp (cdr (org-element-contents row))
				(org-element-contents row))
			      col))
			info))
		      max-width)))
	 info)
	max-width)))

(defun org-e-ascii-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL object from Org to ASCII.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  ;; Determine column width.  When `org-e-ascii-table-widen-columns'
  ;; is nil and some width cookie has set it, use that value.
  ;; Otherwise, compute the maximum width among transcoded data of
  ;; each cell in the column.
  (let ((width (org-e-ascii--table-cell-width table-cell info)))
    ;; When contents are too large, truncate them.
    (unless (or org-e-ascii-table-widen-columns (<= (length contents) width))
      (setq contents (concat (substring contents 0 (- width 2)) "=>")))
    ;; Align contents correctly within the cell.
    (let* ((indent-tabs-mode nil)
	   (data
	    (when contents
	      (org-e-ascii--justify-string
	       contents width
	       (org-export-table-cell-alignment table-cell info)))))
      (setq contents (concat data (make-string (- width (length data)) ? ))))
    ;; Return cell.
    (concat (format " %s " contents)
	    (when (memq 'right (org-export-table-cell-borders table-cell info))
	      (if (eq (plist-get info :ascii-charset) 'utf-8) "│" "|")))))


;;;; Table Row

(defun org-e-ascii-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to ASCII.
CONTENTS is the row contents.  INFO is a plist used as
a communication channel."
  (when (eq (org-element-property :type table-row) 'standard)
    (let ((build-hline
	   (function
	    (lambda (lcorner horiz vert rcorner)
	      (concat
	       (apply
		'concat
		(org-element-map
		 table-row 'table-cell
		 (lambda (cell)
		   (let ((width (org-e-ascii--table-cell-width cell info))
			 (borders (org-export-table-cell-borders cell info)))
		     (concat
		      ;; In order to know if CELL starts the row, do
		      ;; not compare it with the first cell in the row
		      ;; as there might be a special column.  Instead,
		      ;; compare it with the first exportable cell,
		      ;; obtained with `org-element-map'.
		      (when (and (memq 'left borders)
				 (eq (org-element-map
				      table-row 'table-cell 'identity info t)
				     cell))
			lcorner)
		      (make-string (+ 2 width) (string-to-char horiz))
		      (cond
		       ((not (memq 'right borders)) nil)
		       ((eq (car (last (org-element-contents table-row))) cell)
			rcorner)
		       (t vert)))))
		 info)) "\n"))))
	  (utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	  (borders (org-export-table-cell-borders
		    (org-element-map table-row 'table-cell 'identity info t)
		    info)))
      (concat (cond
	       ((and (memq 'top borders) (or utf8p (memq 'above borders)))
		(if utf8p (funcall build-hline "┍" "━" "┯" "┑")
		  (funcall build-hline "+" "-" "+" "+")))
	       ((memq 'above borders)
		(if utf8p (funcall build-hline "├" "─" "┼" "┤")
		  (funcall build-hline "+" "-" "+" "+"))))
	      (when (memq 'left borders) (if utf8p "│" "|"))
	      contents "\n"
	      (when (and (memq 'bottom borders) (or utf8p (memq 'below borders)))
		(if utf8p (funcall build-hline "┕" "━" "┷" "┙")
		  (funcall build-hline "+" "-" "+" "+")))))))


;;;; Target

;; Targets are invisible.


;;;; Timestamp

(defun org-e-ascii-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-translate-time (org-element-property :value timestamp)))
	(range-end
	 (org-translate-time (org-element-property :range-end timestamp)))
	(utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
    (concat value
	    (when range-end (concat (if utf8p "–" "--") range-end)))))


;;;; Underline

(defun org-e-ascii-underline (underline contents info)
  "Transcode UNDERLINE from Org to ASCII.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "_%s_" contents))


;;;; Verbatim

(defun org-e-ascii-verbatim (verbatim contents info)
  "Return a VERBATIM object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format org-e-ascii-verbatim-format
	  (org-element-property :value verbatim)))


;;;; Verse Block

(defun org-e-ascii-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to ASCII.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (let ((verse-width (org-e-ascii--current-text-width verse-block info)))
    (org-e-ascii--indent-string
     (org-e-ascii--justify-string contents verse-width 'left)
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
  (if (not org-e-ascii-headline-spacing) headline
    (let ((blanks (make-string (1+ (cdr org-e-ascii-headline-spacing)) ?\n)))
      (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" blanks headline))))



;;; Interactive function

;;;###autoload
(defun org-e-ascii-export-as-ascii
  (&optional subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

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

Export is done in a buffer named \"*Org E-ASCII Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (let ((outbuf (org-export-to-buffer
		 'e-ascii "*Org E-ASCII Export*"
		 subtreep visible-only body-only ext-plist)))
    (with-current-buffer outbuf (text-mode))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf))))

;;;###autoload
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
