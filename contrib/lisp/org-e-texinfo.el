;;; org-e-texinfo.el --- Texinfo Back-End For Org Export Engine

;; Copyright (C) 2012  Jonathan Leech-Pepin
;; Author: Jonathan Leech-Pepin <jonathan.leechpepin at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;;
;; This file is not part of GNU Emacs.
;;
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
;; This library implements a Texinfo back-end for Org generic
;; exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-texinfo "*Test e-texinfo*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the Texinfo
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.
;;
;; It introduces eight new buffer keywords: "TEXINFO_CLASS",
;; "TEXINFO_FILENAME", "TEXINFO_HEADER", "TEXINFO_DIR_CATEGORY",
;; "TEXINFO_DIR_TITLE", "TEXINFO_DIR_DESC" "SUBTITLE" and "SUBAUTHOR".
;;
;; To include inline code snippets (for example for generating @kbd{}
;; and @key{} commands), the following export-snippet keys are
;; accepted:
;;
;;     info
;;     e-info
;;     e-texinfo
;;
;; You can add them for export snippets via any of the below:
;;
;;    (add-to-list 'org-export-snippet-translation-alist
;;                 '("e-info" . "e-texinfo"))
;;    (add-to-list 'org-export-snippet-translation-alist
;;                 '("e-texinfo" . "e-texinfo"))
;;    (add-to-list 'org-export-snippet-translation-alist
;;                 '("info" . "e-texinfo"))
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'org-export)

(defvar orgtbl-exp-regexp)


;;; Define Back-End

(defvar org-e-texinfo-translate-alist
  '((babel-call . org-e-texinfo-babel-call)
    (bold . org-e-texinfo-bold)
    (center-block . org-e-texinfo-center-block)
    (clock . org-e-texinfo-clock)
    (code . org-e-texinfo-code)
    (comment . org-e-texinfo-comment)
    (comment-block . org-e-texinfo-comment-block)
    (drawer . org-e-texinfo-drawer)
    (dynamic-block . org-e-texinfo-dynamic-block)
    (entity . org-e-texinfo-entity)
    (example-block . org-e-texinfo-example-block)
    (export-block . org-e-texinfo-export-block)
    (export-snippet . org-e-texinfo-export-snippet)
    (fixed-width . org-e-texinfo-fixed-width)
    (footnote-definition . org-e-texinfo-footnote-definition)
    (footnote-reference . org-e-texinfo-footnote-reference)
    (headline . org-e-texinfo-headline)
    (horizontal-rule . org-e-texinfo-horizontal-rule)
    (inline-babel-call . org-e-texinfo-inline-babel-call)
    (inline-src-block . org-e-texinfo-inline-src-block)
    (inlinetask . org-e-texinfo-inlinetask)
    (italic . org-e-texinfo-italic)
    (item . org-e-texinfo-item)
    (keyword . org-e-texinfo-keyword)
    (latex-environment . org-e-texinfo-latex-environment)
    (latex-fragment . org-e-texinfo-latex-fragment)
    (line-break . org-e-texinfo-line-break)
    (link . org-e-texinfo-link)
    (macro . org-e-texinfo-macro)
    (paragraph . org-e-texinfo-paragraph)
    (plain-list . org-e-texinfo-plain-list)
    (plain-text . org-e-texinfo-plain-text)
    (planning . org-e-texinfo-planning)
    (property-drawer . org-e-texinfo-property-drawer)
    (quote-block . org-e-texinfo-quote-block)
    (quote-section . org-e-texinfo-quote-section)
    (radio-target . org-e-texinfo-radio-target)
    (section . org-e-texinfo-section)
    (special-block . org-e-texinfo-special-block)
    (src-block . org-e-texinfo-src-block)
    (statistics-cookie . org-e-texinfo-statistics-cookie)
    (strike-through . org-e-texinfo-strike-through)
    (subscript . org-e-texinfo-subscript)
    (superscript . org-e-texinfo-superscript)
    (table . org-e-texinfo-table)
    (table-cell . org-e-texinfo-table-cell)
    (table-row . org-e-texinfo-table-row)
    (target . org-e-texinfo-target)
    (template . org-e-texinfo-template)
    (timestamp . org-e-texinfo-timestamp)
    (underline . org-e-texinfo-underline)
    (verbatim . org-e-texinfo-verbatim)
    (verse-block . org-e-texinfo-verse-block))
  "Alist between element or object types and translators.")

(defconst org-e-texinfo-options-alist
  '((:texinfo-filename "TEXINFO_FILENAME" nil org-e-texinfo-filename t)
    (:texinfo-class "TEXINFO_CLASS" nil org-e-texinfo-default-class t)
    (:texinfo-header "TEXINFO_HEADER" nil nil newline)
    (:subtitle "SUBTITLE" nil nil newline)
    (:subauthor "SUBAUTHOR" nil nil newline)
    (:texinfo-dircat "TEXINFO_DIR_CATEGORY" nil nil t)
    (:texinfo-dirtitle "TEXINFO_DIR_TITLE" nil nil t)
    (:texinfo-dirdesc "TEXINFO_DIR_DESC" nil nil t))
  "Alist between Texinfo export properties and ways to set them.
See `org-export-options-alist' for more information on the
structure of the values.

SUBAUTHOR and SUBTITLE are for the inclusion of additional author
and title information beyond the initial variable.")

(defconst org-e-texinfo-filters-alist
  '((:filter-headline . org-e-texinfo-filter-section-blank-lines)
    (:filter-section . org-e-texinfo-filter-section-blank-lines))
  "Alist between filters keywords and back-end specific filters.
  See `org-export-filters-alist' for more information")


;;; Internal Variables

;; Add TEXINFO to the list of available of available export blocks.
(add-to-list 'org-element-block-name-alist
	     '("TEXINFO" . org-element-export-block-parser))

;;; User Configurable Variables

(defgroup org-export-e-texinfo nil
  "Options for exporting Org mode files to Texinfo."
  :tag "Org Export Texinfo"
  :group 'org-export)

;;; Preamble

(defcustom org-e-texinfo-filename nil
  "Default filename for texinfo output."
  :group 'org-export-e-texinfo
  :type '(string :tag "Export Filename"))

(defcustom org-e-texinfo-default-class "info"
  "The default Texinfo class."
  :group 'org-export-e-texinfo
  :type '(string :tag "Texinfo class"))

(defcustom org-e-texinfo-classes
  '(("info"
     "\\input texinfo    @c -*- texinfo -*-"
     ("@chapter %s" . "@unnumbered %s")
     ("@section %s" . "@unnumberedsec %s")
     ("@subsection %s" . "@unnumberedsubsec %s")
     ("@subsubsection %s" . "@unnumberedsubsubsec %s")))
  "Alist of Texinfo classes and associated header and structure.
If #+Texinfo_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    \(numbered-section . unnumbered-section\)
    ...\)

The sectioning structure
------------------------

The sectioning structure of the class is given by the elements
following the header string.  For each sectioning level, a number
of strings is specified.  A %s formatter is mandatory in each
section string and will be replaced by the title of the section.

Instead of a list of sectioning commands, you can also specify
a function name.  That function will be called with two
parameters, the \(reduced) level of the headline, and a predicate
non-nil when the headline should be numbered.  It must return
a format string in which the section title will be added."
  :group 'org-export-e-texinfo
  :type '(repeat
	  (list (string :tag "Texinfo class")
		(string :tag "Texinfo header")
		(repeat :tag "Levels" :inline t
			(choice
			 (cons :tag "Heading"
			       (string :tag "  numbered")
			       (string :tag "unnumbered"))
			 (function :tag "Hook computing sectioning"))))))

;;; Headline

(defcustom org-e-texinfo-format-headline-function nil
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags as a list of strings (list of strings or nil).

The function result will be used in the section format string.

As an example, one could set the variable to the following, in
order to reproduce the default set-up:

\(defun org-e-texinfo-format-headline (todo todo-type priority text tags)
  \"Default format function for an headline.\"
  \(concat (when todo
            \(format \"\\\\textbf{\\\\textsc{\\\\textsf{%s}}} \" todo))
	  \(when priority
            \(format \"\\\\framebox{\\\\#%c} \" priority))
	  text
	  \(when tags
            \(format \"\\\\hfill{}\\\\textsc{%s}\"
              \(mapconcat 'identity tags \":\"))))"
  :group 'org-export-e-texinfo
  :type 'function)


;;; Footnotes
;;
;; Footnotes are inserted directly

;;; Timestamps

(defcustom org-e-texinfo-active-timestamp-format "@emph{%s}"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-e-texinfo
  :type 'string)

(defcustom org-e-texinfo-inactive-timestamp-format "@emph{%s}"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-e-texinfo
  :type 'string)

(defcustom org-e-texinfo-diary-timestamp-format "@emph{%s}"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-e-texinfo
  :type 'string)

;;; Links

(defcustom org-e-texinfo-link-with-unknown-path-format "@indicateurl{%s}"
  "Format string for links with unknown path type."
  :group 'org-export-e-texinfo
  :type 'string)

;;; Tables

(defcustom org-e-texinfo-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-e-texinfo
  :type 'boolean)

(defcustom org-e-texinfo-table-scientific-notation "%s\\,(%s)"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-e-texinfo
  :type '(choice
	  (string :tag "Format string")
	  (const :tag "No formatting")))

(defcustom org-e-texinfo-def-table-markup "@samp"
  "Default setting for @table environments.")

;;; Text markup

(defcustom org-e-texinfo-text-markup-alist '((bold . "@strong{%s}")
					     (code . code)
					     (italic . "@emph{%s}")
					     (verbatim . verb)
					     (comment . "@c %s"))
  "Alist of Texinfo expressions to convert text markup.

The key must be a symbol among `bold', `italic' and `comment'.
The value is a formatting string to wrap fontified text with.

Value can also be set to the following symbols: `verb' and
`code'.  For the former, Org will use \"@verb\" to
create a format string and select a delimiter character that
isn't in the string.  For the latter, Org will use \"@code\"
to typeset and try to protect special characters.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-e-texinfo
  :type 'alist
  :options '(bold code italic verbatim comment))

;;; Drawers

(defcustom org-e-texinfo-format-drawer-function nil
  "Function called to format a drawer in Texinfo code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-texinfo-format-drawer-default \(name contents\)
  \"Format a drawer element for Texinfo export.\"
  contents\)"
  :group 'org-export-e-texinfo
  :type 'function)

;;; Inlinetasks

(defcustom org-e-texinfo-format-inlinetask-function nil
  "Function called to format an inlinetask in Texinfo code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-texinfo-format-inlinetask \(todo type priority name tags contents\)
\"Format an inline task element for Texinfo export.\"
  \(let ((full-title
	 \(concat
	  \(when todo
            \(format \"@strong{%s} \" todo))
	  \(when priority (format \"#%c \" priority))
	  title
	  \(when tags
            \(format \":%s:\"
                    \(mapconcat 'identity tags \":\")))))
    \(format (concat \"@center %s\n\n\"
		    \"%s\"
                    \"\n\"))
	    full-title contents))"
  :group 'org-export-e-texinfo
  :type 'function)

;;; Src blocks
;;
;; Src Blocks are example blocks, except for LISP

;;; Plain text

(defcustom org-e-texinfo-quotes
  '(("quotes"
     ("\\(\\s-\\|[[(]\\|^\\)\"" . "``")
     ("\\(\\S-\\)\"" . "''")
     ("\\(\\s-\\|(\\|^\\)'" . "`")))
  "Alist for quotes to use when converting english double-quotes.

The CAR of each item in this alist is the language code.
The CDR of each item in this alist is a list of three CONS:
- the first CONS defines the opening quote;
- the second CONS defines the closing quote;
- the last CONS defines single quotes.

For each item in a CONS, the first string is a regexp
for allowed characters before/after the quote, the second
string defines the replacement string for this quote."
  :group 'org-export-e-texinfo
  :type '(list
	  (cons :tag "Opening quote"
		(string :tag "Regexp for char before")
		(string :tag "Replacement quote     "))
	  (cons :tag "Closing quote"
		(string :tag "Regexp for char after ")
		(string :tag "Replacement quote     "))
	  (cons :tag "Single quote"
		(string :tag "Regexp for char before")
		(string :tag "Replacement quote     "))))

;;; Compilation

(defcustom org-e-texinfo-info-process
  '("makeinfo %f")
  "Commands to process a texinfo file to an INFO file.
This is list of strings, each of them will be given to the shell
as a command.  %f in the command will be replaced by the full
file name, %b by the file base name \(i.e without extension) and
%o by the base directory of the file."
  :group 'org-export-texinfo
  :type '(repeat :tag "Shell command sequence"
		 (string :tag "Shell command")))


;;; Internal Functions

(defun org-e-texinfo-filter-section-blank-lines (headline back-end info)
  "Filter controlling number of blank lines after a section."
  (let ((blanks (make-string 2 ?\n)))
    (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" blanks headline)))

(defun org-e-texinfo--find-copying (info)
  "Retrieve the headline identified by the property :copying:.

INFO is the plist containing the export options and tree.  It is
used to find and extract the single desired headline.  This
cannot be treated as a standard headline since it must be
inserted in a specific location."
  (let (copying)
    (org-element-map (plist-get info :parse-tree) 'headline
		     (lambda (copy)
		       (when (org-element-property :copying copy)
			 (push copy copying))) info 't)
    ;; Retrieve the single entry
    (car copying)))

(defun org-e-texinfo--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

(defun org-e-texinfo--make-option-string (options)
  "Return a comma separated string of keywords and values.
OPTIONS is an alist where the key is the options keyword as
a string, and the value a list containing the keyword value, or
nil."
  (mapconcat (lambda (pair)
	       (concat (first pair)
		       (when (> (length (second pair)) 0)
			 (concat "=" (second pair)))))
	     options
	     ","))

(defun org-e-texinfo--quotation-marks (text info)
  "Export quotation marks using ` and ' as the markers.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
	  (let ((start 0))
	    (while (setq start (string-match (car l) text start))
	      (let ((new-quote (concat (match-string 1 text) (cdr l))))
		(setq text (replace-match new-quote  t t text))))))
	(cdr org-e-texinfo-quotes))
  text)

(defun org-e-texinfo--text-markup (text markup)
  "Format TEXT depending on MARKUP text markup.
See `org-e-texinfo-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-e-texinfo-text-markup-alist))))
    (cond
     ;; No format string: Return raw text.
     ((not fmt) text)
     ((eq 'verb fmt)
      (let ((separator (org-e-texinfo--find-verb-separator text)))
	(concat "@verb{" separator text separator "}")))
     ((eq 'code fmt)
      (let ((start 0)
	    (rtn "")
	    char)
	(while (string-match "[@{}]" text)
	  (setq char (match-string 0 text))
	  (if (> (match-beginning 0) 0)
	      (setq rtn (concat rtn (substring text 0 (match-beginning 0)))))
	  (setq text (substring text (1+ (match-beginning 0))))
	  (setq char (concat "@" char)
		rtn (concat rtn char)))
	(setq text (concat rtn text)
	      fmt "@code{%s}")
	(format fmt text)))
     ;; Else use format string.
     (t (format fmt text)))))

;;; Headline sanitizing

(defun org-e-texinfo--sanitize-headline (headline info)
  "Remove all formatting from the text of a headline for use in
  node and menu listing."
  (mapconcat 'identity
	     (org-e-texinfo--sanitize-headline-contents headline info) " "))

(defun org-e-texinfo--sanitize-headline-contents (headline info)
  "Retrieve the content of the headline.

Any content that can contain further formatting is checked
recursively, to ensure that nested content is also properly
retrieved."
  (loop for contents in headline append
	(cond
	 ;; already a string
	 ((stringp contents)
	  (list (replace-regexp-in-string " $" "" contents)))
	 ;; Is exported as-is (value)
	 ((org-element-map contents '(verbatim code)
			   (lambda (value)
			     (org-element-property :value value))))
	 ;; Has content and recurse into the content
	 ((org-element-contents contents)
	  (org-e-texinfo--sanitize-headline-contents
	   (org-element-contents contents) info)))))

;;; Menu sanitizing

(defun org-e-texinfo--sanitize-menu (title)
  "Remove invalid characters from TITLE for use in menus and
nodes.

Based on TEXINFO specifications, the following must be removed:
@ { } ( ) : . ,"
  (replace-regexp-in-string "[@{}():,.]" "" title))

;;; Content sanitizing

(defun org-e-texinfo--sanitize-content (text)
  "Ensure characters are properly escaped when used in headlines or blocks.

Escape characters are: @ { }"
  (replace-regexp-in-string "\\\([@{}]\\\)" "@\\1" text))

;;; Menu creation

(defun org-e-texinfo--build-menu (tree level info &optional detailed)
  "Create the @menu/@end menu information from TREE at headline
level LEVEL.

TREE contains the parse-tree to work with, either of the entire
document or of a specific parent headline.  LEVEL indicates what
level of headlines to look at when generating the menu.  INFO is
a plist containing contextual information.

Detailed determines whether to build a single level of menu, or
recurse into all children as well."
  (let ((menu (org-e-texinfo--generate-menu-list tree level info))
	output text-menu)
    (cond
     (detailed
      ;; Looping is done within the menu generation.
      (setq text-menu (org-e-texinfo--generate-detailed menu level info)))
     (t
      (setq text-menu (org-e-texinfo--generate-menu-items menu info))))
    (when text-menu
      (setq output (org-e-texinfo--format-menu text-menu))
      (mapconcat 'identity output "\n"))))

(defun org-e-texinfo--generate-detailed (menu level info)
  "Generate a detailed listing of all subheadings within MENU starting at LEVEL.

MENU is the parse-tree to work with.  LEVEL is the starting level
for the menu headlines and from which recursion occurs.  INFO is
a plist containing contextual information."
  (when level
    (let ((max-depth (plist-get info :headline-levels)))
      (when (> max-depth level)
	(loop for headline in menu append
	      (let* ((title (org-e-texinfo--menu-headlines headline info))
		     ;; Create list of menu entries for the next level
		     (sublist (org-e-texinfo--generate-menu-list
			       headline (1+ level) info))
		     ;; Generate the menu items for that level.  If
		     ;; there are none omit that heading completely,
		     ;; otherwise join the title to it's related entries.
		     (submenu (if (org-e-texinfo--generate-menu-items sublist info)
				  (append (list title)
					  (org-e-texinfo--generate-menu-items sublist info))
				'nil))
		     ;; Start the process over the next level down.
		     (recursion (org-e-texinfo--generate-detailed sublist (1+ level) info)))
		(setq recursion (append submenu recursion))
		recursion))))))

(defun org-e-texinfo--generate-menu-list (tree level info)
  "Generate the list of headlines that are within a given level
of the tree for further formatting.

TREE is the parse-tree containing the headlines.  LEVEL is the
headline level to generate a list of.  INFO is a plist holding
contextual information."
  (let (seq)
    (org-element-map
     tree 'headline
     (lambda (head)
       (when (org-element-property :level head)
	 (if (and (eq level (org-element-property :level head))
		  ;; Do not take note of footnotes or copying headlines
		  (not (org-element-property :copying head))
		  (not (org-element-property :footnote-section-p head)))
	     (push head seq)))))
    ;; Return the list of headlines (reverse to have in actual order)
    (reverse seq)))

(defun org-e-texinfo--generate-menu-items (items info)
  "Generate a list of headline information from the listing ITEMS.

ITEMS is a list of the headlines to be converted into entries.
INFO is a plist containing contextual information.

Returns a list containing the following information from each
headline: length, title, description.  This is used to format the
menu using `org-e-texinfo--format-menu'."
  (loop for headline in items collect
	(let* ((title (org-e-texinfo--sanitize-menu
		       (org-e-texinfo--sanitize-headline
			(org-element-property :title headline) info)))
	       (descr (org-export-data
		       (org-element-property :description headline) info))
	       (len (length title))
	       (output (list len title descr)))
	  output)))

(defun org-e-texinfo--menu-headlines (headline info)
  "Retrieve the title from HEADLINE.

INFO is a plist holding contextual information.

Return the headline as a list of (length title description) with
length of -1 and nil description.  This is used in
`org-e-texinfo--format-menu' to identify headlines as opposed to
entries."
  (let ((title (org-export-data
		(org-element-property :title headline) info)))
    (list -1 title 'nil)))

(defun org-e-texinfo--format-menu (text-menu)
  "Format the TEXT-MENU items to be properly printed in the menu.

Each entry in the menu should be provided as (length title
description).

Headlines in the detailed menu are given length -1 to ensure they
are never confused with other entries.  They also have no
description.

Other menu items are output as:
    Title::     description

With the spacing between :: and description based on the length
of the longest menu entry."

  (let* ((lengths (mapcar 'car text-menu))
         (max-length (apply 'max lengths))
	 output)
    (setq output
          (mapcar (lambda (name)
                    (let* ((title (nth 1 name))
                           (desc (nth 2 name))
                           (length (nth 0 name)))
                      (if (> length -1)
                          (concat "* " title ":: "
                                  (make-string
				   (- (+ 3 max-length) length)
				   ?\s)
                                  (if desc
                                      (concat desc)))
                        (concat "\n" title "\n"))))
		  text-menu))
    output))

;;; Template

(defun org-e-texinfo-template (contents info)
  "Return complete document string after Texinfo conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (org-export-data (plist-get info :title) info))
	 (info-filename (or (plist-get info :texinfo-filename)
			    (file-name-nondirectory
			     (org-export-output-file-name ".info"))))
	 (author (org-export-data (plist-get info :author) info))
	 (texinfo-header (plist-get info :texinfo-header))
	 (subtitle (plist-get info :subtitle))
	 (subauthor (plist-get info :subauthor))
	 (class (plist-get info :texinfo-class))
	 (header (nth 1 (assoc class org-e-texinfo-classes)))
	 (copying (org-e-texinfo--find-copying info))
	 (dircat (plist-get info :texinfo-dircat))
	 (dirtitle (plist-get info :texinfo-dirtitle))
	 (dirdesc (plist-get info :texinfo-dirdesc))
	 ;; Spacing to align description (column 32 - 3 for `* ' and
	 ;; `.' in text.
	 (dirspacing (- 29 (length dirtitle)))
	 (menu (org-e-texinfo-make-menu info 'main))
	 (detail-menu (org-e-texinfo-make-menu info 'detailed)))
    (concat
     ;; Header
     header "\n"
     "@c %**start of header\n"
     ;; Filename and Title
     "@setfilename " info-filename "\n"
     "@settitle " title "\n"
     "\n\n"
     "@c Version and Contact Info\n"
     "@set AUTHOR " author "\n"

     ;; Additional Header Options set by `#+TEXINFO_HEADER
     (if texinfo-header
	 (concat "\n"
		 texinfo-header
		 "\n"))

     "@c %**end of header\n"
     "@finalout\n"
     "\n\n"

     ;; Copying
     "@copying\n"
     ;; Only export the content of the headline, do not need the
     ;; initial headline.
     (org-export-data (nth 2 copying) info)
     "@end copying\n"
     "\n\n"

     ;; Info directory information
     ;; Only supply if both title and category are provided
     (if (and dircat dirtitle)
	 (concat "@dircategory " dircat "\n"
		 "@direntry\n"
		 "* " dirtitle "."
		 (make-string dirspacing ?\s)
		 dirdesc "\n"
		 "@end direntry\n"))
     "\n\n"

     ;; Title
     "@titlepage\n"
     "@title " title "\n\n"
     (if subtitle
	 (concat "@subtitle " subtitle "\n"))
     "@author " author "\n"
     (if subauthor
	 (concat subauthor "\n"))
     "\n"
     "@c The following two commands start the copyright page.\n"
     "@page\n"
     "@vskip 0pt plus 1filll\n"
     "@insertcopying\n"
     "@end titlepage\n\n"
     "@c Output the table of contents at the beginning.\n"
     "@contents\n\n"

     ;; Configure Top Node when not for Tex
     "@ifnottex\n"
     "@node Top\n"
     "@top " title " Manual\n"
     "@insertcopying\n"
     "@end ifnottex\n\n"

     ;; Do not output menus if they are empty
     (if menu
	 ;; Menu
	 (concat "@menu\n"
		 menu
		 "\n\n"
		 ;; Detailed Menu
		 (if detail-menu
		     (concat "@detailmenu\n"
			     " --- The Detailed Node Listing ---\n"
			     detail-menu
			     "\n\n"
			     "@end detailmenu\n"))
		 "@end menu\n"))
     "\n\n"

     ;; Document's body.
     contents
     "\n"
     ;; Creator.
     (let ((creator-info (plist-get info :with-creator)))
       (cond
	((not creator-info) "")
	((eq creator-info 'comment)
	 (format "@c %s\n" (plist-get info :creator)))
	(t (concat (plist-get info :creator) "\n"))))
     ;; Document end.
     "\n@bye")))



;;; Transcode Functions

;;; Babel Call
;;
;; Babel Calls are ignored.

;;; Bold

(defun org-e-texinfo-bold (bold contents info)
  "Transcode BOLD from Org to Texinfo.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-e-texinfo--text-markup contents 'bold))

;;; Center Block
;;
;; Center blocks are ignored

;;; Clock

(defun org-e-texinfo-clock (clock contents info)
  "Transcode a CLOCK element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   "@noindent"
   (format "@strong{%s} " org-clock-string)
   (format org-e-texinfo-inactive-timestamp-format
	   (concat (org-translate-time (org-element-property :value clock))
		   (let ((time (org-element-property :time clock)))
		     (and time (format " (%s)" time)))))
   "@*"))

;;; Code

(defun org-e-texinfo-code (code contents info)
  "Transcode a CODE object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-texinfo--text-markup (org-element-property :value code) 'code))

;;; Comment

(defun org-e-texinfo-comment (comment contents info)
  "Transcode a COMMENT object from Org to Texinfo.
CONTENTS is the text in the comment.  INFO is a plist holding
contextual information."
  (org-e-texinfo--text-markup (org-element-property :value comment) 'comment))

;;; Comment Block

(defun org-e-texinfo-comment-block (comment-block contents info)
  "Transcode a COMMENT-BLOCK object from Org to Texinfo.
CONTENTS is the text within the block.  INFO is a plist holding
contextual information."
  (format "@ignore\n%s@end ignore" (org-element-property :value comment-block)))

;;; Drawer

(defun org-e-texinfo-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (if (functionp org-e-texinfo-format-drawer-function)
		     (funcall org-e-texinfo-format-drawer-function
			      name contents)
		   ;; If there's no user defined function: simply
		   ;; display contents of the drawer.
		   contents)))
    output))

;;; Dynamic Block

(defun org-e-texinfo-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  contents)

;;; Entity

(defun org-e-texinfo-entity (entity contents info)
  "Transcode an ENTITY object from Org to Texinfo.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :latex entity)))
    (if (org-element-property :latex-math-p entity) (format "@math{%s}" ent) ent)))

;;; Example Block

(defun org-e-texinfo-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "@verbatim\n%s@end verbatim"
	  (org-export-format-code-default example-block info)))

;;; Export Block

(defun org-e-texinfo-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "TEXINFO")
    (org-remove-indentation (org-element-property :value export-block))))

;;; Export Snippet

(defun org-e-texinfo-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-texinfo)
    (org-element-property :value export-snippet)))

;;; Fixed Width

(defun org-e-texinfo-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "@example\n%s\n@end example"
	  (org-remove-indentation
	   (org-e-texinfo--sanitize-content
	    (org-element-property :value fixed-width)))))

;;; Footnote Definition
;;
;; Footnote Definitions are ignored.

;;; Footnote Reference
;;

(defun org-e-texinfo-footnote-reference (footnote contents info)
  "Create a footnote reference for FOOTNOTE.

FOOTNOTE is the footnote to define.  CONTENTS is nil.  INFO is a
plist holding contextual information."
  (let ((def (org-export-get-footnote-definition footnote info)))
    (format "@footnote{%s}"
	    (org-trim (org-export-data def info)))))

;;; Headline

(defun org-e-texinfo-headline (headline contents info)
  "Transcode an HEADLINE element from Org to Texinfo.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :texinfo-class))
	 (level (org-export-get-relative-level headline info))
	 (numberedp (org-export-numbered-headline-p headline info))
	 (class-sectionning (assoc class org-e-texinfo-classes))
	 ;; Find the index type, if any
	 (index (org-element-property :index headline))
	 ;; Retrieve headline text
	 (text (org-e-texinfo--sanitize-headline
		(org-element-property :title headline) info))
	 ;; Create node info, to insert it before section formatting.
	 (node (format "@node %s\n"
		       (org-e-texinfo--sanitize-menu
			(replace-regexp-in-string "%" "%%" text))))
	 ;; Menus must be generated with first child, otherwise they
	 ;; will not nest properly
	 (menu (let* ((first (org-export-first-sibling-p headline info))
		      (parent (org-export-get-parent-headline headline))
		      (title (org-e-texinfo--sanitize-headline
			      (org-element-property :title parent) info))
		      heading listing
		      (tree (plist-get info :parse-tree)))
		 (if first
		     (org-element-map
		      (plist-get info :parse-tree) 'headline
		      (lambda (ref)
			(if (member title (org-element-property :title ref))
			    (push ref heading)))
		      info 't))
		 (setq listing (org-e-texinfo--build-menu
				(car heading) level info))
	 	 (if listing
	 	     (setq listing (replace-regexp-in-string
				    "%" "%%" listing)
			   listing (format
				    "\n@menu\n%s\n@end menu\n\n" listing))
	 	   'nil)))
	 ;; Section formatting will set two placeholders: one for the
	 ;; title and the other for the contents.
	 (section-fmt
	  (let ((sec (if (and (symbolp (nth 2 class-sectionning))
			      (fboundp (nth 2 class-sectionning)))
			 (funcall (nth 2 class-sectionning) level numberedp)
		       (nth (1+ level) class-sectionning))))
	    (cond
	     ;; No section available for that LEVEL.
	     ((not sec) nil)
	     ;; Section format directly returned by a function.
	     ((stringp sec) sec)
	     ;; (numbered-section . unnumbered-section)
	     ((not (consp (cdr sec)))
	      ;; If an index, always unnumbered
	      (if index
		  (concat menu node (cdr sec) "\n%s")
		;; Otherwise number as needed.
		(concat menu node
			(funcall
			 (if numberedp #'car #'cdr) sec) "\n%s"))))))
	 (todo
	  (and (plist-get info :with-todo-keywords)
	       (let ((todo (org-element-property :todo-keyword headline)))
		 (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 ;; Create the headline text along with a no-tag version.  The
	 ;; latter is required to remove tags from table of contents.
	 (full-text (org-e-texinfo--sanitize-content
		     (if (functionp org-e-texinfo-format-headline-function)
			 ;; User-defined formatting function.
			 (funcall org-e-texinfo-format-headline-function
				  todo todo-type priority text tags)
		       ;; Default formatting.
		       (concat
			(when todo
			  (format "@strong{%s} " todo))
			(when priority (format "@emph{#%s} " priority))
			text
			(when tags
			  (format ":%s:"
				  (mapconcat 'identity tags ":")))))))
	 (full-text-no-tag
	  (org-e-texinfo--sanitize-content
	   (if (functionp org-e-texinfo-format-headline-function)
	       ;; User-defined formatting function.
	       (funcall org-e-texinfo-format-headline-function
			todo todo-type priority text nil)
	     ;; Default formatting.
	     (concat
	      (when todo (format "@strong{%s} " todo))
	      (when priority (format "@emph{#%c} " priority))
	      text))))
	 (pre-blanks
	  (make-string (org-element-property :pre-blank headline) 10)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2: This is the `copying' section: ignore it
     ;;         This is used elsewhere.
     ((org-element-property :copying headline) nil)
     ;; Case 3: An index.  If it matches one of the known indexes,
     ;;         print it as such following the contents, otherwise
     ;;         print the contents and leave the index up to the user.
     (index
      (format
       section-fmt full-text
       (concat pre-blanks contents "\n"
	       (if (member index '("cp" "fn" "ky" "pg" "tp" "vr"))
		   (concat "@printindex " index)))))
     ;; Case 4: This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt) (org-export-low-level-p headline info))
      ;; Build the real contents of the sub-tree.
      (let ((low-level-body
	     (concat
	      ;; If the headline is the first sibling, start a list.
	      (when (org-export-first-sibling-p headline info)
		(format "@%s\n" (if numberedp 'enumerate 'itemize)))
	      ;; Itemize headline
	      "@item\n" full-text "\n" pre-blanks contents)))
	;; If headline is not the last sibling simply return
	;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
	;; blank line.
	(if (not (org-export-last-sibling-p headline info)) low-level-body
	  (replace-regexp-in-string
	   "[ \t\n]*\\'"
	   (format "\n@end %s" (if numberedp 'enumerate 'itemize))
	   low-level-body))))
     ;; Case 5: Standard headline.  Export it as a section.
     (t
      (cond
       ((not (and tags (eq (plist-get info :with-tags) 'not-in-toc)))
	;; Regular section.  Use specified format string.
	(format (replace-regexp-in-string "%]" "%%]" section-fmt) full-text
		(concat pre-blanks contents)))
       ((string-match "\\`@\\(.*?\\){" section-fmt)
	;; If tags should be removed from table of contents, insert
	;; title without tags as an alternative heading in sectioning
	;; command.
	(format (replace-match (concat (match-string 1 section-fmt) "[%s]")
			       nil nil section-fmt 1)
		;; Replace square brackets with parenthesis since
		;; square brackets are not supported in optional
		;; arguments.
		(replace-regexp-in-string
		 "\\[" "("
		 (replace-regexp-in-string
		  "\\]" ")"
		  full-text-no-tag))
		full-text
		(concat pre-blanks contents)))
       (t
	;; Impossible to add an alternative heading.  Fallback to
	;; regular sectioning format string.
	(format (replace-regexp-in-string "%]" "%%]" section-fmt) full-text
		(concat pre-blanks contents))))))))

;;; Horizontal Rule
;;
;; Horizontal rules are ignored

;;; Inline Babel Call
;;
;; Inline Babel Calls are ignored.

;;; Inline Src Block

(defun org-e-texinfo-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block))
	 (separator (org-e-texinfo--find-verb-separator code)))
    (concat "@verb{" separator code separator "}")))

;;; Inlinetask

(defun org-e-texinfo-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-data (org-element-property :title inlinetask) info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-property :todo-keyword inlinetask)))
		     (and todo (org-export-data todo info)))))
	(todo-type (org-element-property :todo-type inlinetask))
	(tags (and (plist-get info :with-tags)
		   (org-export-get-tags inlinetask info)))
	(priority (and (plist-get info :with-priority)
		       (org-element-property :priority inlinetask))))
    ;; If `org-e-texinfo-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-e-texinfo-format-inlinetask-function)
	(funcall org-e-texinfo-format-inlinetask-function
		 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (let ((full-title
	     (concat
	      (when todo (format "@strong{%s} " todo))
	      (when priority (format "#%c " priority))
	      title
	      (when tags (format ":%s:"
				 (mapconcat 'identity tags ":"))))))
	(format (concat "@center %s\n\n"
			"%s"
			"\n")
		full-title contents)))))

;;; Italic

(defun org-e-texinfo-italic (italic contents info)
  "Transcode ITALIC from Org to Texinfo.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-e-texinfo--text-markup contents 'italic))

;;; Item

(defun org-e-texinfo-item (item contents info)
  "Transcode an ITEM element from Org to Texinfo.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((tag (org-element-property :tag item))
	 (desc (org-export-data tag info)))
    (concat "\n@item " (if tag desc) "\n"
	    (org-trim contents) "\n")))

;;; Keyword

(defun org-e-texinfo-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "TEXINFO") value)
     ((string= key "CINDEX") (format "@cindex %s" value))
     ((string= key "FINDEX") (format "@findex %s" value))
     ((string= key "KINDEX") (format "@kindex %s" value))
     ((string= key "PINDEX") (format "@pindex %s" value))
     ((string= key "TINDEX") (format "@tindex %s" value))
     ((string= key "VINDEX") (format "@vindex %s" value)))))

;;; Latex Environment
;;
;; Latex environments are ignored

;;; Latex Fragment
;;
;; Latex fragments are ignored.

;;; Line Break

(defun org-e-texinfo-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "@*")

;;; Link

(defun org-e-texinfo-link (link desc info)
  "Transcode a LINK object from Org to Texinfo.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (path (cond
		((member type '("http" "https" "ftp"))
		 (concat type ":" raw-path))
		((string= type "file")
		 (when (string-match "\\(.+\\)::.+" raw-path)
		   (setq raw-path (match-string 1 raw-path)))
		 (if (file-name-absolute-p raw-path)
		     (concat "file://" (expand-file-name raw-path))
		   (concat "file://" raw-path)))
		(t raw-path)))
	 (email (if (string= type "mailto")
		    (let ((text (replace-regexp-in-string
				 "@" "@@" raw-path)))
		      (concat text (if desc (concat "," desc))))))
	 protocol)
    (cond
     ;; Links pointing to an headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "id"))
      (let ((destination (org-export-resolve-id-link link info)))
	(case (org-element-type destination)
	  ;; Id link points to an external file.
	  (plain-text
	   (if desc (format "@uref{file://%s,%s}" destination desc)
	     (format "@uref{file://%s}" destination)))
	  ;; LINK points to an headline.  Use the headline as the NODE target
	  (headline
	   (format "@ref{%s}"
		   (org-export-data
		    (org-element-property :title destination) info)))
	  (otherwise
	   (let ((path (org-export-solidify-link-text path)))
	     (if (not desc) (format "@ref{%s}" path)
	       (format "@ref{%s,,%s}" path desc)))))))
     ((member type '("fuzzy"))
      (let ((destination (org-export-resolve-fuzzy-link link info)))
	(case (org-element-type destination)
	  ;; Id link points to an external file.
	  (plain-text
	   (if desc (format "@uref{file://%s,%s}" destination desc)
	     (format "@uref{file://%s}" destination)))
	  ;; LINK points to an headline.  Use the headline as the NODE target
	  (headline
	   (format "@ref{%s}"
		   (org-export-data
		    (org-element-property :title destination) info)))
	  (otherwise
	   (let ((path (org-export-solidify-link-text path)))
	     (if (not desc) (format "@ref{%s}" path)
	       (format "@ref{%s,,%s}" path desc)))))))
     ;; Special case for email addresses
     (email
      (format "@email{%s}" email))
     ;; External link with a description part.
     ((and path desc) (format "@uref{%s,%s}" path desc))
     ;; External link without a description part.
     (path (format "@uref{%s}" path))
     ;; No path, only description.  Try to do something useful.
     (t (format org-e-texinfo-link-with-unknown-path-format desc)))))

;;; Macro

(defun org-e-texinfo-macro (macro contents info)
  "Transcode a MACRO element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))

;;; Menu

(defun org-e-texinfo-make-menu (info level)
  "Create the menu for inclusion in the texifo document.

INFO is the parsed buffer that contains the headlines.  LEVEL
determines whether to make the main menu, or the detailed menu.

This is only used for generating the primary menu.  In-Node menus
are generated directly."
  (let* ((parse (plist-get info :parse-tree))
	 ;; Top determines level to build menu from, it finds the
	 ;; level of the first headline in the export.
	 (top (org-element-map
	       parse 'headline
	       (lambda (headline)
		 (org-element-property :level headline)) info 't)))
    (cond
     ;; Generate the main menu
     ((eq level 'main)
      (org-e-texinfo--build-menu parse top info))
     ;; Generate the detailed (recursive) menu
     ((eq level 'detailed)
      ;; Requires recursion
      ;;(org-e-texinfo--build-detailed-menu parse top info)
      (org-e-texinfo--build-menu parse top info 'detailed))
     ;; Otherwise do nothing
     (t))))

;;; Paragraph

(defun org-e-texinfo-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Texinfo.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  contents)

;;; Plain List

(defun org-e-texinfo-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Texinfo.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((attr (org-export-read-attribute :attr_texinfo plain-list))
	 (indic (or (plist-get attr :indic)
		    org-e-texinfo-def-table-markup))
	 (type (org-element-property :type plain-list))
	 (table-type (or (plist-get attr :table-type)
			 "table"))
	 ;; Ensure valid texinfo table type.
	 (table-type (if (memq table-type '("table" "ftable" "vtable"))
			 table-type
		       "table"))
	 (list-type (cond
		     ((eq type 'ordered) "enumerate")
		     ((eq type 'unordered) "itemize")
		     ((eq type 'descriptive) table-type))))
    (format "@%s%s\n@end %s"
	    (if (eq type 'descriptive)
		(concat list-type " " indic)
	      list-type)
	    contents
	    list-type)))

;;; Plain Text

(defun org-e-texinfo-plain-text (text info)
  "Transcode a TEXT string from Org to Texinfo.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; LaTeX into @LaTeX{} and TeX into @TeX{}
  (let ((case-fold-search nil)
	(start 0))
    (while (string-match "\\(\\(?:La\\)?TeX\\)" text start)
      (setq text (replace-match
		  (format "@%s{}" (match-string 1 text)) nil t text)
	    start (match-end 0))))
  ;; Handle quotation marks
  (setq text (org-e-texinfo--quotation-marks text info))
  ;; Convert special strings.
  (when (plist-get info :with-special-strings)
    (while (string-match (regexp-quote "...") text)
      (setq text (replace-match "@dots{}" nil t text))))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " @*\n"
					 text)))
  ;; Return value with @ { and } protected.
  (org-e-texinfo--sanitize-content text))

;;; Planning

(defun org-e-texinfo-planning (planning contents info)
  "Transcode a PLANNING element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   "@noindent"
   (mapconcat
    'identity
    (delq nil
	  (list
	   (let ((closed (org-element-property :closed planning)))
	     (when closed
	       (concat
		(format "@strong%s} " org-closed-string)
		(format org-e-texinfo-inactive-timestamp-format
			(org-translate-time closed)))))
	   (let ((deadline (org-element-property :deadline planning)))
	     (when deadline
	       (concat
		(format "@strong{%s} " org-deadline-string)
		(format org-e-texinfo-active-timestamp-format
			(org-translate-time deadline)))))
	   (let ((scheduled (org-element-property :scheduled planning)))
	     (when scheduled
	       (concat
		(format "@strong{%s} " org-scheduled-string)
		(format org-e-texinfo-active-timestamp-format
			(org-translate-time scheduled)))))))
    " ")
   "@*"))

;;; Property Drawer

(defun org-e-texinfo-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")

;;; Quote Block

(defun org-e-texinfo-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((title (org-element-property :name quote-block))
	 (start-quote (concat "@quotation"
			      (if title
				  (format " %s" title)))))
    (format "%s\n%s@end quotation" start-quote contents)))

;;; Quote Section

(defun org-e-texinfo-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format "@verbatim\n%s@end verbatim" value))))

;;; Radio Target

(defun org-e-texinfo-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Texinfo.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "@anchor{%s}%s"
	  (org-export-solidify-link-text
	   (org-element-property :value radio-target))
	  text))

;;; Section

(defun org-e-texinfo-section (section contents info)
  "Transcode a SECTION element from Org to Texinfo.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

;;; Special Block
;;
;; Are ignored at the moment

;;; Src Block

(defun org-e-texinfo-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
	 (lisp-p (string-match-p "lisp" lang)))
    (cond
     ;; Case 1.  Lisp Block
     (lisp-p
      (format "@lisp\n%s\n@end lisp"
	      (org-export-format-code-default src-block info)))
     ;; Case 2.  Other blocks
     (t
      (format "@example\n%s\n@end example"
	      (org-export-format-code-default src-block info))))))

;;; Statistics Cookie

(defun org-e-texinfo-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))

;;; Strike-Through
;;
;; Strikethrough is ignored

;;; Subscript

(defun org-e-texinfo-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Texinfo.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "@math{_%s}" contents))

;;; Superscript

(defun org-e-texinfo-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Texinfo.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "@math{^%s}" contents))

;;; Table
;;
;; `org-e-texinfo-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to either `org-e-texinfo-table--table.el-table' or
;; `org-e-texinfo-table--org-table' functions, depending of the type of
;; the table.
;;
;; `org-e-texinfo-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-e-texinfo-table (table contents info)
  "Transcode a TABLE element from Org to Texinfo.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or org-e-texinfo-tables-verbatim
	(let ((attr (mapconcat 'identity
			       (org-element-property :attr_latex table)
			       " ")))
	  (and attr (string-match "\\<verbatim\\>" attr))))
    (format "@verbatim \n%s\n@end verbatim"
	    ;; Re-create table, without affiliated keywords.
	    (org-trim
	     (org-element-interpret-data
	      `(table nil ,@(org-element-contents table))))))
   ;; Case 2: table.el table.  Convert it using appropriate tools.
   ((eq (org-element-property :type table) 'table.el)
    (org-e-texinfo-table--table.el-table table contents info))
   ;; Case 3: Standard table.
   (t (org-e-texinfo-table--org-table table contents info))))

(defun org-e-texinfo-table-column-widths (table info)
  "Determine the largest table cell in each column to process alignment.

TABLE is the table element to transcode.  INFO is a plist used as
a communication channel."
  (let* ((rows (org-element-map table 'table-row 'identity info))
	 (collected (loop for row in rows collect
			  (org-element-map
			   row 'table-cell 'identity info)))
	 (number-cells (length (car collected)))
	 cells counts)
    (loop for row in collected do
	  (push (mapcar (lambda (ref)
			  (let* ((start (org-element-property :contents-begin ref))
				 (end (org-element-property :contents-end ref))
				 (length (- end start)))
			    length)) row) cells))
    (setq cells (remove-if #'null cells))
    (push (loop for count from 0 to (- number-cells 1) collect
		(loop for item in cells collect
		      (nth count item))) counts)
    (mapconcat (lambda (size)
		 (make-string size ?a)) (mapcar (lambda (ref)
						  (apply 'max `,@ref)) (car counts))
		 "} {")))

(defun org-e-texinfo-table--org-table (table contents info)
  "Return appropriate Texinfo code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' attribute."
  (let* ((attr (org-export-read-attribute :attr_texinfo table))
	 (col-width (plist-get attr :columns))
	 (columns (if col-width
		      (format "@columnfractions %s"
			      col-width)
		    (format "{%s}"
			    (org-e-texinfo-table-column-widths
			     table info)))))
    ;; Prepare the final format string for the table.
    (cond
     ;; Longtable.
     ;; Others.
     (t (concat
	 (format "@multitable %s\n%s@end multitable"
		 columns
		 contents))))))

(defun org-e-texinfo-table--table.el-table (table contents info)
  "Returns nothing.

Rather than return an invalid table, nothing is returned."
  'nil)

;;; Table Cell

(defun org-e-texinfo-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Texinfo.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat (if (and contents
		   org-e-texinfo-table-scientific-notation
		   (string-match orgtbl-exp-regexp contents))
	      ;; Use appropriate format string for scientific
	      ;; notation.
	      (format org-e-texinfo-table-scientific-notation
		      (match-string 1 contents)
		      (match-string 2 contents))
	    contents)
	  (when (org-export-get-next-element table-cell info) "\n@tab ")))

;;; Table Row

(defun org-e-texinfo-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Texinfo.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (concat "@item " contents "\n")))

;;; Target

(defun org-e-texinfo-target (target contents info)
  "Transcode a TARGET object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "@anchor{%s}"
	  (org-export-solidify-link-text (org-element-property :value target))))

;;; Timestamp

(defun org-e-texinfo-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-translate-time (org-element-property :value timestamp)))
	(type (org-element-property :type timestamp)))
    (cond ((memq type '(active active-range))
	   (format org-e-texinfo-active-timestamp-format value))
	  ((memq type '(inactive inactive-range))
	   (format org-e-texinfo-inactive-timestamp-format value))
	  (t (format org-e-texinfo-diary-timestamp-format value)))))

;;; Underline
;;
;; Underline is ignored

;;; Verbatim

(defun org-e-texinfo-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-texinfo--text-markup (org-element-property :value verbatim) 'verbatim))

;;; Verse Block

(defun org-e-texinfo-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Texinfo.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  ;; In a verse environment, add a line break to each newline
  ;; character and change each white space at beginning of a line
  ;; into a space of 1 em.  Also change each blank line with
  ;; a vertical space of 1 em.
  (progn
    (setq contents (replace-regexp-in-string
		    "^ *\\\\\\\\$" "\\\\vspace*{1em}"
		    (replace-regexp-in-string
		     "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n" contents)))
    (while (string-match "^[ \t]+" contents)
      (let ((new-str (format "\\hspace*{%dem}"
			     (length (match-string 0 contents)))))
	(setq contents (replace-match new-str nil t contents))))
    (format "\\begin{verse}\n%s\\end{verse}" contents)))


;;; Interactive functions

(defun org-e-texinfo-export-to-texinfo
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a Texinfo file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".texi" subtreep pub-dir)))
    (org-export-to-file
     'e-texinfo outfile subtreep visible-only body-only ext-plist)))

(defun org-e-texinfo-export-to-info
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to Texinfo then process through to INFO.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return INFO file's name."
  (interactive)
  (org-e-texinfo-compile
   (org-e-texinfo-export-to-texinfo
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-e-texinfo-compile (texifile)
  "Compile a texinfo file.

TEXIFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-e-texinfo-info-process'.

Return INFO file name or an error if it couldn't be produced."
  (let* ((wconfig (current-window-configuration))
	 (texifile (file-truename texifile))
	 (base (file-name-sans-extension texifile))
	 errors)
    (message (format "Processing Texinfo file %s ..." texifile))
    (unwind-protect
	(progn
	  (cond
	   ;; A function is provided: Apply it.
	   ((functionp org-e-texinfo-info-process)
	    (funcall org-e-texinfo-info-process (shell-quote-argument texifile)))
	   ;; A list is provided: Replace %b, %f and %o with appropriate
	   ;; values in each command before applying it.  Output is
	   ;; redirected to "*Org INFO Texinfo Output*" buffer.
	   ((consp org-e-texinfo-info-process)
	    (let* ((out-dir (or (file-name-directory texifile) "./"))
		   (outbuf (get-buffer-create "*Org Info Texinfo Output*")))
	      (mapc
	       (lambda (command)
		 (shell-command
		  (replace-regexp-in-string
		   "%b" (shell-quote-argument base)
		   (replace-regexp-in-string
		    "%f" (shell-quote-argument texifile)
		    (replace-regexp-in-string
		     "%o" (shell-quote-argument out-dir) command t t) t t) t t)
		  outbuf))
	       org-e-texinfo-info-process)
	      ;; Collect standard errors from output buffer.
	      (setq errors (org-e-texinfo-collect-errors outbuf))))
	   (t (error "No valid command to process to Info")))
	  (let ((infofile (concat base ".info")))
	    ;; Check for process failure.  Provide collected errors if
	    ;; possible.
	    (if (not (file-exists-p infofile))
		(error (concat (format "INFO file %s wasn't produced" infofile)
			       (when errors (concat ": " errors))))
	      ;; Else remove log files, when specified, and signal end of
	      ;; process to user, along with any error encountered.
	      (message (concat "Process completed"
			       (if (not errors) "."
				 (concat " with errors: " errors)))))
	    ;; Return output file name.
	    infofile))
      (set-window-configuration wconfig))))

(defun org-e-texinfo-collect-errors (buffer)
  "Collect some kind of errors from \"makeinfo\" command output.

BUFFER is the buffer containing output.

Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      ;; Find final "makeinfo" run.
      (when t
	(let ((case-fold-search t)
	      (errors ""))
	  (when (save-excursion
		  (re-search-forward "perhaps incorrect sectioning?" nil t))
	    (setq errors (concat errors " [incorrect sectionnng]")))
	  (when (save-excursion
		  (re-search-forward "missing close brace" nil t))
	    (setq errors (concat errors " [syntax error]")))
	  (when (save-excursion
		  (re-search-forward "Unknown command" nil t))
	    (setq errors (concat errors " [undefined @command]")))
	  (when (save-excursion
		  (re-search-forward "No matching @end" nil t))
	    (setq errors (concat errors " [block incomplete]")))
	  (when (save-excursion
		  (re-search-forward "requires a sectioning" nil t))
	    (setq errors (concat errors " [invalid section command]")))
	  (when (save-excursion
		  (re-search-forward "\\[unexpected\]" nil t))
	    (setq errors (concat errors " [unexpected error]")))
	  (when (save-excursion
		  (re-search-forward "misplaced " nil t))
	    (setq errors (concat errors " [syntax error]")))
	  (and (org-string-nw-p errors) (org-trim errors)))))))

(provide 'org-e-texinfo)
;;; org-e-texinfo.el ends here
