;;; ox-texinfo.el --- Texinfo Backend for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2025 Free Software Foundation, Inc.
;; Author: Jonathan Leech-Pepin <jonathan.leechpepin at gmail dot com>
;; Maintainer: Rudolf Adamkovič <rudolf@adamkovic.org>
;; Keywords: outlines, hypermedia, calendar, text

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
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
;;
;; See Org manual for details.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'ox)
(require 'org-element-ast)

(eval-when-compile (require 'subr-x))

(defvar orgtbl-exp-regexp)
(defvar org-texinfo-supports-math--cache)


;;; Define Backend

(org-export-define-backend 'texinfo
  '((bold . org-texinfo-bold)
    (center-block . org-texinfo-center-block)
    (clock . org-texinfo-clock)
    (code . org-texinfo-code)
    (drawer . org-texinfo-drawer)
    (dynamic-block . org-texinfo-dynamic-block)
    (entity . org-texinfo-entity)
    (example-block . org-texinfo-example-block)
    (export-block . org-texinfo-export-block)
    (export-snippet . org-texinfo-export-snippet)
    (fixed-width . org-texinfo-fixed-width)
    (footnote-definition . org-texinfo-footnote-definition)
    (footnote-reference . org-texinfo-footnote-reference)
    (headline . org-texinfo-headline)
    (inline-src-block . org-texinfo-inline-src-block)
    (inlinetask . org-texinfo-inlinetask)
    (italic . org-texinfo-italic)
    (item . org-texinfo-item)
    (keyword . org-texinfo-keyword)
    (latex-environment . org-texinfo-latex-environment)
    (latex-fragment . org-texinfo-latex-fragment)
    (line-break . org-texinfo-line-break)
    (link . org-texinfo-link)
    (node-property . org-texinfo-node-property)
    (paragraph . org-texinfo-paragraph)
    (plain-list . org-texinfo-plain-list)
    (plain-text . org-texinfo-plain-text)
    (planning . org-texinfo-planning)
    (property-drawer . org-texinfo-property-drawer)
    (quote-block . org-texinfo-quote-block)
    (radio-target . org-texinfo-radio-target)
    (section . org-texinfo-section)
    (special-block . org-texinfo-special-block)
    (src-block . org-texinfo-src-block)
    (statistics-cookie . org-texinfo-statistics-cookie)
    (strike-through . org-texinfo-strike-through)
    (subscript . org-texinfo-subscript)
    (superscript . org-texinfo-superscript)
    (table . org-texinfo-table)
    (table-cell . org-texinfo-table-cell)
    (table-row . org-texinfo-table-row)
    (target . org-texinfo-target)
    (template . org-texinfo-template)
    (timestamp . org-texinfo-timestamp)
    (underline . org-texinfo-underline)
    (verbatim . org-texinfo-verbatim)
    (verse-block . org-texinfo-verse-block))
  :filters-alist
  '((:filter-headline . org-texinfo--filter-section-blank-lines)
    (:filter-parse-tree . (org-texinfo--normalize-headlines
			   org-texinfo--separate-definitions))
    (:filter-section . org-texinfo--filter-section-blank-lines)
    (:filter-final-output . org-texinfo--untabify))
  :menu-entry
  '(?i "Export to Texinfo"
       ((?t "As TEXI file" org-texinfo-export-to-texinfo)
	(?i "As INFO file" org-texinfo-export-to-info)
	(?o "As INFO file and open"
	    (lambda (a s v b)
	      (if a (org-texinfo-export-to-info t s v b)
		(org-open-file (org-texinfo-export-to-info nil s v b)))))))
  :options-alist
  '((:texinfo-filename "TEXINFO_FILENAME" nil nil t)
    (:texinfo-class "TEXINFO_CLASS" nil org-texinfo-default-class t)
    (:texinfo-header "TEXINFO_HEADER" nil nil newline)
    (:texinfo-post-header "TEXINFO_POST_HEADER" nil nil newline)
    (:subtitle "SUBTITLE" nil nil parse)
    (:subauthor "SUBAUTHOR" nil nil newline)
    (:texinfo-dircat "TEXINFO_DIR_CATEGORY" nil nil t)
    (:texinfo-dirtitle "TEXINFO_DIR_TITLE" nil nil t) ;Obsolete.
    (:texinfo-dirname "TEXINFO_DIR_NAME" nil nil t)
    (:texinfo-dirdesc "TEXINFO_DIR_DESC" nil nil t)
    (:texinfo-printed-title "TEXINFO_PRINTED_TITLE" nil nil t)
    ;; Other variables.
    (:texinfo-classes nil nil org-texinfo-classes)
    (:texinfo-format-headline-function nil nil org-texinfo-format-headline-function)
    (:texinfo-node-description-column nil nil org-texinfo-node-description-column)
    (:texinfo-active-timestamp-format nil nil org-texinfo-active-timestamp-format)
    (:texinfo-inactive-timestamp-format nil nil org-texinfo-inactive-timestamp-format)
    (:texinfo-diary-timestamp-format nil nil org-texinfo-diary-timestamp-format)
    (:texinfo-link-with-unknown-path-format nil nil org-texinfo-link-with-unknown-path-format)
    (:texinfo-tables-verbatim nil nil org-texinfo-tables-verbatim)
    (:texinfo-table-scientific-notation nil nil org-texinfo-table-scientific-notation)
    (:texinfo-table-default-markup nil nil org-texinfo-table-default-markup)
    (:texinfo-text-markup-alist nil nil org-texinfo-text-markup-alist)
    (:texinfo-format-drawer-function nil nil org-texinfo-format-drawer-function)
    (:texinfo-format-inlinetask-function nil nil org-texinfo-format-inlinetask-function)
    (:texinfo-compact-itemx nil "compact-itemx" org-texinfo-compact-itemx)
    ;; Redefine regular options.
    (:with-latex nil "tex" org-texinfo-with-latex)))


;;; User Configurable Variables

(defgroup org-export-texinfo nil
  "Options for exporting Org mode files to Texinfo."
  :tag "Org Export Texinfo"
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-export)

;;;; Preamble

(defcustom org-texinfo-coding-system nil
  "Default document encoding for Texinfo output.

If nil it will default to `buffer-file-coding-system'."
  :type 'coding-system)

(defcustom org-texinfo-default-class "info"
  "The default Texinfo class."
  :type '(string :tag "Texinfo class"))

(defcustom org-texinfo-classes
  '(("info"
     "@documentencoding AUTO\n@documentlanguage AUTO"
     ("@chapter %s" "@unnumbered %s" "@chapheading %s" "@appendix %s")
     ("@section %s" "@unnumberedsec %s" "@heading %s" "@appendixsec %s")
     ("@subsection %s" "@unnumberedsubsec %s" "@subheading %s"
      "@appendixsubsec %s")
     ("@subsubsection %s" "@unnumberedsubsubsec %s" "@subsubheading %s"
      "@appendixsubsubsec %s")))
  "Alist of Texinfo classes and associated header and structure.
If #+TEXINFO_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of a class
definition:

  (class-name
    header-string
    (numbered-1 unnumbered-1 unnumbered-no-toc-1 appendix-1)
    (numbered-2 unnumbered-2 unnumbered-no-toc-2 appendix-2)
    ...)


The header string
-----------------

The header string is inserted in the header of the generated
document, right after \"@setfilename\" and \"@settitle\"
commands.

If it contains the special string

  \"@documentencoding AUTO\"

\"AUTO\" will be replaced with an appropriate coding system.  See
`org-texinfo-coding-system' for more information.  Likewise, if
the string contains the special string

  \"@documentlanguage AUTO\"

\"AUTO\" will be replaced with the language defined in the
buffer, through #+LANGUAGE keyword, or globally, with
`org-export-default-language', which see.


The sectioning structure
------------------------

The sectioning structure of the class is given by the elements
following the header string.  For each sectioning level, a number
of strings is specified.  A %s formatter is mandatory in each
section string and will be replaced by the title of the section."
  :version "27.1"
  :package-version '(Org . "9.2")
  :type '(repeat
	  (list (string :tag "Texinfo class")
		(string :tag "Texinfo header")
		(repeat :tag "Levels" :inline t
			(choice
			 (list :tag "Heading"
			       (string :tag "         numbered")
			       (string :tag "       unnumbered")
			       (string :tag "unnumbered-no-toc")
			       (string :tag "         appendix")))))))

;;;; Headline

(defcustom org-texinfo-format-headline-function
  'org-texinfo-format-headline-default-function
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags as a list of strings (list of strings or nil).

The function result will be used in the section format string."
  :type 'function
  :version "26.1"
  :package-version '(Org . "8.3"))

;;;; Node listing (menu)

(defcustom org-texinfo-node-description-column 32
  "Column at which to start the description in the node listings.
If a node title is greater than this length, the description will
be placed after the end of the title."
  :type 'integer)

;;;; Timestamps

(defcustom org-texinfo-active-timestamp-format "@emph{%s}"
  "A printf format string to be applied to active timestamps."
  :type 'string)

(defcustom org-texinfo-inactive-timestamp-format "@emph{%s}"
  "A printf format string to be applied to inactive timestamps."
  :type 'string)

(defcustom org-texinfo-diary-timestamp-format "@emph{%s}"
  "A printf format string to be applied to diary timestamps."
  :type 'string)

;;;; Links

(defcustom org-texinfo-link-with-unknown-path-format "@indicateurl{%s}"
  "Format string for links with unknown path type."
  :type 'string)

;;;; Tables

(defcustom org-texinfo-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :type 'boolean)

(defcustom org-texinfo-table-scientific-notation nil
  "Format string to display numbers in scientific notation.

The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :type '(choice
	  (string :tag "Format string")
	  (const :tag "No formatting" nil)))

(defcustom org-texinfo-table-default-markup "@asis"
  "Default markup for first column in two-column tables.

This should an indicating command, e.g., \"@code\", \"@kbd\" or
\"@samp\".

It can be overridden locally using the \":indic\" attribute."
  :type 'string
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe #'stringp)

;;;; Text markup

(defcustom org-texinfo-text-markup-alist '((bold . "@strong{%s}")
					   (code . code)
					   (italic . "@emph{%s}")
					   (verbatim . samp))
  "Alist of Texinfo expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underscore' and `verbatim'.  The value is
a formatting string to wrap fontified text with.

Value can also be set to the following symbols: `verb', `samp'
and `code'.  With the first one, Org uses \"@verb\" to create
a format string and selects a delimiter character that isn't in
the string.  For the other two, Org uses \"@samp\" or \"@code\"
to typeset and protects special characters.

When no association is found for a given markup, text is returned
as-is."
  :version "26.1"
  :package-version '(Org . "9.1")
  :type 'alist
  :options '(bold code italic strike-through underscore verbatim))

;;;; Drawers

(defcustom org-texinfo-format-drawer-function (lambda (_name contents) contents)
  "Function called to format a drawer in Texinfo code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

The default function simply returns the value of CONTENTS."
  :version "24.4"
  :package-version '(Org . "8.2")
  :type 'function)

;;;; Inlinetasks

(defcustom org-texinfo-format-inlinetask-function
  'org-texinfo-format-inlinetask-default-function
  "Function called to format an inlinetask in Texinfo code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported."
  :type 'function)

;;;; LaTeX

(defcustom org-texinfo-with-latex (and org-export-with-latex 'detect)
  "When non-nil, the Texinfo exporter attempts to process LaTeX math.

When set to t, the exporter will process LaTeX environments and
fragments as Texinfo \"@displaymath\" and \"@math\" commands
respectively.  Alternatively, when set to `detect', the exporter
does so only if the installed version of Texinfo supports the
necessary commands."
  :package-version '(Org . "9.6")
  :type '(choice
          (const :tag "Detect" detect)
          (const :tag "Yes" t)
          (const :tag "No" nil)))

;;;; Itemx

(defcustom org-texinfo-compact-itemx nil
  "Non-nil means certain items in description list become `@itemx'.

If this is non-nil and an item in a description list has no
body but is followed by another item, then the second item is
transcoded to `@itemx'.  See info node `(org)Plain lists in
Texinfo export' for how to enable this for individual lists."
  :package-version '(Org . "9.6")
  :type 'boolean
  :safe #'booleanp)

;;;; Compilation

(defcustom org-texinfo-info-process '("makeinfo --no-split %f")
  "Commands to process a Texinfo file to an INFO file.

This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
relative file name, %F by the absolute file name, %b by the file
base name (i.e. without directory and extension parts), %o by the
base directory of the file and %O by the absolute file name of
the output file.

Alternatively, this may be a Lisp function that does the processing,
This function should accept the file name as its single argument."
  :version "26.1"
  :package-version '(Org . "9.1")
  :type '(choice
          (repeat :tag "Shell command sequence"
		  (string :tag "Shell command"))
          (function)))

(defcustom org-texinfo-logfiles-extensions
  '("aux" "toc" "cp" "fn" "ky" "pg" "tp" "vr")
  "The list of file extensions to consider as Texinfo logfiles.
The logfiles will be remove if `org-texinfo-remove-logfiles' is
non-nil."
  :type '(repeat (string :tag "Extension")))

(defcustom org-texinfo-remove-logfiles t
  "Non-nil means remove the logfiles produced by compiling a Texinfo file.
By default, logfiles are files with these extensions: .aux, .toc,
.cp, .fn, .ky, .pg and .tp.  To define the set of logfiles to remove,
set `org-texinfo-logfiles-extensions'."
  :group 'org-export-latex
  :type 'boolean)

;;; Constants

(defconst org-texinfo-max-toc-depth 4
  "Maximum depth for creation of detailed menu listings.
Beyond this depth, Texinfo will not recognize the nodes and will
cause errors.  Left as a constant in case this value ever
changes.")

(defconst org-texinfo-supported-coding-systems
  '("US-ASCII" "UTF-8" "ISO-8859-15" "ISO-8859-1" "ISO-8859-2" "koi8-r" "koi8-u")
  "List of coding systems supported by Texinfo, as strings.
Specified coding system will be matched against these strings.
If two strings share the same prefix (e.g. \"ISO-8859-1\" and
\"ISO-8859-15\"), the most specific one has to be listed first.")

(defconst org-texinfo-inline-image-rules
  (list (cons "file"
	      (regexp-opt '("eps" "pdf" "png" "jpg" "jpeg" "gif" "svg"))))
  "Rules characterizing image files that can be inlined.")

(defvar org-texinfo--quoted-keys-regexp
  (regexp-opt '("BS" "TAB" "RET" "ESC" "SPC" "DEL"
		"LFD" "DELETE" "SHIFT" "Ctrl" "Meta" "Alt"
		"Cmd" "Super" "UP" "LEFT" "RIGHT" "DOWN")
	      'words)
  "Regexp matching keys that have to be quoted using @key{KEY}.")

(defconst org-texinfo--definition-command-alist
  '(("deffn Command" . "Command")
    ("defun" . "Function")
    ("defmac" . "Macro")
    ("defspec" . "Special Form")
    ("defvar" . "Variable")
    ("defopt" . "User Option")
    (nil . "Key"))
  "Alist mapping Texinfo definition commands to output in Info files.")

(defconst org-texinfo--definition-command-regexp
  (format "\\`%s: \\(.+\\)"
	  (regexp-opt
	   (delq nil (mapcar #'cdr org-texinfo--definition-command-alist))
	   t))
  "Regexp used to match definition commands in descriptive lists.")


;;; Internal Functions

(defun org-texinfo--untabify (s _backend _info)
  "Remove TAB characters in string S."
  (replace-regexp-in-string "\t" (make-string tab-width ?\s) s))

(defun org-texinfo--filter-section-blank-lines (headline _backend _info)
  "Filter controlling number of blank lines after a section."
  (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" "\n\n" headline))

(defun org-texinfo--normalize-headlines (tree _backend info)
  "Normalize headlines in TREE.

BACKEND is the symbol specifying backend used for export.
INFO is a plist used as a communication channel.

Make sure every headline in TREE contains a section, since those
are required to install a menu.  Also put exactly one blank line
at the end of each section.

Return new tree."
  (org-element-map tree 'headline
    (lambda (hl)
      (org-element-put-property hl :post-blank 1)
      (let ((contents (org-element-contents hl)))
	(when contents
	  (let ((first (org-element-map contents '(headline section)
			 #'identity info t)))
	    (unless (org-element-type-p first 'section)
              (apply #'org-element-set-contents
                     hl
                     (org-element-create 'section `(:parent ,hl)) contents))))))
    info)
  tree)

(defun org-texinfo--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (cl-loop for c across ll
	     when (not (string-match (regexp-quote (char-to-string c)) s))
	     return (char-to-string c))))

(defun org-texinfo--text-markup (text markup _info)
  "Format TEXT depending on MARKUP text markup.
INFO is a plist used as a communication channel.  See
`org-texinfo-text-markup-alist' for details."
  (pcase (cdr (assq markup org-texinfo-text-markup-alist))
    (`nil text)				;no markup: return raw text
    (`code (format "@code{%s}" (org-texinfo--sanitize-content text)))
    (`samp (format "@samp{%s}" (org-texinfo--sanitize-content text)))
    (`verb
     (let ((separator (org-texinfo--find-verb-separator text)))
       (format "@verb{%s%s%s}" separator text separator)))
    ;; Else use format string.
    (fmt (format fmt text))))

(defun org-texinfo--get-node (datum info)
  "Return node or anchor associated to DATUM.
DATUM is a headline, a radio-target or a target.  INFO is a plist
used as a communication channel.  The function guarantees the
node or anchor name is unique."
  (let ((cache (plist-get info :texinfo-node-cache)))
    (or (cdr (assq datum cache))
	(let* ((salt 0)
	       (basename
		(org-texinfo--sanitize-node
		 (pcase (org-element-type datum)
		   (`headline
		    (org-texinfo--sanitize-title-reference
		     (org-export-get-alt-title datum info) info))
		   (`radio-target
		    (org-export-data (org-element-contents datum) info))
		   (`target
		    (org-element-property :value datum))
		   (_
		    (or (org-element-property :name datum)
			(org-export-get-reference datum info))))))
	       (name basename))
	  ;; Org exports deeper elements before their parents.  If two
	  ;; node names collide -- e.g., they have the same title --
	  ;; within the same hierarchy, the second one would get the
	  ;; smaller node name.  This is counter-intuitive.
	  ;; Consequently, we ensure that every parent headline gets
	  ;; its node beforehand.  As a recursive operation, this
	  ;; achieves the desired effect.
	  (let ((parent (org-element-lineage datum 'headline)))
	    (when (and parent (not (assq parent cache)))
	      (org-texinfo--get-node parent info)
	      (setq cache (plist-get info :texinfo-node-cache))))
	  ;; Ensure NAME is unique and not reserved node name "Top",
          ;; no matter what case is used.
	  (while (or (string-equal "Top" (capitalize name))
                     (rassoc name cache))
	    (setq name (concat basename (format " (%d)" (cl-incf salt)))))
	  (plist-put info :texinfo-node-cache (cons (cons datum name) cache))
	  name))))

(defun org-texinfo--prepend-anchor-maybe (contents node)
  "Maybe prepend NODE anchor before CONTENTS.
When NODE has :name property, prepend anchor named as :name value to
CONTENTS.  Otherwise, return CONTENTS."
  (if-let* ((label (org-element-property :name node)))
      (concat "@anchor{" label "}\n" contents)
    contents))

(defun org-texinfo--sanitize-node (title)
  "Bend string TITLE to node line requirements.
Trim string and collapse multiple whitespace characters as they
are not significant.  Replace leading left parenthesis, when
followed by a right parenthesis, with a square bracket.  Remove
periods, commas and colons."
  (org-trim
   (replace-regexp-in-string
    "[ \t]+" " "
    (replace-regexp-in-string
     "[:,.]" ""
     (replace-regexp-in-string "\\`(\\(.*?)\\)" "[\\1" title)))))

(defun org-texinfo--sanitize-title (title info)
  "Make TITLE suitable as a section name.
TITLE is a string or a secondary string.  INFO is the current
export state, as a plist."
  (org-export-data-with-backend
   title
   (org-export-create-backend
    :parent 'texinfo
    :transcoders `((footnote-reference . ,#'ignore)
                   (radio-target . ,(lambda (_r c _) c))
                   (target . ,#'ignore)))
   info))

(defun org-texinfo--sanitize-title-reference (title info)
  "Make TITLE suitable as a section reference.
TITLE is a string or a secondary string.  INFO is the current
export state, as a plist."
  (org-export-data-with-backend
   title (org-export-toc-entry-backend 'texinfo) info))

(defun org-texinfo--sanitize-content (text)
  "Escape special characters in string TEXT.
Special characters are: @ { } ,"
  (thread-last
    text
    (replace-regexp-in-string "[@{}]" "@\\&")
    (replace-regexp-in-string "," "@comma{}")))

(defun org-texinfo--wrap-float (value info &optional type label caption short)
  "Wrap string VALUE within a @float command.
INFO is the current export state, as a plist.  TYPE is float
type, as a string.  LABEL is the cross reference label for the
float, as a string.  CAPTION and SHORT are, respectively, the
caption and shortcaption used for the float, as secondary
strings (e.g., returned by `org-export-get-caption')."
  (let* ((backend
	  (org-export-toc-entry-backend 'texinfo
	    (cons 'footnote-reference
		  (lambda (f c i) (org-export-with-backend 'texinfo f c i)))))
	 (short-backend
	  (org-export-toc-entry-backend 'texinfo
	    '(inline-src-block . ignore)
	    '(verbatim . ignore)))
	 (short-str
	  (if (and short caption)
	      (format "@shortcaption{%s}\n"
		      (org-export-data-with-backend short short-backend info))
	    ""))
	 (caption-str
	  (if (or short caption)
	      (format "@caption{%s}\n"
		      (org-export-data-with-backend
		       (or caption short)
		       (if (equal short-str "") short-backend backend)
		       info))
	    "")))
    (format "@float %s%s\n%s\n%s%s@end float"
	    type (if label (concat "," label) "") value caption-str short-str)))

(defun org-texinfo--sectioning-structure (info)
  "Return sectioning structure used in the document.
INFO is a plist holding export options."
  (let ((class (plist-get info :texinfo-class)))
    (pcase (assoc class (plist-get info :texinfo-classes))
      (`(,_ ,_ . ,sections) sections)
      (_ (user-error "Unknown Texinfo class: %S" class)))))

(defun org-texinfo--separate-definitions (tree _backend info)
  "Split up descriptive lists in TREE that contain Texinfo definition commands.
INFO is a plist used as a communication channel.
Return new tree."
  (org-element-map tree 'plain-list
    (lambda (plain-list)
      (when (eq (org-element-property :type plain-list) 'descriptive)
	(let ((contents (org-element-contents plain-list))
	      (items nil))
	  (dolist (item contents)
	    (pcase-let ((`(,cmd . ,args) (org-texinfo--match-definition item info)))
	      (cond
	       (cmd
		(when items
		  (org-texinfo--split-plain-list plain-list (nreverse items))
		  (setq items nil))
		(org-texinfo--split-definition plain-list item cmd args))
	       (t
		(when args
		  (org-texinfo--massage-key-item plain-list item args info))
		(push item items)))))
	  (unless (org-element-contents plain-list)
	    (org-element-extract plain-list)))))
    info)
  tree)

(defun org-texinfo--match-definition (item &optional info)
  "Return a cons-cell if ITEM specifies a Texinfo definition command.
The car is the command and the cdr is its arguments.
INFO is the export INFO plist."
  (let ((tag (org-export-data (org-element-property :tag item) info)))
    (and tag
	 (stringp tag)
	 (string-match org-texinfo--definition-command-regexp tag)
	 (pcase-let*
	     ((cmd (car (rassoc (match-string-no-properties 1 tag)
				org-texinfo--definition-command-alist)))
	      (`(,cmd ,category)
	       (and cmd (save-match-data (split-string cmd " "))))
	      (args (match-string-no-properties 2 tag)))
	   (cons cmd (if category (concat category " " args) args))))))

(defun org-texinfo--split-definition (plain-list item cmd args)
  "Insert a definition command before list PLAIN-LIST.
Replace list item ITEM with a special-block that inherits the
contents of ITEM and whose type and Texinfo attributes are
specified by CMD and ARGS."
  (let ((contents (org-element-contents item)))
    (org-element-insert-before
     (apply #'org-element-create 'special-block
	    (list :type cmd
                  :ox-texinfo--options args
                  ;; Option can be nil that cannot be recorgnized
                  ;; literally by `org-export-read-attribute', so we
                  ;; use dedicated property instead
		  ;; :attr_texinfo (list (format ":options %s" args))
		  :post-blank (if contents 1 0))
	    (mapc #'org-element-extract contents))
     plain-list))
  (org-element-extract item))

(defun org-texinfo--split-plain-list (plain-list items)
  "Insert a new plain list before the plain list PLAIN-LIST.
Remove ITEMS from PLAIN-LIST and use them as the contents of the
new plain list."
  (org-element-insert-before
   (apply #'org-element-create 'plain-list
	  (list :type 'descriptive
                :attr_texinfo (org-element-property :attr_texinfo plain-list)
                :post-blank 1)
	  (mapc #'org-element-extract items))
   plain-list))

(defun org-texinfo--massage-key-item (plain-list item args info)
  "In PLAIN-LIST modify ITEM based on ARGS.

Reformat ITEM's tag property and determine the arguments for the
`@findex' and `@kindex' commands for ITEM and store them in ITEM
using the `:findex' and `:kindex' properties.

If PLAIN-LIST is a description list whose `:compact' attribute is
non-nil and ITEM has no content but is followed by another item,
then store the `@findex' and `@kindex' values in the next item.
If the previous item stored its respective values in this item,
then move them to the next item.

INFO is a plist used as a communication channel."
  (let ((key nil)
	(cmd nil))
    (if (string-match (rx (+ " ")
			  "(" (group (+ (not (any "()")))) ")"
			  (* " ")
			  eos)
		      args)
	(setq key (substring args 0 (match-beginning 0))
	      cmd (match-string 1 args))
      (setq key args))
    (org-element-put-property
     item :tag
     (cons (org-export-raw-string (org-texinfo-kbd-macro key t))
	   (and cmd `(" (" (code (:value ,cmd :post-blank 0)) ")"))))
    (let ((findex (org-element-property :findex item))
	  (kindex (org-element-property :kindex item))
	  (next-item (org-export-get-next-element item nil))
	  (mx (string-prefix-p "M-x " key)))
      (when (and (not cmd) mx)
	(setq cmd (substring key 4)))
      (when (and cmd (not (member cmd findex)))
	(setq findex (nconc findex (list cmd))))
      (unless mx
	(setq kindex (nconc kindex (list key))))
      (cond
       ((and next-item
             (or (plist-get info :texinfo-compact-itemx)
	         (org-not-nil
	          (org-export-read-attribute :attr_texinfo plain-list :compact)))
	     (not (org-element-contents item))
	     (eq 1 (org-element-post-blank item)))
	(org-element-put-property next-item :findex findex)
	(org-element-put-property next-item :kindex kindex)
	(org-element-put-property item :findex nil)
	(org-element-put-property item :kindex nil))
       (t
	(org-element-set-contents
	 item
	 (nconc (mapcar (lambda (key) `(keyword (:key "KINDEX" :value ,key))) kindex)
		(mapcar (lambda (cmd) `(keyword (:key "FINDEX" :value ,cmd))) findex)
		(org-element-contents item))))))))

;;; Template

(defun org-texinfo-template (contents info)
  "Return complete document string after Texinfo conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	;; Copying data is the contents of the first headline in
	;; parse tree with a non-nil copying property.
	(copying (org-element-map (plist-get info :parse-tree) 'headline
		   (lambda (hl)
		     (and (org-not-nil (org-element-property :COPYING hl))
			  (org-element-contents hl)))
		   info t)))
    (concat
     "\\input texinfo    @c -*- texinfo -*-\n"
     "@c %**start of header\n"
     (let ((file (or (org-strip-quotes (plist-get info :texinfo-filename))
		     (let ((f (plist-get info :output-file)))
		       (and f (concat (file-name-sans-extension f) ".info"))))))
       (and file (format "@setfilename %s\n" file)))
     (format "@settitle %s\n" title)
     ;; Insert class-defined header.
     (org-element-normalize-string
      (let ((header (nth 1 (assoc (plist-get info :texinfo-class)
				  org-texinfo-classes)))
	    (coding
	     (catch 'coding-system
	       (let ((case-fold-search t)
		     (name (symbol-name (or org-texinfo-coding-system
					    buffer-file-coding-system))))
		 (dolist (system org-texinfo-supported-coding-systems "UTF-8")
		   (when (string-match-p (regexp-quote system) name)
		     (throw 'coding-system system))))))
	    (language (plist-get info :language))
	    (case-fold-search nil))
	;; Auto coding system.
	(replace-regexp-in-string
	 "^@documentencoding \\(AUTO\\)$"
	 coding
	 (replace-regexp-in-string
	  "^@documentlanguage \\(AUTO\\)$" language header t nil 1)
	 t nil 1)))
     ;; Additional header options set by #+TEXINFO_HEADER.
     (let ((texinfo-header (plist-get info :texinfo-header)))
       (and texinfo-header (org-element-normalize-string texinfo-header)))
     "@c %**end of header\n\n"
     ;; Additional options set by #+TEXINFO_POST_HEADER.
     (let ((texinfo-post-header (plist-get info :texinfo-post-header)))
       (and texinfo-post-header
	    (org-element-normalize-string texinfo-post-header)))
     ;; Copying.
     (and copying
	  (format "@copying\n%s@end copying\n\n"
		  (org-element-normalize-string
		   (org-export-data copying info))))
     (let* ((dircat (or (plist-get info :texinfo-dircat) "Misc"))
	    (file (or (org-strip-quotes (plist-get info :texinfo-filename))
		    (plist-get info :output-file)))
	    (file (if file (file-name-sans-extension file)))
	    (dn (or (plist-get info :texinfo-dirname)
	            (plist-get info :texinfo-dirtitle))) ;Obsolete name.
	    ;; Strip any terminating `.' from `dn'.
	    (dn (if (and dn (string-match "\\.\\'" dn)) (substring dn 0 -1) dn))
	    ;; The direntry we need to produce has the shape:
	    ;;     * DIRNAME: NODE.   DESCRIPTION.
	    ;; where NODE is usually just `(FILENAME)', and where
	    ;; `* FILENAME.' is a shorthand for `* FILENAME: (FILENAME).'
	    (dirname
             (cond
              ((and dn (string-match
                        (eval-when-compile
                          (concat "\\`\\(?:"
                                  "\\* \\(?1:.*\\)" ;Starts with `* ' or
                                  "\\|\\(?1:.*(.*).*\\)" ;contains parens.
                                  "\\)\\'"))
                        dn))
               ;; When users provide a `dn' that looks like a complete
               ;; `* DIRNAME: (FILENAME).' thingy, we just trust them to
               ;; provide something valid (just making sure it starts
               ;; with `* ' and ends with `.').
               (format "* %s." (match-string 1 dn)))
              ;; `dn' is presumed to be just the DIRNAME part, so generate
              ;; either `* DIRNAME: (FILENAME).' or `* FILENAME.', whichever
              ;; is shortest.
              (dn
               (format "* %s: (%s)." dn (or file dn)))
              (t (format "* (%s)." file)))))
       (concat "@dircategory " dircat "\n"
	       "@direntry\n"
	       (let ((dirdesc
		      (let ((desc (or (plist-get info :texinfo-dirdesc)
			              title)))
			(cond ((not desc) nil)
			      ((string-suffix-p "." desc) desc)
			      (t (concat desc "."))))))
		 (if dirdesc (format "%-23s %s" dirname dirdesc) dirname))
	       "\n"
	       "@end direntry\n\n"))
     ;; Title
     "@finalout\n"
     "@titlepage\n"
     (when (plist-get info :with-title)
       (concat
	(format "@title %s\n"
		(or (plist-get info :texinfo-printed-title) title ""))
	(let ((subtitle (plist-get info :subtitle)))
	  (when subtitle
	    (format "@subtitle %s\n"
		    (org-export-data subtitle info))))))
     (when (plist-get info :with-author)
       (concat
	;; Primary author.
	(let ((author (org-string-nw-p
		       (org-export-data (plist-get info :author) info)))
	      (email (and (plist-get info :with-email)
			  (org-string-nw-p
			   (org-export-data (plist-get info :email) info)))))
	  (cond ((and author email)
		 (format "@author %s (@email{%s})\n" author email))
		(author (format "@author %s\n" author))
		(email (format "@author @email{%s}\n" email))))
	;; Other authors.
	(let ((subauthor (plist-get info :subauthor)))
	  (and subauthor
	       (org-element-normalize-string
		(replace-regexp-in-string "^" "@author " subauthor))))))
     (and copying "@page\n@vskip 0pt plus 1filll\n@insertcopying\n")
     "@end titlepage\n\n"
     ;; Table of contents.
     (and (plist-get info :with-toc) "@contents\n\n")
     ;; Configure Top Node when not for TeX.  Also include contents
     ;; from the first section of the document.
     "@ifnottex\n"
     "@node Top\n"
     (format "@top %s\n" title)
     (let* ((first-section
	     (org-element-map (plist-get info :parse-tree) 'section
	       #'identity info t '(headline)))
	    (top-contents
	     (org-export-data (org-element-contents first-section) info)))
       (and (org-string-nw-p top-contents) (concat "\n" top-contents)))
     "@end ifnottex\n\n"
     ;; Menu.
     (org-texinfo-make-menu (plist-get info :parse-tree) info 'master)
     "\n"
     ;; Document's body.
     contents "\n"
     ;; Creator.
     (and (plist-get info :with-creator)
	  (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "@bye")))



;;; Transcode Functions

;;;; Bold

(defun org-texinfo-bold (_bold contents info)
  "Transcode BOLD from Org to Texinfo.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-texinfo--text-markup contents 'bold info))

;;;; Center Block

(defun org-texinfo-center-block (center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist used
as a communication channel."
  (org-texinfo--prepend-anchor-maybe
   (replace-regexp-in-string "\\(^\\).*?\\S-" "@center " contents nil nil 1)
   center-block))

;;;; Clock

(defun org-texinfo-clock (clock _contents info)
  "Transcode a CLOCK element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   "@noindent"
   (format "@strong{%s} " org-clock-string)
   (format (plist-get info :texinfo-inactive-timestamp-format)
	   (concat (org-timestamp-translate (org-element-property :value clock))
		   (let ((time (org-element-property :duration clock)))
		     (and time (format " (%s)" time)))))
   "@*"))

;;;; Code

(defun org-texinfo-code (code _contents info)
  "Transcode a CODE object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-texinfo--text-markup (org-element-property :value code) 'code info))

;;;; Drawer

(defun org-texinfo-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (funcall (plist-get info :texinfo-format-drawer-function)
			  name contents)))
    (org-texinfo--prepend-anchor-maybe output drawer)))

;;;; Dynamic Block

(defun org-texinfo-dynamic-block (dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-texinfo--prepend-anchor-maybe contents dynamic-block))

;;;; Entity

(defun org-texinfo-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to Texinfo."
  ;; Since there is not specific Texinfo entry in entities, use
  ;; Texinfo-specific commands whenever possible, and fallback to
  ;; UTF-8 otherwise.
  (pcase (org-element-property :name entity)
    ("AElig"                       "@AE{}")
    ("aelig"                       "@ae{}")
    ((or "bull" "bullet")          "@bullet{}")
    ("copy"                        "@copyright{}")
    ("deg"                         "@textdegree{}")
    ((or "dots" "hellip")          "@dots{}")
    ("equiv"                       "@equiv{}")
    ((or "euro" "EUR")             "@euro{}")
    ((or "ge" "geq")               "@geq{}")
    ("laquo"                       "@guillemetleft{}")
    ("iexcl"                       "@exclamdown{}")
    ("imath"                       "@dotless{i}")
    ("iquest"                      "@questiondown{}")
    ("jmath"                       "@dotless{j}")
    ((or "le" "leq")               "@leq{}")
    ("lsaquo"                      "@guilsinglleft{}")
    ("mdash"                       "---")
    ("minus"                       "@minus{}")
    ("nbsp"                        "@tie{}")
    ("ndash"                       "--")
    ("OElig"                       "@OE{}")
    ("oelig"                       "@oe{}")
    ("ordf"                        "@ordf{}")
    ("ordm"                        "@ordm{}")
    ("pound"                       "@pound{}")
    ("raquo"                       "@guillemetright{}")
    ((or "rArr" "Rightarrow")      "@result{}")
    ("reg"                         "@registeredsymbol{}")
    ((or "rightarrow" "to" "rarr") "@arrow{}")
    ("rsaquo"                      "@guilsinglright{}")
    ("thorn"                       "@th{}")
    ("THORN"                       "@TH{}")
    ((and (pred (string-prefix-p "_")) name) ;spacing entities
     (format "@w{%s}" (substring name 1)))
    (_ (org-element-property :utf-8 entity))))

;;;; Example Block

(defun org-texinfo-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-texinfo--prepend-anchor-maybe
   (format "@example\n%s@end example"
	   (org-texinfo--sanitize-content
	    (org-export-format-code-default example-block info)))
   example-block))

;;; Export Block

(defun org-texinfo-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "TEXINFO")
    (org-remove-indentation (org-element-property :value export-block))))

;;; Export Snippet

(defun org-texinfo-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'texinfo)
    (org-element-property :value export-snippet)))

;;;; Fixed Width

(defun org-texinfo-fixed-width (fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-texinfo--prepend-anchor-maybe
   (format "@example\n%s\n@end example"
	   (org-remove-indentation
	    (org-texinfo--sanitize-content
	     (org-element-property :value fixed-width))))
   fixed-width))

;;;; Footnote Reference

(defun org-texinfo-footnote-reference (footnote _contents info)
  "Create a footnote reference for FOOTNOTE.

FOOTNOTE is the footnote to define.  CONTENTS is nil.  INFO is a
plist holding contextual information."
  (let* ((contents (org-export-get-footnote-definition footnote info))
         (data (org-export-data contents info)))
    (format "@footnote{%s}"
            ;; It is invalid to close a footnote on a line starting
            ;; with "@end".  As a safety net, we leave a newline
            ;; character before the closing brace.  However, when the
            ;; footnote ends with a paragraph, it is visually pleasing
            ;; to move the brace right after its end.
            (if (org-element-type-p (org-last contents) 'paragraph)
                (org-trim data)
              data))))

;;;; Headline

(defun org-texinfo-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Texinfo.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (cond
   ((org-element-property :footnote-section-p headline) nil)
   ((org-not-nil (org-export-get-node-property :COPYING headline t)) nil)
   (t
    (let* ((index (let ((i (org-export-get-node-property :INDEX headline t)))
		    (and (member i '("cp" "fn" "ky" "pg" "tp" "vr")) i)))
	   (numbered? (org-export-numbered-headline-p headline info))
	   (notoc? (org-export-excluded-from-toc-p headline info))
	   (command
	    (and
             (not (org-export-low-level-p headline info))
	     (let ((sections (org-texinfo--sectioning-structure info)))
               (pcase (nth (1- (org-export-get-relative-level headline info))
			   sections)
		 (`(,numbered ,unnumbered ,unnumbered-no-toc ,appendix)
		  (cond
		   ((org-not-nil
		     (org-export-get-node-property :APPENDIX headline t))
		    appendix)
		   (numbered? numbered)
		   (index unnumbered)
		   (notoc? unnumbered-no-toc)
		   (t unnumbered)))
		 (`nil nil)
		 (_ (user-error "Invalid Texinfo class specification: %S"
				(plist-get info :texinfo-class)))))))
	   (todo
	    (and (plist-get info :with-todo-keywords)
		 (let ((todo (org-element-property :todo-keyword headline)))
		   (and todo (org-export-data todo info)))))
	   (todo-type (and todo (org-element-property :todo-type headline)))
	   (tags (and (plist-get info :with-tags)
		      (org-export-get-tags headline info)))
	   (priority (and (plist-get info :with-priority)
			  (org-element-property :priority headline)))
	   (text (funcall (if command
                              #'org-texinfo--sanitize-title-reference
                            #'org-texinfo--sanitize-title)
                          (org-element-property :title headline)
                          info))
	   (full-text
	    (funcall (plist-get info :texinfo-format-headline-function)
		     todo todo-type priority text tags))
	   (contents
	    (concat "\n"
		    (if (org-string-nw-p contents) (concat "\n" contents) "")
		    (and index (format "\n@printindex %s\n" index))))
           (node (org-texinfo--get-node headline info)))
      (if (not command)
	  (concat (and (org-export-first-sibling-p headline info)
		       (format "@%s\n" (if numbered? 'enumerate 'itemize)))
		  (format "@item\n@anchor{%s}%s\n" node full-text)
		  contents
		  (if (org-export-last-sibling-p headline info)
		      (format "@end %s" (if numbered? 'enumerate 'itemize))
		    "\n"))
	(concat
	 ;; Even if HEADLINE is using @subheading and al., leave an
	 ;; anchor so cross-references in the Org document still work.
	 (format (if notoc? "@anchor{%s}\n" "@node %s\n") node)
	 (format command full-text)
	 contents))))))

(defun org-texinfo-format-headline-default-function
    (todo _todo-type priority text tags)
  "Default format function for a headline.
See `org-texinfo-format-headline-function' for details."
  (concat (and todo (format "@strong{%s} " todo))
	  (and priority (format "@emph{#%s} " priority))
	  text
	  (and tags (concat " " (org-make-tag-string tags)))))

;;;; Inline Src Block

(defun org-texinfo-inline-src-block (inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "@code{%s}"
	  (org-texinfo--sanitize-content
	   (org-element-property :value inline-src-block))))

;;;; Inlinetask

(defun org-texinfo-inlinetask (inlinetask contents info)
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
    (funcall (plist-get info :texinfo-format-inlinetask-function)
	     todo todo-type priority title tags contents)))

(defun org-texinfo-format-inlinetask-default-function
    (todo _todo-type priority title tags contents)
  "Default format function for inlinetasks.
See `org-texinfo-format-inlinetask-function' for details."
  (let ((full-title
	 (concat (when todo (format "@strong{%s} " todo))
		 (when priority (format "#%c " priority))
		 title
		 (when tags (org-make-tag-string tags)))))
    (format "@center %s\n\n%s\n" full-title contents)))

;;;; Italic

(defun org-texinfo-italic (_italic contents info)
  "Transcode ITALIC from Org to Texinfo.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-texinfo--text-markup contents 'italic info))

;;;; Item

(defun org-texinfo-item (item contents info)
  "Transcode an ITEM element from Org to Texinfo.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((tag (org-element-property :tag item))
         (plain-list (org-element-parent item))
         (compact (and (eq (org-element-property :type plain-list) 'descriptive)
                       (or (plist-get info :texinfo-compact-itemx)
                           (org-not-nil (org-export-read-attribute
                                       :attr_texinfo plain-list :compact)))))
         (previous-item nil))
    (when (and compact
               (org-export-get-next-element item info)
               (not (org-element-contents item))
               (eq 1 (org-element-post-blank item)))
      (org-element-put-property item :post-blank 0))
    (if (and compact
             (setq previous-item (org-export-get-previous-element item info))
             (not (org-element-contents previous-item))
	     (eq 0 (org-element-post-blank previous-item)))
        (format "@itemx%s\n%s"
                (if tag (concat " " (org-export-data tag info)) "")
                (or contents ""))
      (let* ((split (org-string-nw-p (org-export-read-attribute
                                      :attr_texinfo plain-list :sep)))
	     (items (and tag
		         (let ((tag (org-export-data tag info)))
		           (if split
			       (split-string tag (regexp-quote split)
                                             t "[ \t\n]+")
			     (list tag))))))
        (format "%s\n%s"
	        (pcase items
	          (`nil "@item")
	          (`(,item) (concat "@item " item))
	          (`(,item . ,items)
	           (concat "@item " item "\n"
		           (mapconcat (lambda (i) (concat "@itemx " i))
				      items
				      "\n"))))
	        (or contents ""))))))

;;;; Keyword

(defun org-texinfo-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-element-property :value keyword)))
    (pcase (org-element-property :key keyword)
      ("TEXINFO" value)
      ("CINDEX" (format "@cindex %s" value))
      ("FINDEX" (format "@findex %s" value))
      ("KINDEX" (format "@kindex %s" value))
      ("PINDEX" (format "@pindex %s" value))
      ("TINDEX" (format "@tindex %s" value))
      ("VINDEX" (format "@vindex %s" value))
      ("TOC"
       (cond ((string-match-p "\\<tables\\>" value)
	      (concat "@listoffloats "
		      (org-export-translate "Table" :utf-8 info)))
	     ((string-match-p "\\<listings\\>" value)
	      (concat "@listoffloats "
		      (org-export-translate "Listing" :utf-8 info))))))))

;;;; LaTeX Environment

(defun org-texinfo-latex-environment (environment _contents info)
  "Transcode a LaTeX ENVIRONMENT from Org to Texinfo.
CONTENTS is ignored.  INFO is a plist holding contextual information."
  (let ((with-latex (plist-get info :with-latex)))
    (when (or (eq with-latex t)
              (and (eq with-latex 'detect)
                   (org-texinfo-supports-math-p)))
      (org-texinfo--prepend-anchor-maybe
       (let ((value (org-element-property :value environment)))
         (string-join (list "@displaymath"
                            (string-trim (org-remove-indentation value))
                            "@end displaymath")
                      "\n"))
       environment))))

;;;; LaTeX Fragment

(defun org-texinfo-latex-fragment (fragment _contents info)
  "Transcode a LaTeX FRAGMENT from Org to Texinfo.
INFO is a plist holding contextual information."
  (let ((with-latex (plist-get info :with-latex)))
    (when (or (eq with-latex t)
              (and (eq with-latex 'detect)
                   (org-texinfo-supports-math-p)))
      (let ((value (org-remove-indentation
                    (org-element-property :value fragment))))
        (cond
         ((or (string-match-p "^\\\\\\[" value)
              (string-match-p "^\\$\\$" value))
          (concat "\n"
                  "@displaymath"
                  "\n"
                  (string-trim (substring value 2 -2))
                  "\n"
                  "@end displaymath"
                  "\n"))
         ((string-match-p "^\\$" value)
          (concat "@math{"
                  (string-trim (substring value 1 -1))
                  "}"))
         ((string-match-p "^\\\\(" value)
          (concat "@math{"
                  (string-trim (substring value 2 -2))
                  "}"))
         (t value))))))

;;;; Line Break

(defun org-texinfo-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "@*\n")

;;;; Link

(defun org-texinfo--@ref (datum description info)
  "Return @ref command for element or object DATUM.
DESCRIPTION is the printed name of the section, as a string, or
nil."
  (let ((node-name (org-texinfo--get-node datum info))
	;; Sanitize DESCRIPTION for cross-reference use.  In
	;; particular, remove colons as they seem to cause pain (even
	;; within @asis{...}) to the Texinfo reader.
	(title (and description
		    (replace-regexp-in-string
		     "[ \t]*:+" ""
		     (replace-regexp-in-string "," "@comma{}" description)))))
    (if (not title)
	(format "@ref{%s}" node-name)
      (format "@ref{%s, , %s}" node-name title))))

(defun org-texinfo-link (link desc info)
  "Transcode a LINK object from Org to Texinfo.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (path (org-texinfo--sanitize-content
		(cond
		 ((string-equal type "file")
		  (org-export-file-uri raw-path))
		 (t (concat type ":" raw-path))))))
    (cond
     ((org-export-custom-protocol-maybe link desc 'texinfo info))
     ((org-export-inline-image-p link org-texinfo-inline-image-rules)
      (org-texinfo--inline-image link info))
     ((equal type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (org-texinfo--@ref destination desc info))))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination
	     (if (equal type "fuzzy")
		 (org-export-resolve-fuzzy-link link info)
	       (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  (`nil
	   (format org-texinfo-link-with-unknown-path-format path))
	  ;; Id link points to an external file.
	  (`plain-text
	   (if desc (format "@uref{file://%s,%s}" destination desc)
	     (format "@uref{file://%s}" destination)))
	  ((or `headline
	       ;; Targets within headlines cannot be turned into
	       ;; @anchor{}, so we refer to the headline parent
	       ;; directly.
	       (and `target
		    (guard
		     (org-element-type-p
		      (org-element-parent destination)
                      'headline))))
	   (let ((headline (org-element-lineage destination 'headline t)))
	     (org-texinfo--@ref headline desc info)))
	  (_ (org-texinfo--@ref destination desc info)))))
     ((string= type "mailto")
      (format "@email{%s}"
	      (concat path (and desc (concat ", " desc)))))
     ;; External link with a description part.
     ((and path desc) (format "@uref{%s, %s}" path desc))
     ;; External link without a description part.
     (path (format "@uref{%s}" path))
     ;; No path, only description.  Try to do something useful.
     (t
      (format (plist-get info :texinfo-link-with-unknown-path-format) desc)))))

(defun org-texinfo--inline-image (link info)
  "Return Texinfo code for an inline image.
LINK is the link pointing to the inline image.  INFO is the
current state of the export, as a plist."
  (let* ((parent (org-element-parent-element link))
	 (label (and (org-element-property :name parent)
		     (org-texinfo--get-node parent info)))
	 (caption (org-export-get-caption parent))
	 (shortcaption (org-export-get-caption parent t))
	 (path  (org-element-property :path link))
	 (filename
	  (file-name-sans-extension
	   (if (file-name-absolute-p path)
               (expand-file-name path)
             (file-relative-name path))))
	 (extension (file-name-extension path))
	 (attributes (org-export-read-attribute :attr_texinfo parent))
	 (height (or (plist-get attributes :height) ""))
	 (width (or (plist-get attributes :width) ""))
	 (alt (or (plist-get attributes :alt) ""))
	 (image (format "@image{%s,%s,%s,%s,%s}"
			filename width height alt extension)))
    (cond ((or caption shortcaption)
	   (org-texinfo--wrap-float image
				    info
				    (org-export-translate "Figure" :utf-8 info)
				    label
				    caption
				    shortcaption))
	  (label (concat "@anchor{" label "}\n" image))
	  (t image))))


;;;; Menu

(defun org-texinfo-make-menu (scope info &optional master)
  "Create the menu for inclusion in the Texinfo document.

SCOPE is a headline or a full parse tree.  INFO is the
communication channel, as a plist.

When optional argument MASTER is non-nil, generate a master menu,
including detailed node listing."
  (let ((menu (org-texinfo--build-menu scope info)))
    (when (org-string-nw-p menu)
      (org-element-normalize-string
       (format
	"@menu\n%s@end menu"
	(concat menu
		(when master
		  (let ((detailmenu
			 (org-texinfo--build-menu
			  scope info
			  (let ((toc-depth (plist-get info :with-toc)))
			    (if (wholenump toc-depth) toc-depth
			      org-texinfo-max-toc-depth)))))
		    (when (org-string-nw-p detailmenu)
		      (concat "\n@detailmenu\n"
			      "--- The Detailed Node Listing ---\n\n"
			      detailmenu
			      "@end detailmenu\n"))))))))))

(defun org-texinfo--build-menu (scope info &optional level)
  "Build menu for entries within SCOPE.
SCOPE is a headline or a full parse tree.  INFO is a plist
containing contextual information.  When optional argument LEVEL
is an integer, build the menu recursively, down to this depth."
  (cond
   ((not level)
    (org-texinfo--format-entries (org-texinfo--menu-entries scope info) info))
   ((zerop level) "\n")
   (t
    (mapconcat
     (lambda (headline)
       (let ((entries (org-texinfo--menu-entries headline info)))
	 (when entries
	   (concat
	    (format "%s\n\n%s\n"
		    (org-export-data-with-backend
                     (org-export-get-alt-title headline info)
                     (org-export-toc-entry-backend 'texinfo)
                     info)
		    (org-texinfo--format-entries entries info))
	    (org-texinfo--build-menu headline info (1- level))))))
     (org-texinfo--menu-entries scope info)
     ""))))

(defun org-texinfo--format-entries (entries info)
  "Format all direct menu entries in SCOPE, as a string.
SCOPE is either a headline or a full Org document.  INFO is
a plist containing contextual information."
  (org-element-normalize-string
   (mapconcat
    (lambda (h)
      (let* ((title
	      ;; Colons are used as a separator between title and node
	      ;; name.  Remove them.
	      (replace-regexp-in-string
	       "[ \t]*:+" ""
	       (org-texinfo--sanitize-title-reference
		(org-export-get-alt-title h info) info)))
	     (node (org-texinfo--get-node h info))
	     (entry (concat "* " title ":"
			    (if (string= title node) ":"
			      (concat " " node ". "))))
	     (desc (org-element-property :DESCRIPTION h)))
	(if (not desc) entry
	  (format (format "%%-%ds %%s" org-texinfo-node-description-column)
		  entry desc))))
    entries "\n")))

(defun org-texinfo--menu-entries (scope info)
  "List direct children in SCOPE needing a menu entry.
SCOPE is a headline or a full parse tree.  INFO is a plist
holding contextual information."
  (let* ((cache (or (plist-get info :texinfo-entries-cache)
		    (plist-get (plist-put info :texinfo-entries-cache
					  (make-hash-table :test #'eq))
			       :texinfo-entries-cache)))
	 (cached-entries (gethash scope cache 'no-cache)))
    (if (not (eq cached-entries 'no-cache)) cached-entries
      (let* ((sections (org-texinfo--sectioning-structure info))
             (max-depth (length sections)))
        (puthash scope
	         (cl-remove-if
		  (lambda (h)
		    (or (org-not-nil (org-export-get-node-property :COPYING h t))
                        (< max-depth (org-export-get-relative-level h info))))
		  (org-export-collect-headlines info 1 scope))
	         cache)))))

;;;; Node Property

(defun org-texinfo-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))

;;;; Paragraph

(defun org-texinfo-paragraph (paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to Texinfo.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  ;; Ensure that we do not create multiple paragraphs, when a single
  ;; paragraph is expected.
  ;; Multiple newlines may appear in CONTENTS, for example, when
  ;; certain objects are stripped from export, leaving single newlines
  ;; before and after.
  (org-texinfo--prepend-anchor-maybe
   (org-remove-blank-lines contents)
   paragraph))

;;;; Plain List

(defun org-texinfo-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Texinfo.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((attr (org-export-read-attribute :attr_texinfo plain-list))
	 (indic (let ((i (or (plist-get attr :indic)
			     (plist-get info :texinfo-table-default-markup))))
		  ;; Allow indicating commands with missing @ sign.
		  (if (string-prefix-p "@" i) i (concat "@" i))))
	 (table-type (plist-get attr :table-type))
	 (type (org-element-property :type plain-list))
	 (enum
	  (cond ((not (eq type 'ordered)) nil)
		((plist-member attr :enum) (plist-get attr :enum))
		(t
		 ;; Texinfo only supports initial counters, i.e., it
		 ;; cannot change the numbering mid-list.
		 (let ((first-item (car (org-element-contents plain-list))))
		   (org-element-property :counter first-item)))))
	 (list-type (cond
		     ((eq type 'ordered) "enumerate")
		     ((eq type 'unordered) "itemize")
		     ((member table-type '("ftable" "vtable")) table-type)
		     (t "table"))))
    (org-texinfo--prepend-anchor-maybe
     (format "@%s\n%s@end %s"
	     (cond ((eq type 'descriptive) (concat list-type " " indic))
		   (enum (format "%s %s" list-type enum))
		   (t list-type))
	     contents
	     list-type)
     plain-list)))

;;;; Plain Text

(defun org-texinfo-plain-text (text info)
  "Transcode a TEXT string from Org to Texinfo.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; First protect @, {, }, and commas (,).
  (let ((output (org-texinfo--sanitize-content text)))
    ;; Activate smart quotes.  Be sure to provide original TEXT string
    ;; since OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output
	    (org-export-activate-smart-quotes output :texinfo info text)))
    ;; LaTeX into @LaTeX{} and TeX into @TeX{}
    (let ((case-fold-search nil))
      (setq output (replace-regexp-in-string "\\(?:La\\)?TeX" "@\\&{}" output)))
    ;; Convert special strings.
    (when (plist-get info :with-special-strings)
      (setq output
	    (replace-regexp-in-string
	     "\\.\\.\\." "@dots{}"
	     (replace-regexp-in-string "\\\\-" "@-" output))))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output (replace-regexp-in-string
		    "\\(\\\\\\\\\\)?[ \t]*\n" " @*\n" output)))
    ;; Reverse sentence ending.  A sentence can end with a capital
    ;; letter.  Use non-breaking space if it shouldn't.
    (let ((case-fold-search nil))
      (replace-regexp-in-string
       "[A-Z]\\([.?!]\\)\\(?:[])]\\|'\\{1,2\\}\\)?\\(?: \\|$\\)"
       "@\\1"
       output nil nil 1))))

;;;; Planning

(defun org-texinfo-planning (planning _contents info)
  "Transcode a PLANNING element from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   "@noindent"
   (mapconcat
    #'identity
    (delq nil
	  (list
	   (let ((closed (org-element-property :closed planning)))
	     (when closed
	       (concat
		(format "@strong{%s} " org-closed-string)
		(format (plist-get info :texinfo-inactive-timestamp-format)
			(org-timestamp-translate closed)))))
	   (let ((deadline (org-element-property :deadline planning)))
	     (when deadline
	       (concat
		(format "@strong{%s} " org-deadline-string)
		(format (plist-get info :texinfo-active-timestamp-format)
			(org-timestamp-translate deadline)))))
	   (let ((scheduled (org-element-property :scheduled planning)))
	     (when scheduled
	       (concat
		(format "@strong{%s} " org-scheduled-string)
		(format (plist-get info :texinfo-active-timestamp-format)
			(org-timestamp-translate scheduled)))))))
    " ")
   "@*"))

;;;; Property Drawer

(defun org-texinfo-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to Texinfo.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (format "@verbatim\n%s@end verbatim" contents)))

;;;; Quote Block

(defun org-texinfo-quote-block (quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((tag (org-export-read-attribute :attr_texinfo quote-block :tag))
	(author (org-export-read-attribute :attr_texinfo quote-block :author)))
    (org-texinfo--prepend-anchor-maybe
     (format "@quotation%s\n%s%s\n@end quotation"
	     (if tag (concat " " tag) "")
	     contents
	     (if author (concat "\n@author " author) ""))
     quote-block)))

;;;; Radio Target

(defun org-texinfo-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Texinfo.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "@anchor{%s}%s"
	  (org-texinfo--get-node radio-target info)
	  text))

;;;; Section

(defun org-texinfo-section (section contents info)
  "Transcode a SECTION element from Org to Texinfo.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-element-lineage section 'headline)))
    (when parent   ;first section is handled in `org-texinfo-template'
      (org-trim
       (concat contents
	       "\n"
	       (and (not (org-export-excluded-from-toc-p parent info))
		    (org-texinfo-make-menu parent info)))))))

;;;; Special Block

(defun org-texinfo-special-block (special-block contents _info)
  "Transcode a SPECIAL-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the block.  INFO is a plist used
as a communication channel."
  (let ((opt (or
              ;; See `org-texinfo--split-definition'
              (org-element-property :ox-texinfo--options special-block)
              (org-export-read-attribute :attr_texinfo special-block :options)))
	(type (org-element-property :type special-block)))
    (org-texinfo--prepend-anchor-maybe
     (format "@%s%s\n%s@end %s"
	     type
	     (if opt (concat " " opt) "")
	     (or contents "")
	     type)
     special-block)))

;;;; Src Block

(defun org-texinfo-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to Texinfo.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lisp (string-match-p
                "lisp"
		(or (org-element-property :language src-block) "")))
	 (code (org-texinfo--sanitize-content
		(org-export-format-code-default src-block info)))
	 (value (format
		 (if lisp "@lisp\n%s@end lisp" "@example\n%s@end example")
		 code))
	 (caption (org-export-get-caption src-block))
	 (shortcaption (org-export-get-caption src-block t)))
    (cond
     ((or caption shortcaption)
      (org-texinfo--wrap-float value
			       info
			       (org-export-translate "Listing" :utf-8 info)
                               (org-element-property :name src-block)
			       caption
			       shortcaption))
     (t (org-texinfo--prepend-anchor-maybe value src-block)))))

;;;; Statistics Cookie

(defun org-texinfo-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Strike-through

(defun org-texinfo-strike-through (_strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Texinfo.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-texinfo--text-markup contents 'strike-through info))

;;;; Subscript

(defun org-texinfo-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to Texinfo.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "@math{_%s}" contents))

;;;; Superscript

(defun org-texinfo-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to Texinfo.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "@math{^%s}" contents))

;;;; Table

(defun org-texinfo-table (table contents info)
  "Transcode a TABLE element from Org to Texinfo.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      (org-texinfo--prepend-anchor-maybe
       (format "@verbatim\n%s@end verbatim"
	       (org-element-normalize-string
	        (org-element-property :value table)))
       table)
    (let* ((col-width (org-export-read-attribute :attr_texinfo table :columns))
	   (columns
	    (if col-width (format "@columnfractions %s" col-width)
	      (org-texinfo-table-column-widths table info)))
	   (caption (org-export-get-caption table))
	   (shortcaption (org-export-get-caption table t))
	   (table-str (format "@multitable %s\n%s@end multitable"
			      columns
			      contents)))
      (cond
       ((or caption shortcaption)
        (org-texinfo--wrap-float table-str
				 info
				 (org-export-translate "Table" :utf-8 info)
                                 (org-element-property :name table)
				 caption
				 shortcaption))
       (t (org-texinfo--prepend-anchor-maybe table-str table))))))

(defun org-texinfo-table-column-widths (table info)
  "Determine the largest table cell in each column to process alignment.
TABLE is the table element to transcode.  INFO is a plist used as
a communication channel."
  (let ((widths (make-vector (cdr (org-export-table-dimensions table info)) 0)))
    (org-element-map table 'table-row
      (lambda (row)
	(let ((idx 0))
	  (org-element-map row 'table-cell
	    (lambda (cell)
	      ;; Length of the cell in the original buffer is only an
	      ;; approximation of the length of the cell in the
	      ;; output.  It can sometimes fail (e.g. it considers
	      ;; "/a/" being larger than "ab").
	      (let ((w (- (org-element-contents-end cell)
			  (org-element-contents-begin cell))))
		(aset widths idx (max w (aref widths idx))))
	      (cl-incf idx))
	    info)))
      info)
    (format "{%s}" (mapconcat (lambda (w) (make-string w ?a)) widths "} {"))))

;;;; Table Cell

(defun org-texinfo-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Texinfo.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat
   (let ((scientific-notation
	  (plist-get info :texinfo-table-scientific-notation)))
     (if (and contents
	      scientific-notation
	      (string-match orgtbl-exp-regexp contents))
	 ;; Use appropriate format string for scientific notation.
	 (format scientific-notation
		 (match-string 1 contents)
		 (match-string 2 contents))
       contents))
   (when (org-export-get-next-element table-cell info) "\n@tab ")))

;;;; Table Row

(defun org-texinfo-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Texinfo.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let ((rowgroup-tag
	   (if (and (= 1 (org-export-table-row-group table-row info))
		    (org-export-table-has-header-p
		     (org-element-lineage table-row 'table) info))
	       "@headitem "
	     "@item ")))
      (concat rowgroup-tag contents "\n"))))

;;;; Target

(defun org-texinfo-target (target _contents info)
  "Transcode a TARGET object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "@anchor{%s}" (org-texinfo--get-node target info)))

;;;; Timestamp

(defun org-texinfo-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-texinfo-plain-text
		(org-timestamp-translate timestamp) info)))
    (pcase (org-element-property :type timestamp)
      ((or `active `active-range)
       (format (plist-get info :texinfo-active-timestamp-format) value))
      ((or `inactive `inactive-range)
       (format (plist-get info :texinfo-inactive-timestamp-format) value))
      (_ (format (plist-get info :texinfo-diary-timestamp-format) value)))))

;;;; Underline

(defun org-texinfo-underline (_underline contents info)
  "Transcode UNDERLINE from Org to Texinfo.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-texinfo--text-markup contents 'underline info))

;;;; Verbatim

(defun org-texinfo-verbatim (verbatim _contents info)
  "Transcode a VERBATIM object from Org to Texinfo.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-texinfo--text-markup
   (org-element-property :value verbatim) 'verbatim info))

;;;; Verse Block

(defun org-texinfo-verse-block (verse-block contents _info)
  "Transcode a VERSE-BLOCK element from Org to Texinfo.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (org-texinfo--prepend-anchor-maybe
   (format "@display\n%s@end display" contents)
   verse-block))


;;; Public Functions

(defun org-texinfo-kbd-macro (key &optional noquote)
  "Quote KEY using @kbd{...} and if necessary @key{...}.

This is intended to be used as an Org macro like so:

  #+macro: kbd (eval (org-texinfo-kbd-macro $1))
  Type {{{kbd(C-c SPC)}}}.

Also see info node `(org)Key bindings in Texinfo export'.

If optional NOQOUTE is non-nil, then do not add the quoting
that is necessary when using this in an Org macro."
  (format (if noquote "@kbd{%s}" "@@texinfo:@kbd{@@%s@@texinfo:}@@")
	  (let ((case-fold-search nil))
	    (replace-regexp-in-string
	     org-texinfo--quoted-keys-regexp
	     (if noquote "@key{\\&}" "@@texinfo:@key{@@\\&@@texinfo:}@@")
	     key t))))

;;; Interactive Functions

;;;###autoload
(defun org-texinfo-export-to-texinfo
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Texinfo file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

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

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".texi" subtreep))
	(org-export-coding-system org-texinfo-coding-system))
    (org-export-to-file 'texinfo outfile
      async subtreep visible-only body-only ext-plist)))

(defun org-texinfo-export-to-texinfo-batch ()
  "Export Org file INFILE to Texinfo file OUTFILE, in batch mode.
Overwrites existing output file.
Usage: emacs -batch -f org-texinfo-export-to-texinfo-batch INFILE OUTFILE"
  (or noninteractive (user-error "Batch mode use only"))
  (let ((infile (pop command-line-args-left))
	(outfile (pop command-line-args-left))
	(org-export-coding-system org-texinfo-coding-system)
        (make-backup-files nil))
    (unless (file-readable-p infile)
      (message "File `%s' not readable" infile)
      (kill-emacs 1))
    (with-temp-buffer
      (insert-file-contents infile)
      (org-export-to-file 'texinfo outfile))))

;;;###autoload
(defun org-texinfo-export-to-info
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to Texinfo then process through to INFO.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

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

Return INFO file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".texi" subtreep))
	(org-export-coding-system org-texinfo-coding-system))
    (org-export-to-file 'texinfo outfile
      async subtreep visible-only body-only ext-plist
      #'org-texinfo-compile)))

;;;###autoload
(defun org-texinfo-publish-to-texinfo (plist filename pub-dir)
  "Publish an org file to Texinfo.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'texinfo filename ".texi" plist pub-dir))

;;;###autoload
(defun org-texinfo-convert-region-to-texinfo ()
  "Assume the current region has Org syntax, and convert it to Texinfo.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an Texinfo buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'texinfo))

(defalias 'org-export-region-to-texinfo #'org-texinfo-convert-region-to-texinfo)

(defun org-texinfo-compile (file)
  "Compile a texinfo file.

FILE is the name of the file being compiled.  Processing is done
through the command specified in `org-texinfo-info-process',
which see.  Output is redirected to \"*Org INFO Texinfo Output*\"
buffer.

Return INFO file name or an error if it couldn't be produced."
  (message "Processing Texinfo file %s..." file)
  (let* ((log-name "*Org INFO Texinfo Output*")
	 (log (get-buffer-create log-name))
	 (output
	  (org-compile-file file org-texinfo-info-process "info"
			    (format "See %S for details" log-name)
			    log)))
    (when org-texinfo-remove-logfiles
      (let ((base (file-name-sans-extension output)))
	(dolist (ext org-texinfo-logfiles-extensions)
	  (let ((file (concat base "." ext)))
	    (when (file-exists-p file) (delete-file file))))))
    (message "Process completed.")
    output))

(defun org-texinfo-supports-math-p ()
  "Return t if the installed version of Texinfo supports \"@math\".

Once computed, the results remain cached."
  (unless (boundp 'org-texinfo-supports-math--cache)
    (setq org-texinfo-supports-math--cache
          (let ((math-example "1 + 1 = 2"))
            (let* ((input-file (make-temp-file "test" nil ".texi"))
                   (output-file
                    (concat (file-name-sans-extension input-file) ".info"))
                   (input-content (string-join
                                   (list (format "@setfilename %s" output-file)
                                         "@node Top"
                                         "@displaymath"
                                         math-example
                                         "@end displaymath")
                                   "\n")))
              (with-temp-file input-file
                (insert input-content))
              (when-let* ((output-file
                           ;; If compilation fails, consider math to
                           ;; be not supported.
                           (ignore-errors (let ((inhibit-message t))
                                            (org-texinfo-compile input-file))))
                          (output-content (with-temp-buffer
                                            (insert-file-contents output-file)
                                            (buffer-string))))
                (let ((result (string-match-p (regexp-quote math-example)
                                              output-content)))
                  (delete-file input-file)
                  (delete-file output-file)
                  (if result t nil)))))))
  org-texinfo-supports-math--cache)

(provide 'ox-texinfo)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-texinfo.el ends here
