;;; org-export.el --- Generic Export Engine For Org

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

;; This library implements a generic export engine for Org, built on
;; its syntactical parser: Org Elements.

;; Besides that parser, the generic exporter is made of three distinct
;; parts:

;; - The communication channel consists in a property list, which is
;;   created and updated during the process.  Its use is to offer
;;   every piece of information, would it be export options or
;;   contextual data, all in a single place.  The exhaustive list of
;;   properties is given in "The Communication Channel" section of
;;   this file.

;; - The transcoder walks the parse tree, ignores or treat as plain
;;   text elements and objects according to export options, and
;;   eventually calls back-end specific functions to do the real
;;   transcoding, concatenating their return value along the way.

;; - The filter system is activated at the very beginning and the very
;;   end of the export process, and each time an element or an object
;;   has been converted.  It is the entry point to fine-tune standard
;;   output from back-end transcoders.

;; The core function is `org-export-as'.  It returns the transcoded
;; buffer as a string.

;; In order to derive an exporter out of this generic implementation,
;; one can define a transcode function for each element or object.
;; Such function should return a string for the corresponding element,
;; without any trailing space, or nil.  It must accept three
;; arguments:
;; 1. the element or object itself,
;; 2. its contents, or nil when it isn't recursive,
;; 3. the property list used as a communication channel.

;; If no such function is found, that element or object type will
;; simply be ignored, along with any separating blank line.  The same
;; will happen if the function returns the nil value.  If that
;; function returns the empty string, the type will be ignored, but
;; the blank lines will be kept.

;; Contents, when not nil, are stripped from any global indentation
;; (although the relative one is preserved).  They also always end
;; with a single newline character.

;; These functions must follow a strict naming convention:
;; `org-BACKEND-TYPE' where, obviously, BACKEND is the name of the
;; export back-end and TYPE the type of the element or object handled.

;; Moreover, two additional functions can be defined.  On the one
;; hand, `org-BACKEND-template' returns the final transcoded string,
;; and can be used to add a preamble and a postamble to document's
;; body.  It must accept two arguments: the transcoded string and the
;; property list containing export options.  On the other hand,
;; `org-BACKEND-plain-text', when defined, is to be called on every
;; text not recognized as an element or an object.  It must accept two
;; arguments: the text string and the information channel.

;; Any back-end can define its own variables.  Among them, those
;; customizables should belong to the `org-export-BACKEND' group.
;; Also, a special variable, `org-BACKEND-option-alist', allows to
;; define buffer keywords and "#+options:" items specific to that
;; back-end.  See `org-export-option-alist' for supported defaults and
;; syntax.

;; Tools for common tasks across back-ends are implemented in the
;; penultimate part of this file.  A dispatcher for standard back-ends
;; is provided in the last one.

;;; Code:
(eval-when-compile (require 'cl))
(require 'org-element)
;; Require major back-ends
(require 'org-e-ascii "../../EXPERIMENTAL/org-e-ascii.el")
(require 'org-e-latex "../../EXPERIMENTAL/org-e-latex.el")


;;; Internal Variables

;; Among internal variables, the most important is
;; `org-export-option-alist'.  This variable define the global export
;; options, shared between every exporter, and how they are acquired.

(defconst org-export-max-depth 19
  "Maximum nesting depth for headlines, counting from 0.")

(defconst org-export-option-alist
  '((:author "AUTHOR" nil user-full-name t)
    (:creator "CREATOR" nil org-export-creator-string)
    (:date "DATE" nil nil t)
    (:description "DESCRIPTION" nil nil newline)
    (:email "EMAIL" nil user-mail-address t)
    (:exclude-tags "EXPORT_EXCLUDE_TAGS" nil org-export-exclude-tags split)
    (:filter-babel-call nil nil org-export-filter-babel-call-functions)
    (:filter-center-block nil nil org-export-filter-center-block-functions)
    (:filter-comment nil nil org-export-filter-comment-functions)
    (:filter-comment-block nil nil org-export-filter-comment-block-functions)
    (:filter-drawer nil nil org-export-filter-drawer-functions)
    (:filter-dynamic-block nil nil org-export-filter-dynamic-block-functions)
    (:filter-emphasis nil nil org-export-filter-emphasis-functions)
    (:filter-entity nil nil org-export-filter-entity-functions)
    (:filter-example-block nil nil org-export-filter-example-block-functions)
    (:filter-export-block nil nil org-export-filter-export-block-functions)
    (:filter-export-snippet nil nil org-export-filter-export-snippet-functions)
    (:filter-final-output nil nil org-export-filter-final-output-functions)
    (:filter-fixed-width nil nil org-export-filter-fixed-width-functions)
    (:filter-footnote-definition nil nil org-export-filter-footnote-definition-functions)
    (:filter-footnote-reference nil nil org-export-filter-footnote-reference-functions)
    (:filter-headline nil nil org-export-filter-headline-functions)
    (:filter-horizontal-rule nil nil org-export-filter-horizontal-rule-functions)
    (:filter-inline-babel-call nil nil org-export-filter-inline-babel-call-functions)
    (:filter-inline-src-block nil nil org-export-filter-inline-src-block-functions)
    (:filter-inlinetask nil nil org-export-filter-inlinetask-functions)
    (:filter-item nil nil org-export-filter-item-functions)
    (:filter-keyword nil nil org-export-filter-keyword-functions)
    (:filter-latex-environment nil nil org-export-filter-latex-environment-functions)
    (:filter-latex-fragment nil nil org-export-filter-latex-fragment-functions)
    (:filter-line-break nil nil org-export-filter-line-break-functions)
    (:filter-link nil nil org-export-filter-link-functions)
    (:filter-macro nil nil org-export-filter-macro-functions)
    (:filter-paragraph nil nil org-export-filter-paragraph-functions)
    (:filter-parse-tree nil nil org-export-filter-parse-tree-functions)
    (:filter-plain-list nil nil org-export-filter-plain-list-functions)
    (:filter-plain-text nil nil org-export-filter-plain-text-functions)
    (:filter-property-drawer nil nil org-export-filter-property-drawer-functions)
    (:filter-quote-block nil nil org-export-filter-quote-block-functions)
    (:filter-quote-section nil nil org-export-filter-quote-section-functions)
    (:filter-radio-target nil nil org-export-filter-radio-target-functions)
    (:filter-section nil nil org-export-filter-section-functions)
    (:filter-special-block nil nil org-export-filter-special-block-functions)
    (:filter-src-block nil nil org-export-filter-src-block-functions)
    (:filter-statistics-cookie nil nil org-export-filter-statistics-cookie-functions)
    (:filter-subscript nil nil org-export-filter-subscript-functions)
    (:filter-superscript nil nil org-export-filter-superscript-functions)
    (:filter-table nil nil org-export-filter-table-functions)
    (:filter-target nil nil org-export-filter-target-functions)
    (:filter-time-stamp nil nil org-export-filter-time-stamp-functions)
    (:filter-verbatim nil nil org-export-filter-verbatim-functions)
    (:filter-verse-block nil nil org-export-filter-verse-block-functions)
    (:headline-levels nil "H" org-export-headline-levels)
    (:keywords "KEYWORDS" nil nil space)
    (:language "LANGUAGE" nil org-export-default-language t)
    (:preserve-breaks nil "\\n" org-export-preserve-breaks)
    (:section-numbers nil "num" org-export-with-section-numbers)
    (:select-tags "EXPORT_SELECT_TAGS" nil org-export-select-tags split)
    (:time-stamp-file nil "timestamp" org-export-time-stamp-file)
    (:title "TITLE" nil nil space)
    (:with-archived-trees nil "arch" org-export-with-archived-trees)
    (:with-author nil "author" org-export-with-author)
    (:with-creator nil "creator" org-export-with-creator)
    (:with-drawers nil "d" org-export-with-drawers)
    (:with-email nil "email" org-export-with-email)
    (:with-emphasize nil "*" org-export-with-emphasize)
    (:with-entities nil "e" org-export-with-entities)
    (:with-fixed-width nil ":" org-export-with-fixed-width)
    (:with-footnotes nil "f" org-export-with-footnotes)
    (:with-priority nil "pri" org-export-with-priority)
    (:with-special-strings nil "-" org-export-with-special-strings)
    (:with-sub-superscript nil "^" org-export-with-sub-superscripts)
    (:with-toc nil "toc" org-export-with-toc)
    (:with-tables nil "|" org-export-with-tables)
    (:with-tags nil "tags" org-export-with-tags)
    (:with-tasks nil "tasks" org-export-with-tasks)
    (:with-timestamps nil "<" org-export-with-timestamps)
    (:with-todo-keywords nil "todo" org-export-with-todo-keywords))
  "Alist between export properties and ways to set them.

The car of the alist is the property name, and the cdr is a list
like \(KEYWORD OPTION DEFAULT BEHAVIOUR\) where:

KEYWORD is a string representing a buffer keyword, or nil.
OPTION is a string that could be found in an #+OPTIONS: line.
DEFAULT is the default value for the property.
BEHAVIOUR determine how Org should handle multiple keywords for
the same property.  It is a symbol among:
  nil       Keep old value and discard the new one.
  t         Replace old value with the new one.
  `space'   Concatenate the values, separating them with a space.
  `newline' Concatenate the values, separating them with
	    a newline.
  `split'   Split values at white spaces, and cons them to the
	    previous list.

KEYWORD and OPTION have precedence over DEFAULT.

All these properties should be back-end agnostic.  For back-end
specific properties, define a similar variable named
`org-BACKEND-option-alist', replacing BACKEND with the name of
the appropriate back-end.  You can also redefine properties
there, as they have precedence over these.")

(defconst org-export-special-keywords
  '("SETUP_FILE" "OPTIONS" "MACRO")
  "List of in-buffer keywords that require special treatment.
These keywords are not directly associated to a property.  The
way they are handled must be hard-coded into
`org-export-get-inbuffer-options' function.")



;;; User-configurable Variables

;; Configuration for the masses.

;; They should never be evaled directly, as their value is to be
;; stored in a property list (cf. `org-export-option-alist').

(defgroup org-export nil
  "Options for exporting Org mode files."
  :tag "Org Export"
  :group 'org)

(defgroup org-export-general nil
  "General options for export engine."
  :tag "Org Export General"
  :group 'org-export)

(defcustom org-export-with-archived-trees 'headline
  "Whether sub-trees with the ARCHIVE tag should be exported.

This can have three different values:
nil         Do not export, pretend this tree is not present.
t           Do export the entire tree.
`headline'  Only export the headline, but skip the tree below it.

This option can also be set with the #+OPTIONS line,
e.g. \"arch:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Not at all" nil)
	  (const :tag "Headline only" 'headline)
	  (const :tag "Entirely" t)))

(defcustom org-export-with-author t
  "Non-nil means insert author name into the exported file.
This option can also be set with the #+OPTIONS line,
e.g. \"author:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-creator 'comment
  "Non-nil means the postamble should contain a creator sentence.

The sentence can be set in `org-export-creator-string' and
defaults to \"Generated by Org mode XX in Emacs XXX.\".

If the value is `comment' insert it as a comment."
  :group 'org-export-general
  :type '(choice
	  (const :tag "No creator sentence" nil)
	  (const :tag "Sentence as a comment" 'comment)
	  (const :tag "Insert the sentence" t)))

(defcustom org-export-creator-string
  (format "Generated by Org mode %s in Emacs %s." org-version emacs-version)
  "String to insert at the end of the generated document."
  :group 'org-export-general
  :type '(string :tag "Creator string"))

(defcustom org-export-with-drawers t
  "Non-nil means export contents of standard drawers.

When t, all drawers are exported.  This may also be a list of
drawer names to export.  This variable doesn't apply to
properties drawers.

This option can also be set with the #+OPTIONS line,
e.g. \"d:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "All drawers" t)
	  (const :tag "None" nil)
	  (repeat :tag "Selected drawers"
		  (string :tag "Drawer name"))))

(defcustom org-export-with-email nil
  "Non-nil means insert author email into the exported file.
This option can also be set with the #+OPTIONS line,
e.g. \"email:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-emphasize t
  "Non-nil means interpret *word*, /word/, and _word_ as emphasized text.

If the export target supports emphasizing text, the word will be
typeset in bold, italic, or underlined, respectively.  Not all
export backends support this.

This option can also be set with the #+OPTIONS line, e.g. \"*:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-exclude-tags '("noexport")
  "Tags that exclude a tree from export.
All trees carrying any of these tags will be excluded from
export.  This is without condition, so even subtrees inside that
carry one of the `org-export-select-tags' will be removed."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

(defcustom org-export-with-fixed-width t
  "Non-nil means lines starting with \":\" will be in fixed width font.

This can be used to have pre-formatted text, fragments of code
etc.  For example:
  : ;; Some Lisp examples
  : (while (defc cnt)
  :   (ding))
will be looking just like this in also HTML.  See also the QUOTE
keyword.  Not all export backends support this.

This option can also be set with the #+OPTIONS line, e.g. \"::nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-footnotes t
  "Non-nil means Org footnotes should be exported.
This option can also be set with the #+OPTIONS line,
e.g. \"f:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.

Inferior levels will produce itemize lists when exported.  Note
that a numeric prefix argument to an exporter function overrides
this setting.

This option can also be set with the #+OPTIONS line, e.g. \"H:2\"."
  :group 'org-export-general
  :type 'integer)

(defcustom org-export-default-language "en"
  "The default language for export and clocktable translations, as a string.
This may have an association in
`org-clock-clocktable-language-setup'."
  :group 'org-export-general
  :type '(string :tag "Language"))

(defcustom org-export-preserve-breaks nil
  "Non-nil means preserve all line breaks when exporting.

Normally, in HTML output paragraphs will be reformatted.

This option can also be set with the #+OPTIONS line,
e.g. \"\\n:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-entities t
  "Non-nil means interpret entities when exporting.

For example, HTML export converts \\alpha to &alpha; and \\AA to
&Aring;.

For a list of supported names, see the constant `org-entities'
and the user option `org-entities-user'.

This option can also be set with the #+OPTIONS line,
e.g. \"e:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-priority nil
  "Non-nil means include priority cookies in export.
When nil, remove priority cookies for export."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-section-numbers t
  "Non-nil means add section numbers to headlines when exporting.

When set to an integer n, numbering will only happen for
headlines whose relative level is higher or equal to n.

This option can also be set with the #+OPTIONS line,
e.g. \"num:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-select-tags '("export")
  "Tags that select a tree for export.
If any such tag is found in a buffer, all trees that do not carry
one of these tags will be deleted before export.  Inside trees
that are selected like this, you can still deselect a subtree by
tagging it with one of the `org-export-exclude-tags'."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

(defcustom org-export-with-special-strings t
  "Non-nil means interpret \"\-\", \"--\" and \"---\" for export.

When this option is turned on, these strings will be exported as:

  Org     HTML     LaTeX
 -----+----------+--------
  \\-    &shy;      \\-
  --    &ndash;    --
  ---   &mdash;    ---
  ...   &hellip;   \ldots

This option can also be set with the #+OPTIONS line,
e.g. \"-:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-sub-superscripts t
  "Non-nil means interpret \"_\" and \"^\" for export.

When this option is turned on, you can use TeX-like syntax for
sub- and superscripts.  Several characters after \"_\" or \"^\"
will be considered as a single item - so grouping with {} is
normally not needed.  For example, the following things will be
parsed as single sub- or superscripts.

 10^24   or   10^tau     several digits will be considered 1 item.
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible - so when in doubt use {} to enclose
the sub/superscript.  If you set this variable to the symbol
`{}', the braces are *required* in order to trigger
interpretations as sub/superscript.  This can be helpful in
documents that need \"_\" frequently in plain text.

This option can also be set with the #+OPTIONS line,
e.g. \"^:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Interpret them" t)
	  (const :tag "Curly brackets only" {})
	  (const :tag "Do not interpret them" nil)))

(defcustom org-export-with-toc t
  "Non-nil means create a table of contents in exported files.

The TOC contains headlines with levels up
to`org-export-headline-levels'.  When an integer, include levels
up to N in the toc, this may then be different from
`org-export-headline-levels', but it will not be allowed to be
larger than the number of headline levels.  When nil, no table of
contents is made.

This option can also be set with the #+OPTIONS line,
e.g. \"toc:nil\" or \"toc:3\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "No Table of Contents" nil)
	  (const :tag "Full Table of Contents" t)
	  (integer :tag "TOC to level")))

(defcustom org-export-with-tables t
  "If non-nil, lines starting with \"|\" define a table.
For example:

  | Name        | Address  | Birthday  |
  |-------------+----------+-----------|
  | Arthur Dent | England  | 29.2.2100 |

This option can also be set with the #+OPTIONS line, e.g. \"|:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-tags t
  "If nil, do not export tags, just remove them from headlines.

If this is the symbol `not-in-toc', tags will be removed from
table of contents entries, but still be shown in the headlines of
the document.

This option can also be set with the #+OPTIONS line,
e.g. \"tags:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Off" nil)
	  (const :tag "Not in TOC" not-in-toc)
	  (const :tag "On" t)))

(defcustom org-export-with-tasks t
  "Non-nil means include TODO items for export.
This may have the following values:
t                    include tasks independent of state.
todo                 include only tasks that are not yet done.
done                 include only tasks that are already done.
nil                  remove all tasks before export
list of keywords     keep only tasks with these keywords"
  :group 'org-export-general
  :type '(choice
	  (const :tag "All tasks" t)
	  (const :tag "No tasks" nil)
	  (const :tag "Not-done tasks" todo)
	  (const :tag "Only done tasks" done)
	  (repeat :tag "Specific TODO keywords"
		  (string :tag "Keyword"))))

(defcustom org-export-time-stamp-file t
  "Non-nil means insert a time stamp into the exported file.
The time stamp shows when the file was created.

This option can also be set with the #+OPTIONS line,
e.g. \"timestamp:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-timestamps t
  "If nil, do not export time stamps and associated keywords."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-todo-keywords t
  "Non-nil means include TODO keywords in export.
When nil, remove all these keywords from the export.")

(defcustom org-export-allow-BIND 'confirm
  "Non-nil means allow #+BIND to define local variable values for export.
This is a potential security risk, which is why the user must
confirm the use of these lines."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "Ask a confirmation for each file" confirm)))

(defcustom org-export-snippet-translation-alist nil
  "Alist between export snippets back-ends and exporter back-ends.

This variable allows to provide shortcuts for export snippets.

For example, with a value of '\(\(\"h\" . \"html\"\)\), the HTML
back-end will recognize the contents of \"@h{<b>}\" as HTML code
while every other back-end will ignore it."
  :group 'org-export-general
  :type '(repeat
	  (cons
	   (string :tag "Shortcut")
	   (string :tag "Back-end"))))

(defcustom org-export-coding-system nil
  "Coding system for the exported file."
  :group 'org-export-general
  :type 'coding-system)

(defcustom org-export-copy-to-kill-ring t
  "Non-nil means exported stuff will also be pushed onto the kill ring."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-initial-scope 'buffer
  "The initial scope when exporting with `org-export-dispatch'.
This variable can be either set to `buffer' or `subtree'."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Export current buffer" 'buffer)
	  (const :tag "Export current subtree" 'subtree)))

(defcustom org-export-show-temporary-export-buffer t
  "Non-nil means show buffer after exporting to temp buffer.
When Org exports to a file, the buffer visiting that file is ever
shown, but remains buried.  However, when exporting to a temporary
buffer, that buffer is popped up in a second window.  When this variable
is nil, the buffer remains buried also in these cases."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-dispatch-use-expert-ui nil
  "Non-nil means using a non-intrusive `org-export-dispatch'.
In that case, no help buffer is displayed.  Though, an indicator
for current export scope is added to the prompt \(i.e. \"b\" when
output is restricted to body only, \"s\" when it is restricted to
the current subtree and \"v\" when only visible elements are
considered for export\).  Also, \[?] allows to switch back to
standard mode."
  :group 'org-export-general
  :type 'boolean)



;;; The Communication Channel

;; During export process, every function has access to a number of
;; properties.  They are of three types:

;; 1. Export options are collected once at the very beginning of the
;;    process, out of the original buffer and environment.  The task
;;    is handled by `org-export-collect-options' function.
;;
;;    All export options are defined through the
;;    `org-export-option-alist' variable.
;;
;; 2. Tree properties are extracted directly from the parsed tree, by
;;    `org-export-collect-tree-properties' and depend on export
;;    options (whole trees may be filtered out of the export process).
;;
;; 3. Local options are updated during parsing, and their value
;;    depends on the level of recursion.  For now, only `:genealogy'
;;    belongs to that category.

;; Here is the full list of properties available during transcode
;; process, with their category (option, tree or local), their
;; value type and the function updating them, when appropriate.

;; + `author' :: Author's name.
;;   - category :: option
;;   - type :: string

;; + `back-end' :: Current back-end used for transcoding.
;;   - category :: tree
;;   - type :: symbol

;; + `creator' :: String to write as creation information.
;;   - category :: option
;;   - type :: string

;; + `date' :: String to use as date.
;;   - category :: option
;;   - type :: string

;; + `description' :: Description text for the current data.
;;   - category :: option
;;   - type :: string

;; + `email' :: Author's email.
;;   - category :: option
;;   - type :: string

;; + `exclude-tags' :: Tags for exclusion of subtrees from export
;;      process.
;;   - category :: option
;;   - type :: list of strings

;; + `footnote-definition-alist' :: Alist between footnote labels and
;;     their definition, as parsed data.  Only non-inlined footnotes
;;     are represented in this alist.  Also, every definition isn't
;;     guaranteed to be referenced in the parse tree.  The purpose of
;;     this property is to preserve definitions from oblivion
;;     (i.e. when the parse tree comes from a part of the original
;;     buffer), it isn't meant for direct use in a back-end.  To
;;     retrieve a definition relative to a reference, use
;;     `org-export-get-footnote-definition' instead.
;;   - category :: option
;;   - type :: alist (STRING . LIST)

;; + `genealogy' :: Flat list of current object or element's parents
;;      from closest to farthest.
;;   - category :: local
;;   - type :: list of elements and objects

;; + `headline-levels' :: Maximum level being exported as an
;;      headline.  Comparison is done with the relative level of
;;      headlines in the parse tree, not necessarily with their
;;      actual level.
;;   - category :: option
;;   - type :: integer

;; + `headline-offset' :: Difference between relative and real level
;;      of headlines in the parse tree.  For example, a value of -1
;;      means a level 2 headline should be considered as level
;;      1 (cf. `org-export-get-relative-level').
;;   - category :: tree
;;   - type :: integer

;; + `headline-numbering' :: Alist between headlines' beginning
;;      position and their numbering, as a list of numbers
;;      (cf. `org-export-get-headline-number').
;;   - category :: tree
;;   - type :: alist (INTEGER . LIST)

;; + `keywords' :: List of keywords attached to data.
;;   - category :: option
;;   - type :: string

;; + `language' :: Default language used for translations.
;;   - category :: option
;;   - type :: string

;; + `macro-input-file' :: File name of input file, or nil.
;;   - category :: option
;;   - type :: string or nil

;; + `parse-tree' :: Whole parse tree, available at any time during
;;                   transcoding.
;;   - category :: global
;;   - type :: list (as returned by `org-element-parse-buffer')

;; + `preserve-breaks' :: Non-nil means transcoding should preserve
;;      all line breaks.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `section-numbers' :: Non-nil means transcoding should add
;;      section numbers to headlines.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `select-tags' :: List of tags enforcing inclusion of sub-trees in
;;                    transcoding.  When such a tag is present,
;;                    subtrees without it are de facto excluded from
;;                    the process.  See `use-select-tags'.
;;   - category :: option
;;   - type :: list of strings

;; + `target-list' :: List of targets encountered in the parse tree.
;;                    This is used to partly resolve "fuzzy" links
;;                    (cf. `org-export-resolve-fuzzy-link').
;;   - category :: tree
;;   - type :: list of strings

;; + `time-stamp-file' :: Non-nil means transcoding should insert
;;      a time stamp in the output.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `use-select-tags' :: When non-nil, a select tags has been found
;;      in the parse tree.  Thus, any headline without one will be
;;      filtered out.  See `select-tags'.
;;   - category :: tree
;;   - type :: interger or nil

;; + `with-archived-trees' :: Non-nil when archived subtrees should
;;      also be transcoded.  If it is set to the `headline' symbol,
;;      only the archived headline's name is retained.
;;   - category :: option
;;   - type :: symbol (nil, t, `headline')

;; + `with-author' :: Non-nil means author's name should be included
;;                    in the output.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-creator' :: Non-nild means a creation sentence should be
;;      inserted at the end of the transcoded string.  If the value
;;      is `comment', it should be commented.
;;   - category :: option
;;   - type :: symbol (`comment', nil, t)

;; + `with-drawers' :: Non-nil means drawers should be exported.  If
;;      its value is a list of names, only drawers with such names
;;      will be transcoded.
;;   - category :: option
;;   - type :: symbol (nil, t) or list of strings

;; + `with-email' :: Non-nil means output should contain author's
;;                   email.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-emphasize' :: Non-nil means emphasized text should be
;;      interpreted.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-fixed-width' :: Non-nil if transcoder should interpret
;;      strings starting with a colon as a fixed-with (verbatim)
;;      area.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-footnotes' :: Non-nil if transcoder should interpret
;;      footnotes.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-priority' :: Non-nil means transcoding should include
;;      priority cookies.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-special-strings' :: Non-nil means transcoding should
;;      interpret special strings in plain text.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-sub-superscript' :: Non-nil means transcoding should
;;      interpret subscript and superscript.  With a value of "{}",
;;      only interpret those using curly brackets.
;;   - category :: option
;;   - type :: symbol (nil, {}, t)

;; + `with-tables' :: Non-nil means transcoding should interpret
;;                    tables.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-tags' :: Non-nil means transcoding should keep tags in
;;                  headlines.  A `not-in-toc' value will remove them
;;                  from the table of contents, if any, nonetheless.
;;   - category :: option
;;   - type :: symbol (nil, t, `not-in-toc')

;; + `with-tasks' :: Non-nil means transcoding should include
;;                   headlines with a TODO keyword.  A `todo' value
;;                   will only include headlines with a todo type
;;                   keyword while a `done' value will do the
;;                   contrary.  If a list of strings is provided, only
;;                   tasks with keywords belonging to that list will
;;                   be kept.
;;   - category :: option
;;   - type :: symbol (t, todo, done, nil) or list of strings

;; + `with-timestamps' :: Non-nil means transcoding should include
;;      time stamps and associated keywords.  Otherwise, completely
;;      remove them.
;;   - category :: option
;;   - type :: symbol: (t, nil)

;; + `with-toc' :: Non-nil means that a table of contents has to be
;;                 added to the output.  An integer value limits its
;;                 depth.
;;   - category :: option
;;   - type :: symbol (nil, t or integer)

;; + `with-todo-keywords' :: Non-nil means transcoding should
;;      include TODO keywords.
;;   - category :: option
;;   - type :: symbol (nil, t)

;;;; Export Options

;; Export options come from five sources, in increasing precedence
;; order:

;; - Global variables,
;; - External options provided at export time,
;; - Options keyword symbols,
;; - Buffer keywords,
;; - Subtree properties.

;; The central internal function with regards to export options is
;; `org-export-collect-options'.  It updates global variables with
;; "#+BIND:" keywords, then retrieve and prioritize properties from
;; the different sources.

;;  The internal functions doing the retrieval are:
;;  `org-export-parse-option-keyword' ,
;;  `org-export-get-subtree-options' ,
;;  `org-export-get-inbuffer-options' and
;;  `org-export-get-global-options'.
;;
;;  Some properties do not rely on the previous sources but still
;;  depend on the original buffer are taken care of in
;;  `org-export-initial-options'.

;; Also, `org-export-confirm-letbind' and `org-export-install-letbind'
;; take care of the part relative to "#+BIND:" keywords.

(defun org-export-collect-options (backend subtreep ext-plist)
  "Collect export options from the current buffer.

BACKEND is a symbol specifying the back-end to use.

When SUBTREEP is non-nil, assume the export is done against the
current sub-tree.

EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings."
  ;; First install #+BIND variables.
  (org-export-install-letbind-maybe)
  ;; Get and prioritize export options...
  (let ((options (org-combine-plists
		  ;; ... from global variables...
		  (org-export-get-global-options backend)
		  ;; ... from buffer's name (default title)...
		  `(:title
		    ,(or (let ((file (buffer-file-name (buffer-base-buffer))))
			   (and file
				(file-name-sans-extension
				 (file-name-nondirectory file))))
			 (buffer-name (buffer-base-buffer))))
		  ;; ... from an external property list...
		  ext-plist
		  ;; ... from in-buffer settings...
		  (org-export-get-inbuffer-options
		   (org-with-wide-buffer (buffer-string)) backend
		   (and buffer-file-name
			(org-remove-double-quotes buffer-file-name)))
		  ;; ... and from subtree, when appropriate.
		  (and subtreep
		       (org-export-get-subtree-options)))))
    ;; Add initial options.
    (setq options (append (org-export-initial-options options) options))
    ;; Return plist.
    options))

(defun org-export-parse-option-keyword (options backend)
  "Parse an OPTIONS line and return values as a plist.
BACKEND is a symbol specifying the back-end to use."
  (let* ((all (append org-export-option-alist
			 (let ((var (intern
				     (format "org-%s-option-alist" backend))))
			   (and (boundp var) (eval var)))))
	 ;; Build an alist between #+OPTION: item and property-name.
	 (alist (delq nil
		      (mapcar (lambda (e)
				(when (nth 2 e) (cons (regexp-quote (nth 2 e))
						      (car e))))
			      all)))
	 plist)
    (mapc (lambda (e)
	    (when (string-match (concat "\\(\\`\\|[ \t]\\)"
					(car e)
					":\\(([^)\n]+)\\|[^ \t\n\r;,.]*\\)")
				options)
	      (setq plist (plist-put plist
				     (cdr e)
				     (car (read-from-string
					   (match-string 2 options)))))))
	  alist)
    plist))

(defun org-export-get-subtree-options ()
  "Get export options in subtree at point.

Assume point is at subtree's beginning.

Return options as a plist."
  (let (prop plist)
    (when (setq prop (progn (looking-at org-todo-line-regexp)
			    (or (save-match-data
				  (org-entry-get (point) "EXPORT_TITLE"))
				(org-match-string-no-properties 3))))
      (setq plist
	    (plist-put
	     plist :title
	     (org-element-parse-secondary-string
	      prop
	      (cdr (assq 'keyword org-element-string-restrictions))))))
    (when (setq prop (org-entry-get (point) "EXPORT_TEXT"))
      (setq plist (plist-put plist :text prop)))
    (when (setq prop (org-entry-get (point) "EXPORT_AUTHOR"))
      (setq plist (plist-put plist :author prop)))
    (when (setq prop (org-entry-get (point) "EXPORT_DATE"))
      (setq plist (plist-put plist :date prop)))
    (when (setq prop (org-entry-get (point) "EXPORT_OPTIONS"))
      (setq plist (org-export-add-options-to-plist plist prop)))
    plist))

(defun org-export-get-inbuffer-options (buffer-string backend files)
  "Return in-buffer options as a plist.
BUFFER-STRING is the string of the buffer.  BACKEND is a symbol
specifying which back-end should be used.  FILES is a list of
setup files names read so far, used to avoid circular
dependencies."
  (let ((case-fold-search t) plist)
    ;; 1. Special keywords, as in `org-export-special-keywords'.
    (let ((start 0)
	  (special-re (org-make-options-regexp org-export-special-keywords)))
      (while (string-match special-re buffer-string start)
	(setq start (match-end 0))
	(let ((key (upcase (org-match-string-no-properties 1 buffer-string)))
	      ;; Special keywords do not have their value expanded.
	      (val (org-match-string-no-properties 2 buffer-string)))
	  (setq plist
		(org-combine-plists
		 (cond
		  ((string= key "SETUP_FILE")
		   (let ((file (expand-file-name
				(org-remove-double-quotes (org-trim val)))))
		     ;; Avoid circular dependencies.
		     (unless (member file files)
		       (org-export-get-inbuffer-options
			(org-file-contents file 'noerror)
			backend
			(cons file files)))))
		  ((string= key "OPTIONS")
		   (org-export-parse-option-keyword val backend))
		  ((string= key "MACRO")
		   (when (string-match
			  "^\\([-a-zA-Z0-9_]+\\)\\(?:[ \t]+\\(.*?\\)[ \t]*$\\)?"
			  val)
		     (let ((key (intern
				 (concat ":macro-"
					 (downcase (match-string 1 val)))))
			   (value (match-string 2 val)))
		       (cond
			((not value) "")
			((string-match "\\`(eval\\>" value) (list key value))
			(t
			 (list
			  key
			  ;; If user explicitly asks for a newline, be
			  ;; sure to preserve it from further filling
			  ;; with `hard-newline'.
			  (replace-regexp-in-string
			   "\\\\n" hard-newline value))))))))
		 plist)))))
    ;; 2. Standard options, as in `org-export-option-alist'.
    (let* ((all (append org-export-option-alist
			(let ((var (intern
				    (format "org-%s-option-alist" backend))))
			  (and (boundp var) (eval var)))))
	   ;; Build alist between keyword name and property name.
	   (alist (delq nil (mapcar (lambda (e)
				      (when (nth 1 e) (cons (nth 1 e) (car e))))
				    all)))
	   ;; Build regexp matching all keywords associated to export
	   ;; options.  Note: the search is case insensitive.
	   (opt-re (org-make-options-regexp
		    (delq nil (mapcar (lambda (e) (nth 1 e)) all))))
	   (start 0))
      (while (string-match opt-re buffer-string start)
	(setq start (match-end 0))
	(let* ((key (upcase (org-match-string-no-properties 1 buffer-string)))
	       ;; Expand value, applying restrictions for keywords.
	       (val (org-match-string-no-properties 2 buffer-string))
	       (prop (cdr (assoc key alist)))
	       (behaviour (nth 4 (assq prop all))))
	  (setq plist
		(plist-put
		 plist prop
		 ;; Handle value depending on specified BEHAVIOUR.
		 (case behaviour
		   (space (if (plist-get plist prop)
			      (concat (plist-get plist prop) " " (org-trim val))
			    (org-trim val)))
		   (newline (org-trim
			     (concat
			      (plist-get plist prop) "\n" (org-trim val))))
		   (split `(,@(plist-get plist prop) ,@(org-split-string val)))
		   ('t val)
		   (otherwise (plist-get plist prop)))))))
      ;; Parse keywords specified in `org-element-parsed-keywords'.
      (mapc
       (lambda (key)
	 (let* ((prop (cdr (assoc (upcase key) alist)))
		(value (and prop (plist-get plist prop))))
	   (when (stringp value)
	     (setq plist
		   (plist-put
		    plist prop
		    (org-element-parse-secondary-string
		     value
		     (cdr (assq 'keyword org-element-string-restrictions))))))))
       org-element-parsed-keywords))
    ;; Return final value.
    plist))

(defun org-export-get-global-options (backend)
  "Return global export options as a plist.
BACKEND is a symbol specifying which back-end should be used."
  (let ((all (append org-export-option-alist
		     (let ((var (intern
				 (format "org-%s-option-alist" backend))))
		       (and (boundp var) (eval var)))))
	;; Output value.
	plist)
    (mapc (lambda (cell)
	    (setq plist
		  (plist-put plist (car cell) (eval (nth 3 cell)))))
	  all)
    ;; Return value.
    plist))

(defun org-export-initial-options (options)
  "Return a plist with non-optional properties.
OPTIONS is the export options plist computed so far."
  (list
   ;; `:macro-date', `:macro-time' and `:macro-property' could as well
   ;; be initialized as tree properties, since they don't depend on
   ;; initial environment.  Though, it may be more logical to keep
   ;; them close to other ":macro-" properties.
   :macro-date "(eval (format-time-string \"$1\"))"
   :macro-time "(eval (format-time-string \"$1\"))"
   :macro-property "(eval (org-entry-get nil \"$1\" 'selective))"
   :macro-modification-time
   (and (buffer-file-name (buffer-base-buffer))
	(file-exists-p (buffer-file-name (buffer-base-buffer)))
	(concat "(eval (format-time-string \"$1\" '"
		(prin1-to-string
		 (nth 5 (file-attributes
			 (buffer-file-name (buffer-base-buffer)))))
		"))"))
   ;; Store input file name.
   :macro-input-file (and (buffer-file-name (buffer-base-buffer))
			  (file-name-nondirectory
			   (buffer-file-name (buffer-base-buffer))))
   ;; Footnotes definitions must be collected in the original buffer,
   ;; as there's no insurance that they will still be in the parse
   ;; tree, due to some narrowing.
   :footnote-definition-alist
   (let (alist)
     (org-with-wide-buffer
      (goto-char (point-min))
      (while (re-search-forward org-footnote-definition-re nil t)
	(let ((def (org-footnote-at-definition-p)))
	  (when def
	    (org-skip-whitespace)
	    (push (cons (car def)
			(save-restriction
			  (narrow-to-region (point) (nth 2 def))
			  ;; Like `org-element-parse-buffer', but
			  ;; makes sure the definition doesn't start
			  ;; with a section element.
			  (nconc
			   (list 'org-data nil)
			   (org-element-parse-elements
			    (point-min) (point-max) nil nil nil nil nil))))
		  alist))))
      alist))))

(defvar org-export-allow-BIND-local nil)
(defun org-export-confirm-letbind ()
  "Can we use #+BIND values during export?
By default this will ask for confirmation by the user, to divert
possible security risks."
  (cond
   ((not org-export-allow-BIND) nil)
   ((eq org-export-allow-BIND t) t)
   ((local-variable-p 'org-export-allow-BIND-local) org-export-allow-BIND-local)
   (t (org-set-local 'org-export-allow-BIND-local
		     (yes-or-no-p "Allow BIND values in this buffer? ")))))

(defun org-export-install-letbind-maybe ()
  "Install the values from #+BIND lines as local variables.
Variables must be installed before in-buffer options are
retrieved."
  (let (letbind pair)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward (org-make-options-regexp '("BIND")) nil t)
       (when (org-export-confirm-letbind)
	 (push (read (concat "(" (org-match-string-no-properties 2) ")"))
	       letbind))))
    (while (setq pair (pop letbind))
      (org-set-local (car pair) (nth 1 pair)))))


;;;; Tree Properties

;; They are initialized at the beginning of the transcoding process by
;; `org-export-collect-tree-properties'.

;; Dedicated functions focus on computing the value of specific tree
;; properties during initialization.  Thus,
;; `org-export-use-select-tag-p' determines if an headline makes use
;; of an export tag enforcing inclusion. `org-export-get-min-level'
;; gets the minimal exportable level, used as a basis to compute
;; relative level for headlines. `org-export-get-point-max' returns
;; the maximum exportable ending position in the parse tree.
;; Eventually `org-export-collect-headline-numbering' builds an alist
;; between headlines' beginning position and their numbering.

(defun org-export-collect-tree-properties (data info backend)
  "Extract tree properties from parse tree.

DATA is the parse tree from which information is retrieved.  INFO
is a list holding export options.  BACKEND is the back-end called
for transcoding, as a symbol.

Following tree properties are set:
`:back-end'        Back-end used for transcoding.

`:headline-offset' Offset between true level of headlines and
		   local level.  An offset of -1 means an headline
		   of level 2 should be considered as a level
		   1 headline in the context.

`:headline-numbering' Alist of all headlines' beginning position
		   as key an the associated numbering as value.

`:parse-tree'      Whole parse tree.

`:target-list'     List of all targets in the parse tree.

`:use-select-tags' Non-nil when parsed tree use a special tag to
		   enforce transcoding of the headline."
  ;; First, set `:use-select-tags' property, as it will be required
  ;; for further computations.
  (setq info
	(org-combine-plists
	 info `(:use-select-tags ,(org-export-use-select-tags-p data info))))
  ;; Then get `:headline-offset' in order to be able to use
  ;; `org-export-get-relative-level'.
  (setq info
	(org-combine-plists
	 info `(:headline-offset ,(- 1 (org-export-get-min-level data info)))))
  ;; Now, get the rest of the tree properties, now `:use-select-tags'
  ;; is set...
  (nconc
   `(:parse-tree
     ,data
     :target-list
     ,(org-element-map data 'target (lambda (target local) target) info)
     :headline-numbering ,(org-export-collect-headline-numbering data info)
     :back-end ,backend)
   info))

(defun org-export-use-select-tags-p (data options)
  "Non-nil when data use a tag enforcing transcoding.
DATA is parsed data as returned by `org-element-parse-buffer'.
OPTIONS is a plist holding export options."
  (org-element-map
   data
   'headline
   (lambda (headline info)
     (let ((tags (org-element-get-property :with-tags headline)))
       (and tags (string-match
		  (format ":%s:" (plist-get info :select-tags)) tags))))
   options
   'stop-at-first-match))

(defun org-export-get-min-level (data options)
  "Return minimum exportable headline's level in DATA.
DATA is parsed tree as returned by `org-element-parse-buffer'.
OPTIONS is a plist holding export options."
  (catch 'exit
    (let ((min-level 10000))
      (mapc (lambda (blob)
	      (when (and (eq (car blob) 'headline)
			 (not (org-export-skip-p blob options)))
		(setq min-level
		      (min (org-element-get-property :level blob) min-level)))
	      (when (= min-level 1) (throw 'exit 1)))
	    (org-element-get-contents data))
      ;; If no headline was found, for the sake of consistency, set
      ;; minimum level to 1 nonetheless.
      (if (= min-level 10000) 1 min-level))))

(defun org-export-collect-headline-numbering (data options)
  "Return numbering of all exportable headlines in a parse tree.

DATA is the parse tree.  OPTIONS is the plist holding export
options.

Return an alist whose key is an headline and value is its
associated numbering \(in the shape of a list of numbers\)."
  (let ((numbering (make-vector org-export-max-depth 0)))
    (org-element-map
     data
     'headline
     (lambda (headline info)
       (let ((relative-level
	      (1- (org-export-get-relative-level headline info))))
	 (cons
	  headline
	  (loop for n across numbering
		for idx from 0 to org-export-max-depth
		when (< idx relative-level) collect n
		when (= idx relative-level) collect (aset numbering idx (1+ n))
		when (> idx relative-level) do (aset numbering idx 0)))))
     options)))



;;; The Transcoder

;; This function reads Org data (obtained with, i.e.
;; `org-element-parse-buffer') and transcodes it into a specified
;; back-end output.  It takes care of updating local properties,
;; filtering out elements or objects according to export options and
;; organizing the output blank lines and white space are preserved.

;; Though, this function is inapropriate for secondary strings, which
;; require a fresh copy of the plist passed as INFO argument.  Thus,
;; `org-export-secondary-string' is provided for that specific task.

;; Internally, three functions handle the filtering of objects and
;; elements during the export.  More precisely, `org-export-skip-p'
;; determines if the considered object or element should be ignored
;; altogether, `org-export-interpret-p' tells which elements or
;; objects should be seen as real Org syntax and `org-export-expand'
;; transforms the others back into their original shape.

(defun org-export-data (data backend info)
  "Convert DATA to a string into BACKEND format.

DATA is a nested list as returned by `org-element-parse-buffer'.

BACKEND is a symbol among supported exporters.

INFO is a plist holding export options and also used as
a communication channel between elements when walking the nested
list.  See `org-export-update-info' function for more
details.

Return transcoded string."
  (mapconcat
   ;; BLOB can be an element, an object, a string, or nil.
   (lambda (blob)
     (cond
      ((not blob) nil)
      ;; BLOB is a string.  Check if the optional transcoder for plain
      ;; text exists, and call it in that case.  Otherwise, simply
      ;; return string.  Also update INFO and call
      ;; `org-export-filter-plain-text-functions'.
      ((stringp blob)
       (let ((transcoder (intern (format "org-%s-plain-text" backend))))
	 (org-export-filter-apply-functions
	  (plist-get info :filter-plain-text)
	  (if (fboundp transcoder) (funcall transcoder blob info) blob)
	  backend info)))
      ;; BLOB is an element or an object.
      (t
       (let* ((type (if (stringp blob) 'plain-text (car blob)))
	      ;; 1. Determine the appropriate TRANSCODER.
	      (transcoder
	       (cond
		;; 1.0 A full Org document is inserted.
		((eq type 'org-data) 'identity)
		;; 1.1. BLOB should be ignored.
		((org-export-skip-p blob info) nil)
		;; 1.2. BLOB shouldn't be transcoded.  Interpret it
		;;      back into Org syntax.
		((not (org-export-interpret-p blob info))
		 'org-export-expand)
		;; 1.3. Else apply naming convention.
		(t (let ((trans (intern
				 (format "org-%s-%s" backend type))))
		     (and (fboundp trans) trans)))))
	      ;; 2. Compute CONTENTS of BLOB.
	      (contents
	       (cond
		;; Case 0. No transcoder defined: ignore BLOB.
		((not transcoder) nil)
		;; Case 1. Transparently export an Org document.
		((eq type 'org-data) (org-export-data blob backend info))
		;; Case 2. For a recursive object.
		((memq type org-element-recursive-objects)
		 (org-export-data
		  blob backend
		  (org-combine-plists
		   info
		   `(:genealogy ,(cons blob (plist-get info :genealogy))))))
		;; Case 3. For a recursive element.
		((memq type org-element-greater-elements)
		 ;; Ignore contents of an archived tree
		 ;; when `:with-archived-trees' is `headline'.
		 (unless (and
			  (eq type 'headline)
			  (eq (plist-get info :with-archived-trees) 'headline)
			  (org-element-get-property :archivedp blob))
		   (org-element-normalize-string
		    (org-export-data
		     blob backend
		     (org-combine-plists
		      info `(:genealogy
			     ,(cons blob (plist-get info :genealogy))))))))
		;; Case 4. For a paragraph.
		((eq type 'paragraph)
		 (let ((paragraph
			(org-element-normalize-contents
			 blob
			 ;; When normalizing contents of an item or
			 ;; a footnote definition, ignore first line's
			 ;; indentation: there is none and it might be
			 ;; misleading.
			 (and (not (org-export-get-previous-element blob info))
			      (let ((parent (caar (plist-get info :genealogy))))
				(memq parent '(footnote-definition item)))))))
		   (org-export-data
		    paragraph backend
		    (org-combine-plists
		     info `(:genealogy
			    ,(cons paragraph (plist-get info :genealogy)))))))))
	      ;; 3. Transcode BLOB into RESULTS string.
	      (results (cond
			((not transcoder) nil)
			((eq transcoder 'org-export-expand)
			 (org-export-data
			  `(org-data nil ,(funcall transcoder blob contents))
			  backend info))
			(t (funcall transcoder blob contents info)))))
	 ;; 4. Discard nil results.  Otherwise, update INFO, append
	 ;;    the same white space between elements or objects as in
	 ;;    the original buffer, and call appropriate filters.
	 (when results
	   ;; No filter for a full document.
	   (if (eq type 'org-data) results
	     (org-export-filter-apply-functions
	      (plist-get info (intern (format ":filter-%s" type)))
	      (let ((post-blank (org-element-get-property :post-blank blob)))
		(if (memq type org-element-all-elements)
		    (concat (org-element-normalize-string results)
			    (make-string post-blank ?\n))
		  (concat results (make-string post-blank ? ))))
	      backend info)))))))
   (org-element-get-contents data) ""))

(defun org-export-secondary-string (secondary backend info)
  "Convert SECONDARY string into BACKEND format.

SECONDARY is a nested list as returned by
`org-element-parse-secondary-string'.

BACKEND is a symbol among supported exporters.  INFO is a plist
used as a communication channel.

Return transcoded string."
  ;; Make SECONDARY acceptable for `org-export-data'.
  (let ((s (if (listp secondary) secondary (list secondary))))
    (org-export-data `(org-data nil ,@s) backend (copy-sequence info))))

(defun org-export-skip-p (blob info)
  "Non-nil when element or object BLOB should be skipped during export.
INFO is the plist holding export options."
  ;; Check headline.
  (unless (stringp blob)
    (case (car blob)
      ('headline
       (let ((with-tasks (plist-get info :with-tasks))
	     (todo (org-element-get-property :todo-keyword blob))
	     (todo-type (org-element-get-property :todo-type blob))
	     (archived (plist-get info :with-archived-trees))
	     (tag-list (let ((tags (org-element-get-property :tags blob)))
			 (and tags (org-split-string tags ":")))))
	 (or
	  ;; Ignore subtrees with an exclude tag.
	  (loop for k in (plist-get info :exclude-tags)
		thereis (member k tag-list))
	  ;; Ignore subtrees without a select tag, when such tag is found
	  ;; in the buffer.
	  (and (plist-get info :use-select-tags)
	       (loop for k in (plist-get info :select-tags)
		     never (member k tag-list)))
	  ;; Ignore commented sub-trees.
	  (org-element-get-property :commentedp blob)
	  ;; Ignore archived subtrees if `:with-archived-trees' is nil.
	  (and (not archived) (org-element-get-property :archivedp blob))
	  ;; Ignore tasks, if specified by `:with-tasks' property.
	  (and todo (not with-tasks))
	  (and todo
	       (memq with-tasks '(todo done))
	       (not (eq todo-type with-tasks)))
	  (and todo
	       (consp with-tasks)
	       (not (member todo with-tasks))))))
      ;; Check time-stamp.
      ('time-stamp (not (plist-get info :with-timestamps)))
      ;; Check drawer.
      ('drawer
       (or (not (plist-get info :with-drawers))
	   (and (consp (plist-get info :with-drawers))
		(not (member (org-element-get-property :drawer-name blob)
			     (plist-get info :with-drawers))))))
      ;; Check export snippet.
      ('export-snippet
       (let* ((raw-back-end (org-element-get-property :back-end blob))
	      (true-back-end
	       (or (cdr (assoc raw-back-end org-export-snippet-translation-alist))
		   raw-back-end)))
	 (not (string= (symbol-name (plist-get info :back-end))
		       true-back-end)))))))

(defun org-export-interpret-p (blob info)
  "Non-nil if element or object BLOB should be interpreted as Org syntax.
Check is done according to export options INFO, stored as
a plist."
  (case (car blob)
    ;; ... entities...
    (entity (plist-get info :with-entities))
    ;; ... emphasis...
    (emphasis (plist-get info :with-emphasize))
    ;; ... fixed-width areas.
    (fixed-width (plist-get info :with-fixed-width))
    ;; ... footnotes...
    ((footnote-definition footnote-reference)
     (plist-get info :with-footnotes))
    ;; ... sub/superscripts...
    ((subscript superscript)
     (let ((sub/super-p (plist-get info :with-sub-superscript)))
       (if (eq sub/super-p '{})
	   (org-element-get-property :use-brackets-p blob)
	 sub/super-p)))
    ;; ... tables...
    (table (plist-get info :with-tables))
    (otherwise t)))

(defsubst org-export-expand (blob contents)
  "Expand a parsed element or object to its original state.
BLOB is either an element or an object.  CONTENTS is its
contents, as a string or nil."
  (funcall
   (intern (format "org-element-%s-interpreter" (car blob))) blob contents))



;;; The Filter System

;; Filters allow end-users to tweak easily the transcoded output.
;; They are the functional counterpart of hooks, as every filter in
;; a set is applied to the return value of the previous one.

;; Every set is back-end agnostic.  Although, a filter is always
;; called, in addition to the string it applies to, with the back-end
;; used as argument, so it's easy enough for the end-user to add
;; back-end specific filters in the set.  The communication channel,
;; as a plist, is required as the third argument.

;; Filters sets are defined below. There are of four types:

;; - `org-export-filter-parse-tree-functions' applies directly on the
;;   complete parsed tree.  It's the only filters set that doesn't
;;   apply to a string.
;; - `org-export-filter-final-output-functions' applies to the final
;;   transcoded string.
;; - `org-export-filter-plain-text-functions' applies to any string
;;   not recognized as Org syntax.
;; - `org-export-filter-TYPE-functions' applies on the string returned
;;   after an element or object of type TYPE has been transcoded.

;; All filters sets are applied through
;; `org-export-filter-apply-functions' function.  Filters in a set are
;; applied in a LIFO fashion.  It allows developers to be sure that
;; their filters will be applied first.

;;;; Special Filters
(defvar org-export-filter-parse-tree-functions nil
  "Filter, or list of filters, applied to the parsed tree.
Each filter is called with three arguments: the parse tree, as
returned by `org-element-parse-buffer', the back-end, as
a symbol, and the communication channel, as a plist.  It must
return the modified parse tree to transcode.")

(defvar org-export-filter-final-output-functions nil
  "Filter, or list of filters, applied to the transcoded string.
Each filter is called with three arguments: the full transcoded
string, the back-end, as a symbol, and the communication channel,
as a plist.  It must return a string that will be used as the
final export output.")

(defvar org-export-filter-plain-text-functions nil
  "Filter, or list of filters, applied to plain text.
Each filter is called with three arguments: a string which
contains no Org syntax, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")


;;;; Elements Filters

(defvar org-export-filter-center-block-functions nil
  "List of functions applied to a transcoded center block.
Each filter is called with three arguments: the transcoded center
block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-drawer-functions nil
  "List of functions applied to a transcoded drawer.
Each filter is called with three arguments: the transcoded
drawer, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-dynamic-block-functions nil
  "List of functions applied to a transcoded dynamic-block.
Each filter is called with three arguments: the transcoded
dynamic-block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-headline-functions nil
  "List of functions applied to a transcoded headline.
Each filter is called with three arguments: the transcoded
headline, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-inlinetask-functions nil
  "List of functions applied to a transcoded inlinetask.
Each filter is called with three arguments: the transcoded
inlinetask, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-plain-list-functions nil
  "List of functions applied to a transcoded plain-list.
Each filter is called with three arguments: the transcoded
plain-list, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-item-functions nil
  "List of functions applied to a transcoded item.
Each filter is called with three arguments: the transcoded item,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-comment-functions nil
  "List of functions applied to a transcoded comment.
Each filter is called with three arguments: the transcoded
comment, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-comment-block-functions nil
  "List of functions applied to a transcoded comment-comment.
Each filter is called with three arguments: the transcoded
comment-block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-example-block-functions nil
  "List of functions applied to a transcoded example-block.
Each filter is called with three arguments: the transcoded
example-block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-export-block-functions nil
  "List of functions applied to a transcoded export-block.
Each filter is called with three arguments: the transcoded
export-block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-fixed-width-functions nil
  "List of functions applied to a transcoded fixed-width.
Each filter is called with three arguments: the transcoded
fixed-width, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-footnote-definition-functions nil
  "List of functions applied to a transcoded footnote-definition.
Each filter is called with three arguments: the transcoded
footnote-definition, as a string, the back-end, as a symbol, and
the communication channel, as a plist.  It must return a string
or nil.")

(defvar org-export-filter-horizontal-rule-functions nil
  "List of functions applied to a transcoded horizontal-rule.
Each filter is called with three arguments: the transcoded
horizontal-rule, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-keyword-functions nil
  "List of functions applied to a transcoded keyword.
Each filter is called with three arguments: the transcoded
keyword, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-latex-environment-functions nil
  "List of functions applied to a transcoded latex-environment.
Each filter is called with three arguments: the transcoded
latex-environment, as a string, the back-end, as a symbol, and
the communication channel, as a plist.  It must return a string
or nil.")

(defvar org-export-filter-babel-call-functions nil
  "List of functions applied to a transcoded babel-call.
Each filter is called with three arguments: the transcoded
babel-call, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-paragraph-functions nil
  "List of functions applied to a transcoded paragraph.
Each filter is called with three arguments: the transcoded
paragraph, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-property-drawer-functions nil
  "List of functions applied to a transcoded property-drawer.
Each filter is called with three arguments: the transcoded
property-drawer, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-quote-block-functions nil
  "List of functions applied to a transcoded quote block.
Each filter is called with three arguments: the transcoded quote
block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-quote-section-functions nil
  "List of functions applied to a transcoded quote-section.
Each filter is called with three arguments: the transcoded
quote-section, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-section-functions nil
  "List of functions applied to a transcoded section.
Each filter is called with three arguments: the transcoded
section, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-special-block-functions nil
  "List of functions applied to a transcoded special block.
Each filter is called with three arguments: the transcoded
special block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-src-block-functions nil
  "List of functions applied to a transcoded src-block.
Each filter is called with three arguments: the transcoded
src-block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-table-functions nil
  "List of functions applied to a transcoded table.
Each filter is called with three arguments: the transcoded table,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-verse-block-functions nil
  "List of functions applied to a transcoded verse block.
Each filter is called with three arguments: the transcoded verse
block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")


;;;; Objects Filters

(defvar org-export-filter-emphasis-functions nil
  "List of functions applied to a transcoded emphasis.
Each filter is called with three arguments: the transcoded
emphasis, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-entity-functions nil
  "List of functions applied to a transcoded entity.
Each filter is called with three arguments: the transcoded
entity, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-export-snippet-functions nil
  "List of functions applied to a transcoded export-snippet.
Each filter is called with three arguments: the transcoded
export-snippet, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-footnote-reference-functions nil
  "List of functions applied to a transcoded footnote-reference.
Each filter is called with three arguments: the transcoded
footnote-reference, as a string, the back-end, as a symbol, and
the communication channel, as a plist.  It must return a string
or nil.")

(defvar org-export-filter-inline-babel-call-functions nil
  "List of functions applied to a transcoded inline-babel-call.
Each filter is called with three arguments: the transcoded
inline-babel-call, as a string, the back-end, as a symbol, and
the communication channel, as a plist.  It must return a string
or nil.")

(defvar org-export-filter-inline-src-block-functions nil
  "List of functions applied to a transcoded inline-src-block.
Each filter is called with three arguments: the transcoded
inline-src-block, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-latex-fragment-functions nil
  "List of functions applied to a transcoded latex-fragment.
Each filter is called with three arguments: the transcoded
latex-fragment, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-line-break-functions nil
  "List of functions applied to a transcoded line-break.
Each filter is called with three arguments: the transcoded
line-break, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-link-functions nil
  "List of functions applied to a transcoded link.
Each filter is called with three arguments: the transcoded link,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-macro-functions nil
  "List of functions applied to a transcoded macro.
Each filter is called with three arguments: the transcoded macro,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-radio-target-functions nil
  "List of functions applied to a transcoded radio-target.
Each filter is called with three arguments: the transcoded
radio-target, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-statistics-cookie-functions nil
  "List of functions applied to a transcoded statistics-cookie.
Each filter is called with three arguments: the transcoded
statistics-cookie, as a string, the back-end, as a symbol, and
the communication channel, as a plist.  It must return a string
or nil.")

(defvar org-export-filter-subscript-functions nil
  "List of functions applied to a transcoded subscript.
Each filter is called with three arguments: the transcoded
subscript, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-superscript-functions nil
  "List of functions applied to a transcoded superscript.
Each filter is called with three arguments: the transcoded
superscript, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-target-functions nil
  "List of functions applied to a transcoded target.
Each filter is called with three arguments: the transcoded
target, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-time-stamp-functions nil
  "List of functions applied to a transcoded time-stamp.
Each filter is called with three arguments: the transcoded
time-stamp, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-verbatim-functions nil
  "List of functions applied to a transcoded verbatim.
Each filter is called with three arguments: the transcoded
verbatim, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defun org-export-filter-apply-functions (filters value backend info)
  "Call every function in FILTERS with arguments VALUE, BACKEND and INFO.
Functions are called in a LIFO fashion, to be sure that developer
specified filters, if any, are called first."
  (loop for filter in filters
	if (not value) return nil else
	do (setq value (funcall filter value backend info)))
  value)



;;; Core functions

;; This is the room for the main function, `org-export-as', along with
;; its derivatives, `org-export-to-buffer' and `org-export-to-file'.
;; They differ only by the way they output the resulting code.

;; `org-export-output-file-name' is an auxiliary function meant to be
;; used with `org-export-to-file'.  With a given extension, it tries
;; to provide a canonical file name to write export output to.

;; Note that `org-export-as' doesn't really parse the current buffer,
;; but a copy of it (with the same buffer-local variables and
;; visibility), where include keywords are expanded and Babel blocks
;; are executed, if appropriate.
;; `org-export-with-current-buffer-copy' macro prepares that copy.

;; File inclusion is taken care of by
;; `org-export-expand-include-keyword' and
;; `org-export-prepare-file-contents'.  Structure wise, including
;; a whole Org file in a buffer often makes little sense.  For
;; example, if the file contains an headline and the include keyword
;; was within an item, the item should contain the headline.  That's
;; why file inclusion should be done before any structure can be
;; associated to the file, that is before parsing.

(defun org-export-as (backend
		      &optional subtreep visible-only body-only ext-plist)
  "Transcode current Org buffer into BACKEND code.

If narrowing is active in the current buffer, only transcode its
narrowed part.

If a region is active, transcode that region.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without preamble nor postamble.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return code as a string."
  (save-excursion
    (save-restriction
      ;; Narrow buffer to an appropriate region for parsing.
      (when (org-region-active-p)
	(narrow-to-region (region-beginning) (region-end))
	(goto-char (point-min)))
      (when (and subtreep (not (org-at-heading-p)))
	;; Ensure point is at sub-tree's beginning.
	(org-with-limited-levels (org-back-to-heading (not visible-only))))
      ;; Retrieve export options (INFO) and parsed tree (RAW-DATA),
      ;; Then options can be completed with tree properties.  Note:
      ;; Buffer isn't parsed directly.  Instead, a temporary copy is
      ;; created, where include keywords are expanded and code blocks
      ;; are evaluated.  RAW-DATA is the parsed tree of the buffer
      ;; resulting from that process.  Eventually call
      ;; `org-export-filter-parse-tree-functions'.
      (let* ((info (org-export-collect-options backend subtreep ext-plist))
	     (raw-data (progn
			 (when subtreep		; Only parse subtree contents.
			   (let ((end (save-excursion (org-end-of-subtree t))))
			     (narrow-to-region
			      (progn (forward-line) (point)) end)))
			 (org-export-filter-apply-functions
			  (plist-get info :filter-parse-tree)
			  (org-export-with-current-buffer-copy
			   (org-export-expand-include-keyword nil)
			   (let ((org-current-export-file (current-buffer)))
			     (org-export-blocks-preprocess))
			   (org-element-parse-buffer nil visible-only))
			  backend info))))
	;; Now get full initial options with tree properties.
	(setq info
	      (org-combine-plists
	       info
	       (org-export-collect-tree-properties raw-data info backend)))
	;; Now transcode RAW-DATA.  Also call
	;; `org-export-filter-final-output-functions'.
	(let* ((body (org-element-normalize-string
		      (org-export-data raw-data backend info)))
	       (template (intern (format "org-%s-template" backend)))
	       (output (org-export-filter-apply-functions
			(plist-get info :filter-final-output)
			(if (or (not (fboundp template)) body-only) body
			  (funcall template body info))
			backend info)))
	  ;; Maybe add final OUTPUT to kill ring before returning it.
	  (when org-export-copy-to-kill-ring (org-kill-new output))
	  output)))))

(defun org-export-to-buffer (backend buffer &optional subtreep visible-only
				     body-only ext-plist)
  "Call `org-export-as' with output to a specified buffer.

BACKEND is the back-end used for transcoding, as a symbol.

BUFFER is the output buffer.  If it already exists, it will be
erased first, otherwise, it will be created.

Arguments SUBTREEP, VISIBLE-ONLY, BODY-ONLY and EXT-PLIST are
similar to those used in `org-export-as', which see.

Return buffer."
  (let ((out (org-export-as backend subtreep visible-only body-only ext-plist))
	(buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert out)
      (goto-char (point-min)))
    buffer))

(defun org-export-to-file (backend file &optional subtreep visible-only
				   body-only ext-plist)
  "Call `org-export-as' with output to a specified file.

BACKEND is the back-end used for transcoding, as a symbol.  FILE
is the name of the output file, as a string.

Optional arguments SUBTREEP, VISIBLE-ONLY, BODY-ONLY and
EXT-PLIST are similar to those used in `org-export-as', which
see.

Return output file's name."
  ;; Checks for FILE permissions.  `write-file' would do the same, but
  ;; we'd rather avoid needless transcoding of parse tree.
  (unless (file-writable-p file) (error "Output file not writable"))
  ;; Insert contents to a temporary buffer and write it to FILE.
  (let ((out (org-export-as
	      backend subtreep visible-only body-only ext-plist)))
    (with-temp-buffer
      (insert out)
      (let ((coding-system-for-write org-export-coding-system))
	(write-file file))))
  ;; Return full path.
  file)

(defun org-export-output-file-name (extension &optional subtreep pub-dir)
  "Return output file's name according to buffer specifications.

EXTENSION is a string representing the output file extension,
with the leading dot.

With a non-nil optional argument SUBTREEP, try to determine
output file's name by looking for \"EXPORT_FILE_NAME\" property
of subtree at point.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return file name as a string, or nil if it couldn't be
determined."
  (let ((base-name
	 ;; File name may come from EXPORT_FILE_NAME subtree property,
	 ;; assuming point is at beginning of said sub-tree.
	 (file-name-sans-extension
	  (or (and subtreep
		   (org-entry-get
		    (save-excursion
		      (ignore-errors
			(org-back-to-heading (not visible-only)) (point)))
		    "EXPORT_FILE_NAME" t))
	      ;; File name may be extracted from buffer's associated
	      ;; file, if any.
	      (buffer-file-name (buffer-base-buffer))
	      ;; Can't determine file name on our own: Ask user.
	      (let ((read-file-name-function
		     (and org-completion-use-ido 'ido-read-file-name)))
		(read-file-name
		 "Output file: " pub-dir nil nil nil
		 (lambda (name)
		   (string= (file-name-extension name t) extension))))))))
    ;; Build file name. Enforce EXTENSION over whatever user may have
    ;; come up with. PUB-DIR, if defined, always has precedence over
    ;; any provided path.
    (cond
     (pub-dir
      (concat (file-name-as-directory pub-dir)
	      (file-name-nondirectory base-name)
	      extension))
     ((string= (file-name-nondirectory base-name) base-name)
      (concat (file-name-as-directory ".") base-name extension))
     (t (concat base-name extension)))))

(defmacro org-export-with-current-buffer-copy (&rest body)
  "Apply BODY in a copy of the current buffer.

The copy preserves local variables and visibility of the original
buffer.

Point is at buffer's beginning when BODY is applied."
  (org-with-gensyms (original-buffer offset buffer-string overlays)
    `(let ((,original-buffer ,(current-buffer))
	   (,offset ,(1- (point-min)))
	   (,buffer-string ,(buffer-string))
	   (,overlays (mapcar
		       'copy-overlay (overlays-in (point-min) (point-max)))))
       (with-temp-buffer
	 (let ((buffer-invisibility-spec nil))
	   (org-clone-local-variables
	    ,original-buffer
	    "^\\(org-\\|orgtbl-\\|major-mode$\\|outline-\\(regexp\\|level\\)$\\)")
	   (insert ,buffer-string)
	   (mapc (lambda (ov)
		   (move-overlay
		    ov
		    (- (overlay-start ov) ,offset)
		    (- (overlay-end ov) ,offset)
		    (current-buffer)))
		 ,overlays)
	   (goto-char (point-min))
	   (progn ,@body))))))
(def-edebug-spec org-export-with-current-buffer-copy (body))

(defun org-export-expand-include-keyword (included)
  "Expand every include keyword in buffer.
INCLUDED is a list of included file names along with their line
restriction, when appropriate.  It is used to avoid infinite
recursion."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+include: \\(.*\\)" nil t)
      (when (eq (car (save-match-data (org-element-at-point))) 'keyword)
	(beginning-of-line)
	;; Extract arguments from keyword's value.
	(let* ((value (match-string 1))
	       (ind (org-get-indentation))
	       (file (and (string-match "^\"\\(\\S-+\\)\"" value)
			  (prog1 (expand-file-name (match-string 1 value))
			    (setq value (replace-match "" nil nil value)))))
	       (lines
		(and (string-match
		      ":lines +\"\\(\\(?:[0-9]+\\)?-\\(?:[0-9]+\\)?\\)\"" value)
		     (prog1 (match-string 1 value)
		       (setq value (replace-match "" nil nil value)))))
	       (env (cond ((string-match "\\<example\\>" value) 'example)
			  ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
			   (match-string 1 value))))
	       ;; Minimal level of included file defaults to the child
	       ;; level of the current headline, if any, or one.  It
	       ;; only applies is the file is meant to be included as
	       ;; an Org one.
	       (minlevel
		(and (not env)
		     (if (string-match ":minlevel +\\([0-9]+\\)" value)
			 (prog1 (string-to-number (match-string 1 value))
			   (setq value (replace-match "" nil nil value)))
		       (let ((cur (org-current-level)))
			 (if cur (1+ (org-reduced-level cur)) 1))))))
	  ;; Remove keyword.
	  (delete-region (point) (progn (forward-line) (point)))
	  (cond
	   ((not (file-readable-p file)) (error "Cannot include file %s" file))
	   ;; Check if files has already been parsed.  Look after
	   ;; inclusion lines too, as different parts of the same file
	   ;; can be included too.
	   ((member (list file lines) included)
	    (error "Recursive file inclusion: %s" file))
	   (t
	    (cond
	     ((eq env 'example)
	      (insert
	       (let ((ind-str (make-string ind ? ))
		     (contents
		      ;; Protect sensitive contents with commas.
		      (replace-regexp-in-string
		       "\\(^\\)\\([*]\\|[ \t]*#\\+\\)" ","
		       (org-export-prepare-file-contents file lines)
		       nil nil 1)))
		 (format "%s#+begin_example\n%s%s#+end_example\n"
			 ind-str contents ind-str))))
	     ((stringp env)
	      (insert
	       (let ((ind-str (make-string ind ? ))
		     (contents
		      ;; Protect sensitive contents with commas.
		      (replace-regexp-in-string
		       (if (string= env "org") "\\(^\\)\\(.\\)"
			 "\\(^\\)\\([*]\\|[ \t]*#\\+\\)") ","
			 (org-export-prepare-file-contents file lines)
			 nil nil 1)))
		 (format "%s#+begin_src %s\n%s%s#+end_src\n"
			 ind-str env contents ind-str))))
	     (t
	      (insert
	       (with-temp-buffer
		 (org-mode)
		 (insert
		  (org-export-prepare-file-contents file lines ind minlevel))
		 (org-export-expand-include-keyword
		  (cons (list file lines) included))
		 (buffer-string))))))))))))

(defun org-export-prepare-file-contents (file &optional lines ind minlevel)
  "Prepare the contents of FILE for inclusion and return them as a string.

When optional argument LINES is a string specifying a range of
lines, include only those lines.

Optional argument IND, when non-nil, is an integer specifying the
global indentation of returned contents.  Since its purpose is to
allow an included file to stay in the same environment it was
created \(i.e. a list item), it doesn't apply past the first
headline encountered.

Optional argument MINLEVEL, when non-nil, is an integer
specifying the level that any top-level headline in the included
file should have."
  (with-temp-buffer
    (insert-file-contents file)
    (when lines
      (let* ((lines (split-string lines "-"))
	     (lbeg (string-to-number (car lines)))
	     (lend (string-to-number (cadr lines)))
	     (beg (if (zerop lbeg) (point-min)
		    (goto-char (point-min))
		    (forward-line (1- lbeg))
		    (point)))
	     (end (if (zerop lend) (point-max)
		    (goto-char (point-min))
		    (forward-line (1- lend))
		    (point))))
	(narrow-to-region beg end)))
    ;; Remove blank lines at beginning and end of contents.  The logic
    ;; behind that removal is that blank lines around include keyword
    ;; override blank lines in included file.
    (goto-char (point-min))
    (org-skip-whitespace)
    (beginning-of-line)
    (delete-region (point-min) (point))
    (goto-char (point-max))
    (skip-chars-backward " \r\t\n")
    (forward-line)
    (delete-region (point) (point-max))
    ;; If IND is set, preserve indentation of include keyword until
    ;; the first headline encountered.
    (when ind
      (unless (eq major-mode 'org-mode) (org-mode))
      (goto-char (point-min))
      (let ((ind-str (make-string ind ? )))
	(while (not (or (eobp) (looking-at org-outline-regexp-bol)))
	  ;; Do not move footnote definitions out of column 0.
	  (unless (and (looking-at org-footnote-definition-re)
		       (eq (car (org-element-at-point)) 'footnote-definition))
	    (insert ind-str))
	  (forward-line))))
    ;; When MINLEVEL is specified, compute minimal level for headlines
    ;; in the file (CUR-MIN), and remove stars to each headline so
    ;; that headlines with minimal level have a level of MINLEVEL.
    (when minlevel
      (unless (eq major-mode 'org-mode) (org-mode))
      (let ((levels (org-map-entries
		     (lambda () (org-reduced-level (org-current-level))))))
	(when levels
	  (let ((offset (- minlevel (apply 'min levels))))
	    (unless (zerop offset)
	      (when org-odd-levels-only (setq offset (* offset 2)))
	      ;; Only change stars, don't bother moving whole
	      ;; sections.
	      (org-map-entries
	       (lambda () (if (< offset 0) (delete-char (abs offset))
		       (insert (make-string offset ?*))))))))))
    (buffer-string)))


;;; Tools For Back-Ends

;; A whole set of tools is available to help build new exporters.  Any
;; function general enough to have its use across many back-ends
;; should be added here.

;; As of now, functions operating on footnotes, headlines, links,
;; macros, references, src-blocks, tables and tables of contents are
;; implemented.

;;;; For Footnotes

;; `org-export-collect-footnote-definitions' is a tool to list
;; actually used footnotes definitions in the whole parse tree, or in
;; an headline, in order to add footnote listings throughout the
;; transcoded data.

;; `org-export-footnote-first-reference-p' is a predicate used by some
;; back-ends, when they need to attach the footnote definition only to
;; the first occurrence of the corresponding label.

;; `org-export-get-footnote-definition' and
;; `org-export-get-footnote-number' provide easier access to
;; additional information relative to a footnote reference.

(defun org-export-collect-footnote-definitions (data info)
  "Return an alist between footnote numbers, labels and definitions.

DATA is the parse tree from which definitions are collected.
INFO is the plist used as a communication channel.

Definitions are sorted by order of references.  They either
appear as Org data \(transcoded with `org-export-data'\) or as
a secondary string for inlined footnotes \(transcoded with
`org-export-secondary-string'\).  Unreferenced definitions are
ignored."
  (let (refs)
    ;; Collect seen references in REFS.
    (org-element-map
     data 'footnote-reference
     (lambda (footnote local)
       (when (org-export-footnote-first-reference-p footnote local)
	 (list (org-export-get-footnote-number footnote local)
	       (org-element-get-property :label footnote)
	       (org-export-get-footnote-definition footnote local))))
     info)))

(defun org-export-footnote-first-reference-p (footnote-reference info)
  "Non-nil when a footnote reference is the first one for its label.

FOOTNOTE-REFERENCE is the footnote reference being considered.
INFO is the plist used as a communication channel."
  (let ((label (org-element-get-property :label footnote-reference)))
    (or (not label)
	(equal
	 footnote-reference
	 (org-element-map
	  (plist-get info :parse-tree) 'footnote-reference
	  (lambda (footnote local)
	    (when (string= (org-element-get-property :label footnote) label)
	      footnote))
	  info 'first-match)))))

(defun org-export-get-footnote-definition (footnote-reference info)
  "Return definition of FOOTNOTE-REFERENCE as parsed data.
INFO is the plist used as a communication channel."
  (let ((label (org-element-get-property :label footnote-reference)))
    (or (org-element-get-property :inline-definition footnote-reference)
        (cdr (assoc label (plist-get info :footnote-definition-alist))))))

(defun org-export-get-footnote-number (footnote info)
  "Return number associated to a footnote.

FOOTNOTE is either a footnote reference or a footnote definition.
INFO is the plist used as a communication channel."
  (let ((label (org-element-get-property :label footnote)) seen-refs)
    (org-element-map
     (plist-get info :parse-tree) 'footnote-reference
     (lambda (fn local)
       (let ((fn-lbl (org-element-get-property :label fn)))
	 (cond
	  ((and (not fn-lbl) (equal fn footnote)) (1+ (length seen-refs)))
	  ((and label (string= label fn-lbl)) (1+ (length seen-refs)))
	  ;; Anonymous footnote: it's always a new one.  Also, be sure
	  ;; to return nil from the `cond' so `first-match' doesn't
	  ;; get us out of the loop.
	  ((not fn-lbl) (push 'inline seen-refs) nil)
	  ;; Label not seen so far: add it so SEEN-REFS.  Again,
	  ;; return nil to stay in the loop.
	  ((not (member fn-lbl seen-refs)) (push fn-lbl seen-refs) nil))))
     info 'first-match)))


;;;; For Headlines

;; `org-export-get-relative-level' is a shortcut to get headline
;; level, relatively to the lower headline level in the parsed tree.

;; `org-export-get-headline-number' returns the section number of an
;; headline, while `org-export-number-to-roman' allows to convert it
;; to roman numbers.

;; `org-export-low-level-p', `org-export-first-sibling-p' and
;; `org-export-last-sibling-p' are three useful predicates when it
;; comes to fulfill the `:headline-levels' property.

(defun org-export-get-relative-level (headline info)
  "Return HEADLINE relative level within current parsed tree.
INFO is a plist holding contextual information."
  (+ (org-element-get-property :level headline)
     (or (plist-get info :headline-offset) 0)))

(defun org-export-low-level-p (headline info)
  "Non-nil when HEADLINE is considered as low level.

INFO is a plist used as a communication channel.

A low level headlines has a relative level greater than
`:headline-levels' property value.

Return value is the difference between HEADLINE relative level
and the last level being considered as high enough, or nil."
  (let ((limit (plist-get info :headline-levels)))
    (when (wholenump limit)
      (let ((level (org-export-get-relative-level headline info)))
        (and (> level limit) (- level limit))))))

(defun org-export-get-headline-number (headline info)
  "Return HEADLINE numbering as a list of numbers.
INFO is a plist holding contextual information."
  (cdr (assoc headline (plist-get info :headline-numbering))))

(defun org-export-number-to-roman (n)
  "Convert integer N into a roman numeral."
  (let ((roman '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
		 ( 100 . "C") ( 90 . "XC") ( 50 . "L") ( 40 . "XL")
		 (  10 . "X") (  9 . "IX") (  5 . "V") (  4 . "IV")
		 (   1 . "I")))
	(res ""))
    (if (<= n 0)
	(number-to-string n)
      (while roman
	(if (>= n (caar roman))
	    (setq n (- n (caar roman))
		  res (concat res (cdar roman)))
	  (pop roman)))
      res)))

(defun org-export-first-sibling-p (headline info)
  "Non-nil when HEADLINE is the first sibling in its sub-tree.
INFO is the plist used as a communication channel."
  (not (eq (car (org-export-get-previous-element headline info)) 'headline)))

(defun org-export-last-sibling-p (headline info)
  "Non-nil when HEADLINE is the last sibling in its sub-tree.
INFO is the plist used as a communication channel."
  (equal
   (car (last (org-element-get-contents (car (plist-get info :genealogy)))))
   headline))


;;;; For Links

;; `org-export-solidify-link-text' turns a string into a safer version
;; for links, replacing most non-standard characters with hyphens.

;; `org-export-get-coderef-format' returns an appropriate format
;; string for coderefs.

;; `org-export-inline-image-p' returns a non-nil value when the link
;; provided should be considered as an inline image.

;; `org-export-resolve-fuzzy-link' searches destination of fuzzy links
;; (i.e. links with "fuzzy" as type) within the parsed tree, and
;; returns an appropriate unique identifier when found, or nil.

;; `org-export-resolve-id-link' returns the first headline with
;; specified id or custom-id in parse tree, or nil when none was
;; found.

;; `org-export-resolve-coderef' associates a reference to a line
;; number in the element it belongs, or returns the reference itself
;; when the element isn't numbered.

(defun org-export-solidify-link-text (s)
  "Take link text S and make a safe target out of it."
  (save-match-data
    (mapconcat 'identity (org-split-string s "[^a-zA-Z0-9_\\.-]+") "-")))

(defun org-export-get-coderef-format (path desc)
  "Return format string for code reference link.
PATH is the link path.  DESC is its description."
  (save-match-data
    (cond ((string-match (regexp-quote (concat "(" path ")")) desc)
	   (replace-match "%s" t t desc))
	  ((string= desc "") "%s")
	  (t desc))))

(defun org-export-inline-image-p (link &optional extensions)
  "Non-nil if LINK object points to an inline image.

When non-nil, optional argument EXTENSIONS is a list of valid
extensions for image files, as strings.  Otherwise, a default
list is provided \(cf `org-image-file-name-regexp'\)."
  (and (not (org-element-get-contents link))
       (string= (org-element-get-property :type link) "file")
       (org-file-image-p
	(expand-file-name (org-element-get-property :path link))
	extensions)))

(defun org-export-resolve-fuzzy-link (link info)
  "Return LINK destination.

INFO is a plist holding contextual information.

Return value can be an object, an element, or nil:

- If LINK path exactly matches any target, return the target
  object.

- If LINK path exactly matches any headline name, return that
  element.  If more than one headline share that name, priority
  will be given to the one with the closest common ancestor, if
  any, or the first one in the parse tree otherwise.

- Otherwise, return nil.

Assume LINK type is \"fuzzy\"."
  (let ((path (org-element-get-property :path link)))
    ;; Link points to a target: return it.
    (or (loop for target in (plist-get info :target-list)
	      when (string= (org-element-get-property :raw-value target) path)
	      return target)
	;; Link either points to an headline or nothing.  Try to find
	;; the source, with priority given to headlines with the closest
	;; common ancestor.  If such candidate is found, return its
	;; beginning position as an unique identifier, otherwise return
	;; nil.
	(let ((find-headline
	       (function
		;; Return first headline whose `:raw-value' property
		;; is NAME in parse tree DATA, or nil.
		(lambda (name data)
		  (org-element-map
		   data 'headline
		   (lambda (headline local)
		     (when (string=
			    (org-element-get-property :raw-value headline)
			    name)
		       headline))
		   info 'first-match)))))
	  ;; Search among headlines sharing an ancestor with link,
	  ;; from closest to farthest.
	  (or (catch 'exit
		(mapc
		 (lambda (parent)
		   (when (eq (car parent) 'headline)
		     (let ((foundp (funcall find-headline path parent)))
		       (when foundp (throw 'exit foundp)))))
		 (plist-get info :genealogy)) nil)
	      ;; No match with a common ancestor: try the full parse-tree.
	      (funcall find-headline path (plist-get info :parse-tree)))))))

(defun org-export-resolve-id-link (link info)
  "Return headline referenced as LINK destination.

INFO is a plist used as a communication channel.

Return value can be an headline element or nil.  Assume LINK type
is either \"id\" or \"custom-id\"."
  (let ((id (org-element-get-property :path link)))
    (org-element-map
     (plist-get info :parse-tree) 'headline
     (lambda (headline local)
       (when (or (string= (org-element-get-property :id headline) id)
                 (string= (org-element-get-property :custom-id headline) id))
         headline))
     info 'first-match)))

(defun org-export-resolve-coderef (ref info)
  "Resolve a code reference REF.

INFO is a plist used as a communication channel.

Return associated line number in source code, or REF itself,
depending on src-block or example element's switches."
  (org-element-map
   (plist-get info :parse-tree) '(src-block example)
   (lambda (el local)
     (let ((switches (or (org-element-get-property :switches el) "")))
       (with-temp-buffer
         (insert (org-trim (org-element-get-property :value el)))
         ;; Build reference regexp.
         (let* ((label
                 (or (and (string-match "-l +\"\\([^\"\n]+\\)\"" switches)
                          (match-string 1 switches))
                     org-coderef-label-format))
                (ref-re
                 (format "^.*?\\S-.*?\\([ \t]*\\(%s\\)\\)[ \t]*$"
                         (replace-regexp-in-string "%s" ref label nil t))))
           ;; Element containing REF is found.  Only associate REF to
           ;; a line number if element has "+n" or "-n" and "-k" or
           ;; "-r" as switches.  When it has "+n", count accumulated
           ;; locs before, too.
           (when (re-search-backward ref-re nil t)
             (cond
              ((not (string-match "-[kr]\\>" switches)) ref)
              ((string-match "-n\\>" switches) (line-number-at-pos))
	      ((string-match "\\+n\\>" switches)
	       (+ (org-export-get-loc el local) (line-number-at-pos)))
              (t ref)))))))
   info 'first-match))


;;;; For Macros

;; `org-export-expand-macro' simply takes care of expanding macros.

(defun org-export-expand-macro (macro info)
  "Expand MACRO and return it as a string.
INFO is a plist holding export options."
  (let* ((key (org-element-get-property :key macro))
	 (args (org-element-get-property :args macro))
	 ;; User's macros are stored in the communication channel with
	 ;; a ":macro-" prefix.
	 (value (plist-get info (intern (format ":macro-%s" key)))))
    ;; Replace arguments in VALUE. A nil VALUE removes the macro call
    ;; from export.
    (when (stringp value)
      (let ((s 0) n)
	(while (string-match "\\$\\([0-9]+\\)" value s)
	  (setq s (1+ (match-beginning 0))
		n (string-to-number (match-string 1 value)))
	  (and (>= (length args) n)
	       (setq value (replace-match (nth (1- n) args) t t value)))))
      ;; VALUE starts with "(eval": it is a s-exp, `eval' it.
      (when (string-match "\\`(eval\\>" value)
	(setq value (eval (read value))))
      ;; Return string.
      (format "%s" (or value "")))))


;;;; For References

;; `org-export-get-ordinal' associates a sequence number to any object
;; or element.

(defun org-export-get-ordinal
  (element info &optional types within-section predicate)
  "Return ordinal number of an element or object.

ELEMENT is the element or object considered.  INFO is the plist
used as a communication channel.

Optional argument TYPES, when non-nil, is a list of element or
object types, as symbols, that should also be counted in.
Otherwise, only provided element's type is considered.

When optional argument WITHIN-SECTION is non-nil, narrow counting
to the section containing ELEMENT.

Optional argument PREDICATE is a function returning a non-nil
value if the current element or object should be counted in.  It
accepts one argument: the element or object being considered.
This argument allows to count only a certain type of objects,
like inline images, which are a subset of links \(in that case,
`org-export-inline-image-p' might be an useful predicate\)."
  (let ((counter 0)
        ;; Determine if search should apply to current section, in
        ;; which case it should be retrieved first, or to full parse
        ;; tree.  As a special case, an element or object without
        ;; a parent headline will also trigger a full search,
        ;; notwithstanding WITHIN-SECTION value.
        (data
         (if (not within-section) (plist-get info :parse-tree)
	   (or (org-export-get-parent-headline element info)
	       (plist-get info :parse-tree)))))
    ;; Increment counter until ELEMENT is found again.
    (org-element-map
     data (or types (car element))
     (lambda (el local)
       (cond
        ((equal element el) (1+ counter))
	((not predicate) (incf counter) nil)
	((funcall predicate el) (incf counter) nil)))
     info 'first-match)))


;;;; For Src-Blocks

;; `org-export-get-loc' counts number of code lines accumulated in
;; src-block or example-block elements with a "+n" switch until
;; a given element, excluded.  Note: "-n" switches reset that count.

;; `org-export-handle-code' takes care of line numbering and reference
;; cleaning in source code, when appropriate.

(defun org-export-get-loc (element info)
  "Return accumulated lines of code up to ELEMENT.

INFO is the plist used as a communication channel.

ELEMENT is excluded from count."
  (let ((loc 0))
    (org-element-map
     (plist-get info :parse-tree) `(src-block example-block ,(car element))
     (lambda (el local)
       (cond
        ;; ELEMENT is reached: Quit the loop.
        ((equal el element) t)
        ;; Only count lines from src-block and example-block elements
        ;; with a "+n" or "-n" switch.  A "-n" switch resets counter.
        ((not (memq (car el) '(src-block example-block))) nil)
        ((let ((switches (org-element-get-property :switches el)))
           (when (and switches (string-match "\\([-+]\\)n\\>" switches))
	     ;; Accumulate locs or reset them.
	     (let ((accumulatep (string= (match-string 1 switches) "-"))
		   (lines (org-count-lines
			   (org-trim (org-element-get-property :value el)))))
	       (setq loc (if accumulatep lines (+ loc lines))))))
	 ;; Return nil to stay in the loop.
         nil)))
     info 'first-match)
    ;; Return value.
    loc))

(defun org-export-handle-code (element info &optional num-fmt ref-fmt delayed)
  "Handle line numbers and code references in ELEMENT.

ELEMENT has either a `src-block' an `example-block' type.  INFO
is a plist used as a communication channel.

If optional argument NUM-FMT is a string, it will be used as
a format string for numbers at beginning of each line.

If optional argument REF-FMT is a string, it will be used as
a format string for each line of code containing a reference.

When optional argument DELAYED is non-nil, `org-loc' and
`org-coderef' properties, set to an adequate value, are applied
to, respectively, numbered lines and lines with a reference.  No
line numbering is done and all references are stripped from the
resulting string.  Both NUM-FMT and REF-FMT arguments are ignored
in that situation.

Return new code as a string."
  (let* ((switches (or (org-element-get-property :switches element) ""))
	 (code (org-element-get-property :value element))
	 (numberp (string-match "[-+]n\\>" switches))
	 (accumulatep (string-match "\\+n\\>" switches))
	 ;; Initialize loc counter when any kind of numbering is
	 ;; active.
	 (total-LOC (cond
		     (accumulatep (org-export-get-loc element info))
		     (numberp 0)))
	 ;; Get code and clean it.  Remove blank lines at its
	 ;; beginning and end.  Also remove protective commas.
	 (preserve-indent-p (or org-src-preserve-indentation
				(string-match "-i\\>" switches)))
	 (replace-labels (when (string-match "-r\\>" switches)
			   (if (string-match "-k\\>" switches) 'keep t)))
	 (code (let ((c (replace-regexp-in-string
			 "\\`\\([ \t]*\n\\)+" ""
			 (replace-regexp-in-string
			  "\\(:?[ \t]*\n\\)*[ \t]*\\'" "\n" code))))
		 ;; If appropriate, remove global indentation.
		 (unless preserve-indent-p (setq c (org-remove-indentation c)))
		 ;; Free up the protected lines.  Note: Org blocks
		 ;; have commas at the beginning or every line.
		 (if (string=
		      (or (org-element-get-property :language element) "")
		      "org")
		     (replace-regexp-in-string "^," "" c)
		   (replace-regexp-in-string
		    "^\\(,\\)\\(:?\\*\\|[ \t]*#\\+\\)" "" c nil nil 1))))
	 ;; Split code to process it line by line.
	 (code-lines (org-split-string code "\n"))
	 ;; If numbering is active, ensure line numbers will be
	 ;; correctly padded before applying the format string.
	 (num-fmt
	  (when (and (not delayed) numberp)
	    (format (if (stringp num-fmt) num-fmt "%s:  ")
		    (format "%%%ds"
			    (length (number-to-string
				     (+ (length code-lines) total-LOC)))))))
	 ;; Get format used for references.
	 (label-fmt (or (and (string-match "-l +\"\\([^\"\n]+\\)\"" switches)
			     (match-string 1 switches))
			org-coderef-label-format))
	 ;; Build a regexp matching a loc with a reference.
	 (with-ref-re (format "^.*?\\S-.*?\\([ \t]*\\(%s\\)\\)[ \t]*$"
			      (replace-regexp-in-string
			       "%s" "\\([-a-zA-Z0-9_ ]+\\)" label-fmt nil t))))
    (org-element-normalize-string
     (mapconcat
      (lambda (loc)
	;; Maybe add line number to current line of code (LOC).
	(when numberp
	  (incf total-LOC)
	  (setq loc (if delayed (org-add-props loc nil 'org-loc total-LOC)
		      (concat (format num-fmt total-LOC) loc))))
	;; Take action if at a ref line.
	(when (string-match with-ref-re loc)
	  (let ((ref (match-string 3 loc)))
	    (setq loc
		  ;; Option "-r" without "-k" removes labels.
		  ;; A non-nil DELAYED removes labels unconditionally.
		  (if (or delayed
			  (and replace-labels (not (eq replace-labels 'keep))))
		      (replace-match "" nil nil loc 1)
		    (replace-match (format "(%s)" ref) nil nil loc 2)))
	    ;; Store REF in `org-coderef' property if DELAYED asks to.
	    (cond (delayed (setq loc (org-add-props loc nil 'org-coderef ref)))
		  ;; If REF-FMT is defined, apply it to current LOC.
		  ((stringp ref-fmt) (setq loc (format ref-fmt loc))))))
	;; Return updated LOC for concatenation.
	loc)
      code-lines "\n"))))


;;;; For Tables

;; `org-export-table-format-info' extracts formatting information
;; (alignment, column groups and presence of a special column) from
;; a raw table and returns it as a property list.
;;
;; `org-export-clean-table' cleans the raw table from any Org
;; table-specific syntax.

(defun org-export-table-format-info (table)
  "Extract info from TABLE.
Return a plist whose properties and values are:
`:alignment'        vector of strings among \"r\", \"l\" and \"c\",
`:column-groups'    vector of symbols among `start', `end', `start-end',
`:row-groups'       list of integers representing row groups.
`:special-column-p' non-nil if table has a special column.
`:width'            vector of integers representing desired width of
		    current column, or nil."
  (with-temp-buffer
    (insert table)
    (goto-char 1)
    (org-table-align)
    (let ((align (vconcat (mapcar (lambda (c) (if c "r" "l"))
				  org-table-last-alignment)))
	  (width (make-vector (length org-table-last-alignment) nil))
	  (colgroups (make-vector (length org-table-last-alignment) nil))
	  (row-group 0)
	  (rowgroups)
	  (special-column-p 'empty))
      (mapc (lambda (row)
	      (if (string-match "^[ \t]*|[-+]+|[ \t]*$" row)
		  (incf row-group)
		;; Determine if a special column is present by looking
		;; for special markers in the first column.  More
		;; accurately, the first column is considered special
		;; if it only contains special markers and, maybe,
		;; empty cells.
		(setq special-column-p
		      (cond
		       ((not special-column-p) nil)
		       ((string-match "^[ \t]*| *\\\\?\\([/#!$*_^]\\) *|" row)
			'special)
		       ((string-match "^[ \t]*| +|" row) special-column-p))))
	      (cond
	       ;; Read forced alignment and width information, if any,
	       ;; and determine final alignment for the table.
	       ((org-table-cookie-line-p row)
		(let ((col 0))
		  (mapc (lambda (field)
			  (when (string-match
				 "<\\([lrc]\\)?\\([0-9]+\\)?>" field)
			    (let ((align-data (match-string 1 field)))
			      (when align-data (aset align col align-data)))
			    (let ((w-data (match-string 2 field)))
			      (when w-data
				(aset width col (string-to-number w-data)))))
			  (incf col))
			(org-split-string row "[ \t]*|[ \t]*"))))
	       ;; Read column groups information.
	       ((org-table-colgroup-line-p row)
		(let ((col 0))
		  (mapc (lambda (field)
			  (aset colgroups col
				(cond ((string= "<" field) 'start)
				      ((string= ">" field) 'end)
				      ((string= "<>" field) 'start-end)))
			  (incf col))
			(org-split-string row "[ \t]*|[ \t]*"))))
	       ;; Contents line.
	       (t (push row-group rowgroups))))
	    (org-split-string table "\n"))
      ;; Return plist.
      (list :alignment align
	    :column-groups colgroups
	    :row-groups (reverse rowgroups)
	    :special-column-p (eq special-column-p 'special)
	    :width width))))

(defun org-export-clean-table (table specialp)
  "Clean string TABLE from its formatting elements.
Remove any row containing column groups or formatting cookies and
rows starting with a special marker.  If SPECIALP is non-nil,
assume the table contains a special formatting column and remove
it also."
  (let ((rows (org-split-string table "\n")))
    (mapconcat 'identity
	       (delq nil
		     (mapcar
		      (lambda (row)
			(cond
			 ((org-table-colgroup-line-p row) nil)
			 ((org-table-cookie-line-p row) nil)
			 ;; Ignore rows starting with a special marker.
			 ((string-match "^[ \t]*| *[!_^/] *|" row) nil)
			 ;; Remove special column.
			 ((and specialp
			       (or (string-match "^\\([ \t]*\\)|-+\\+" row)
				   (string-match "^\\([ \t]*\\)|[^|]*|" row)))
			  (replace-match "\\1|" t nil row))
			 (t row)))
		      rows))
	       "\n")))


;;;; For Tables Of Contents

;; `org-export-collect-headlines' builds a list of all exportable
;; headline elements, maybe limited to a certain depth.  One can then
;; easily parse it and transcode it.

;; Building lists of tables, figures or listings is quite similar.
;; Once the generic function `org-export-collect-elements' is defined,
;; `org-export-collect-tables', `org-export-collect-figures' and
;; `org-export-collect-listings' can be derived from it.

(defun org-export-collect-headlines (info &optional n)
  "Collect headlines in order to build a table of contents.

INFO is a plist used as a communication channel.

When non-nil, optional argument N must be an integer.  It
specifies the depth of the table of contents.

Return a list of all exportable headlines as parsed elements."
  (org-element-map
   (plist-get info :parse-tree)
   'headline
   (lambda (headline local)
     ;; Strip contents from HEADLINE.
     (let ((relative-level (org-export-get-relative-level headline local)))
       (unless (and n (> relative-level n)) headline)))
   info))

(defun org-export-collect-elements (type info &optional predicate)
  "Collect referenceable elements of a determined type.

TYPE can be a symbol or a list of symbols specifying element
types to search.  Only elements with a caption or a name are
collected.

INFO is a plist used as a communication channel.

When non-nil, optional argument PREDICATE is a function accepting
one argument, an element of type TYPE.  It returns a non-nil
value when that element should be collected.

Return a list of all elements found, in order of appearance."
  (org-element-map
   (plist-get info :parse-tree) type
   (lambda (element local)
     (and (or (org-element-get-property :caption element)
	      (org-element-get-property :name element))
	  (or (not predicate) (funcall predicate element))
	  element)) info))

(defun org-export-collect-tables (info)
  "Build a list of tables.

INFO is a plist used as a communication channel.

Return a list of table elements with a caption or a name
affiliated keyword."
  (org-export-collect-elements 'table info))

(defun org-export-collect-figures (info predicate)
  "Build a list of figures.

INFO is a plist used as a communication channel.  PREDICATE is
a function which accepts one argument: a paragraph element and
whose return value is non-nil when that element should be
collected.

A figure is a paragraph type element, with a caption or a name,
verifying PREDICATE.  The latter has to be provided since
a \"figure\" is a vague concept that may depend on back-end.

Return a list of elements recognized as figures."
  (org-export-collect-elements 'paragraph info predicate))

(defun org-export-collect-listings (info)
  "Build a list of src blocks.

INFO is a plist used as a communication channel.

Return a list of src-block elements with a caption or a name
affiliated keyword."
  (org-export-collect-elements 'src-block info))


;;;; Topology

(defun org-export-get-parent-headline (blob info)
  "Return BLOB's closest parent headline or nil.
INFO is a plist used as a communication channel."
  (catch 'exit
    (mapc
     (lambda (el) (when (eq (car el) 'headline) (throw 'exit el)))
     (plist-get info :genealogy))
    nil))

(defun org-export-get-previous-element (blob info)
  "Return previous element or object.

BLOB is an element or object.  INFO is a plist used as
a communication channel.

Return previous element or object, a string, or nil."
  (let ((parent (car (plist-get info :genealogy))))
    (cadr (member blob (reverse (org-element-get-contents parent))))))

(defun org-export-get-next-element (blob info)
  "Return next element or object.

BLOB is an element or object.  INFO is a plist used as
a communication channel.

Return next element or object, a string, or nil."
  (let ((parent (car (plist-get info :genealogy))))
    (cadr (member blob (org-element-get-contents parent)))))



;;; The Dispatcher

;; `org-export-dispatch' is the standard interactive way to start an
;; export process.  It uses `org-export-dispatch-ui' as a subroutine
;; for its interface.  Most commons back-ends should have an entry in
;; it.

(defun org-export-dispatch ()
  "Export dispatcher for Org mode.

It provides an access to common export related tasks in a buffer.
Its interface comes in two flavours: standard and expert.  While
both share the same set of bindings, only the former displays the
valid keys associations.  Set `org-export-dispatch-use-expert-ui'
to switch to one or the other.

Return an error if key pressed has no associated command."
  (interactive)
  (let* ((input (org-export-dispatch-ui
		 (if (listp org-export-initial-scope) org-export-initial-scope
		   (list org-export-initial-scope))
		 org-export-dispatch-use-expert-ui))
	 (raw-key (car input))
	 (scope (cdr input)))
    ;; Translate "C-a", "C-b"... into "a", "b"... Then take action
    ;; depending on user's key pressed.
    (case (if (< raw-key 27) (+ raw-key 96) raw-key)
      ;; Export with `e-ascii' back-end.
      ((?A ?N ?U)
       (let ((outbuf
	      (org-export-to-buffer
	       'e-ascii "*Org E-ASCII Export*"
	       (memq 'subtree scope) (memq 'visible scope) (memq 'body scope)
	       `(:ascii-charset
		 ,(case raw-key (?A 'ascii) (?N 'latin1) (t 'utf-8))))))
	 (with-current-buffer outbuf (text-mode))
	 (when org-export-show-temporary-export-buffer
	   (switch-to-buffer-other-window outbuf))))
      ((?a ?n ?u)
       (org-e-ascii-export-to-ascii
	(memq 'subtree scope) (memq 'visible scope) (memq 'body scope)
	`(:ascii-charset ,(case raw-key (?a 'ascii) (?n 'latin1) (t 'utf-8)))))
      ;; Export with `e-latex' back-end.
      (?L
       (let ((outbuf
	      (org-export-to-buffer
	       'e-latex "*Org E-LaTeX Export*"
	       (memq 'subtree scope) (memq 'visible scope) (memq 'body scope))))
	 (with-current-buffer outbuf (latex-mode))
	 (when org-export-show-temporary-export-buffer
	   (switch-to-buffer-other-window outbuf))))
      (?l (org-e-latex-export-to-latex
	   (memq 'subtree scope) (memq 'visible scope) (memq 'body scope)))
      (?p (org-e-latex-export-to-pdf
	   (memq 'subtree scope) (memq 'visible scope) (memq 'body scope)))
      (?d (org-open-file
	   (org-e-latex-export-to-pdf
	    (memq 'subtree scope) (memq 'visible scope) (memq 'body scope))))
      ;; Undefined command.
      (t (error "No command associated with key %s"
		(char-to-string raw-key))))))

(defun org-export-dispatch-ui (scope expertp)
  "Handle interface for `org-export-dispatch'.

SCOPE is a list containing current interactive options set for
export.  It can contain any of the following symbols:
`body'    toggles a body-only export
`subtree' restricts export to current subtree
`visible' restricts export to visible part of buffer.

EXPERTP, when non-nil, triggers expert UI.  In that case, no help
buffer is provided, but indications about currently active
options are given in the prompt.  Moreover, \[?] allows to switch
back to standard interface.

Return value is a list with key pressed as car and a list of
final interactive export options as cdr."
  (let ((help (format "-------------------  General Options  --------------------
\[1] Body only:     %s
\[2] Export scope:  %s
\[3] Visible only:  %s

--------------  ASCII/Latin-1/UTF-8 Export  --------------
\[a/n/u] to TXT file            [A/N/U] to temporary buffer

---------------------  LaTeX Export  ---------------------
\[l] to TEX file                [L] to temporary buffer
\[p] to PDF file                [d] ... and open it"
		      (if (memq 'body scope) "On" "Off")
		      (if (memq 'subtree scope) "Subtree" "Buffer")
		      (if (memq 'visible scope) "On" "Off")))
	(standard-prompt "Export command: ")
	(expert-prompt (format "Export command (%s%s%s): "
			       (if (memq 'body scope) "b" "-")
			       (if (memq 'subtree scope) "s" "-")
			       (if (memq 'visible scope) "v" "-")))
	(handle-keypress
	 (function
	  ;; Read a character from command input, toggling interactive
	  ;; options when applicable.  PROMPT is the displayed prompt,
	  ;; as a string.
	  (lambda (prompt)
	    (let ((key (read-char-exclusive prompt)))
	      (cond
	       ;; Ignore non-standard characters (i.e. "M-a").
	       ((not (characterp key)) (org-export-dispatch-ui scope expertp))
	       ;; Switch back to standard interface.
	       ((and (eq key ??) expertp) (org-export-dispatch-ui scope nil))
	       ((eq key ?1)
		(org-export-dispatch-ui
		 (if (memq 'body scope) (remq 'body scope) (cons 'body scope))
		 expertp))
	       ((eq key ?2)
		(org-export-dispatch-ui
		 (if (memq 'subtree scope) (remq 'subtree scope)
		   (cons 'subtree scope))
		 expertp))
	       ((eq key ?3)
		(org-export-dispatch-ui
		 (if (memq 'visible scope) (remq 'visible scope)
		   (cons 'visible scope))
		 expertp))
	       (t (cons key scope))))))))
    ;; With expert UI, just read key with a fancy prompt.  In standard
    ;; UI, display an intrusive help buffer.
    (if expertp (funcall handle-keypress expert-prompt)
      (save-window-excursion
	(delete-other-windows)
	(with-output-to-temp-buffer "*Org Export/Publishing Help*" (princ help))
	(org-fit-window-to-buffer
	 (get-buffer-window "*Org Export/Publishing Help*"))
	(funcall handle-keypress standard-prompt)))))


(provide 'org-export)
;;; org-export.el ends here
