;;; org-e-latex.el --- LaTeX Back-End For Org Export Engine

;; Copyright (C) 2011  Free Software Foundation, Inc.

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

;; This library implements a LaTeX back-end for Org generic exporter.

;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-latex "*Test e-LaTeX*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the LaTeX
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.

;; It introduces three new buffer keywords: "LATEX_CLASS",
;; "LATEX_CLASS_OPTIONS" and "LATEX_HEADER".

;;; Code:

(eval-when-compile (require 'cl))
(require 'org-element)
(require 'org-export)



;;; Internal Variables

(defconst org-e-latex-option-alist
  '((:date "DATE" nil org-e-latex-date-format t)
    (:latex-class "LATEX_CLASS" nil org-e-latex-default-class t)
    (:latex-class-options "LATEX_CLASS_OPTIONS" nil nil t)
    (:latex-header-extra "LATEX_HEADER" nil nil newline))
  "Alist between LaTeX export properties and ways to set them.
See `org-export-option-alist' for more information on the
structure of the value.")



;;; User Configurable Variables

(defgroup org-export-latex nil
  "Options for exporting Org mode files to LaTeX."
  :tag "Org Export LaTeX"
  :group 'org-export)


;;;; Preamble

(defcustom org-e-latex-default-class "article"
  "The default LaTeX class."
  :group 'org-export-latex
  :type '(string :tag "LaTeX class"))

(defcustom org-e-latex-classes
  '(("article"
     "\\documentclass[11pt]{article}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ("report"
     "\\documentclass[11pt]{report}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("book"
     "\\documentclass[11pt]{book}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  "Alist of LaTeX classes and associated header and structure.
If #+LaTeX_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    \(numbered-section . unnumbered-section\)
    ...\)

The header string
-----------------

The HEADER-STRING is the header that will be inserted into the LaTeX file.
It should contain the \\documentclass macro, and anything else that is needed
for this setup.  To this header, the following commands will be added:

- Calls to \\usepackage for all packages mentioned in the variables
  `org-e-latex-default-packages-alist' and
  `org-e-latex-packages-alist'.  Thus, your header definitions should
  avoid to also request these packages.

- Lines specified via \"#+LaTeX_HEADER:\"

If you need more control about the sequence in which the header is built
up, or if you want to exclude one of these building blocks for a particular
class, you can use the following macro-like placeholders.

 [DEFAULT-PACKAGES]      \\usepackage statements for default packages
 [NO-DEFAULT-PACKAGES]   do not include any of the default packages
 [PACKAGES]              \\usepackage statements for packages
 [NO-PACKAGES]           do not include the packages
 [EXTRA]                 the stuff from #+LaTeX_HEADER
 [NO-EXTRA]              do not include #+LaTeX_HEADER stuff
 [BEAMER-HEADER-EXTRA]   the beamer extra headers

So a header like

  \\documentclass{article}
  [NO-DEFAULT-PACKAGES]
  [EXTRA]
  \\providecommand{\\alert}[1]{\\textbf{#1}}
  [PACKAGES]

will omit the default packages, and will include the #+LaTeX_HEADER lines,
then have a call to \\providecommand, and then place \\usepackage commands
based on the content of `org-e-latex-packages-alist'.

If your header or `org-e-latex-default-packages-alist' inserts
\"\\usepackage[AUTO]{inputenc}\", AUTO will automatically be replaced with
a coding system derived from `buffer-file-coding-system'.  See also the
variable `org-e-latex-inputenc-alist' for a way to influence this
mechanism.

The sectioning structure
------------------------

The sectioning structure of the class is given by the elements following
the header string.  For each sectioning level, a number of strings is
specified.  A %s formatter is mandatory in each section string and will
be replaced by the title of the section.

Instead of a cons cell \(numbered . unnumbered\), you can also
provide a list of 2 or 4 elements,

  \(numbered-open numbered-close\)

or

  \(numbered-open numbered-close unnumbered-open unnumbered-close\)

providing opening and closing strings for a LaTeX environment that should
represent the document section.  The opening clause should have a %s
to represent the section title.

Instead of a list of sectioning commands, you can also specify a
function name.  That function will be called with two parameters,
the (reduced) level of the headline, and a predicate non-nil when
the headline should be numbered.  It must return a format string in
which the section title will be added."
  :group 'org-export-latex
  :type '(repeat
	  (list (string :tag "LaTeX class")
		(string :tag "LaTeX header")
		(repeat :tag "Levels" :inline t
			(choice
			 (cons :tag "Heading"
			       (string :tag "  numbered")
			       (string :tag "unnumbered"))
			 (list :tag "Environment"
			       (string :tag "Opening   (numbered)")
			       (string :tag "Closing   (numbered)")
			       (string :tag "Opening (unnumbered)")
			       (string :tag "Closing (unnumbered)"))
			 (function :tag "Hook computing sectioning"))))))

(defcustom org-e-latex-inputenc-alist nil
  "Alist of inputenc coding system names, and what should really be used.
For example, adding an entry

      (\"utf8\" . \"utf8x\")

will cause \\usepackage[utf8x]{inputenc} to be used for buffers that
are written as utf8 files."
  :group 'org-export-latex
  :type '(repeat
	  (cons
	   (string :tag "Derived from buffer")
	   (string :tag "Use this instead"))))

(defcustom org-e-latex-date-format
  "\\today"
  "Format string for \\date{...}."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-e-latex-title-command "\\maketitle"
  "The command used to insert the title just after \\begin{document}.
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-latex
  :type 'string)


;;;; Headline

(defcustom org-e-latex-format-headline-function nil
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword \(string or nil\).
TODO-TYPE the type of todo \(symbol: `todo', `done', nil\)
PRIORITY  the priority of the headline \(integer or nil\)
TEXT      the main headline text \(string\).
TAGS      the tags string, separated with colons \(string or nil\).

The function result will be used in the section format string.

As an example, one could set the variable to the following, in
order to reproduce the default set-up:

\(defun org-e-latex-format-headline-default \(todo todo-type priority text tags\)
  \"Default format function for an headline.\"
  \(concat \(when todo \(format \"\\\\textbf{\\\\textsc{\\\\textsf{%s}}} \" todo\)\)
	  \(when priority \(format \"\\\\framebox{\\\\#%c} \" priority\)\)
	  text
	  \(when tags \(format \"\\\\hfill{}\\\\textsc{%s}\" tags\)\)\)\)"
  :group 'org-export-latex
  :type 'function)


;;;; Emphasis

(defcustom org-e-latex-emphasis-alist
  '(("*" . "\\textbf{%s}")
    ("/" . "\\emph{%s}")
    ("_" . "\\underline{%s}")
    ("+" . "\\st{%s}")
    ("=" . protectedtexttt)
    ("~" . verb))
  "Alist of LaTeX expressions to convert emphasis fontifiers.

The key is the character used as a marker for fontification.  The
value is a formatting string to wrap fontified text with.

Value can also be set to the following symbols: `verb' and
`protectedtexttt'.  For the former, Org will use \"\\verb\" to
create a format string and select a delimiter character that
isn't in the string.  For the latter, Org will use \"\\texttt\"
to typeset and try to protect special characters."
  :group 'org-export-latex
  :type 'alist)


;;;; Footnotes

(defcustom org-e-latex-footnote-separator "\\textsuperscript{,}\\,"
  "Text used to separate footnotes."
  :group 'org-export-latex
  :type 'string)


;;;; Time-stamps

(defcustom org-e-latex-active-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to active time-stamps."
  :group 'org-export-latex
  :type 'string)

(defcustom org-e-latex-inactive-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to inactive time-stamps."
  :group 'org-export-latex
  :type 'string)

(defcustom org-e-latex-diary-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to diary time-stamps."
  :group 'org-export-latex
  :type 'string)


;;;; Links

(defcustom org-e-latex-image-default-option "width=.9\\linewidth"
  "Default option for images."
  :group 'org-export-latex
  :type 'string)

(defcustom org-e-latex-default-figure-position "htb"
  "Default position for latex figures."
  :group 'org-export-latex
  :type 'string)

(defcustom org-e-latex-inline-image-extensions
  '("pdf" "jpeg" "jpg" "png" "ps" "eps")
  "Extensions of image files that can be inlined into LaTeX.

Note that the image extension *actually* allowed depend on the
way the LaTeX file is processed.  When used with pdflatex, pdf,
jpg and png images are OK.  When processing through dvi to
Postscript, only ps and eps are allowed.  The default we use here
encompasses both."
  :group 'org-export-latex
  :type '(repeat (string :tag "Extension")))


;;;; Tables

(defcustom org-e-latex-default-table-environment "tabular"
  "Default environment used to build tables."
  :group 'org-export-latex
  :type 'string)

(defcustom org-e-latex-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-e-latex-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-e-latex-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-latex
  :type 'boolean)


;;;; Drawers

(defcustom org-e-latex-format-drawer-function nil
  "Function called to format a drawer in LaTeX code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-latex-format-drawer-default \(name contents\)
  \"Format a drawer element for LaTeX export.\"
  contents\)"
  :group 'org-export-latex
  :type 'function)


;;;; Inlinetasks

(defcustom org-e-latex-format-inlinetask-function nil
  "Function called to format an inlinetask in LaTeX code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a string.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-latex-format-inlinetask-default \(todo type priority name tags contents\)
\"Format an inline task element for LaTeX export.\"
  \(let \(\(full-title
	 \(concat
	  \(when todo \(format \"\\\\textbf{\\\\textsf{\\\\textsc{%s}}} \" todo\)\)
	  \(when priority \(format \"\\\\framebox{\\\\#%c} \" priority\)\)
	  title
	  \(when tags \(format \"\\\\hfill{}\\\\textsc{%s}\" tags\)\)\)\)\)
    \(format \(concat \"\\\\begin{center}\\n\"
		    \"\\\\fbox{\\n\"
		    \"\\\\begin{minipage}[c]{.6\\\\textwidth}\\n\"
		    \"%s\\n\\n\"
		    \"\\\\rule[.8em]{\\\\textwidth}{2pt}\\n\\n\"
		    \"%s\"
		    \"\\\\end{minipage}}\"
		    \"\\\\end{center}\"\)
	    full-title contents\)\)"
  :group 'org-export-latex
  :type 'function)


;; Src blocks

(defcustom org-e-latex-listings nil
  "Non-nil means export source code using the listings package.
This package will fontify source code, possibly even with color.
If you want to use this, you also need to make LaTeX use the
listings package, and if you want to have color, the color
package.  Just add these to `org-e-latex-packages-alist',
for example using customize, or with something like

  (require 'org-e-latex)
  (add-to-list 'org-export-latex-packages-alist '(\"\" \"listings\"))
  (add-to-list 'org-export-latex-packages-alist '(\"\" \"color\"))

Alternatively,

  (setq org-e-latex-listings 'minted)

causes source code to be exported using the minted package as
opposed to listings.  If you want to use minted, you need to add
the minted package to `org-e-latex-packages-alist', for
example using customize, or with

  (require 'org-e-latex)
  (add-to-list 'org-e-latex-packages-alist '(\"\" \"minted\"))

In addition, it is necessary to install
pygments (http://pygments.org), and to configure the variable
`org-e-latex-to-pdf-process' so that the -shell-escape option is
passed to pdflatex."
  :group 'org-export-latex
  :type '(choice
	  (const :tag "Use listings" t)
	  (const :tag "Use minted" 'minted)
	  (const :tag "Export verbatim" nil)))

(defcustom org-e-latex-listings-langs
  '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp")
    (c "C") (cc "C++")
    (fortran "fortran")
    (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby")
    (html "HTML") (xml "XML")
    (tex "TeX") (latex "TeX")
    (shell-script "bash")
    (gnuplot "Gnuplot")
    (ocaml "Caml") (caml "Caml")
    (sql "SQL") (sqlite "sql"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language parameter
for the listings package.  If the mode name and the listings name are
the same, the language does not need an entry in this list - but it does not
hurt if it is present."
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (symbol :tag "Major mode       ")
	   (string :tag "Listings language"))))

(defcustom org-e-latex-listings-options nil
  "Association list of options for the latex listings package.

These options are supplied as a comma-separated list to the
\\lstset command. Each element of the association list should be
a list containing two strings: the name of the option, and the
value. For example,

  (setq org-export-latex-listings-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (string :tag "Listings option name ")
	   (string :tag "Listings option value"))))

(defcustom org-e-latex-minted-langs
  '((emacs-lisp "common-lisp")
    (cc "c++")
    (cperl "perl")
    (shell-script "bash")
    (caml "ocaml"))
  "Alist mapping languages to their minted language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language parameter
for the minted package.  If the mode name and the listings name are
the same, the language does not need an entry in this list - but it does not
hurt if it is present.

Note that minted uses all lower case for language identifiers,
and that the full list of language identifiers can be obtained
with:
pygmentize -L lexers"
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (symbol :tag "Major mode     ")
	   (string :tag "Minted language"))))

(defcustom org-e-latex-minted-options nil
  "Association list of options for the latex minted package.

These options are supplied within square brackets in
\\begin{minted} environments. Each element of the alist should be
a list containing two strings: the name of the option, and the
value. For example,

  (setq org-export-latex-minted-options
    '((\"bgcolor\" \"bg\") (\"frame\" \"lines\")))

will result in src blocks being exported with

\\begin{minted}[bgcolor=bg,frame=lines]{<LANG>}

as the start of the minted environment. Note that the same
options will be applied to blocks of all languages."
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (string :tag "Minted option name ")
	   (string :tag "Minted option value"))))

(defvar org-e-latex-custom-lang-environments nil
  "Association list mapping languages to language-specific latex
environments used during export of src blocks by the listings and
minted latex packages. For example,

  (setq org-export-latex-custom-lang-environments
     '((python \"pythoncode\")))

would have the effect that if org encounters begin_src python
during latex export it will output

  \\begin{pythoncode}
  <src block body>
  \\end{pythoncode}")


;;;; Plain text

(defcustom org-e-latex-quotes
  '(("fr" ("\\(\\s-\\|[[(]\\)\"" . "«~") ("\\(\\S-\\)\"" . "~»") ("\\(\\s-\\|(\\)'" . "'"))
    ("en" ("\\(\\s-\\|[[(]\\)\"" . "``") ("\\(\\S-\\)\"" . "''") ("\\(\\s-\\|(\\)'" . "`")))
  "Alist for quotes to use when converting english double-quotes.

The CAR of each item in this alist is the language code.
The CDR of each item in this alist is a list of three CONS:
- the first CONS defines the opening quote;
- the second CONS defines the closing quote;
- the last CONS defines single quotes.

For each item in a CONS, the first string is a regexp
for allowed characters before/after the quote, the second
string defines the replacement string for this quote."
  :group 'org-export-latex
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



;;; Internal Functions

(defun org-e-latex--caption/label-string (caption label info)
  "Return caption and label LaTeX string for floats.

CAPTION is a secondary string \(a list of strings and Org
objects\) and LABEL a string representing the label. INFO is
a plist holding contextual information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-latex--wrap-label'."
  (let ((caption-str (and caption
			  (org-export-secondary-string
			   caption 'e-latex info)))
	(label-str (if label (format "\\label{%s}" label) "")))
    (cond
     ((and (not caption-str) (not label)) "")
     ((not caption-str) (format "\\label{%s}\n" label))
     ;; Option caption format with short name.
     ((string-match "\\[\\([^][]*\\)\\]{\\([^{}]*\\)}" caption-str)
      (format "\\caption[%s]{%s%s}\n"
	      (org-match-string-no-properties 1 caption-str)
	      label-str
	      (org-match-string-no-properties 2 caption-str)))
     ;; Standard caption format.
     (t (format "\\caption{%s%s}\n" label-str caption-str)))))

(defun org-e-latex--guess-inputenc (header)
  "Set the coding system in inputenc to what the buffer is.

HEADER is the LaTeX header string.

Return the new header."
  (let* ((cs (or (ignore-errors
		   (latexenc-coding-system-to-inputenc
		    buffer-file-coding-system))
		 "utf8")))
    (if (not cs)
	header
      ;; First translate if that is requested.
      (setq cs (or (cdr (assoc cs org-e-latex-inputenc-alist)) cs))
      ;; Then find the \usepackage statement and replace the option.
      (replace-regexp-in-string "\\\\usepackage\\[\\(AUTO\\)\\]{inputenc}"
				cs header t nil 1))))

(defun org-e-latex--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

(defun org-e-latex--make-option-string (options)
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

(defun org-e-latex--quotation-marks (text info)
  "Export quotation marks depending on language conventions."
  (mapc (lambda(l)
	  (let ((start 0))
	    (while (setq start (string-match (car l) text start))
	      (let ((new-quote (concat (match-string 1 text) (cdr l))))
		(setq text (replace-match new-quote  t t text))))))
	(cdr (or (assoc (plist-get info :language) org-e-latex-quotes)
		 ;; Falls back on English.
		 (assoc "en" org-e-latex-quotes))))
  text)

(defun org-e-latex--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats. See
`org-e-latex--caption/label-string'."
  (let ((label (org-element-get-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
	output
      (concat (format "\\label{%s}\n" label) output))))



;;; Template

(defun org-e-latex-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (let ((title (org-export-secondary-string
		(plist-get info :title) 'e-latex info)))
    (concat
     ;; 1. Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; 2. Document class and packages.
     (let ((class (plist-get info :latex-class))
	   (class-options (plist-get info :latex-class-options)))
       (org-element-normalize-string
	(let* ((header (nth 1 (assoc class org-e-latex-classes)))
	       (document-class-string
		(and (stringp header)
		     (if class-options
			 (replace-regexp-in-string
			  "^[ \t]*\\\\documentclass\\(\\[.*?\\]\\)"
			  class-options header t nil 1)
		       header))))
	  (org-e-latex--guess-inputenc
	   (org-splice-latex-header
	    document-class-string
	    org-export-latex-default-packages-alist ; defined in org.el
	    org-export-latex-packages-alist nil ; defined in org.el
	    (plist-get info :latex-header-extra))))))
     ;; 3. Define alert if not yet defined.
     "\\providecommand{\\alert}[1]{\\textbf{#1}}\n"
     ;; 4. Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-secondary-string
				     auth 'e-latex info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-secondary-string
			(plist-get info :email) 'e-latex info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     (author (format "\\author{%s}\n" author))
	     (t "\\author{}\n")))
     ;; 5. Date.
     (let ((date (plist-get info :date)))
       (and date (format "\\date{%s}\n" date)))
     ;; 6. Title
     (format "\\title{%s}\n" title)
     ;; 7. Hyperref options.
     (format "\\hypersetup{\n  pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}}\n"
	     (or (plist-get info :keywords) "")
	     (or (plist-get info :description) "")
	     (let ((creator-info (plist-get info :with-creator)))
	       (cond
		((not creator-info) "")
		((eq creator-info 'comment) "")
		(t (plist-get info :creator)))))
     ;; 7. Document start.
     "\\begin{document}\n\n"
     ;; 8. Title command.
     (org-element-normalize-string
      (cond ((string= "" title) nil)
	    ((not (stringp org-e-latex-title-command)) nil)
	    ((string-match "\\(?:[^%]\\|^\\)%s"
			   org-e-latex-title-command)
	     (format org-e-latex-title-command title))
	    (t org-e-latex-title-command)))
     ;; 9. Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat (when (wholenump depth)
		   (format "\\setcounter{tocdepth}{%d}\n" depth))
		 "\\tableofcontents\n\\vspace*{1cm}\n\n")))
     ;; 10. Document's body.
     contents
     ;; 11. Creator.
     (let ((creator-info (plist-get info :with-creator)))
       (cond
	((not creator-info))
	((eq creator-info 'comment)
	 (format "%% %s\n" (plist-get info :creator)))
	(t (concat (plist-get info :creator) "\n"))))
     ;; 12. Document end.
     "\\end{document}")))



;;; Transcode Functions

;;;; Block

(defun org-e-latex-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-latex--wrap-label
   center-block
   (format "\\begin{center}\n%s\\end{center}" contents)))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-latex-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-get-property :drawer-name drawer))
	 (output (if (functionp org-e-latex-format-drawer-function)
		     (funcall org-e-latex-format-drawer-function
			      name contents)
		   ;; If there's no user defined function: simply
		   ;; display contents of the drawer.
		   contents)))
    (org-e-latex--wrap-label drawer output)))


;;;; Dynamic Block

(defun org-e-latex-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See
`org-export-data'."
  (org-e-latex--wrap-label dynamic-block contents))


;;;; Emphasis

(defun org-e-latex-emphasis (emphasis contents info)
  "Transcode EMPHASIS from Org to LaTeX.
CONTENTS is the contents of the emphasized text.  INFO is a plist
holding contextual information.."
  (format (cdr (assoc (org-element-get-property :marker emphasis)
		      org-e-latex-emphasis-alist))
	  contents))


;;;; Entity

(defun org-e-latex-entity (entity contents info)
  "Transcode an ENTITY object from Org to LaTeX.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-get-property :latex entity)))
    (if (org-element-get-property :latex-math-p entity)
	(format "$%s$" ent)
      ent)))


;;;; Example Block

(defun org-e-latex-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((options (or (org-element-get-property :options example-block) ""))
	 (value (org-export-handle-code
		 (org-element-get-property :value example-block) options info)))
    (org-e-latex--wrap-label example-block value)))


;;;; Export Snippet

(defun org-e-latex-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-get-property :value export-snippet))


;;;; Export Block

(defun org-e-latex-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-get-property :type export-block) "latex")
    (org-remove-indentation (org-element-get-property :value export-block))))


;;;; Fixed Width

(defun org-e-latex-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((value (org-element-normalize-string
		 (replace-regexp-in-string
		  "^[ \t]*: ?" ""
		  (org-element-get-property :value fixed-width)))))
    (org-e-latex--wrap-label
     fixed-width
     (format "\\begin{verbatim}\n%s\\end{verbatim}" value))))


;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-e-latex-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to LaTeX.
CONTENTS is nil. INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (when (eq (plist-get info :previous-object) 'footnote-reference)
     org-e-latex-footnote-separator)
   ;; Use \footnotemark if the footnote has already been defined.
   ;; Otherwise, define it with \footnote command.
   (cond
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     (format "\\footnotemark[%s]"
	     (org-export-get-footnote-number footnote-reference info)))
    ;; Inline definitions are secondary strings.
    ((eq (org-element-get-property :type footnote-reference) 'inline)
     (format "\\footnote{%s}"
	     (org-trim
	      (org-export-secondary-string
	       (org-export-get-footnote-definition footnote-reference info)
	       'e-latex info))))
    ;; Non-inline footnotes definitions are full Org data.
    (t
     (format "\\footnote{%s}"
	     (org-trim
	      (org-export-data
	       (org-export-get-footnote-definition footnote-reference info)
	       'e-latex info)))))))


;;;; Headline

(defun org-e-latex-headline (headline contents info)
  "Transcode an HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :latex-class))
	 (numberedp (plist-get info :section-numbers))
	 ;; Get level relative to current parsed data.
	 (level (+ (org-element-get-property :level headline)
		   (plist-get info :headline-offset)))
	 (class-sectionning (assoc class org-e-latex-classes))
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
	      (concat (funcall (if numberedp #'car #'cdr) sec) "\n%s"))
	     ;; (numbered-open numbered-close)
	     ((= (length sec) 2)
	      (when numberedp (concat (car sec) "\n%s" (nth 1 sec))))
	     ;; (num-in num-out no-num-in no-num-out)
	     ((= (length sec) 4)
	      (if numberedp
		  (concat (car sec) "\n%s" (nth 1 sec))
		(concat (nth 2 sec) "\n%s" (nth 3 sec)))))))
	 (text (org-export-secondary-string
		(org-element-get-property :title headline) 'e-latex info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-get-property
				 :todo-keyword headline)))
		      (and todo
			   (org-export-secondary-string todo 'e-latex info)))))
	 (todo-type (and todo (org-element-get-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-element-get-property :tags headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-get-property :priority headline)))
	 ;; Create the headline text.
	 (full-text (if (functionp org-e-latex-format-headline-function)
			;; User-defined formatting function.
			(funcall org-e-latex-format-headline-function
				 todo todo-type priority text tags)
		      ;; Default formatting.
		      (concat
		       (when todo
			 (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
		       (when priority (format "\\framebox{\\#%c} " priority))
		       text
		       (when tags (format "\\hfill{}\\textsc{%s}" tags)))))
	 ;; Associate some \label to the headline for internal links.
	 (headline-labels (mapconcat
			   (lambda (p)
			     (let ((val (org-element-get-property p headline)))
			       (when val (format "\\label{%s}\n"
						 (if (eq p :begin)
						     (format "headline-%s" val)
						   val)))))
			   '(:custom-id :id :begin) ""))
	 (pre-blanks (make-string (org-element-get-property :pre-blank headline)
				  10)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-get-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt)
	  (and (wholenump (plist-get info :headline-levels))
	       (> level (plist-get info :headline-levels))))
      ;; Build the real contents of the sub-tree.
      (let ((low-level-body
	     (concat
	      ;; If the headline is the first sibling, start a list.
	      (when (org-export-first-sibling-p headline info)
		(format "\\begin{%s}\n" (if numberedp 'enumerate 'itemize)))
	      ;; Itemize headline
	      "\\item " full-text "\n" headline-labels pre-blanks contents)))
	;; If headline in the last sibling, close the list, before any
	;; blank line.  Otherwise, simply return LOW-LEVEL-BODY.
	(if (org-export-last-sibling-p headline info)
	    (replace-regexp-in-string
	     "[ \t\n]*\\'"
	     (format "\n\\\\end{%s}" (if numberedp 'enumerate 'itemize))
	     low-level-body)
	  low-level-body)))
     ;; Case 3. Standard headline.  Export it as a section.
     (t (format section-fmt full-text
		(concat headline-labels pre-blanks contents))))))


;;;; Horizontal Rule

(defun org-e-latex-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((attr (mapconcat #'identity
			 (org-element-get-property :attr_latex horizontal-rule)
			 " ")))
    (org-e-latex--wrap-label horizontal-rule (concat "\\hrule " attr))))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-latex-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-get-property :value inline-src-block))
	 (separator (org-e-latex--find-verb-separator code)))
    (cond
     ;; Do not use a special package: transcode it verbatim.
     ((not org-e-latex-listings)
      (concat "\\verb" separator code separator))
     ;; Use minted package.
     ((eq org-e-latex-listings 'minted)
      (let* ((org-lang (org-element-get-property :language inline-src-block))
	     (mint-lang (or (cadr (assq (intern org-lang)
					org-e-latex-minted-langs))
			    org-lang))
	     (options (org-e-latex--make-option-string
		       org-e-latex-minted-options)))
	(concat (format "\\mint%s{%s}"
			(if (string= options "") "" (format "[%s]" options))
			mint-lang)
		separator code separator)))
     ;; Use listings package.
     (t
      ;; Maybe translate language's name.
      (let* ((org-lang (org-element-get-property :language inline-src-block))
	     (lst-lang (or (cadr (assq (intern org-lang)
				       org-e-latex-listings-langs))
			   org-lang))
	     (options (org-e-latex--make-option-string
		       (append org-e-latex-listings-options
			       `(("language" ,lst-lang))))))
	(concat (format "\\lstinline[%s]" options)
		separator code separator))))))


;;;; Inlinetask

(defun org-e-latex-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-secondary-string
	       (org-element-get-property :title inlinetask) 'e-latex info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-get-property
				:todo-keyword inlinetask)))
		     (and todo
			  (org-export-secondary-string todo 'e-latex info)))))
	(todo-type (org-element-get-property :todo-type inlinetask))
	(tags (and (plist-get info :with-tags)
		   (org-element-get-property :tags inlinetask)))
	(priority (and (plist-get info :with-priority)
		       (org-element-get-property :priority inlinetask))))
    ;; If `org-e-latex-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-e-latex-format-inlinetask-function)
	(funcall org-e-latex-format-inlinetask-function
		 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-e-latex--wrap-label
       inlinetask
       (let ((full-title
	      (concat
	       (when todo (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
	       (when priority (format "\\framebox{\\#%c} " priority))
	       title
	       (when tags (format "\\hfill{}\\textsc{%s}" tags)))))
	 (format (concat "\\begin{center}\n"
			 "\\fbox{\n"
			 "\\begin{minipage}[c]{.6\\textwidth}\n"
			 "%s\n\n"
			 "\\rule[.8em]{\\textwidth}{2pt}\n\n"
			 "%s"
			 "\\end{minipage}\n"
			 "}\n"
			 "\\end{center}")
		 full-title contents))))))


;;;; Item

(defun org-e-latex-item (item contents info)
  "Transcode an ITEM element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((level (plist-get (plist-get info :parent-properties) :level))
	 (counter (let ((count (org-element-get-property :counter item)))
		    (and count
			 (< level 4)
			 (format "\\setcounter{enum%s}{%s}\n"
				 (nth level '("i" "ii" "iii" "iv"))
				 (1- count)))))
	 (checkbox (let ((checkbox (org-element-get-property :checkbox item)))
		     (cond ((eq checkbox 'on) "$\\boxtimes$ ")
			   ((eq checkbox 'off) "$\\Box$ ")
			   ((eq checkbox 'trans) "$\\boxminus$ "))))
	 (tag (let ((tag (org-element-get-property :tag item)))
		(and tag
		     (format "[%s]" (org-export-secondary-string
				     tag 'e-latex info))))))
    (concat counter "\\item" tag " " checkbox contents)))


;;;; Keyword

(defun org-e-latex-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (downcase (org-element-get-property :key keyword)))
	(value (org-element-get-property :value keyword)))
    (cond
     ((string= key "latex") value)
     ((string= key "index") (format "\\index{%s}" value))
     ((string= key "target")
      (format "\\label{%s}" (org-export-solidify-link-text value)))
     ((string= key "toc")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (concat
	     (when (wholenump depth)
	       (format "\\setcounter{tocdepth}{%s}\n" depth))
	     "\\tableofcontents")))
	 ((string= "tables" value) "\\listoftables")
	 ((string= "figures" value) "\\listoffigures")
	 ((string= "listings" value) "\\listoflistings"))))
     ((string= key "include")
      (org-export-included-file keyword 'e-latex info)))))


;;;; Latex Environment

(defun org-e-latex-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-latex--wrap-label
   latex-environment
   (org-remove-indentation (org-element-get-property :value latex-environment))))


;;;; Latex Fragment

(defun org-e-latex-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-get-property :value latex-fragment))


;;;; Line Break

(defun org-e-latex-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "\\\\")


;;;; Link

(defun org-e-latex-link--inline-image (path info)
  "Return LaTeX code for an image at PATH.
INFO is a plist containing export options."
  (let* ((parent-props (plist-get info :parent-properties))
	 (caption (org-e-latex--caption/label-string
		   (plist-get parent-props :caption)
		   (plist-get parent-props :name)
		   info))
	 ;; Retrieve latex attributes from the element around.
	 (attr (let ((raw-attr
		      (mapconcat #'identity
				 (plist-get parent-props :attr_latex) " ")))
		 (unless (string= raw-attr "") raw-attr)))
	 (disposition
	  (cond
	   ((and attr (string-match "\\<wrap\\>" attr)) 'wrap)
	   ((and attr (string-match "\\<multicolumn\\>" attr)) 'multicolumn)
	   ((or (and attr (string-match "\\<float\\>" attr))
		(not (string= caption "")))
	    'float)))
	 (placement
	  (cond
	   ((and attr (string-match "\\<placement=\\(\\S-+\\)" attr))
	    (org-match-string-no-properties 1 attr))
	   ((eq disposition 'wrap) "{l}{0.5\\textwidth}")
	   ((eq disposition 'float)
	    (concat "[" org-e-latex-default-figure-position "]"))
	   (t ""))))
    ;; Now clear ATTR from any special keyword and set a default
    ;; value if nothing is left.
    (if (not attr)
	(setq attr "")
      (while (string-match "\\(wrap\\|multicolumn\\|float\\|placement=\\S-+\\)"
			   attr)
	(replace-match "" nil nil attr))
      (setq attr (org-trim attr)))
    (setq attr (cond ((not (string= attr "")) attr)
		     ((eq disposition 'float) "width=0.7\\textwidth")
		     ((eq disposition 'wrap) "width=0.48\\textwidth")
		     (t (or org-e-latex-image-default-option ""))))
    ;; Return proper string, depending on DISPOSITION.
    (case disposition
      ('wrap (format "\\begin{wrapfigure}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{wrapfigure}" placement attr path caption))
      ('mulicolumn (format "\\begin{figure*}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{figure*}" placement attr path caption))
      ('float (format "\\begin{figure}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{figure}" placement attr path caption))
      (t (format "\\includegraphics[%s]{%s}" attr path)))))

(defun org-e-latex-link (link desc info)
  "Transcode a LINK object from Org to LaTeX.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information. See
`org-export-data'."
  (let* ((type (org-element-get-property :type link))
	 (raw-path (org-element-get-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (imagep (org-export-inline-image-p
		  link org-e-latex-inline-image-extensions))
	 (path (cond
		((member type '("http" "https" "ftp" "mailto"))
		 (concat type ":" raw-path))
		((and (not imagep) (string= type "file"))
		 (when (string-match "\\(.+\\)::.+" raw-path)
		   (setq raw-path (match-string 1 raw-path)))
		 (if (file-name-absolute-p raw-path)
		     (concat "file://" (expand-file-name raw-path))
		   ;; TODO: Not implemented yet.  Concat also:
		   ;; (org-export-directory :LaTeX info)
		   (concat "file://" raw-path)))
		(t raw-path)))
	 protocol)
    (cond
     ;; Image file.
     (imagep (org-e-latex-link--inline-image path info))
     ;; Id: for now, assume it's an internal link. TODO: do something
     ;; to check if it isn't in the current file.
     ((string= type "id")
      (format "\\hyperref[%s]{%s}" path (or desc path)))
     ;; Custom-id, target or radioed target: replace link with the
     ;; normalized custom-id/target name.
     ((member type '("custom-id" "target" "radio"))
      (format "\\hyperref[%s]{%s}"
	      (org-export-solidify-link-text path)
	      (or desc (org-export-secondary-string path 'e-latex info))))
     ;; Fuzzy: With the help of `org-export-resolve-fuzzy-link', find
     ;; the destination of the link.
     ((string= type "fuzzy")
      (let ((destination (org-export-resolve-fuzzy-link link info)))
	(cond
	 ;; Target match.
	 ((stringp destination)
	  (format "\\hyperref[%s]{%s}"
		  (org-export-solidify-link-text destination)
		  (or desc
		      (org-export-secondary-string
		       (org-element-get-property :raw-link link) 'e-latex info))))
	 ;; Headline match.
	 ((integerp destination)
	  (format "\\hyperref[headline-%d]{%s}"
		  destination
		  (or desc
		      (org-export-secondary-string
		       (org-element-get-property :raw-link link) 'e-latex info))))
	 ;; No match.
	 (t (format "\\texttt{%s}"
		    (or desc
			(org-export-secondary-string
			 (org-element-get-property :raw-link link)
			 'e-latex info)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path (or desc ""))
	      (cdr (assoc path (plist-get info :code-refs)))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'latex))
     ;; External link with a description part.
     ((and path desc) (format "\\href{%s}{%s}" path desc))
     ;; External link without a description part.
     (path (format "\\url{%s}" path))
     ;; No path, only description.  Try to do something useful.
     (t (format "\\texttt{%s}" desc)))))


;;;; Babel Call

;; Babel Calls are ignored.


;;;; Macro

(defun org-e-latex-macro (macro contents info)
  "Transcode a MACRO element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-latex-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to LaTeX.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  contents)


;;;; Plain List

(defun org-e-latex-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to LaTeX.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-get-property :type plain-list))
	 (paralist-types '("inparaenum" "asparaenum" "inparaitem" "asparaitem"
			   "inparadesc" "asparadesc"))
	 (paralist-regexp (concat
			   "\\("
			   (mapconcat 'identity paralist-types "\\|")
			   "\\)"))
	 (attr (mapconcat #'identity
			  (org-element-get-property :attr_latex plain-list)
			  " "))
	 (latex-type (cond
		      ((and attr
			    (string-match
			     (format "\\<%s\\>" paralist-regexp) attr))
		       (match-string 1 attr))
		      ((eq type 'ordered) "enumerate")
		      ((eq type 'unordered) "itemize")
		      ((eq type 'descriptive) "description"))))
    (org-e-latex--wrap-label
     plain-list
     (format "\\begin{%s}%s\n%s\\end{%s}"
	     latex-type
	     ;; Once special environment, if any, has been removed, the
	     ;; rest of the attributes will be optional arguments.
	     ;; They will be put inside square brackets if necessary.
	     (let ((opt (replace-regexp-in-string
			 (format " *%s *" paralist-regexp) "" attr)))
	       (cond ((string= opt "") "")
		     ((string-match "\\`\\[[^][]+\\]\\'" opt) opt)
		     (t (format "[%s]" opt))))
	     contents
	     latex-type))))


;;;; Plain Text

(defun org-e-latex-plain-text (text info)
  "Transcode a TEXT string from Org to LaTeX.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect %, #, &, $, ~, ^, _,  { and }.
  (while (string-match "\\([^\\]\\|^\\)\\([%$#&{}~^_]\\)" text)
    (setq text
	  (replace-match (format "\\%s" (match-string 2 text)) nil t text 2)))
  ;; Protect \
  (setq text (replace-regexp-in-string
	      "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
	      "$\\backslash$" text nil t 1))
  ;; LaTeX into \LaTeX{} and TeX into \TeX{}.
  (let ((case-fold-search nil)
	(start 0))
    (while (string-match "\\<\\(\\(?:La\\)?TeX\\)\\>" text start)
      (setq text (replace-match
		  (format "\\%s{}" (match-string 1 text)) nil t text)
	    start (match-end 0))))
  ;; Handle quotation marks
  (setq text (org-e-latex--quotation-marks text info))
  ;; Convert special strings.
  (when (plist-get info :with-special-strings)
    (while (string-match (regexp-quote "...") text)
      (setq text (replace-match "\\ldots{}" nil t text))))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
					 text)))
  ;; Return value.
  text)


;;;; Property Drawer

(defun org-e-latex-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")


;;;; Quote Block

(defun org-e-latex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-latex--wrap-label
   quote-block
   (format "\\begin{quote}\n%s\\end{quote}" contents)))


;;;; Quote Section

(defun org-e-latex-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-get-property :value quote-section))))
    (when value (format "\\begin{verbatim}\n%s\\end{verbatim}" value))))


;;;; Radio Target

(defun org-e-latex-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to LaTeX.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\label{%s}%s"
	  (org-export-solidify-link-text
	   (org-element-get-property :raw-value radio-target))
	  text))


;;;; Special Block

(defun org-e-latex-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-get-property :type special-block))))
    (org-e-latex--wrap-label
     special-block
     (format "\\begin{%s}\n%s\\end{%s}" type contents type))))


;;;; Src Block

(defun org-e-latex-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-get-property :language src-block))
	 (code (org-export-handle-code
		(org-element-get-property :value src-block)
		(org-element-get-property :switches src-block)
		info lang))
	 (caption (org-element-get-property :caption src-block))
	 (label (org-element-get-property :name src-block))
	 (custom-env (and lang
			  (cadr (assq (intern lang)
				      org-e-latex-custom-lang-environments)))))
    (cond
     ;; No source fontification.
     ((not org-e-latex-listings)
      (let ((caption-str (org-e-latex--caption/label-string
			  caption label info))
	    (float-env (when caption "\\begin{figure}[H]\n%s\n\\end{figure}")))
	(format (or float-env "%s")
		(concat
		 caption-str
		 (format "\\begin{verbatim}\n%s\\end{verbatim}" code)))))
     ;; Custom environment.
     (custom-env
      (format "\\begin{%s}\n%s\\end{%s}\n" custom-env code custom-env))
     ;; Use minted package.
     ((eq org-e-latex-listings 'minted)
      (let* ((mint-lang (or (cadr (assq (intern lang) org-e-latex-minted-langs))
			    lang))
	     (float-env (when (or label caption)
			  (format "\\begin{listing}[H]\n%%s\n%s\\end{listing}"
				  (org-e-latex--caption/label-string
				   caption label info))))
	     (body (format "\\begin{minted}[%s]{%s}\n%s\\end{minted}"
			   (org-e-latex--make-option-string
			    org-e-latex-minted-options)
			   mint-lang code)))
	(if float-env (format float-env body) body)))
     ;; Use listings package.
     (t
      (let ((lst-lang (or (cadr (assq (intern lang) org-e-latex-listings-langs))
			  lang))
	    (caption-str (and caption
			      (org-export-secondary-string
			       (org-element-get-property :caption src-block)
			       'e-latex info))))
	(concat (format "\\lstset{%s}\n"
			(org-e-latex--make-option-string
			 (append org-e-latex-listings-options
				 `(("language" ,lst-lang))
				 (when label `(("label" ,label)))
				 (when caption-str
				   `(("caption" ,caption-str))))))
		(format "\\begin{lstlisting}\n%s\\end{lstlisting}" code)))))))


;;;; Statistics Cookie

(defun org-e-latex-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-get-property :value statistics-cookie))


;;;; Subscript

(defun org-e-latex-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format (if (= (length contents) 1) "$_%s$" "$_{\\mathrm{%s}}$") contents))


;;;; Superscript

(defun org-e-latex-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format (if (= (length contents) 1) "$^%s$" "$^{\\mathrm{%s}}$") contents))


;;;; Table

(defun org-e-latex-table--format-string (table info)
  "Return an appropriate format string for TABLE.

INFO is the plist containing format info about the table, as
returned by `org-export-table-format-info'.

The format string one placeholder for the body of the table."
  (let* ((label (org-element-get-property :name table))
	 (caption (org-e-latex--caption/label-string
		   (org-element-get-property :caption table) label info))
	 (attr (mapconcat #'identity
			  (org-element-get-property :attr_latex table)
			  " "))
	 ;; Determine alignment string.
	 (alignment (org-e-latex-table--align-string attr info))
	 ;; Determine environment for the table: longtable, tabular...
	 (table-env (cond
		     ((not attr) org-e-latex-default-table-environment)
		     ((string-match "\\<longtable\\>" attr) "longtable")
		     ((string-match "\\(tabular.\\)" attr)
		      (org-match-string-no-properties 1 attr))
		     (t org-e-latex-default-table-environment)))
	 ;; If table is a float, determine environment: table or table*.
	 (float-env (cond
		     ((string= "longtable" table-env) nil)
		     ((and attr
			   (or (string-match (regexp-quote "table*") attr)
			       (string-match "\\<multicolumn\\>" attr)))
		      "table*")
		     ((or (not (string= caption "")) label) "table")))
	 ;; Extract others display options.
	 (width (and attr
		     (string-match "\\<width=\\(\\S-+\\)" attr)
		     (org-match-string-no-properties 1 attr)))
	 (placement (if (and attr
			     (string-match "\\<placement=\\(\\S-+\\)" attr))
			(org-match-string-no-properties 1 attr)
		      (concat "["
			      org-e-latex-default-figure-position
			      "]"))))
    ;; Prepare the final format string for the table.
    (cond
     ;; Longtable.
     ((string= "longtable" table-env)
      (format "\\begin{longtable}{%s}\n%s\n%%s\n%s\\end{longtable}"
	      alignment
	      (if (or (not org-e-latex-table-caption-above)
		      (string= "" caption))
		  ""
		(concat (org-trim caption) "\\\\"))
	      (if (or org-e-latex-table-caption-above
		      (string= "" caption))
		  ""
		(concat (org-trim caption) "\\\\\n"))))
     ;; Others.
     (t (concat (when float-env
		  (concat
		   (format "\\begin{%s}%s\n" float-env placement)
		   (if org-e-latex-table-caption-above caption "")))
		(when org-e-latex-tables-centered "\\begin{center}\n")
		(format "\\begin{%s}%s{%s}\n%%s\n\\end{%s}"
			table-env
			(if width (format "{%s}" width) "")
			alignment
			table-env)
		(when org-e-latex-tables-centered "\n\\end{center}")
		(when float-env
		  (concat (if org-e-latex-table-caption-above "" caption)
			  (format "\n\\end{%s}" float-env))))))))

(defun org-e-latex-table--align-string (attr info)
  "Return an appropriate LaTeX alignment string.

INFO is the plist containing format info about the table, as
returned by `org-export-table-format-info'."
  (or (and attr
	   (string-match "\\<align=\\(\\S-+\\)" attr)
	   (match-string 1 attr))
      (let* ((align (copy-sequence (plist-get info :alignment)))
	     (colgroups (copy-sequence (plist-get info :column-groups)))
	     (cols (length align))
	     (separators (make-vector (1+ cols) "")))
	;; Ignore the first column if it's special.
	(when (plist-get info :special-column-p)
	  (aset align 0 "") (aset colgroups 0 nil))
	(let ((col 0))
	  (mapc (lambda (el)
		  (let ((gr (aref colgroups col)))
		    (when (memq gr '(start start-end))
		      (aset separators col "|"))
		    (when (memq gr '(end start-end))
		      (aset separators (1+ col) "|")))
		  (incf col))
		align))
	;; Build the LaTeX specific alignment string.
	(loop for al across align
	      for sep across separators
	      concat (concat sep al) into output
	      finally return (concat output (aref separators cols))))))

(defun org-e-latex-table (table contents info)
  "Transcode a TABLE element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((attr (mapconcat #'identity
			 (org-element-get-property :attr_latex table)
			 " "))
	(raw-table (org-element-get-property :raw-table table)))
    (cond
     ;; Case 1: verbatim table.
     ((or org-e-latex-tables-verbatim
	  (and attr (string-match "\\<verbatim\\>" attr)))
      (format "\\begin{verbatim}\n%s\n\\end{verbatim}"
	      (org-export-clean-table
	       raw-table
	       (plist-get (org-export-table-format-info raw-table)
			  :special-column-p))))
     ;; Case 2: table.el table.  Convert it using appropriate tools.
     ((eq (org-element-get-property :type table) 'table.el)
      (require 'table)
      ;; Ensure "*org-export-table*" buffer is empty.
      (and (get-buffer "*org-export-table*")
	   (kill-buffer (get-buffer "*org-export-table*")))
      (let ((output (with-temp-buffer
		      (insert raw-table)
		      (goto-char 1)
		      (re-search-forward "^[ \t]*|[^|]" nil t)
		      (table-generate-source 'latex "*org-export-table*")
		      (with-current-buffer "*org-export-table*"
			(org-trim (buffer-string))))))
	(kill-buffer (get-buffer "*org-export-table*"))
	;; Remove left out comments.
	(while (string-match "^%.*\n" output)
	  (setq output (replace-match "" t t output)))
	;; When the "rmlines" attribute is provided, remove all hlines
	;; but the the one separating heading from the table body.
	(when (and attr (string-match "\\<rmlines\\>" attr))
	  (let ((n 0) (pos 0))
	    (while (and (< (length output) pos)
			(setq pos (string-match "^\\\\hline\n?" output pos)))
	      (incf n)
	      (unless (= n 2) (setq output (replace-match "" nil nil output))))))
	(if org-e-latex-tables-centered
	    (format "\\begin{center}\n%s\n\\end{center}" output)
	  output)))
     ;; Case 3: Standard table.
     (t
      (let* ((table-info (org-export-table-format-info raw-table))
	     (clean-table (org-export-clean-table
			   raw-table (plist-get table-info :special-column-p)))
	     (columns-number (length (plist-get table-info :alignment))))
	;; Convert ROWS to send them to `orgtbl-to-latex'.  In
	;; particular, send each cell to
	;; `org-element-parse-secondary-string' to expand any Org
	;; object within.  Eventually, flesh the format string out with
	;; the table.
	(format (org-e-latex-table--format-string table table-info)
		(orgtbl-to-latex
		 (mapcar
		  (lambda (row)
		    (if (string-match org-table-hline-regexp row)
			'hline
		      (mapcar
		       (lambda (cell)
			 (org-export-secondary-string
			  (org-element-parse-secondary-string
			   cell
			   (cdr (assq 'table org-element-string-restrictions)))
			  'e-latex info))
		       (org-split-string row "[ \t]*|[ \t]*"))))
		  (org-split-string clean-table "\n"))
		 `(:tstart nil :tend nil
			   ;; Longtable environment requires specific
			   ;; header line end.
			   :hlend ,(and attr
					(string-match "\\<longtable\\>" attr)
					(format "\\\\
\\hline
\\endhead
\\hline\\multicolumn{%d}{r}{Continued on next page}\\\\
\\endfoot
\\endlastfoot"
						columns-number))))))))))


;;;; Target

(defun org-e-latex-target (target text info)
  "Transcode a TARGET object from Org to LaTeX.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\label{%s}%s"
	  (org-export-solidify-link-text
	   (org-element-get-property :raw-value target))
	  text))


;;;; Time-stamp

(defun org-e-latex-time-stamp (time-stamp contents info)
  "Transcode a TIME-STAMP object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-element-get-property :value time-stamp))
	(type (org-element-get-property :type time-stamp))
	(appt-type (org-element-get-property :appt-type time-stamp)))
    (concat (cond ((eq appt-type 'scheduled)
		   (format "\\textbf{\\textsc{%s}} " org-scheduled-string))
		  ((eq appt-type 'deadline)
		   (format "\\textbf{\\textsc{%s}} " org-deadline-string))
		  ((eq appt-type 'closed)
		   (format "\\textbf{\\textsc{%s}} " org-closed-string)))
	    (cond ((memq type '(active active-range))
		   (format org-e-latex-active-timestamp-format value))
		  ((memq type '(inactive inactive-range))
		   (format org-e-latex-inactive-timestamp-format value))
		  (t
		   (format org-e-latex-diary-timestamp-format value))))))


;;;; Verbatim

(defun org-e-latex-verbatim (element contents info)
  "Return verbatim text in LaTeX."
  (let ((fmt (cdr (assoc (org-element-get-property :marker element)
			 org-e-latex-emphasis-alist)))
	(value (org-element-get-property :value element)))
    (cond
     ;; Handle the `verb' special case.
     ((eq 'verb fmt)
      (let ((separator (org-e-latex--find-verb-separator value)))
	(concat "\\verb" separator value separator)))
     ;; Handle the `protectedtexttt' special case.
     ((eq 'protectedtexttt fmt)
      (let ((start 0)
	    (trans '(("\\" . "\\textbackslash{}")
		     ("~" . "\\textasciitilde{}")
		     ("^" . "\\textasciicircum{}")))
	    (rtn "")
	    char)
	(while (string-match "[\\{}$%&_#~^]" value)
	  (setq char (match-string 0 value))
	  (if (> (match-beginning 0) 0)
	      (setq rtn (concat rtn (substring value 0 (match-beginning 0)))))
	  (setq value (substring value (1+ (match-beginning 0))))
	  (setq char (or (cdr (assoc char trans)) (concat "\\" char))
		rtn (concat rtn char)))
	(setq value (concat rtn value)
	      fmt "\\texttt{%s}")
	(while (string-match "--" value)
	  (setq value (replace-match "-{}-" t t value)))
	(format fmt value)))
     ;; Else use format string.
     (t (format fmt value)))))


;;;; Verse Block

(defun org-e-latex-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to LaTeX.
CONTENTS is nil. INFO is a plist holding contextual information."
  (org-e-latex--wrap-label
   verse-block
   ;; In a verse environment, add a line break to each newline
   ;; character and change each white space at beginning of a line
   ;; into a space of 1 em.  Also change each blank line with
   ;; a vertical space of 1 em.
   (progn
     (setq contents (replace-regexp-in-string
		     "^ *\\\\\\\\$" "\\\\vspace*{1em}"
		     (replace-regexp-in-string
		      "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
		      (org-remove-indentation
		       (org-export-secondary-string
			(org-element-get-property :value verse-block)
			'e-latex info)))))
     (while (string-match "^[ \t]+" contents)
       (let ((new-str (format "\\hspace*{%dem}"
			      (length (match-string 0 contents)))))
	 (setq contents (replace-match new-str nil t contents))))
     (format "\\begin{verse}\n%s\\end{verse}" contents))))


(provide 'org-e-latex)
;;; org-e-latex.el ends here
