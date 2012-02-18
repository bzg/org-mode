;;; org-e-html.el --- HTML Back-End For Org Export Engine

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

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

;; This library implements a HTML back-end for Org generic exporter.

;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-html "*Test e-HTML*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the HTML
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.

;; It introduces three new buffer keywords: "LATEX_CLASS",
;; "LATEX_CLASS_OPTIONS" and "LATEX_HEADER".

;;; Code:

;;; org-xhtml.el

(defvar org-e-html-debug nil)
(defvar org-e-html-pp nil)

(defun org-e-html-debug (fmt &rest args)
  (when org-e-html-debug
    (with-current-buffer (get-buffer "*debug*")
      (insert "\n" (apply 'format fmt args)))))

(defun org-element-debug (header text)
  (insert  "\n"  "===== [" header "] =====")
  (insert  "\n" (pp-to-string text)))

(defun org-elements-debug (args)
  (with-current-buffer "*debug*"
    (insert  "\n\n\n\n\n-------------------------\n")
    (while args
      (let* ((header (pop args))
	     (text (pop args)))
	(org-element-debug (format "%s" header) text)))
    (insert  "\n--------------------------\n")))

(defvar org-elements-debug-depth 0)
(defmacro org-e-html-pp (&rest args)
  (if org-e-html-pp
      (let ((newargs))
	(while args
	  (let ((e (pop args)))
	    (setq newargs (append newargs (list e (eval e))))))
	;; (pp-eval-expression 'newargs)

	`(org-elements-debug  (quote ,newargs)))
    (list 'ignore)))

(require 'org-exp)
(require 'format-spec)
(eval-when-compile (require 'cl) (require 'table))

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))

(defgroup org-export-e-html nil
  "Options specific for HTML export of Org-mode files."
  :tag "Org Export HTML"
  :group 'org-export)

(defconst org-export-e-html-special-string-regexps
  '(("\\\\-" . "&shy;")
    ("---\\([^-]\\)" . "&mdash;\\1")
    ("--\\([^-]\\)" . "&ndash;\\1")
    ("\\.\\.\\." . "&hellip;"))
  "Regular expressions for special string conversion.")

(defcustom org-export-e-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-e-html
  :type 'string)


(defcustom org-export-e-html-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-coding-system nil
  "Coding system for HTML export, defaults to `buffer-file-coding-system'."
  :group 'org-export-e-html
  :type 'coding-system)

(defcustom org-export-e-html-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations."
  :group 'org-export-e-html
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

(defcustom org-export-e-html-style-include-scripts t
  "Non-nil means include the JavaScript snippets in exported HTML files.
The actual script is defined in `org-export-e-html-scripts' and should
not be modified."
  :group 'org-export-e-html
  :type 'boolean)

(defconst org-export-e-html-scripts
"<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>"
"Basic JavaScript that is needed by HTML files produced by Org-mode.")

(defconst org-export-e-html-style-default
"<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: Times, serif; font-size: 12pt; }
  .title  { text-align: center; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  {margin-left:auto; margin-right:0px;  text-align:right;}
  .left   {margin-left:0px;  margin-right:auto; text-align:left;}
  .center {margin-left:auto; margin-right:auto; text-align:center;}
  p.verse { margin-left: 3% }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top;  }
  th.right  { text-align:center;  }
  th.left   { text-align:center;   }
  th.center { text-align:center; }
  td.right  { text-align:right;  }
  td.left   { text-align:left;   }
  td.center { text-align:center; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  div.inlinetask {
    padding:10px;
    border:2px solid gray;
    margin:10px;
    background: #ffffcc;
  }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported HTML files.
Please use the variables `org-export-e-html-style' and
`org-export-e-html-style-extra' to add to this style.  If you wish to not
have the default style included, customize the variable
`org-export-e-html-style-include-default'.")

(defcustom org-export-e-html-style-include-default t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-export-e-html-style-default' and should
not be modified.  Use the variables `org-export-e-html-style' to add
your own style information."
  :group 'org-export-e-html
  :type 'boolean)
;;;###autoload
(put 'org-export-e-html-style-include-default 'safe-local-variable 'booleanp)

(defcustom org-export-e-html-style ""
  "Org-wide style definitions for exported HTML files.

This variable needs to contain the full HTML structure to provide a style,
including the surrounding HTML tags.  If you set the value of this variable,
you should consider to include definitions for the following classes:
 title, todo, done, timestamp, timestamp-kwd, tag, target.

For example, a valid value would be:

   <style type=\"text/css\">
    <![CDATA[
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .timestamp-kwd { color: red; }
      .done { color: green; }
    ]]>
   </style>

If you'd like to refer to an external style file, use something like

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to add arbitrary text to the header.
See also the variable `org-export-e-html-style-extra'."
  :group 'org-export-e-html
  :type 'string)
;;;###autoload
(put 'org-export-e-html-style 'safe-local-variable 'stringp)

(defcustom org-export-e-html-style-extra ""
  "Additional style information for HTML export.
The value of this variable is inserted into the HTML buffer right after
the value of `org-export-e-html-style'.  Use this variable for per-file
settings of style information, and do not forget to surround the style
settings with <style>...</style> tags."
  :group 'org-export-e-html
  :type 'string)
;;;###autoload
(put 'org-export-e-html-style-extra 'safe-local-variable 'stringp)

(defcustom org-export-e-html-mathjax-options
  '((path  "http://orgmode.org/mathjax/MathJax.js")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml nil))
  "Options for MathJax setup.

path        The path where to find MathJax
scale       Scaling for the HTML-CSS backend, usually between 100 and 133
align       How to align display math: left, center, or right
indent      If align is not center, how far from the left/right side?
mathml      Should a MathML player be used if available?
            This is faster and reduces bandwidth use, but currently
            sometimes has lower spacing quality.  Therefore, the default is
            nil.  When browsers get better, this switch can be flipped.

You can also customize this for each buffer, using something like

#+MATHJAX: scale:\"133\" align:\"right\" mathml:t path:\"/MathJax/\""
  :group 'org-export-e-html
  :type '(list :greedy t
	      (list :tag "path   (the path from where to load MathJax.js)"
		    (const :format "       " path) (string))
	      (list :tag "scale  (scaling for the displayed math)"
		    (const :format "       " scale) (string))
	      (list :tag "align  (alignment of displayed equations)"
		    (const :format "       " align) (string))
	      (list :tag "indent (indentation with left or right alignment)"
		    (const :format "       " indent) (string))
	      (list :tag "mathml (should MathML display be used is possible)"
		    (const :format "       " mathml) (boolean))))

(defun org-export-e-html-mathjax-config (template options in-buffer)
  "Insert the user setup into the matchjax template."
  (let (name val (yes "   ") (no "// ") x)
    (mapc
     (lambda (e)
       (setq name (car e) val (nth 1 e))
       (if (string-match (concat "\\<" (symbol-name name) ":") in-buffer)
	   (setq val (car (read-from-string
			   (substring in-buffer (match-end 0))))))
       (if (not (stringp val)) (setq val (format "%s" val)))
       (if (string-match (concat "%" (upcase (symbol-name name))) template)
	   (setq template (replace-match val t t template))))
     options)
    (setq val (nth 1 (assq 'mathml options)))
    (if (string-match (concat "\\<mathml:") in-buffer)
	(setq val (car (read-from-string
			(substring in-buffer (match-end 0))))))
    ;; Exchange prefixes depending on mathml setting
    (if (not val) (setq x yes yes no no x))
    ;; Replace cookies to turn on or off the config/jax lines
    (if (string-match ":MMLYES:" template)
	(setq template (replace-match yes t t template)))
    (if (string-match ":MMLNO:" template)
	(setq template (replace-match no t t template)))
    ;; Return the modified template
    template))

(defcustom org-export-e-html-mathjax-template
  "<script type=\"text/javascript\" src=\"%PATH\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>"
  "The MathJax setup for XHTML files."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-preamble t
  "Non-nil means insert a preamble in HTML export.

When `t', insert a string as defined by one of the formatting
strings in `org-export-e-html-preamble-format'.  When set to a
string, this string overrides `org-export-e-html-preamble-format'.
When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-e-html
  :type '(choice (const :tag "No preamble" nil)
		 (const :tag "Default preamble" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-export-e-html-preamble-format '(("en" ""))
  "The format for the HTML preamble.

%t stands for the title.
%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When `t', insert a string as defined by the formatting string in
`org-export-e-html-postamble-format'.  When set to a string, this
string overrides `org-export-e-html-postamble-format'.  When set to
'auto, discard `org-export-e-html-postamble-format' and honor
`org-export-author/email/creator-info' variables.  When set to a
function, apply this function and insert the returned string.
The function takes the property list of export options as its
only argument.

Setting :html-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-e-html
  :type '(choice (const :tag "No postamble" nil)
		 (const :tag "Auto preamble" 'auto)
		 (const :tag "Default formatting string" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-export-e-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">Generated by %c</p>
<p class=\"xhtml-validation\">%v</p>
"))
  "The format for the HTML postamble.

%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.
%c will be replaced by information about Org/Emacs versions.
%v will be replaced by `org-export-e-html-validation-link'.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-home/up-format
  "<div id=\"org-div-home-and-up\" style=\"text-align:right;font-size:70%%;white-space:nowrap;\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-export-e-html-link-up' and
`org-export-e-html-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-e-html-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When org-mode is exporting an org-mode file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked org-mode file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-e-html
  :type 'boolean)

(defcustom org-export-e-html-inline-images 'maybe
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-e-html
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

(defcustom org-export-e-html-inline-image-extensions
  '("png" "jpeg" "jpg" "gif" "svg")
  "Extensions of image files that can be inlined into HTML."
  :group 'org-export-e-html
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-e-html-table-tag
  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">"
  "The HTML tag that is used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-export-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-export-e-html-table-use-header-tags-for-first-column'.
See also the variable `org-export-e-html-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-data-tags '("<td%s>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-export-e-html-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-row-tags '("<tr>" . "</tr>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be evaluated
for each row in order to construct the table row tags.  During evaluation,
the variable `head' will be true when this is a header line, nil when this
is a body line.  And the variable `nline' will contain the line number,
starting from 1 in the first header line.  For example

  (setq org-export-table-row-tags
        (cons '(if head
                   \"<tr>\"
                 (if (= (mod nline 2) 1)
                     \"<tr class=\\\"tr-odd\\\">\"
                   \"<tr class=\\\"tr-even\\\">\"))
              \"</tr>\"))

will give even lines the class \"tr-even\" and odd lines the class \"tr-odd\"."
  :group 'org-export-tables
  :type '(cons
	  (choice :tag "Opening tag"
		  (string :tag "Specify")
		  (sexp))
	  (choice :tag "Closing tag"
		  (string :tag "Specify")
		  (sexp))))

(defcustom org-export-e-html-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-e-html-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-e-html-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"
  "Link to HTML validation service."
  :group 'org-export-e-html
  :type 'string)

;; FIXME Obsolete since Org 7.7
;; Use the :timestamp option or `org-export-time-stamp-file' instead
(defvar org-export-e-html-with-timestamp nil
  "If non-nil, write container for HTML-helper-mode timestamp.")

;; FIXME Obsolete since Org 7.7
(defvar org-export-e-html-html-helper-timestamp
  "\n<p><br/><br/>\n<!-- hhmts start --> <!-- hhmts end --></p>\n"
  "The HTML tag used as timestamp delimiter for HTML-helper-mode.")

(defcustom org-export-e-html-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-e-html-protect'."
  :group 'org-export-e-html
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "HTML equivalent"))))

(defgroup org-export-e-htmlize nil
  "Options for processing examples with htmlize.el."
  :tag "Org Export Htmlize"
  :group 'org-export-e-html)

(defcustom org-export-e-htmlize-output-type 'inline-css
  "Output type to be used by htmlize when formatting code snippets.
Choices are `css', to export the CSS selectors only, or `inline-css', to
export the CSS attribute values inline in the HTML.  We use as default
`inline-css', in order to make the resulting HTML self-containing.

However, this will fail when using Emacs in batch mode for export, because
then no rich font definitions are in place.  It will also not be good if
people with different Emacs setup contribute HTML files to a website,
because the fonts will represent the individual setups.  In these cases,
it is much better to let Org/Htmlize assign classes only, and to use
a style file to define the look of these classes.
To get a start for your css file, start Emacs session and make sure that
all the faces you are interested in are defined, for example by loading files
in all modes you want.  Then, use the command
\\[org-export-e-htmlize-generate-css] to extract class definitions."
  :group 'org-export-e-htmlize
  :type '(choice (const css) (const inline-css)))

(defcustom org-export-e-htmlize-css-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-e-htmlize
  :type 'string)

(defcustom org-export-e-htmlized-org-css-url nil
  "URL pointing to a CSS file defining text colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer, htmlize will
create CSS to define the font colors.  However, this does not work when
converting in batch mode, and it also can look bad if different people
with different fontification setup work on the same website.
When this variable is non-nil, creating an htmlized version of an Org buffer
using `org-export-as-org' will remove the internal CSS section and replace it
with a link to this URL."
  :group 'org-export-e-htmlize
  :type '(choice
	  (const :tag "Keep internal css" nil)
	  (string :tag "URL or local href")))

;; FIXME: The following variable is obsolete since Org 7.7 but is
;; still declared and checked within code for compatibility reasons.
;; Use the custom variables `org-export-e-html-divs' instead.
(defvar org-export-e-html-content-div "content"
  "The name of the container DIV that holds all the page contents.

This variable is obsolete since Org version 7.7.
Please set `org-export-e-html-divs' instead.")

(defcustom org-export-e-html-divs '("preamble" "content" "postamble")
  "The name of the main divs for HTML export.
This is a list of three strings, the first one for the preamble
DIV, the second one for the content DIV and the third one for the
postamble DIV."
  :group 'org-export-e-html
  :type '(list
	  (string :tag " Div for the preamble:")
	  (string :tag "  Div for the content:")
	  (string :tag "Div for the postamble:")))

;;; Hooks

(defvar org-export-e-html-after-blockquotes-hook nil
  "Hook run during HTML export, after blockquote, verse, center are done.")

(defvar org-export-e-html-final-hook nil
  "Hook run at the end of HTML export, in the new buffer.")

(defun org-export-e-html-preprocess-latex-fragments ()
  (when (equal org-lparse-backend 'html)
    (org-export-e-html-do-preprocess-latex-fragments)))

(defvar org-lparse-opt-plist)		    ; bound during org-do-lparse
(defun org-export-e-html-do-preprocess-latex-fragments ()
  "Convert HTML fragments to images."
  (let* ((latex-frag-opt (plist-get org-lparse-opt-plist :HTML-fragments))
	 (latex-frag-opt-1		; massage the options
	  (cond
	   ((eq latex-frag-opt 'verbatim) 'verbatim)
	   ((eq latex-frag-opt 'mathjax ) 'mathjax)
	   ((eq latex-frag-opt t        ) 'mathjax)
	   ((eq latex-frag-opt 'dvipng  ) 'dvipng)
	   (t nil))))
    (when (and org-current-export-file latex-frag-opt)
      (org-format-latex
       (concat "ltxpng/" (file-name-sans-extension
			  (file-name-nondirectory
			   org-current-export-file)))
       org-current-export-dir nil "Creating HTML image %s"
       nil nil latex-frag-opt-1))))

(defun org-export-e-html-preprocess-label-references ()
  (goto-char (point-min))
  (let (label l1)
    (while (re-search-forward "\\\\ref{\\([^{}\n]+\\)}" nil t)
      (org-if-unprotected-at (match-beginning 1)
	(setq label (match-string 1))
	(save-match-data
	  (if (string-match "\\`[a-z]\\{1,10\\}:\\(.+\\)" label)
	      (setq l1 (substring label (match-beginning 1)))
	    (setq l1 label)))
	(replace-match (format "[[#%s][%s]]" label l1) t t)))))

(defun org-export-e-html-preprocess (parameters)
  (org-export-e-html-preprocess-label-references))

;; Process latex fragments as part of
;; `org-export-preprocess-after-blockquote-hook'. Note that this hook
;; is the one that is closest and well before the call to
;; `org-export-attach-captions-and-attributes' in
;; `org-export-preprocess-stirng'.  The above arrangement permits
;; captions, labels and attributes to be attached to png images
;; generated out of latex equations.
(add-hook 'org-export-preprocess-after-blockquote-hook
	  'org-export-e-html-preprocess-latex-fragments)

(defvar html-table-tag nil) ; dynamically scoped into this.


;; FIXME: it already exists in org-e-html.el
(defconst org-e-html-cvt-link-fn
   nil
   "Function to convert link URLs to exportable URLs.
Takes two arguments, TYPE and PATH.
Returns exportable url as (TYPE PATH), or nil to signal that it
didn't handle this case.
Intended to be locally bound around a call to `org-export-as-html'." )


;; FIXME: it already exists in org-e-html.el
(defun org-e-html-cvt-org-as-html (opt-plist type path)
   "Convert an org filename to an equivalent html filename.
If TYPE is not file, just return `nil'.
See variable `org-export-e-html-link-org-files-as-html'"

   (save-match-data
      (and
	 org-export-e-html-link-org-files-as-html
	 (string= type "file")
	 (string-match "\\.org$" path)
	 (progn
	    (list
	       "file"
	       (concat
		  (substring path 0 (match-beginning 0))
		  "."
		  (plist-get opt-plist :html-extension)))))))

(defun org-e-html-format-org-link (opt-plist type-1 path fragment desc attr
					    descp)
  "Make an HTML link.
OPT-PLIST is an options list.
TYPE is the device-type of the link (THIS://foo.html).
PATH is the path of the link (http://THIS#location).
FRAGMENT is the fragment part of the link, if any (foo.html#THIS).
DESC is the link description, if any.
ATTR is a string of other attributes of the \"a\" element."
  (declare (special org-lparse-par-open))
  (save-match-data
    (when (string= type-1 "coderef")
      (let ((ref fragment))
	(setq desc (format (org-export-get-coderef-format ref (and descp desc))
			   (cdr (assoc ref org-export-code-refs)))
	      fragment (concat  "coderef-" ref)
	      attr (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, '%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
			   fragment fragment))))
    (let* ((may-inline-p
	    (and (member type-1 '("http" "https" "file"))
		 (org-lparse-should-inline-p path descp)
		 (not fragment)))
	   (type (if (equal type-1 "id") "file" type-1))
	   (filename path)
	   ;;First pass.  Just sanity stuff.
	   (components-1
	    (cond
	     ((string= type "file")
	      (list
	       type
	       ;;Substitute just if original path was absolute.
	       ;;(Otherwise path must remain relative)
	       (if (file-name-absolute-p path)
		   (concat "file://" (expand-file-name path))
		 path)))
	     ((string= type "")
	      (list nil path))
	     (t (list type path))))

	   ;;Second pass.  Components converted so they can refer
	   ;;to a remote site.
	   (components-2
	    (or
	     (and org-e-html-cvt-link-fn
		  (apply org-e-html-cvt-link-fn
			 opt-plist components-1))
	     (apply #'org-e-html-cvt-org-as-html
		    opt-plist components-1)
	     components-1))
	   (type    (first  components-2))
	   (thefile (second components-2)))


      ;;Third pass.  Build final link except for leading type
      ;;spec.
      (cond
       ((or
	 (not type)
	 (string= type "http")
	 (string= type "https")
	 (string= type "file")
	 (string= type "coderef"))
	(if fragment
	    (setq thefile (concat thefile "#" fragment))))

       (t))

      ;;Final URL-build, for all types.
      (setq thefile
	    (let
		((str (org-xml-format-href thefile)))
	      (if (and type (not (or (string= "file" type)
				     (string= "coderef" type))))
		  (concat type ":" str)
		str)))

      (if may-inline-p
	  (org-e-html-format-image thefile)
	(org-lparse-format
	 'LINK (org-xml-format-desc desc) thefile attr)))))

(defun org-e-html-format-inline-image (path &optional caption label attr)
  ;; FIXME: alt text missing here?
  (let ((inline-image (format "<img src=\"%s\" alt=\"%s\"/>"
			      path (file-name-nondirectory path))))
    (if (not label) inline-image
      (org-e-html-format-section inline-image "figure" label))))

;; FIXME: the org-lparse defvar belongs to org-lparse.el
(defvar org-lparse-link-description-is-image)

(defun org-e-html-format-image (src)
  "Create image tag with source and attributes."
  (save-match-data
    (let* ((caption (org-find-text-property-in-string 'org-caption src))
	   (attr (org-find-text-property-in-string 'org-attributes src))
	   (label (org-find-text-property-in-string 'org-label src))
	   (caption (and caption (org-xml-encode-org-text caption)))
	   (img-extras (if (string-match "^ltxpng/" src)
			   (format " alt=\"%s\""
				   (org-find-text-property-in-string
				    'org-latex-src src))
			 (if (string-match "\\<alt=" (or attr ""))
			     (concat " " attr )
			   (concat " " attr " alt=\"" src "\""))))
	   (img (format "<img src=\"%s\"%s />" src img-extras))
	   (extra (concat
		   (and label
			(format "id=\"%s\" " (org-solidify-link-text label)))
		   "class=\"figure\"")))
      (if caption
	  (with-temp-buffer
	    (with-org-lparse-preserve-paragraph-state
	     (insert
	      (org-lparse-format
	       '("<div %s>" . "\n</div>")
	       (concat
		(org-lparse-format '("\n<p>" . "</p>") img)
		(org-lparse-format '("\n<p>" . "</p>") caption))
	       extra)))
	    (buffer-string))
	img))))

(defun org-export-e-html-get-bibliography ()
  "Find bibliography, cut it out and return it."
  (catch 'exit
    (let (beg end (cnt 1) bib)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^[ \t]*<div \\(id\\|class\\)=\"bibliography\"" nil t)
	  (setq beg (match-beginning 0))
	  (while (re-search-forward "</?div\\>" nil t)
	    (setq cnt (+ cnt (if (string= (match-string 0) "<div") +1 -1)))
	    (when (= cnt 0)
	      (and (looking-at ">") (forward-char 1))
	      (setq bib (buffer-substring beg (point)))
	      (delete-region beg (point))
	    (throw 'exit bib))))
	nil))))

(defmacro with-org-lparse-backend (backend &rest body)
  `(let* ((org-lparse-backend ,backend)
	  (org-lparse-entity-control-callbacks-alist
	   (org-lparse-get 'ENTITY-CONTROL))
	  (org-lparse-entity-format-callbacks-alist
	   (org-lparse-get 'ENTITY-FORMAT)))
     ,@body))

(defun org-e-html-format-table (lines olines)
  (let ((org-e-html-format-table-no-css nil))
    (org-lparse-format-table lines olines)))

;; Following variable is defined for native tables i.e., when
;; `org-lparse-table-is-styled' evals to t.
(defvar org-e-html-format-table-no-css)
(defvar org-table-number-regexp) ; defined in org-table.el

(defun org-format-table-html (lines olines &optional no-css)
  "Find out which HTML converter to use and return the HTML code.
NO-CSS is passed to the exporter."
  (with-org-lparse-backend
   'html (let* ((org-e-html-format-table-no-css no-css))
	   (org-lparse-format-table lines olines))))

(defun org-format-org-table-html (lines &optional splice no-css)
  (with-org-lparse-backend
   'html (let* ((org-e-html-format-table-no-css no-css))
	   (org-lparse-format-org-table lines splice))))

(defun org-export-splice-attributes (tag attributes)
  "Read attributes in string ATTRIBUTES, add and replace in HTML tag TAG."
  (if (not attributes)
      tag
    (let (oldatt newatt)
      (setq oldatt (org-extract-attributes-from-string tag)
	    tag (pop oldatt)
	    newatt (cdr (org-extract-attributes-from-string attributes)))
      (while newatt
	(setq oldatt (plist-put oldatt (pop newatt) (pop newatt))))
      (if (string-match ">" tag)
	  (setq tag
		(replace-match (concat (org-attributes-to-string oldatt) ">")
			       t t tag)))
      tag)))

(defun org-format-table-table-html (lines)
  (with-org-lparse-backend
   'html (org-lparse-format-table-table lines)))

(defun org-export-splice-style (style extra)
  "Splice EXTRA into STYLE, just before \"</style>\"."
  (if (and (stringp extra)
	   (string-match "\\S-" extra)
	   (string-match "</style>" style))
      (concat (substring style 0 (match-beginning 0))
	      "\n" extra "\n"
	      (substring style (match-beginning 0)))
    style))

(defvar htmlize-buffer-places)  ; from htmlize.el
(defun org-export-e-htmlize-region-for-paste (beg end)
  "Convert the region to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-export-e-htmlize-output-type)
	 (htmlize-css-name-prefix org-export-e-htmlize-css-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-export-e-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-e-htmlize-output-type' to `css', calls to
the function `org-export-e-htmlize-region-for-paste' will produce code
that uses these same face definitions."
  (interactive)
  (require 'htmlize)
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (org-pop-to-buffer-same-window "*html*")
  (goto-char (point-min))
  (if (re-search-forward "<style" nil t)
      (delete-region (point-min) (match-beginning 0)))
  (if (re-search-forward "</style>" nil t)
      (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (if (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(defvar body-only) ; dynamically scoped into this.

;; Following variable is let bound when `org-do-lparse' is in
;; progress. See org-lparse.el.

;; FIXME: the org-lparse defvar belongs to org-lparse.el
(defvar org-lparse-toc)
(defvar org-lparse-footnote-definitions)
(defvar org-lparse-dyn-first-heading-pos)

(defun org-e-html-end-export ()
  ;; insert the table of contents
  (when (and org-export-with-toc (not body-only) org-lparse-toc)
    (org-e-html-insert-toc org-lparse-toc))

  ;; remove empty paragraphs
  (goto-char (point-min))
  (while (re-search-forward "<p>[ \r\n\t]*</p>" nil t)
    (replace-match ""))

  ;; Convert whitespace place holders
  (goto-char (point-min))
  (let (beg end n)
    (while (setq beg (next-single-property-change (point) 'org-whitespace))
      (setq n (get-text-property beg 'org-whitespace)
	    end (next-single-property-change beg 'org-whitespace))
      (goto-char beg)
      (delete-region beg end)
      (insert (format "<span style=\"visibility:hidden;\">%s</span>"
		      (make-string n ?x)))))

  ;; Remove empty lines at the beginning of the file.
  (goto-char (point-min))
  (when (looking-at "\\s-+\n") (replace-match ""))

  ;; Remove display properties
  (remove-text-properties (point-min) (point-max) '(display t))

  ;; Run the hook
  (run-hooks 'org-export-e-html-final-hook))

(defun org-e-html-format-toc-entry (snumber todo headline tags href)
  (setq headline (concat
		  (and org-export-with-section-numbers
		       (concat snumber " "))
		  headline
		  (and tags
		    (concat
		     (org-lparse-format 'SPACES 3)
		     (org-lparse-format 'FONTIFY tags "tag")))))
  (when todo
    (setq headline (org-lparse-format 'FONTIFY headline "todo")))
  (org-lparse-format 'LINK headline (concat  "#" href)))

(defun org-e-html-format-toc-item (toc-entry level org-last-level)
  (when (> level org-last-level)
    (let ((cnt (- level org-last-level)))
      (while (>= (setq cnt (1- cnt)) 0)
	(org-lparse-begin-list 'unordered)
	(org-lparse-begin-list-item 'unordered))))
  (when (< level org-last-level)
    (let ((cnt (- org-last-level level)))
      (while (>= (setq cnt (1- cnt)) 0)
	(org-lparse-end-list-item-1)
	(org-lparse-end-list 'unordered))))

  (org-lparse-end-list-item-1)
  (org-lparse-begin-list-item 'unordered)
  (insert toc-entry))

(defun org-e-html-begin-toc (lang-specific-heading max-level)
  (org-lparse-insert-tag "<div id=\"table-of-contents\">")
  (insert
   (org-lparse-format 'HEADING lang-specific-heading
		     (or (org-lparse-get 'TOPLEVEL-HLEVEL) 1)))
  (org-lparse-insert-tag "<div id=\"text-table-of-contents\">")
  (org-lparse-begin-list 'unordered)
  (org-lparse-begin-list-item 'unordered))

(defun org-e-html-end-toc ()
  (while (> org-last-level (1- org-min-level))
    (setq org-last-level (1- org-last-level))
    (org-lparse-end-list-item-1)
    (org-lparse-end-list 'unordered))
  (org-lparse-insert-tag "</div>")
  (org-lparse-insert-tag "</div>")

  ;; cleanup empty list items in toc
  (while (re-search-backward "<li>[ \r\n\t]*</li>\n?" (point-min) t)
    (replace-match "")))

;;;###autoload
;; (defun org-export-as-html-and-open (arg)
;;   "Export the outline as HTML and immediately open it with a browser.
;; If there is an active region, export only the region.
;; The prefix ARG specifies how many levels of the outline should become
;; headlines.  The default is 3.  Lower levels will become bulleted lists."
;;   (interactive "P")
;;   (org-lparse-and-open "html" "html" arg))

;;;###autoload
;; (defun org-export-as-html-batch ()
;;   "Call the function `org-lparse-batch'.
;; This function can be used in batch processing as:
;; emacs   --batch
;;         --load=$HOME/lib/emacs/org.el
;;         --eval \"(setq org-export-headline-levels 2)\"
;;         --visit=MyFile --funcall org-export-as-html-batch"
;;   (org-lparse-batch "html"))

;;;###autoload
;; (defun org-export-as-html-to-buffer (arg)
;;   "Call `org-lparse-to-buffer` with output to a temporary buffer.
;; No file is created.  The prefix ARG is passed through to `org-lparse-to-buffer'."
;;   (interactive "P")
;;   (org-lparse-to-buffer "html" arg))

;;;###autoload
;; (defun org-replace-region-by-html (beg end)
;;   "Assume the current region has org-mode syntax, and convert it to HTML.
;; This can be used in any buffer.  For example, you could write an
;; itemized list in org-mode syntax in an HTML buffer and then use this
;; command to convert it."
;;   (interactive "r")
;;   (org-replace-region-by "html" beg end))

;;;###autoload
;; (defun org-export-region-as-html (beg end &optional body-only buffer)
;;   "Convert region from BEG to END in `org-mode' buffer to HTML.
;; If prefix arg BODY-ONLY is set, omit file header, footer, and table of
;; contents, and only produce the region of converted text, useful for
;; cut-and-paste operations.
;; If BUFFER is a buffer or a string, use/create that buffer as a target
;; of the converted HTML.  If BUFFER is the symbol `string', return the
;; produced HTML as a string and leave not buffer behind.  For example,
;; a Lisp program could call this function in the following way:

;;   (setq html (org-export-region-as-html beg end t 'string))

;; When called interactively, the output buffer is selected, and shown
;; in a window.  A non-interactive call will only return the buffer."
;;   (interactive "r\nP")
;;   (org-lparse-region "html" beg end body-only buffer))

;;; org-export-as-html
;;;###autoload
;; (defun org-export-as-html (arg &optional hidden ext-plist
;; 			       to-buffer body-only pub-dir)
;;   "Export the outline as a pretty HTML file.
;; Use `org-lparse' internally to perform the actual export. This
;; routine merely binds the TARGET-BACKEND and NATIVE-BACKEND args
;; of `org-lparse' to \"html\"."
;;   (interactive "P")
;;   (org-lparse "html" "html" arg hidden ext-plist to-buffer body-only pub-dir))

(defvar org-e-html-entity-control-callbacks-alist
  `((EXPORT
     . (org-e-html-begin-export org-e-html-end-export))
    (DOCUMENT-CONTENT
     . (org-e-html-begin-document-content org-e-html-end-document-content))
    (DOCUMENT-BODY
     . (org-e-html-begin-document-body org-e-html-end-document-body))
    (TOC
     . (org-e-html-begin-toc org-e-html-end-toc))
    (ENVIRONMENT
     . (org-e-html-begin-environment org-e-html-end-environment))
    (FOOTNOTE-DEFINITION
     . (org-e-html-begin-footnote-definition org-e-html-end-footnote-definition))
    (TABLE
     . (org-e-html-begin-table org-e-html-end-table))
    (TABLE-ROWGROUP
     . (org-e-html-begin-table-rowgroup org-e-html-end-table-rowgroup))
    (LIST
     . (org-e-html-begin-list org-e-html-end-list))
    (LIST-ITEM
     . (org-e-html-begin-list-item org-e-html-end-list-item))
    (OUTLINE
     . (org-e-html-begin-outline org-e-html-end-outline))
    (OUTLINE-TEXT
     . (org-e-html-begin-outline-text org-e-html-end-outline-text))
    (PARAGRAPH
     . (org-e-html-begin-paragraph org-e-html-end-paragraph)))
  "Alist of control callbacks registered with the exporter.
Each element is of the form (ENTITY . (BEGIN-ENTITY-FUNCTION
END-ENTITY-FUNCTION)).  ENTITY is one of PARAGRAPH, LIST etc as
seen above.  BEGIN-ENTITY-FUNCTION and END-ENTITY-FUNCTION are
functions that get called when the exporter needs to begin or end
an entity in the currently exported file.  The signatures of
these callbacks are specific to the ENTITY being emitted.  These
callbacks always get called with exported file as the current
buffer and need to insert the appropriate tags into the current
buffer.  For example, `org-e-html-begin-paragraph' inserts <p> and
`org-e-html-end-paragraph' inserts </p> in to the current buffer.
These callbacks are invoked via `org-lparse-begin' and
`org-lparse-end'.")

(defvar org-e-html-entity-format-callbacks-alist
  `((EXTRA-TARGETS . org-lparse-format-extra-targets)
    (ORG-TAGS . org-lparse-format-org-tags)
    (SECTION-NUMBER . org-lparse-format-section-number)
    (HEADLINE . org-e-html-format-headline)
    (TOC-ENTRY . org-e-html-format-toc-entry)
    (TOC-ITEM . org-e-html-format-toc-item)
    (TAGS . org-e-html-format-tags)
    (SPACES . org-e-html-format-spaces)
    (TABS . org-e-html-format-tabs)
    (LINE-BREAK . org-e-html-format-line-break)
    (FONTIFY . org-e-html-format-fontify)
    (TODO . org-lparse-format-todo)
    (ORG-LINK . org-e-html-format-org-link)
    (LINK . org-e-html-format-link)
    (INLINE-IMAGE . org-e-html-format-inline-image)
    (HEADING . org-e-html-format-heading)
    (ANCHOR . org-e-html-format-anchor)
    (TABLE . org-e-html-format-table)
    (TABLE-ROW . org-e-html-format-table-row)
    (TABLE-CELL . org-e-html-format-table-cell)
    (FOOTNOTES-SECTION . org-e-html-format-footnotes-section)
    (FOOTNOTE-REFERENCE . org-e-html-format-footnote-reference)
    (HORIZONTAL-LINE . org-e-html-format-horizontal-line)
    (LINE . org-e-html-format-line)
    (COMMENT . org-e-html-format-comment)
    (ORG-ENTITY . org-e-html-format-org-entity))
  "Alist of format callbacks registered with the exporter.
Each element is of the form (ENTITY . ENTITY-FORMAT-FUNCTION).
ENTITY is one of LINE, HEADING, COMMENT, LINK, TABLE-ROW etc as
seen above.  ENTITY-FORMAT-FUNCTION is a functions that gets
called when the exporter needs to format a string in `org-mode'
buffer in a backend specific way.  The signatures of the
formatting callback is specific to the ENTITY being passed in.
These callbacks always need to encode the incoming entity in
backend specific way and return the same.  These callbacks do not
make any modifications to the exporter file.  For example,
`org-e-html-format-table-row' encloses incoming entity in <tr>
</tr> tags and returns it.  See also `org-lparse-format'.")

(defun org-e-html-unload-function ()
  (org-lparse-unregister-backend 'html)
  (remove-hook 'org-export-preprocess-after-blockquote-hook
	       'org-export-e-html-preprocess-latex-fragments)
  nil)

(defun org-e-html-begin-body (info)
)

(defun org-e-html-begin-document-content (info)
)

(defun org-e-html-end-document-content ()
)

(defun org-e-html-begin-outline (level1 snumber title tags
				      target extra-targets extra-class)
  (let* ((class (format "outline-%d" level1))
	 (class (if extra-class (concat  class " " extra-class) class))
	 (id (format "outline-container-%s"
		     (org-lparse-suffix-from-snumber snumber)))
	 (extra (concat (when id (format " id=\"%s\"" id))
			(when class (format " class=\"%s\"" class)))))
    (org-lparse-insert-tag "<div%s>" extra)
    (insert
     (org-lparse-format 'HEADING
		       (org-lparse-format
			'HEADLINE title extra-targets tags snumber level1)
		       level1 target))))

(defun org-e-html-end-outline ()
  (org-lparse-insert-tag  "</div>"))


;; (defun org-e-html-format-heading (text level &optional id)
;;   (let* ((extra (concat (when id (format " id=\"%s\"" id)))))
;;     (concat (format "<h%d%s>" level extra) text (format "</h%d>" level))))

(defun org-e-html-suffix-from-snumber (snumber)
  (let* ((snu (replace-regexp-in-string "\\." "-" snumber))
	 (href (cdr (assoc (concat "sec-" snu)
			   org-export-preferred-target-alist))))
    (org-solidify-link-text (or href snu))))

(defun org-e-html-format-outline (contents level1 snumber title
					 tags target extra-targets extra-class)
  (let* ((class (format "outline-%d" level1))
	 (class (if extra-class (concat  class " " extra-class) class))
	 (id (and snumber ;; FIXME
		  (format "outline-container-%s"
			  (org-e-html-suffix-from-snumber snumber))))
	 (extra (concat (when id (format " id=\"%s\"" id))
			(when class (format " class=\"%s\"" class)))))
    (concat
     (format "<div%s>\n" extra)
     (org-e-html-format-heading
      (org-e-html-format-headline title extra-targets tags snumber level1)
      level1 target)

     contents

     "</div>")))

(defun org-e-html-begin-outline-text (level1 snumber extra-class)
  (let* ((class (format "outline-text-%d" level1))
	 (class (if extra-class (concat  class " " extra-class) class))
	 (id (format "text-%s" (org-lparse-suffix-from-snumber snumber)))
	 (extra (concat (when id (format " id=\"%s\"" id))
			(when class (format " class=\"%s\"" class)))))
    (org-lparse-insert-tag "<div%s>" extra)))

(defun org-e-html-end-outline-text ()
  (org-lparse-insert-tag "</div>"))

(defun org-e-html-begin-paragraph (&optional style)
  (let* ((class (cdr (assoc style '((footnote . "footnote")
				    (verse . nil)))))
	 (extra (if class (format " class=\"%s\"" class) "")))
    (org-lparse-insert-tag "<p%s>" extra)))

(defun org-e-html-end-paragraph ()
  (insert "</p>"))

(defun org-e-html-format-environment (style beg-end)
  (assert (memq style '(blockquote center verse fixedwidth quote native)) t)
  (case style
    (blockquote
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "<blockquote>\n")
	(org-lparse-begin-paragraph))
       (END
	(org-lparse-end-paragraph)
	(insert "\n</blockquote>\n")
	(org-lparse-begin-paragraph))))
    (verse
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "\n<p class=\"verse\">\n")
	(setq org-lparse-par-open t))
       (END
	(insert "</p>\n")
	(setq org-lparse-par-open nil)
	(org-lparse-begin-paragraph))))
    (center
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "\n<div style=\"text-align: center\">")
	(org-lparse-begin-paragraph))
       (END
	(org-lparse-end-paragraph)
	(insert "\n</div>")
	(org-lparse-begin-paragraph))))
    (fixedwidth
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "<pre class=\"example\">\n"))
       (END
	(insert "</pre>\n")
	(org-lparse-begin-paragraph))))
    (quote
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "<pre>"))
       (END
	(insert "</pre>\n")
	(org-lparse-begin-paragraph))))
    (native
     (case beg-end
       (BEGIN (org-lparse-end-paragraph))
       (END (org-lparse-begin-paragraph))))
    (t (error "Unknown environment %s" style))))

(defun org-e-html-begin-environment (style env-options-plist)
  (org-e-html-format-environment style 'BEGIN))

(defun org-e-html-end-environment (style env-options-plist)
  (org-e-html-format-environment style 'END))

(defun org-e-html-begin-list (ltype)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered (let* ((arg1 nil)
		    (extra (if arg1 (format " start=\"%d\"" arg1) "")))
	       (org-lparse-insert-tag "<ol%s>" extra)))
    (unordered (org-lparse-insert-tag "<ul>"))
    (description (org-lparse-insert-tag "<dl>"))
    (t (error "Unknown list type: %s"  ltype))))

(defun org-e-html-end-list (ltype)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))

  (org-lparse-insert-tag
     (case ltype
       (ordered "</ol>")
       (unordered "</ul>")
       (description "</dl>")
       (t (error "Unknown list type: %s" ltype)))))

(defun org-e-html-begin-list-item (ltype &optional arg headline)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered
     (assert (not headline) t)
     (let* ((counter arg)
	   (extra (if counter (format " value=\"%s\"" counter) "")))
       (org-lparse-insert-tag "<li%s>" extra)))
    (unordered
     (let* ((id arg)
	   (extra (if id (format " id=\"%s\"" id) "")))
       (org-lparse-insert-tag "<li%s>" extra)
       (when headline
	 (insert headline (org-lparse-format 'LINE-BREAK) "\n"))))
    (description
     (assert (not headline) t)
     (let* ((desc-tag (or arg "(no term)")))
       (insert
	(org-e-html-format-tags '("<dt>" . "</dt>") desc-tag))
       (org-lparse-insert-tag "<dd>")))
    (t (error "Unknown list type"))))

(defun org-e-html-end-list-item (ltype)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered (org-lparse-insert-tag "</li>"))
    (unordered (org-lparse-insert-tag "</li>"))
    (description (org-lparse-insert-tag "</dd>"))
    (t (error "Unknown list type"))))

;; Following variables are let bound when table emission is in
;; progress. See org-lparse.el.

;; FIXME: the org-lparse defvar belongs to org-lparse.el
(defvar org-lparse-table-begin-marker)
(defvar org-lparse-table-ncols)
(defvar org-lparse-table-rowgrp-open)
(defvar org-lparse-table-rownum)
(defvar org-lparse-table-cur-rowgrp-is-hdr)
(defvar org-lparse-table-is-styled)
(defvar org-lparse-table-rowgrp-info)
(defvar org-lparse-table-colalign-vector)
(defvar org-lparse-table-num-numeric-items-per-column)

(defun org-e-html-begin-footnote-definition (n)
  (org-lparse-begin-paragraph 'footnote)
  (insert
   (format
    (format org-export-e-html-footnote-format
	    "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>")
    n n n)))

(defun org-e-html-end-footnote-definition (n)
  (org-lparse-end-paragraph))


(defun org-e-html-format-footnote-definition (contents n)
  (concat
   (format
    (format org-export-e-html-footnote-format
	    "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>")
    n n n)

   contents))

;; (defun org-e-html-format-spaces (n)
;;   (let ((space (or (and org-lparse-encode-pending "\\nbsp") "&nbsp;")) out)
;;     (while (> n 0)
;;       (setq out (concat out space))
;;       (setq n (1- n))) out))

(defun org-e-html-format-tabs (&optional n)
  (ignore))

(defun org-e-html-format-line-break ()
  (org-e-html-format-tags "<br/>" ""))

(defun org-e-html-format-horizontal-line ()
  (concat  "\n" "<hr/>" "\n"))

(defun org-e-html-format-line (line)
  (case org-lparse-dyn-current-environment
    ((quote fixedwidth) (concat (org-e-html-encode-plain-text line) "\n"))
    (t (concat line "\n"))))

(defun org-e-html-format-comment (fmt &rest args)
  (let ((comment (apply 'format fmt args)))
    (format "\n<!-- %s  -->\n" comment)))

(defun org-e-html-fix-class-name (kwd) 	; audit callers of this function
  "Turn todo keyword into a valid class name.
Replaces invalid characters with \"_\"."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  kwd)

(defun org-e-html-format-fontify (text style &optional id)
  (let (class extra how)
    (cond
     ((eq style 'underline)
      (setq extra " style=\"text-decoration:underline;\"" ))
     ((setq how (cdr (assoc style
			    '((bold . ("<b>" . "</b>"))
			      (emphasis . ("<i>" . "</i>"))
			      (code . ("<code>" . "</code>"))
			      (verbatim . ("<code>" . "</code>"))
			      (strike . ("<del>" . "</del>"))
			      (subscript . ("<sub>" . "</sub>"))
			      (superscript . ("<sup>" . "</sup>")))))))
     ((listp style)
      (setq class (mapconcat 'identity style " ")))
     ((stringp style)
      (setq class style))
     (t (error "Unknown style %S" style)))

    (setq extra (concat (when class (format " class=\"%s\"" class))
			(when id (format " id=\"%s\""  id))
			extra))

    (let ((tags (or how '("<span%s>" . "</span>"))))
      (concat (format (car tags) extra) text  (cdr tags)))))

(defun org-e-html-format-link (text href &optional extra)
  (let ((extra (concat (format " href=\"%s\"" href)
		       (and extra (concat  " " extra)))))
    (format "<a%s>%s</a>" extra text)))

(defun org-e-html-format-internal-link (text href &optional extra)
  (org-e-html-format-link text (concat "#" href) extra))

(defun org-e-html-format-heading (text level &optional id)
  (let* ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<h%d%s>" level extra) text (format "</h%d>" level))))

;; (defun org-e-html-format-headline (title extra-targets tags
;; 					    &optional snumber level)
;;   (concat
;;    (org-lparse-format 'EXTRA-TARGETS extra-targets)
;;    (concat (org-lparse-format 'SECTION-NUMBER snumber level) " ")
;;    title
;;    (and tags (concat (org-lparse-format 'SPACES 3)
;; 		     (org-lparse-format 'ORG-TAGS tags)))))

(defun org-e-html-format-anchor (text name &optional class)
  (let* ((id name)
	 (extra (concat
		 (when name (format " name=\"%s\""  name))
		 (when id (format " id=\"%s\""  id))
		 (when class (format " class=\"%s\""  class)))))
    (format "<a%s>%s</a>" extra text)))

(defun org-e-html-format-extra-targets (extra-targets)
  (if (not extra-targets) ""
    (mapconcat (lambda (x)
		 (when x
		   (setq x (org-solidify-link-text
			    (if (org-uuidgen-p x) (concat "ID-" x) x)))
		   (org-e-html-format-anchor "" x))) extra-targets "")))

(defun org-e-html-format-spaces (n)
  (let (out) (dotimes (i n out) (setq out (concat out "&nbsp;")))))

(defun org-e-html-format-org-tags (tags)
  (if (not tags) ""
    (org-e-html-format-fontify
     (mapconcat
      (lambda (x)
	(org-e-html-format-fontify
	 x (concat org-export-e-html-tag-class-prefix
		   (org-e-html-fix-class-name x))))
      (org-split-string tags ":")
      (org-e-html-format-spaces 1)) "tag")))

(defun org-e-html-format-section-number (&optional snumber level)
  ;; FIXME
  (and org-export-with-section-numbers
       ;; (not org-lparse-body-only)
       snumber level
       (org-e-html-format-fontify snumber (format "section-number-%d" level))))

(defun org-e-html-format-headline (title extra-targets tags
				       &optional snumber level)
  (concat
   (org-e-html-format-extra-targets extra-targets)
   (concat (org-e-html-format-section-number snumber level) " ")
   title
   (and tags (concat (org-e-html-format-spaces 3)
		     (org-e-html-format-org-tags tags)))))

(defun org-e-html-format-footnote-reference (n def refcnt)
  (let ((extra (if (= refcnt 1) "" (format ".%d"  refcnt))))
    (format org-export-e-html-footnote-format
	    (format
	     "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>"
	     n extra n n))))

(defun org-e-html-format-footnotes-section (section-name definitions)
  (if (not definitions) ""
    (format org-export-e-html-footnotes-section section-name definitions)))

(defun org-e-html-format-org-entity (wd)
  (org-entity-get-representation wd 'html))

(defun org-e-html-format-tags (tag text &rest args)
  (let ((prefix (when org-lparse-encode-pending "@"))
	(suffix (when org-lparse-encode-pending "@")))
    (apply 'org-lparse-format-tags tag text prefix suffix args)))

(defun org-e-html-get (what &optional opt-plist)
  (case what
    (BACKEND 'html)
    (INIT-METHOD nil)
    (SAVE-METHOD nil)
    (CLEANUP-METHOD nil)
    ;; (OTHER-BACKENDS
    ;;  ;; There is a provision to register a per-backend converter and
    ;;  ;; output formats. Refer `org-lparse-get-converter' and
    ;;  ;; `org-lparse-get-other-backends'.

    ;;  ;; The default behaviour is to use `org-lparse-convert-process'
    ;;  ;; and `org-lparse-convert-capabilities'.
    ;;  )
    ;; (CONVERT-METHOD
    ;;  ;; See note above
    ;;  )
    (EXPORT-DIR (org-export-directory :html opt-plist))
    (FILE-NAME-EXTENSION (plist-get opt-plist :html-extension))
    (EXPORT-BUFFER-NAME "*Org HTML Export*")
    (ENTITY-CONTROL org-e-html-entity-control-callbacks-alist)
    (ENTITY-FORMAT org-e-html-entity-format-callbacks-alist)
    (TOPLEVEL-HLEVEL org-export-e-html-toplevel-hlevel)
    (SPECIAL-STRING-REGEXPS org-export-e-html-special-string-regexps)
    (CODING-SYSTEM-FOR-WRITE org-export-e-html-coding-system)
    (CODING-SYSTEM-FOR-SAVE org-export-e-html-coding-system)
    (INLINE-IMAGES org-export-e-html-inline-images)
    (INLINE-IMAGE-EXTENSIONS org-export-e-html-inline-image-extensions)
    (PLAIN-TEXT-MAP org-export-e-html-protect-char-alist)
    (TABLE-FIRST-COLUMN-AS-LABELS
     org-export-e-html-table-use-header-tags-for-first-column)
    (TODO-KWD-CLASS-PREFIX org-export-e-html-todo-kwd-class-prefix)
    (TAG-CLASS-PREFIX org-export-e-html-tag-class-prefix)
    (FOOTNOTE-SEPARATOR org-export-e-html-footnote-separator)
    (t (error "Unknown property: %s"  what))))

(defun org-e-html-get-coding-system-for-write ()
  (or org-export-e-html-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

(defun org-e-html-get-coding-system-for-save ()
  (or org-export-e-html-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

(defun org-e-html-insert-toc (toc)
  ;; locate where toc needs to be inserted
  (goto-char (point-min))
  (cond
   ((or (re-search-forward "<p>\\s-*\\[TABLE-OF-CONTENTS\\]\\s-*</p>" nil t)
	(re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t))
    (goto-char (match-beginning 0))
    (replace-match "")
    (insert toc))
   (org-lparse-dyn-first-heading-pos
    (goto-char org-lparse-dyn-first-heading-pos)
    (when (looking-at "\\s-*</p>")
      (goto-char (match-end 0))
      (insert "\n"))
    (insert toc))
   (t (ignore))))

(defun org-e-html-format-date (info)
  (let ((date (plist-get info :date)))
    (cond
     ((and date (string-match "%" date))
      (format-time-string date))
     (date date)
     (t (format-time-string "%Y-%m-%d %T %Z")))))

(defun org-e-html-footnote-section (info)
  (when org-e-html-footnotes-alist
    ;; (setq org-e-html-footnotes-alist
    ;; 	  (sort org-e-html-footnotes-alist
    ;; 		(lambda (n1 n2) (< (or (nth 1 n1) most-positive-fixnum)
    ;; 				   (or (nth 1 n2) most-positive-fixnum)))))

    ;; (setq org-e-html-footnote-alist (nreverse org-e-html-footnotes-alist))

    (setq org-e-html-footnotes-alist (nreverse org-e-html-footnotes-alist))
    (org-e-html-format-footnotes-section
     (nth 4 (or (assoc (plist-get info :language)
		       org-export-language-setup)
		(assoc "en" org-export-language-setup)))

     (format "
<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">
%s
</table>

"
	     (mapconcat
	      (lambda (x)
		(let ((n (car x))
		      (def (cdr x)))
		  (format "
<tr>
<td>%s</td>
<td>%s</td>
</tr>
"
			  (format
			   (format org-export-e-html-footnote-format
				   "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>")
			   n n n) def)))
	      org-e-html-footnotes-alist "\n")
	     )

     )))

(defun org-e-html-bibliography ()
  (org-export-e-html-get-bibliography))

(defun org-e-html-expand (s)
  (with-org-lparse-backend 'html (org-xml-encode-org-text-skip-links s)))

(defun org-e-html-protect (s)
  (with-org-lparse-backend 'html (org-e-html-encode-plain-text s)))

(defun org-e-html-do-expand (s)
  (with-org-lparse-backend 'html (org-xml-encode-org-text s)))

(defun org-export-e-html-format-href (s)
  (org-xml-format-href s))

(defun org-export-e-html-format-desc (s)
  (org-xml-format-desc s))

(eval-when-compile (require 'cl))
;;; org-e-html.el

(defvar org-export-latex-default-packages-alist)
(defvar org-export-latex-packages-alist)

(declare-function org-element-get-property "org-element" (property element))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-parse-secondary-string
		  "org-element" (string restriction &optional buffer))
(defvar org-element-string-restrictions)

(declare-function org-export-clean-table "org-export" (table specialp))
(declare-function org-export-data "org-export" (data backend info))
(declare-function org-export-directory "org-export" (type plist))
(declare-function org-export-expand-macro "org-export" (macro info))
(declare-function org-export-first-sibling-p "org-export" (headline info))
(declare-function org-export-footnote-first-reference-p "org-export"
		  (footnote-reference info))
(declare-function org-export-get-coderef-format "org-export" (path desc))
(declare-function org-export-get-footnote-definition "org-export"
		  (footnote-reference info))
(declare-function org-export-get-footnote-number "org-export" (footnote info))
(declare-function org-export-get-previous-element "org-export" (blob info))
(declare-function org-export-get-relative-level "org-export" (headline info))
(declare-function org-export-handle-code
		  "org-export" (element info &optional num-fmt ref-fmt delayed))
(declare-function org-export-included-file "org-export" (keyword backend info))
(declare-function org-export-inline-image-p "org-export"
		  (link &optional extensions))
(declare-function org-export-last-sibling-p "org-export" (headline info))
(declare-function org-export-low-level-p "org-export" (headline info))
(declare-function org-export-output-file-name
		  "org-export" (extension &optional subtreep pub-dir))
(declare-function org-export-resolve-coderef "org-export" (ref info))
(declare-function org-export-resolve-fuzzy-link "org-export" (link info))
(declare-function org-export-secondary-string "org-export"
		  (secondary backend info))
(declare-function org-export-solidify-link-text "org-export" (s))
(declare-function org-export-table-format-info "org-export" (table))
(declare-function
 org-export-to-buffer "org-export"
 (backend buffer &optional subtreep visible-only body-only ext-plist))
(declare-function
 org-export-to-file "org-export"
 (backend file &optional subtreep visible-only body-only ext-plist))



;;; Internal Variables

;; (defconst org-e-html-option-alist
;;   '((:date "DATE" nil org-e-html-date-format t)
;;     (:latex-class "LATEX_CLASS" nil org-e-html-default-class t)
;;     (:latex-class-options "LATEX_CLASS_OPTIONS" nil nil t)
;;     (:latex-header-extra "LATEX_HEADER" nil nil newline))
;;   "Alist between HTML export properties and ways to set them.
;; See `org-export-option-alist' for more information on the
;; structure of the value.")

(defconst org-e-html-option-alist
  '((:agenda-style nil nil org-agenda-export-html-style)
    (:convert-org-links nil nil org-export-e-html-link-org-files-as-html)
    ;; (:expand-quoted-html nil "@" org-export-e-html-expand) FIXME
    (:inline-images nil nil org-export-e-html-inline-images)
    ;; (:link-home nil nil org-export-e-html-link-home) FIXME
    ;; (:link-up nil nil org-export-e-html-link-up) FIXME
    (:style nil nil org-export-e-html-style)
    (:style-extra nil nil org-export-e-html-style-extra)
    (:style-include-default nil nil org-export-e-html-style-include-default)
    (:style-include-scripts nil nil org-export-e-html-style-include-scripts)
    (:timestamp nil nil org-export-e-html-with-timestamp)
    (:html-extension nil nil org-export-e-html-extension)
    (:html-postamble nil nil org-export-e-html-postamble)
    (:html-preamble nil nil org-export-e-html-preamble)
    (:html-table-tag nil nil org-export-e-html-table-tag)
    (:xml-declaration nil nil org-export-e-html-xml-declaration)
    (:LaTeX-fragments nil "LaTeX" org-export-with-LaTeX-fragments))
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



;;; User Configurable Variables

(defgroup org-export-e-html nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)


;;;; Preamble

(defcustom org-e-html-default-class "article"
  "The default HTML class."
  :group 'org-export-e-html
  :type '(string :tag "HTML class"))

(defcustom org-e-html-classes
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
  "Alist of HTML classes and associated header and structure.
If #+HTML_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    \(numbered-section . unnumbered-section\)
    ...\)

The header string
-----------------

The HEADER-STRING is the header that will be inserted into the
HTML file.  It should contain the \\documentclass macro, and
anything else that is needed for this setup.  To this header, the
following commands will be added:

- Calls to \\usepackage for all packages mentioned in the
  variables `org-export-latex-default-packages-alist' and
  `org-export-latex-packages-alist'.  Thus, your header
  definitions should avoid to also request these packages.

- Lines specified via \"#+HTML_HEADER:\"

If you need more control about the sequence in which the header
is built up, or if you want to exclude one of these building
blocks for a particular class, you can use the following
macro-like placeholders.

 [DEFAULT-PACKAGES]      \\usepackage statements for default packages
 [NO-DEFAULT-PACKAGES]   do not include any of the default packages
 [PACKAGES]              \\usepackage statements for packages
 [NO-PACKAGES]           do not include the packages
 [EXTRA]                 the stuff from #+HTML_HEADER
 [NO-EXTRA]              do not include #+HTML_HEADER stuff
 [BEAMER-HEADER-EXTRA]   the beamer extra headers

So a header like

  \\documentclass{article}
  [NO-DEFAULT-PACKAGES]
  [EXTRA]
  \\providecommand{\\alert}[1]{\\textbf{#1}}
  [PACKAGES]

will omit the default packages, and will include the
#+HTML_HEADER lines, then have a call to \\providecommand, and
then place \\usepackage commands based on the content of
`org-export-latex-packages-alist'.

If your header, `org-export-latex-default-packages-alist' or
`org-export-latex-packages-alist' inserts
\"\\usepackage[AUTO]{inputenc}\", AUTO will automatically be
replaced with a coding system derived from
`buffer-file-coding-system'.  See also the variable
`org-e-html-inputenc-alist' for a way to influence this
mechanism.

The sectioning structure
------------------------

The sectioning structure of the class is given by the elements
following the header string.  For each sectioning level, a number
of strings is specified.  A %s formatter is mandatory in each
section string and will be replaced by the title of the section.

Instead of a cons cell \(numbered . unnumbered\), you can also
provide a list of 2 or 4 elements,

  \(numbered-open numbered-close\)

or

  \(numbered-open numbered-close unnumbered-open unnumbered-close\)

providing opening and closing strings for a HTML environment
that should represent the document section.  The opening clause
should have a %s to represent the section title.

Instead of a list of sectioning commands, you can also specify
a function name.  That function will be called with two
parameters, the \(reduced) level of the headline, and a predicate
non-nil when the headline should be numbered.  It must return
a format string in which the section title will be added."
  :group 'org-export-e-html
  :type '(repeat
	  (list (string :tag "HTML class")
		(string :tag "HTML header")
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

(defcustom org-e-html-inputenc-alist nil
  "Alist of inputenc coding system names, and what should really be used.
For example, adding an entry

      (\"utf8\" . \"utf8x\")

will cause \\usepackage[utf8x]{inputenc} to be used for buffers that
are written as utf8 files."
  :group 'org-export-e-html
  :type '(repeat
	  (cons
	   (string :tag "Derived from buffer")
	   (string :tag "Use this instead"))))

(defcustom org-e-html-date-format
  "\\today"
  "Format string for \\date{...}."
  :group 'org-export-e-html
  :type 'boolean)

(defcustom org-e-html-title-command "\\maketitle"
  "The command used to insert the title just after \\begin{document}.
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-e-html
  :type 'string)


;;;; Headline

(defcustom org-e-html-format-headline-function nil
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

\(defun org-e-html-format-headline \(todo todo-type priority text tags\)
  \"Default format function for an headline.\"
  \(concat \(when todo
            \(format \"\\\\textbf{\\\\textsc{\\\\textsf{%s}}} \" todo\)\)
	  \(when priority
            \(format \"\\\\framebox{\\\\#%c} \" priority\)\)
	  text
	  \(when tags \(format \"\\\\hfill{}\\\\textsc{%s}\" tags\)\)\)\)"
  :group 'org-export-e-html
  :type 'function)


;;;; Emphasis

(defcustom org-e-html-emphasis-alist
  '(("*" . "\\textbf{%s}")
    ("/" . "\\emph{%s}")
    ("_" . "\\underline{%s}")
    ("+" . "\\st{%s}")
    ("=" . protectedtexttt)
    ("~" . verb))
  "Alist of HTML expressions to convert emphasis fontifiers.

The key is the character used as a marker for fontification.  The
value is a formatting string to wrap fontified text with.

Value can also be set to the following symbols: `verb' and
`protectedtexttt'.  For the former, Org will use \"\\verb\" to
create a format string and select a delimiter character that
isn't in the string.  For the latter, Org will use \"\\texttt\"
to typeset and try to protect special characters."
  :group 'org-export-e-html
  :type 'alist)


;;;; Footnotes

(defcustom org-e-html-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-e-html
  :type 'string)


;;;; Time-stamps

(defcustom org-e-html-active-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to active time-stamps."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-inactive-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to inactive time-stamps."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-diary-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to diary time-stamps."
  :group 'org-export-e-html
  :type 'string)


;;;; Links

(defcustom org-e-html-image-default-option "width=.9\\linewidth"
  "Default option for images."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-default-figure-position "htb"
  "Default position for latex figures."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-inline-image-rules
  '(("file" . "\\.\\(pdf\\|jpeg\\|jpg\\|png\\|ps\\|eps\\)\\'"))
  "Rules characterizing image files that can be inlined into HTML.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extension *actually* allowed
depend on the way the HTML file is processed.  When used with
pdflatex, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-e-html
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))


;;;; Tables

(defcustom org-e-html-default-table-environment "tabular"
  "Default environment used to build tables."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-e-html
  :type 'boolean)

(defcustom org-e-html-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-e-html
  :type 'boolean)

(defcustom org-e-html-tables-booktabs nil
  "When non-nil, display tables in a formal \"booktabs\" style.
This option assumes that the \"booktabs\" package is properly
loaded in the header of the document.  This value can be ignored
locally with \"booktabs=yes\" and \"booktabs=no\" HTML
attributes."
  :group 'org-export-e-html
  :type 'boolean)

(defcustom org-e-html-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-e-html
  :type 'boolean)


;;;; Drawers

(defcustom org-e-html-format-drawer-function nil
  "Function called to format a drawer in HTML code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-html-format-drawer-default \(name contents\)
  \"Format a drawer element for HTML export.\"
  contents\)"
  :group 'org-export-e-html
  :type 'function)


;;;; Inlinetasks

(defcustom org-e-html-format-inlinetask-function nil
  "Function called to format an inlinetask in HTML code.

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

\(defun org-e-html-format-inlinetask \(todo type priority name tags contents\)
\"Format an inline task element for HTML export.\"
  \(let \(\(full-title
	 \(concat
	  \(when todo
            \(format \"\\\\textbf{\\\\textsf{\\\\textsc{%s}}} \" todo\)\)
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
  :group 'org-export-e-html
  :type 'function)


;; Src blocks

(defcustom org-e-html-listings nil
  "Non-nil means export source code using the listings package.
This package will fontify source code, possibly even with color.
If you want to use this, you also need to make HTML use the
listings package, and if you want to have color, the color
package.  Just add these to `org-export-latex-packages-alist',
for example using customize, or with something like:

  \(require 'org-e-html)
  \(add-to-list 'org-export-latex-packages-alist '\(\"\" \"listings\"))
  \(add-to-list 'org-export-latex-packages-alist '\(\"\" \"color\"))

Alternatively,

  \(setq org-e-html-listings 'minted)

causes source code to be exported using the minted package as
opposed to listings.  If you want to use minted, you need to add
the minted package to `org-export-latex-packages-alist', for
example using customize, or with

  \(require 'org-e-html)
  \(add-to-list 'org-export-latex-packages-alist '\(\"\" \"minted\"))

In addition, it is necessary to install pygments
\(http://pygments.org), and to configure the variable
`org-e-html-pdf-process' so that the -shell-escape option is
passed to pdflatex."
  :group 'org-export-e-html
  :type '(choice
	  (const :tag "Use listings" t)
	  (const :tag "Use minted" 'minted)
	  (const :tag "Export verbatim" nil)))

(defcustom org-e-html-listings-langs
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
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-e-html
  :type '(repeat
	  (list
	   (symbol :tag "Major mode       ")
	   (string :tag "Listings language"))))

(defcustom org-e-html-listings-options nil
  "Association list of options for the latex listings package.

These options are supplied as a comma-separated list to the
\\lstset command.  Each element of the association list should be
a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-e-html-listings-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-e-html
  :type '(repeat
	  (list
	   (string :tag "Listings option name ")
	   (string :tag "Listings option value"))))

(defcustom org-e-html-minted-langs
  '((emacs-lisp "common-lisp")
    (cc "c++")
    (cperl "perl")
    (shell-script "bash")
    (caml "ocaml"))
  "Alist mapping languages to their minted language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the minted package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present.

Note that minted uses all lower case for language identifiers,
and that the full list of language identifiers can be obtained
with:

  pygmentize -L lexers"
  :group 'org-export-e-html
  :type '(repeat
	  (list
	   (symbol :tag "Major mode     ")
	   (string :tag "Minted language"))))

(defcustom org-e-html-minted-options nil
  "Association list of options for the latex minted package.

These options are supplied within square brackets in
\\begin{minted} environments.  Each element of the alist should
be a list containing two strings: the name of the option, and the
value.  For example,

  \(setq org-e-html-minted-options
    '\((\"bgcolor\" \"bg\") \(\"frame\" \"lines\")))

will result in src blocks being exported with

\\begin{minted}[bgcolor=bg,frame=lines]{<LANG>}

as the start of the minted environment. Note that the same
options will be applied to blocks of all languages."
  :group 'org-export-e-html
  :type '(repeat
	  (list
	   (string :tag "Minted option name ")
	   (string :tag "Minted option value"))))

(defvar org-e-html-custom-lang-environments nil
  "Alist mapping languages to language-specific HTML environments.

It is used during export of src blocks by the listings and minted
latex packages.  For example,

  \(setq org-e-html-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during latex export it will output

  \\begin{pythoncode}
  <src block body>
  \\end{pythoncode}")


;;;; Plain text

(defcustom org-e-html-quotes
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
  :group 'org-export-e-html
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


;;;; Compilation

(defcustom org-e-html-pdf-process
  '("pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f")
  "Commands to process a HTML file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name \(i.e. without
extension) and %o by the base directory of the file.

The reason why this is a list is that it usually takes several
runs of `pdflatex', maybe mixed with a call to `bibtex'.  Org
does not have a clever mechanism to detect which of these
commands have to be run to get to a stable result, and it also
does not do any error checking.

By default, Org uses 3 runs of `pdflatex' to do the processing.
If you have texi2dvi on your system and if that does not cause
the infamous egrep/locale bug:

     http://lists.gnu.org/archive/html/bug-texinfo/2010-03/msg00031.html

then `texi2dvi' is the superior choice.  Org does offer it as one
of the customize options.

Alternatively, this may be a Lisp function that does the
processing, so you could use this to apply the machinery of
AUCTeX or the Emacs HTML mode.  This function should accept the
file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
	  (repeat :tag "Shell command sequence"
		  (string :tag "Shell command"))
	  (const :tag "2 runs of pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "3 runs of pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "pdflatex,bibtex,pdflatex,pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "bibtex %b"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "texi2dvi"
		 ("texi2dvi -p -b -c -V %f"))
	  (const :tag "rubber"
		 ("rubber -d --into %o %f"))
	  (function)))

(defcustom org-e-html-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as HTML logfiles."
  :group 'org-export-e-html
  :type '(repeat (string :tag "Extension")))

(defcustom org-e-html-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-e-html
  :type 'boolean)



;;; Internal Functions

(defun org-e-html--caption/label-string (caption label info)
  "Return caption and label HTML string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-html--wrap-label'."
  (setq label nil) ;; FIXME

  (let ((label-str (if label (format "\\label{%s}" label) "")))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\label{%s}\n" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "\\caption[%s]{%s%s}\n"
	      (org-export-secondary-string (cdr caption) 'e-html info)
	      label-str
	      (org-export-secondary-string (car caption) 'e-html info)))
     ;; Standard caption format.
     ;; (t (format "\\caption{%s%s}\n"
     ;; 		label-str
     ;; 		(org-export-secondary-string (car caption) 'e-html info)))

     (t (org-export-secondary-string (car caption) 'e-html info)))))

(defun org-e-html--guess-inputenc (header)
  "Set the coding system in inputenc to what the buffer is.

HEADER is the HTML header string.

Return the new header."
  (let* ((cs (or (ignore-errors
		   (latexenc-coding-system-to-inputenc
		    buffer-file-coding-system))
		 "utf8")))
    (if (not cs)
	header
      ;; First translate if that is requested.
      (setq cs (or (cdr (assoc cs org-e-html-inputenc-alist)) cs))
      ;; Then find the \usepackage statement and replace the option.
      (replace-regexp-in-string "\\\\usepackage\\[\\(AUTO\\)\\]{inputenc}"
				cs header t nil 1))))

(defun org-e-html--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

(defun org-e-html--make-option-string (options)
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

(defun org-e-html--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
	  (let ((start 0))
	    (while (setq start (string-match (car l) text start))
	      (let ((new-quote (concat (match-string 1 text) (cdr l))))
		(setq text (replace-match new-quote  t t text))))))
	(cdr (or (assoc (plist-get info :language) org-e-html-quotes)
		 ;; Falls back on English.
		 (assoc "en" org-e-html-quotes))))
  text)

(defun org-e-html--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-html--caption/label-string'."
  ;; (let ((label (org-element-get-property :name element)))
  ;;   (if (or (not output) (not label) (string= output "") (string= label ""))
  ;; 	output
  ;;     (concat (format "\\label{%s}\n" label) output)))
  output)



;;; Template

;; (defun org-e-html-template (contents info)
;;   "Return complete document string after HTML conversion.
;; CONTENTS is the transcoded contents string.  INFO is a plist
;; holding export options."
;;   (let ((title (org-export-secondary-string
;; 		(plist-get info :title) 'e-html info)))
;;     (concat
;;      ;; 1. Time-stamp.
;;      (and (plist-get info :time-stamp-file)
;; 	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
;;      ;; 2. Document class and packages.
;;      (let ((class (plist-get info :latex-class))
;; 	   (class-options (plist-get info :latex-class-options)))
;;        (org-element-normalize-string
;; 	(let* ((header (nth 1 (assoc class org-e-html-classes)))
;; 	       (document-class-string
;; 		(and (stringp header)
;; 		     (if class-options
;; 			 (replace-regexp-in-string
;; 			  "^[ \t]*\\\\documentclass\\(\\[.*?\\]\\)"
;; 			  class-options header t nil 1)
;; 		       header))))
;; 	  (org-e-html--guess-inputenc
;; 	   (org-splice-latex-header
;; 	    document-class-string
;; 	    org-export-latex-default-packages-alist ; defined in org.el
;; 	    org-export-latex-packages-alist nil ; defined in org.el
;; 	    (plist-get info :latex-header-extra))))))
;;      ;; 3. Define alert if not yet defined.
;;      "\\providecommand{\\alert}[1]{\\textbf{#1}}\n"
;;      ;; 4. Possibly limit depth for headline numbering.
;;      (let ((sec-num (plist-get info :section-numbers)))
;;        (when (integerp sec-num)
;; 	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
;;      ;; 5. Author.
;;      (let ((author (and (plist-get info :with-author)
;; 			(let ((auth (plist-get info :author)))
;; 			  (and auth (org-export-secondary-string
;; 				     auth 'e-html info)))))
;; 	   (email (and (plist-get info :with-email)
;; 		       (org-export-secondary-string
;; 			(plist-get info :email) 'e-html info))))
;;        (cond ((and author email (not (string= "" email)))
;; 	      (format "\\author{%s\\thanks{%s}}\n" author email))
;; 	     (author (format "\\author{%s}\n" author))
;; 	     (t "\\author{}\n")))
;;      ;; 6. Date.
;;      (let ((date (plist-get info :date)))
;;        (and date (format "\\date{%s}\n" date)))
;;      ;; 7. Title
;;      (format "\\title{%s}\n" title)
;;      ;; 8. Hyperref options.
;;      (format "\\hypersetup{\n  pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}}\n"
;; 	     (or (plist-get info :keywords) "")
;; 	     (or (plist-get info :description) "")
;; 	     (if (not (plist-get info :with-creator)) ""
;; 	       (plist-get info :creator)))
;;      ;; 9. Document start.
;;      "\\begin{document}\n\n"
;;      ;; 10. Title command.
;;      (org-element-normalize-string
;;       (cond ((string= "" title) nil)
;; 	    ((not (stringp org-e-html-title-command)) nil)
;; 	    ((string-match "\\(?:[^%]\\|^\\)%s"
;; 			   org-e-html-title-command)
;; 	     (format org-e-html-title-command title))
;; 	    (t org-e-html-title-command)))
;;      ;; 11. Table of contents.
;;      (let ((depth (plist-get info :with-toc)))
;;        (when depth
;; 	 (concat (when (wholenump depth)
;; 		   (format "\\setcounter{tocdepth}{%d}\n" depth))
;; 		 "\\tableofcontents\n\\vspace*{1cm}\n\n")))
;;      ;; 12. Document's body.
;;      contents
;;      ;; 13. Creator.
;;      (let ((creator-info (plist-get info :with-creator)))
;;        (cond
;; 	((not creator-info) "")
;; 	((eq creator-info 'comment)
;; 	 (format "%% %s\n" (plist-get info :creator)))
;; 	(t (concat (plist-get info :creator) "\n"))))
;;      ;; 14. Document end.
;;      "\\end{document}")))

(defun org-e-html-meta-info (info)
  (concat
   (format "
<title>%s</title>" (plist-get info :title))
   (format "
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>"
	   (and coding-system-for-write
		(fboundp 'coding-system-get)
		(coding-system-get coding-system-for-write
				   'mime-charset)))
   (format "
<meta name=\"title\" content=\"%s\"/>" (plist-get info :title))
   "
<meta name=\"generator\" content=\"Org-mode\"/>"
   (format "
<meta name=\"generated\" content=\"%s\"/>" (org-e-html-format-date info))
   (format "
<meta name=\"author\" content=\"%s\"/>" (plist-get info :author))
   (format "
<meta name=\"description\" content=\"%s\"/>" (plist-get info :description))
   (format "
<meta name=\"keywords\" content=\"%s\"/>" (plist-get info :keywords))))

(defun org-e-html-style (info)
  (concat
   (when (plist-get info :style-include-default)
     org-export-e-html-style-default)
   (plist-get info :style)
   (plist-get info :style-extra)
   "\n"
   (when (plist-get info :style-include-scripts)
     org-export-e-html-scripts)))

(defun org-e-html-mathjax (info)
  (when (or (eq (plist-get info :HTML-fragments) 'mathjax)
	    (and org-export-have-math
		 (eq (plist-get info :HTML-fragments) t)))
    (org-export-e-html-mathjax-config
     org-export-e-html-mathjax-template
     org-export-e-html-mathjax-options
     (or (plist-get info :mathjax) ""))))

(defun org-e-html-preamble (info)
  (when (plist-get info :html-preamble)
    (let* ((title (plist-get info :title))
	   (date (org-e-html-format-date info))
	   (author (plist-get info :author))
	   (lang-words (or (assoc (plist-get info :language)
				  org-export-language-setup)
			   (assoc "en" org-export-language-setup)))
	   (email (plist-get info :email))
	   (html-pre-real-contents
	    (cond
	     ((functionp (plist-get info :html-preamble))
	      (with-temp-buffer
		(funcall (plist-get info :html-preamble))
		(buffer-string)))
	     ((stringp (plist-get info :html-preamble))
	      (format-spec (plist-get info :html-preamble)
			   `((?t . ,title) (?a . ,author)
			     (?d . ,date) (?e . ,email))))
	     (t
	      (format-spec
	       (or (cadr (assoc (nth 0 lang-words)
				org-export-e-html-preamble-format))
		   (cadr (assoc "en" org-export-e-html-preamble-format)))
	       `((?t . ,title) (?a . ,author)
		 (?d . ,date) (?e . ,email)))))))
      (when (not (equal html-pre-real-contents ""))
	(concat
	 (format "
<div id=\"%s\"> "  (nth 0 org-export-e-html-divs))
	 "
"
	 html-pre-real-contents
	 "
</div>")))))

;; (defun org-e-html-footnote-section (info)
;;   (when org-e-html-footnotes-alist
;;     (setq org-e-html-footnotes-alist
;; 	  (sort org-e-html-footnotes-alist
;; 		(lambda (n1 n2) (< (or (nth 1 n1) most-positive-fixnum)
;; 				   (or (nth 1 n2) most-positive-fixnum)))))
;;     (org-e-html-format-footnotes-section
;;      (nth 4 (or (assoc (plist-get info :language)
;; 		       org-export-language-setup)
;; 		(assoc "en" org-export-language-setup)))
;;      (mapconcat
;;       (lambda (x)
;; 	(org-e-html-format-footnote-definition (nth 2 x) (nth 1 x)))
;;       org-e-html-footnotes-alist "\n"))))

(defun org-e-html-bibliography ()
  (org-export-e-html-get-bibliography))

(defun org-e-html-postamble (info)
  (concat
   (when (and (not body-only)
	      (plist-get info :html-postamble))
     (let* ((html-post (plist-get info :html-postamble))
	    (date (org-e-html-format-date info))
	    (author (plist-get info :author))
	    (email  (plist-get info :email))
	    (lang-words (or (assoc (plist-get info :language)
				   org-export-language-setup)
			    (assoc "en" org-export-language-setup)))
	    (email
	     (mapconcat (lambda(e)
			  (format "<a href=\"mailto:%s\">%s</a>" e e))
			(split-string email ",+ *")
			", "))
	    (html-validation-link (or org-export-e-html-validation-link ""))
	    (creator-info
	     (concat "Org version " org-version " with Emacs version "
		     (number-to-string emacs-major-version))))
       (concat
	;; begin postamble
	"
<div id=\"" (nth 2 org-export-e-html-divs) "\">"
	(cond
	 ;; auto postamble
	 ((eq (plist-get info :html-postamble) 'auto)
	  (concat
	   (when (plist-get info :time-stamp-file)
	     (format "
<p class=\"date\"> %s: %s </p> "  (nth 2 lang-words) date))
	   (when (and (plist-get info :with-author) author)
	     (format "
<p class=\"author\"> %s : %s</p>"  (nth 1 lang-words) author))
	   (when (and (plist-get info :with-email) email)
	     (format "
<p class=\"email\"> %s </p>" email))
	   (when (plist-get info :with-creator)
	     (format "
<p class=\"creator\"> %s </p>"  creator-info))
	   html-validation-link "\n"))
	 ;; postamble from a string
	 ((stringp (plist-get info :html-postamble))
	  (format-spec (plist-get info :html-postamble)
		       `((?a . ,author) (?e . ,email)
			 (?d . ,date)   (?c . ,creator-info)
			 (?v . ,html-validation-link))))

	 ;; postamble from a function
	 ((functionp (plist-get info :html-postamble))
	  (with-temp-buffer
	    (funcall (plist-get info :html-postamble))
	    (buffer-string)))
	 ;; default postamble
	 (t
	  (format-spec
	   (or (cadr (assoc (nth 0 lang-words)
			    org-export-e-html-postamble-format))
	       (cadr (assoc "en" org-export-e-html-postamble-format)))
	   `((?a . ,author) (?e . ,email)
	     (?d . ,date)   (?c . ,creator-info)
	     (?v . ,html-validation-link)))))
	"
</div>")))
   org-export-e-html-html-helper-timestamp))

(defun org-e-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."
  (concat
   (format
    (or (and (stringp org-export-e-html-xml-declaration)
	     org-export-e-html-xml-declaration)
	(cdr (assoc (plist-get info :html-extension)
		    org-export-e-html-xml-declaration))
	(cdr (assoc "html" org-export-e-html-xml-declaration))

	"")
    (or (and coding-system-for-write
	     (fboundp 'coding-system-get)
	     (coding-system-get coding-system-for-write
				'mime-charset))
	"iso-8859-1"))
   "
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
	       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
   (format "
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\"> "
	   (plist-get info :language) (plist-get info :language))
   "
<head>"
   (org-e-html-meta-info info)		; meta
   (org-e-html-style info)		; style
   (org-e-html-mathjax info)		; mathjax
   "
</head>"

   "
<body>"
   (let ((link-up (and (plist-get info :link-up)
		       (string-match "\\S-" (plist-get info :link-up))
		       (plist-get info :link-up)))
	 (link-home (and (plist-get info :link-home)
			 (string-match "\\S-" (plist-get info :link-home))
			 (plist-get info :link-home))))
     (when (or link-up link-home)
       (format org-export-e-html-home/up-format
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; preamble
   (org-e-html-preamble info)

   ;; content
   (format "
<div id=\"%s\">" (or org-export-e-html-content-div
		     (nth 1 org-export-e-html-divs)))
   (format "
<h1 class=\"title\"> %s </h1>\n" (plist-get info :title))

   contents
   (org-e-html-footnote-section info)
   (org-e-html-bibliography)

   (unless body-only
     "
</div>")

   ;; postamble
   (org-e-html-postamble info)

   (unless body-only
     "
</body>")
   "
</html>"))



;;; Transcode Functions

;;;; Block

(defun org-e-html-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  ;; (org-e-html--wrap-label
  ;;  center-block
  ;;  (format "\\begin{center}\n%s\\end{center}" contents))
  (org-e-html--wrap-label
   center-block
   (format "<div style=\"text-align: center\">\n%s</div>" contents)))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-html-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-get-property :drawer-name drawer))
	 (output (if (functionp org-e-html-format-drawer-function)
		     (funcall org-e-html-format-drawer-function
			      name contents)
		   ;; If there's no user defined function: simply
		   ;; display contents of the drawer.
		   contents)))
    (org-e-html--wrap-label drawer output)))


;;;; Dynamic Block

(defun org-e-html-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See
`org-export-data'."
  (org-e-html--wrap-label dynamic-block contents))


;;;; Emphasis

(defun org-e-html-emphasis (emphasis contents info)
  "Transcode EMPHASIS from Org to HTML.
CONTENTS is the contents of the emphasized text.  INFO is a plist
holding contextual information.."
  ;; (format (cdr (assoc (org-element-get-property :marker emphasis)
  ;; 		      org-e-html-emphasis-alist))
  ;; 	  contents)
  (org-e-html-format-fontify
   contents (cadr (assoc
		   (org-element-get-property :marker emphasis)
		   '(("*" bold)
		     ("/" emphasis)
		     ("_" underline)
		     ("=" code)
		     ("~" verbatim)
		     ("+" strike))))))


;;;; Entity

(defun org-e-html-entity (entity contents info)
  "Transcode an ENTITY object from Org to HTML.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  ;; (let ((ent (org-element-get-property :latex entity)))
  ;;   (if (org-element-get-property :latex-math-p entity)
  ;; 	(format "$%s$" ent)
  ;;     ent))
  (org-element-get-property :html entity))


;;;; Example Block


;; (defun org-odt-format-source-code-or-example-colored
;;   (lines lang caption textareap cols rows num cont rpllbl fmt))

(defun org-e-html-format-source-code-or-example-plain
  (lines lang caption textareap cols rows num cont rpllbl fmt)
  (setq lines
	(concat
	 "<pre class=\"example\">\n"
	 (cond
	  (textareap
	   (concat
	    (format "<p>\n<textarea cols=\"%d\" rows=\"%d\">"
		    cols rows)
	    lines "</textarea>\n</p>\n"))
	  (t
	   (with-temp-buffer
	     (insert lines)
	     (goto-char (point-min))
	     (while (re-search-forward "[<>&]" nil t)
	       (replace-match (cdr (assq (char-before)
					 '((?&."&amp;")(?<."&lt;")(?>."&gt;"))))
			      t t))
	     (buffer-string))))
	 "</pre>\n"))

  (unless textareap
    (setq lines (org-export-number-lines lines 1 1 num cont rpllbl fmt)))

  ;; (when (string-match "\\(\\`<[^>]*>\\)\n" lines)
  ;;   (setq lines (replace-match "\\1" t nil lines)))

  lines)

(defun org-e-html-format-source-code-or-example-colored
  (lines lang caption textareap cols rows num cont rpllbl fmt)
  (let* ((lang-m (when lang
		   (or (cdr (assoc lang org-src-lang-modes))
		       lang)))
	 (mode (and lang-m (intern
			    (concat
			     (if (symbolp lang-m)
				 (symbol-name lang-m)
			       lang-m)
			     "-mode"))))
	 (org-inhibit-startup t)
	 (org-startup-folded nil))
    (setq lines
	  (with-temp-buffer
	    (insert lines)
	    (if (functionp mode)
		(funcall mode)
	      (fundamental-mode))
	    (font-lock-fontify-buffer)
	    ;; markup each line separately
	    (org-remove-formatting-on-newlines-in-region
	     (point-min) (point-max))
	    (org-src-mode)
	    (set-buffer-modified-p nil)
	    (org-export-e-htmlize-region-for-paste
	     (point-min) (point-max))))

    (when (string-match "<pre\\([^>]*\\)>\n*" lines)
      (setq lines (replace-match
		   (format "<pre class=\"src src-%s\">\n" lang) t t lines)))

    (when caption
      (setq lines
	    (concat
	     "<div class=\"org-src-container\">"
	     (format "<label class=\"org-src-name\">%s</label>" caption)
	     lines "</div>")))

    (unless textareap
      (setq lines (org-export-number-lines lines 1 1 num cont rpllbl fmt)))

    ;; (when (string-match "\\(\\`<[^>]*>\\)\n" lines)
    ;;   (setq lines (replace-match "\\1" t nil lines)))
    lines))

(defun org-e-html-format-source-code-or-example
  (lang code &optional opts indent caption)
  "Format CODE from language LANG and return it formatted for export.
The CODE is marked up in `org-export-current-backend' format.

Check if a function by name
\"org-<backend>-format-source-code-or-example\" is bound. If yes,
use it as the custom formatter. Otherwise, use the default
formatter. Default formatters are provided for docbook, html,
latex and ascii backends. For example, use
`org-e-html-format-source-code-or-example' to provide a custom
formatter for export to \"html\".

If LANG is nil, do not add any fontification.
OPTS contains formatting options, like `-n' for triggering numbering lines,
and `+n' for continuing previous numbering.
Code formatting according to language currently only works for HTML.
Numbering lines works for all three major backends (html, latex, and ascii).
INDENT was the original indentation of the block."
  (save-match-data
    (let* ((backend-formatter 'org-e-html-format-source-code-or-example-plain)
	   num cont rtn rpllbl keepp textareap preserve-indentp cols rows fmt)
      (setq opts (or opts "")
	    num (string-match "[-+]n\\>" opts)
	    cont (string-match "\\+n\\>" opts)
	    rpllbl (string-match "-r\\>" opts)
	    keepp (string-match "-k\\>" opts)
	    textareap (string-match "-t\\>" opts)
	    preserve-indentp (or org-src-preserve-indentation
				 (string-match "-i\\>" opts))
	    cols (if (string-match "-w[ \t]+\\([0-9]+\\)" opts)
		     (string-to-number (match-string 1 opts))
		   80)
	    rows (if (string-match "-h[ \t]+\\([0-9]+\\)" opts)
		     (string-to-number (match-string 1 opts))
		   (org-count-lines code))
	    fmt (if (string-match "-l[ \t]+\"\\([^\"\n]+\\)\"" opts)
		    (match-string 1 opts)))
      (when (and textareap
		 ;; (eq org-export-current-backend 'html)
		 )
	;; we cannot use numbering or highlighting.
	(setq num nil cont nil lang nil))
      (if keepp (setq rpllbl 'keep))
      (setq rtn (if preserve-indentp code (org-remove-indentation code)))
      (when (string-match "^," rtn)
	(setq rtn (with-temp-buffer
		    (insert rtn)
		    ;; Free up the protected lines
		    (goto-char (point-min))
		    (while (re-search-forward "^," nil t)
		      (if (or (equal lang "org")
			      (save-match-data
				(looking-at "\\([*#]\\|[ \t]*#\\+\\)")))
			  (replace-match ""))
		      (end-of-line 1))
		    (buffer-string))))
      (when lang
	(if (featurep 'xemacs)
	    (require 'htmlize)
	  (require 'htmlize nil t)))

      (setq backend-formatter
	    (cond
	     ((fboundp 'htmlize-region-for-paste)
	      'org-e-html-format-source-code-or-example-colored)
	     (t
	      (message
	       "htmlize.el 1.34 or later is needed for source code formatting")
	      'org-e-html-format-source-code-or-example-plain)))
      (funcall backend-formatter rtn lang caption textareap cols rows
	       num cont rpllbl fmt))))

(defun org-e-html-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((options (or (org-element-get-property :options example-block) ""))
	 (value (org-export-handle-code example-block info)))
    ;; (org-e-html--wrap-label
    ;;  example-block (format "\\begin{verbatim}\n%s\\end{verbatim}" value))
    (org-e-html--wrap-label
     example-block (org-e-html-format-source-code-or-example nil value))))


;;;; Export Snippet

(defun org-e-html-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-get-property :value export-snippet))


;;;; Export Block

(defun org-e-html-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-get-property :type export-block) "latex")
    (org-remove-indentation (org-element-get-property :value export-block))))


;;;; Fixed Width

(defun org-e-html-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((value (org-element-normalize-string
		 (replace-regexp-in-string
		  "^[ \t]*: ?" ""
		  (org-element-get-property :value fixed-width)))))
    ;; (org-e-html--wrap-label
    ;;  fixed-width (format "\\begin{verbatim}\n%s\\end{verbatim}" value))
    (org-e-html--wrap-label
     fixed-width (org-e-html-format-source-code-or-example nil value)) ;; FIXME
    ))


;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-e-html-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (and (listp prev) (eq (car prev) 'footnote-reference))
       org-e-html-footnote-separator))
   ;; Use \footnotemark if the footnote has already been defined.
   ;; Otherwise, define it with \footnote command.
   (cond
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     ;; (format "\\footnotemark[%s]"
     ;; 	     (org-export-get-footnote-number footnote-reference info))

     (org-e-html-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "FIXME" 100) ;; FIXME

     )
    ;; Inline definitions are secondary strings.
    ((eq (org-element-get-property :type footnote-reference) 'inline)
     ;; (format "\\footnote{%s}"
     ;; 	     (org-trim
     ;; 	      (org-export-secondary-string
     ;; 	       (org-export-get-footnote-definition footnote-reference info)
     ;; 	       'e-html info)))


     (let ((n (org-export-get-footnote-number footnote-reference info))
	   (def (format
		 "<p>%s</p>"
		 (org-trim
		  (org-export-secondary-string
		   (org-export-get-footnote-definition footnote-reference info)
		   'e-html info)))))


       (push (cons n def) org-e-html-footnotes-alist)

       (org-e-html-format-footnote-reference n def 1)))
    ;; Non-inline footnotes definitions are full Org data.
    (t
     ;; (format "\\footnote{%s}"
     ;; 	     (org-trim
     ;; 	      (org-export-data
     ;; 	       (org-export-get-footnote-definition footnote-reference info)
     ;; 	       'e-html info)))

     (let ((n (org-export-get-footnote-number footnote-reference info))
	   (def (org-trim
		 (org-export-data
		  (org-export-get-footnote-definition footnote-reference info)
		  'e-html info))))

       (push (cons n def) org-e-html-footnotes-alist)
       (org-e-html-format-footnote-reference n def 1))

     ))))


;;;; Headline

(defun org-e-html-todo (todo)
  (when todo
    (org-e-html-format-fontify
     (concat
      ;; (ignore-errors (org-lparse-get 'TODO-KWD-CLASS-PREFIX))
      org-export-e-html-todo-kwd-class-prefix
      (org-e-html-fix-class-name todo))
     (list (if (member todo org-done-keywords) "done" "todo")
	   todo))))

(defun org-e-html-headline (headline contents info)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :latex-class))
	 (numberedp (plist-get info :section-numbers))
	 ;; Get level relative to current parsed data.
	 (level (org-export-get-relative-level headline info))
	 (class-sectionning (assoc class org-e-html-classes))
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
		(org-element-get-property :title headline) 'e-html info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-get-property
				 :todo-keyword headline)))
		      (and todo
			   (org-export-secondary-string todo 'e-html info)))))
	 (todo-type (and todo (org-element-get-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-element-get-property :tags headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-get-property :priority headline)))
	 ;; Create the headline text.
	 (full-text (if (functionp org-e-html-format-headline-function)
			;; User-defined formatting function.
			(funcall org-e-html-format-headline-function
				 todo todo-type priority text tags)
		      ;; Default formatting.
		      (concat
		       ;; (when todo
		       ;; 	 (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
		       (org-e-html-todo todo) " "
		       (when priority (format "\\framebox{\\#%c} " priority))
		       text
		       ;; (when tags (format "\\hfill{}\\textsc{%s}" tags))
		       )))
	 ;; Associate some \label to the headline for internal links.
	 ;; (headline-label
	 ;;  (format "\\label{sec-%s}\n"
	 ;; 	  (mapconcat 'number-to-string
	 ;; 		     (org-export-get-headline-number headline info)
	 ;; 		     "-")))

	 ;; FIXME - begin
	 (headline-no (org-export-get-headline-number headline info))
	 (headline-label
	  (format "sec-%s" (mapconcat 'number-to-string headline-no "-")))
	 (headline-labels (list headline-label))
	 (headline-no (org-export-get-headline-number headline info))
	 (section-no (mapconcat 'number-to-string headline-no "."))
	 ;; FIXME - end

	 (pre-blanks (make-string
		      (org-element-get-property :pre-blank headline) 10)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-get-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ;; ((or (not section-fmt) (org-export-low-level-p headline info)) FIXME
     ;;  ;; Build the real contents of the sub-tree.
     ;;  (let ((low-level-body
     ;; 	     (concat
     ;; 	      ;; If the headline is the first sibling, start a list.
     ;; 	      (when (org-export-first-sibling-p headline info)
     ;; 		(format "\\begin{%s}\n" (if numberedp 'enumerate 'itemize)))
     ;; 	      ;; Itemize headline
     ;; 	      "\\item " full-text "\n" headline-label pre-blanks contents)))
     ;; 	;; If headline in the last sibling, close the list, before any
     ;; 	;; blank line.  Otherwise, simply return LOW-LEVEL-BODY.
     ;; 	(if (org-export-last-sibling-p headline info)
     ;; 	    (replace-regexp-in-string
     ;; 	     "[ \t\n]*\\'"
     ;; 	     (format "\n\\\\end{%s}" (if numberedp 'enumerate 'itemize))
     ;; 	     low-level-body)
     ;; 	  low-level-body)))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      ;; (format section-fmt full-text
      ;; 	(concat headline-label pre-blanks contents))

      (org-e-html-format-outline contents level section-no full-text tags
				 (car (last headline-labels))
				 (butlast headline-labels) nil)))))


;;;; Horizontal Rule

(defun org-e-html-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((attr (mapconcat #'identity
			 (org-element-get-property :attr_html horizontal-rule)
			 " ")))
    (org-e-html--wrap-label horizontal-rule
			    (org-e-html-format-horizontal-line))))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-html-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-get-property :value inline-src-block))
	 (separator (org-e-html--find-verb-separator code)))
    (cond
     ;; Do not use a special package: transcode it verbatim.
     ((not org-e-html-listings)
      (concat "\\verb" separator code separator))
     ;; Use minted package.
     ((eq org-e-html-listings 'minted)
      (let* ((org-lang (org-element-get-property :language inline-src-block))
	     (mint-lang (or (cadr (assq (intern org-lang)
					org-e-html-minted-langs))
			    org-lang))
	     (options (org-e-html--make-option-string
		       org-e-html-minted-options)))
	(concat (format "\\mint%s{%s}"
			(if (string= options "") "" (format "[%s]" options))
			mint-lang)
		separator code separator)))
     ;; Use listings package.
     (t
      ;; Maybe translate language's name.
      (let* ((org-lang (org-element-get-property :language inline-src-block))
	     (lst-lang (or (cadr (assq (intern org-lang)
				       org-e-html-listings-langs))
			   org-lang))
	     (options (org-e-html--make-option-string
		       (append org-e-html-listings-options
			       `(("language" ,lst-lang))))))
	(concat (format "\\lstinline[%s]" options)
		separator code separator))))))


;;;; Inlinetask

(defun org-e-html-format-section (text class &optional id)
  (let ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<div class=\"%s\"%s>\n" class extra) text "</div>\n")))

(defun org-e-html-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-secondary-string
	       (org-element-get-property :title inlinetask) 'e-html info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-get-property
				:todo-keyword inlinetask)))
		     (and todo
			  (org-export-secondary-string todo 'e-html info)))))
	(todo-type (org-element-get-property :todo-type inlinetask))
	(tags (and (plist-get info :with-tags)
		   (org-element-get-property :tags inlinetask)))
	(priority (and (plist-get info :with-priority)
		       (org-element-get-property :priority inlinetask))))
    ;; If `org-e-html-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-e-html-format-inlinetask-function)
	(funcall org-e-html-format-inlinetask-function
		 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-e-html--wrap-label
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

(defun org-e-html-format-list-item (contents type &optional arg headline)
  (setq headline nil)		; FIXME
  (concat
   (case type
     (ordered
      (let* ((counter arg)
	     (extra (if counter (format " value=\"%s\"" counter) "")))
	(format "<li%s>" extra)))
     (unordered
      (let* ((id arg)
	     (extra (if id (format " id=\"%s\"" id) "")))
	(concat
	 (format "<li%s>" extra)
	(when headline (concat headline "<br/>")))))
     (descriptive
      (let* ((desc-tag (or arg "(no term)")))
	(concat (format "<dt> %s </dt>" desc-tag) "<dd>"))))
   contents
   (case type
     (ordered "</li>")
     (unordered "</li>")
     (descriptive "</dd>"))))

(defun org-e-html-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  ;; Grab `:level' from plain-list properties, which is always the
  ;; first element above current item.
  (let* ((plain-list (car (org-export-get-genealogy item info)))
	 (type (org-element-get-property :type plain-list))
	 (level (org-element-get-property
		 :level (car (plist-get info :genealogy))))
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
				     tag 'e-html info))))))
    ;; (concat counter "\\item" tag " " checkbox contents)

    (org-e-html-format-list-item contents type nil)

    ))


;;;; Keyword

(defun org-e-html-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
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
	 ((string= "listings" value)
	  (cond
	   ((eq org-e-html-listings 'minted) "\\listoflistings")
	   (org-e-html-listings "\\lstlistoflistings")
	   ;; At the moment, src blocks with a caption are wrapped
	   ;; into a figure environment.
	   (t "\\listoffigures")))))))))


;;;; Latex Environment

(defun org-e-html-format-latex (latex-frag processing-type)
  (let* ((cache-relpath
	  (concat "ltxpng/" (file-name-sans-extension
			     (file-name-nondirectory (buffer-file-name)))))
	 (cache-dir (file-name-directory (buffer-file-name )))
	 (display-msg "Creating LaTeX Image..."))

    (with-temp-buffer
      (insert latex-frag)
      (org-format-latex cache-relpath cache-dir nil display-msg
			nil nil processing-type)
      (buffer-string))))

(defun org-e-html-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; (org-e-html--wrap-label
  ;;  latex-environment
  ;;  (org-remove-indentation (org-element-get-property :value latex-environment)))

  (org-e-html--wrap-label
   latex-environment
   (let ((latex-frag
	  (org-remove-indentation
	   (org-element-get-property :value latex-environment)))
	 (processing-type (plist-get info :LaTeX-fragments)))
     (cond
      ((member processing-type '(t mathjax))
       (org-e-html-format-latex latex-frag 'mathjax))
      ((equal processing-type 'dvipng)
       (let* ((formula-link (org-e-html-format-latex
			     latex-frag processing-type)))
	 (when (and formula-link
		    (string-match "file:\\([^]]*\\)" formula-link))
	   (setq formula-file (match-string 1 formula-link))
	   (org-e-html-format-inline-image formula-file))))
      (t
       latex-frag)))))


;;;; Latex Fragment

(defun org-e-html-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; (org-element-get-property :value latex-fragment)
  (let* ((latex-frag (org-element-get-property :value latex-fragment)))
    (cond
     ((string-match "\\\\ref{\\([^{}\n]+\\)}" latex-frag)
      (let* ((label (match-string 1 latex-frag))
	     (href (and label (org-export-solidify-link-text label)))
	     (text (if (string-match "\\`[a-z]\\{1,10\\}:\\(.+\\)" label)
		       (substring label (match-beginning 1))
		     label)))
	(org-e-html-format-internal-link text href)))
     (t
      (let ((processing-type (plist-get info :LaTeX-fragments)))
	(cond
	 ((member processing-type '(t mathjax))
	  (org-e-html-format-latex latex-frag 'mathjax))
	 ((equal processing-type 'dvipng)
	  (let* ((formula-link (org-e-html-format-latex
				latex-frag processing-type)))
	    (when (and formula-link
		       (string-match "file:\\([^]]*\\)" formula-link))
	      (setq formula-file (match-string 1 formula-link))
	      (org-e-html-format-inline-image formula-file))))
	 (t
	  latex-frag)))))))


;;;; Line Break

(defun org-e-html-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "<br/>")


;;;; Link

(defun org-e-html-link--inline-image (link info)
  "Return HTML code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-paragraph link info))
	 (path (let ((raw-path (org-element-get-property :path link)))
		 (if (not (file-name-absolute-p raw-path)) raw-path
		   (expand-file-name raw-path))))
	 (caption (org-e-html--caption/label-string
		   (org-element-get-property :caption parent)
		   (org-element-get-property :name parent)
		   info))
	 (label (org-element-get-property :name parent))
	 ;; Retrieve latex attributes from the element around.
	 (attr (let ((raw-attr
		      (mapconcat #'identity
				 (org-element-get-property :attr_html parent)
				 " ")))
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
	    (concat "[" org-e-html-default-figure-position "]"))
	   (t ""))))
    ;; Now clear ATTR from any special keyword and set a default
    ;; value if nothing is left.
    (setq attr
	  (if (not attr) ""
	    (org-trim
	     (replace-regexp-in-string
	      "\\(wrap\\|multicolumn\\|float\\|placement=\\S-+\\)" "" attr))))
    (setq attr (cond ((not (string= attr "")) attr)
		     ((eq disposition 'float) "width=0.7\\textwidth")
		     ((eq disposition 'wrap) "width=0.48\\textwidth")
		     (t (or org-e-html-image-default-option ""))))
    ;; Return proper string, depending on DISPOSITION.
    (case disposition
      (wrap (format "\\begin{wrapfigure}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{wrapfigure}" placement attr path caption))
      (mulicolumn (format "\\begin{figure*}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{figure*}" placement attr path caption))
      (float (format "\\begin{figure}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{figure}" placement attr path caption))
      (t (format "\\includegraphics[%s]{%s}" attr path)))

    (let ((href (and label (org-export-solidify-link-text label))))
      (org-e-html-format-inline-image path caption href attr))))

(defun org-e-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-get-property :type link))
	 (raw-path (org-element-get-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (imagep (org-export-inline-image-p
		  link org-e-html-inline-image-rules))
	 (path (cond
		((member type '("http" "https" "ftp" "mailto"))
		 (concat type ":" raw-path))
		((string= type "file")
		 (when (string-match "\\(.+\\)::.+" raw-path)
		   (setq raw-path (match-string 1 raw-path)))
		 (if (file-name-absolute-p raw-path)
		     (concat "file://" (expand-file-name raw-path))
		   ;; TODO: Not implemented yet.  Concat also:
		   ;; (org-export-directory :HTML info)
		   (concat "file://" raw-path)))
		(t raw-path)))
	 protocol)
    (cond
     ;; Image file.
     (imagep (org-e-html-link--inline-image link info))
     ;; Target or radioed target: replace link with the normalized
     ;; custom-id/target name.
     ((member type '("target" "radio"))
      ;; (format "\\hyperref[%s]{%s}"
      ;; 	      (org-export-solidify-link-text path)
      ;; 	      (or desc (org-export-secondary-string path 'e-html info)))

      (org-e-html-format-internal-link
       (or desc (org-export-secondary-string path 'e-html info))
       (org-export-solidify-link-text path)))
     ;; Links pointing to an headline: Find destination and build
     ;; appropriate referencing commanding.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	;; Fuzzy link points to a target.  Do as above.
	(case (car destination)
	  (target
	   ;; (format "\\hyperref[%s]{%s}"
	   ;; 	   (org-export-solidify-link-text
	   ;; 	    (org-element-get-property :raw-value destination))
	   ;; 	   (or desc
	   ;; 	       (org-export-secondary-string
	   ;; 		(org-element-get-property :raw-link link)
	   ;; 		'e-html info)))

	   (org-e-html-format-internal-link
	    (or desc
		(org-export-secondary-string
		 (org-element-get-property :raw-link link)
		 'e-html info))
	    (org-export-solidify-link-text
	     (org-element-get-property :raw-value destination))))
	  ;; Fuzzy link points to an headline.  If headlines are
	  ;; numbered and the link has no description, display
	  ;; headline's number.  Otherwise, display description or
	  ;; headline's title.
	  (headline
	   (let ((label
		  (format "sec-%s"
			  (mapconcat
			   'number-to-string
			   (org-export-get-headline-number destination info)
			   "-"))))
	     (if (and (plist-get info :section-numbers) (not desc))
		 (format "\\ref{%s}" label)
	       ;; (format "\\hyperref[%s]{%s}" label
	       ;; 	       (or desc
	       ;; 		   (org-export-secondary-string
	       ;; 		    (org-element-get-property :title destination)
	       ;; 		    'e-html info)))

	       (org-e-html-format-internal-link
		(or desc
		    (org-export-secondary-string
		     (org-element-get-property :title destination)
		     'e-html info)) label))))
	  ;; Fuzzy link points nowhere.
	  (otherwise
	   ;; (format "\\texttt{%s}"
	   ;; 	   (or desc
	   ;; 	       (org-export-secondary-string
	   ;; 		(org-element-get-property :raw-link link)
	   ;; 		'e-html info)))

	   (org-e-html-format-fontify
	    (or desc
		(org-export-secondary-string
		 (org-element-get-property :raw-link link)
		 'e-html info)) 'emphasis)))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path (or desc ""))
	      (org-export-resolve-coderef path info)))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'html))
     ;; External link with a description part.
     ((and path desc)
      ;; (format "\\href{%s}{%s}" path desc)
      (org-e-html-format-link desc path))
     ;; External link without a description part.
     (path
      ;; (format "\\url{%s}" path)
      (org-e-html-format-link path path))
     ;; No path, only description.  Try to do something useful.
     (t
      ;; (format "\\texttt{%s}" desc)
      (org-e-html-format-fontify desc 'emphasis)))))


;;;; Babel Call

;; Babel Calls are ignored.


;;;; Macro

(defun org-e-html-macro (macro contents info)
  "Transcode a MACRO element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-html-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((style nil)			; FIXME
	 (class (cdr (assoc style '((footnote . "footnote")
				    (verse . nil)))))
	 (extra (if class (format " class=\"%s\"" class) ""))
	 (parent (car (org-export-get-genealogy paragraph info))))
    (cond
     ;; is this the first paragraph in a list item


     ;; (plain-list (car (org-export-get-genealogy item info)))
     ;; (type (org-element-get-property :type plain-list))

     ((and (equal parent 'item)
	   (= (org-element-get-property :begin paragraph)
	      (plist-get (plist-get info :parent-properties)
			 :contents-begin)))
      contents)
     (t
      (concat (format "<p%s> " extra) contents "</p>")))))


;;;; Plain List

(defun org-e-html-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
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
			  (org-element-get-property :attr_html plain-list)
			  " "))
	 (latex-type (cond
		      ((and attr
			    (string-match
			     (format "\\<%s\\>" paralist-regexp) attr))
		       (match-string 1 attr))
		      ((eq type 'ordered) "enumerate")
		      ((eq type 'unordered) "itemize")
		      ((eq type 'descriptive) "description")))
	 arg1 ;; FIXME
	 )
    (org-e-html--wrap-label
     plain-list
     ;; (format "\\begin{%s}%s\n%s\\end{%s}"
     ;; 	     latex-type
     ;; 	     ;; Once special environment, if any, has been removed, the
     ;; 	     ;; rest of the attributes will be optional arguments.
     ;; 	     ;; They will be put inside square brackets if necessary.
     ;; 	     (let ((opt (replace-regexp-in-string
     ;; 			 (format " *%s *" paralist-regexp) "" attr)))
     ;; 	       (cond ((string= opt "") "")
     ;; 		     ((string-match "\\`\\[[^][]+\\]\\'" opt) opt)
     ;; 		     (t (format "[%s]" opt))))
     ;; 	     contents
     ;; 	     latex-type)

     (format "%s\n%s%s"
	     (case type
	       (ordered
		(format "<ol%s>" (if arg1
				     (format " start=\"%d\"" arg1)
				   "")))
	       (unordered "<ul>")
	       (descriptive "<dl>"))
	     contents
	     (case type
	       (ordered "</ol>")
	       (unordered "</ul>")
	       (descriptive "</dl>")))


     )))


;;;; Plain Text

(defun org-e-html-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-export-e-html-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(setq string (replace-match rpl t nil string))))
    string))

(defun org-e-html-encode-plain-text (s)
  "Convert plain text characters to HTML equivalent.
Possible conversions are set in `org-export-html-protect-char-alist'."
  (let ((cl org-export-e-html-protect-char-alist) c)
    (while (setq c (pop cl))
      (let ((start 0))
	(while (string-match (car c) s start)
	  (setq s (replace-match (cdr c) t t s)
		start (1+ (match-beginning 0))))))
    s))

(defun org-e-html-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (setq text (org-e-html-encode-plain-text text))
  ;; Protect %, #, &, $, ~, ^, _,  { and }.
  ;; (while (string-match "\\([^\\]\\|^\\)\\([%$#&{}~^_]\\)" text)
  ;;   (setq text
  ;; 	  (replace-match (format "\\%s" (match-string 2 text)) nil t text 2)))
  ;; Protect \
  ;; (setq text (replace-regexp-in-string
  ;; 	      "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
  ;; 	      "$\\backslash$" text nil t 1))
  ;; HTML into \HTML{} and TeX into \TeX{}.
  ;; (let ((case-fold-search nil)
  ;; 	(start 0))
  ;;   (while (string-match "\\<\\(\\(?:La\\)?TeX\\)\\>" text start)
  ;;     (setq text (replace-match
  ;; 		  (format "\\%s{}" (match-string 1 text)) nil t text)
  ;; 	    start (match-end 0))))
  ;; Handle quotation marks
  ;; (setq text (org-e-html--quotation-marks text info))
  ;; Convert special strings.
  ;; (when (plist-get info :with-special-strings)
  ;;   (while (string-match (regexp-quote "...") text)
  ;;     (setq text (replace-match "\\ldots{}" nil t text))))
  (when (plist-get info :with-special-strings)
    (setq text (org-e-html-convert-special-strings text)))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
					 text)))
  ;; Return value.
  text)


;;;; Property Drawer

(defun org-e-html-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")


;;;; Quote Block

(defun org-e-html-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  ;; (org-e-html--wrap-label
  ;;  quote-block
  ;;  (format "\\begin{quote}\n%s\\end{quote}" contents))
  (org-e-html--wrap-label
   quote-block (format "<blockquote>\n%s</blockquote>" contents)))


;;;; Quote Section

(defun org-e-html-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-get-property :value quote-section))))
    (when value
      ;; (format "\\begin{verbatim}\n%s\\end{verbatim}" value)
      (format "<pre>\n%s</pre>" value))))


;;;; Section

(defun org-e-html-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;;; Radio Target

(defun org-e-html-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  ;; (format "\\label{%s}%s"
  ;; 	  (org-export-solidify-link-text
  ;; 	   (org-element-get-property :raw-value radio-target))
  ;; 	  text)
  (org-e-html-format-anchor
   text
   (org-export-solidify-link-text
    (org-element-get-property :raw-value radio-target))))


;;;; Special Block

(defun org-e-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-get-property :type special-block))))
    (org-e-html--wrap-label
     special-block
     (format "\\begin{%s}\n%s\\end{%s}" type contents type))))


;;;; Src Block

(defun org-e-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-get-property :language src-block))
	 (code (org-export-handle-code src-block info))
	 (caption (org-element-get-property :caption src-block))
	 (label (org-element-get-property :name src-block))
	 (custom-env (and lang
			  (cadr (assq (intern lang)
				      org-e-html-custom-lang-environments)))))
    ;; FIXME: Handle caption
    (org-e-html-format-source-code-or-example lang code)

    ;; (cond
    ;;  ;; No source fontification.
    ;;  ((not org-e-html-listings)
    ;;   (let ((caption-str (org-e-html--caption/label-string
    ;; 			  caption label info))
    ;; 	    (float-env (when caption "\\begin{figure}[H]\n%s\n\\end{figure}")))
    ;; 	(format (or float-env "%s")
    ;; 		(concat
    ;; 		 caption-str
    ;; 		 (format "\\begin{verbatim}\n%s\\end{verbatim}" code)))))
    ;;  ;; Custom environment.
    ;;  (custom-env
    ;;   (format "\\begin{%s}\n%s\\end{%s}\n" custom-env code custom-env))
    ;;  ;; Use minted package.
    ;;  ((eq org-e-html-listings 'minted)
    ;;   (let* ((mint-lang (or (cadr (assq (intern lang) org-e-html-minted-langs))
    ;; 			    lang))
    ;; 	     (float-env (when (or label caption)
    ;; 			  (format "\\begin{listing}[H]\n%%s\n%s\\end{listing}"
    ;; 				  (org-e-html--caption/label-string
    ;; 				   caption label info))))
    ;; 	     (body (format "\\begin{minted}[%s]{%s}\n%s\\end{minted}"
    ;; 			   (org-e-html--make-option-string
    ;; 			    org-e-html-minted-options)
    ;; 			   mint-lang code)))
    ;; 	(if float-env (format float-env body) body)))
    ;;  ;; Use listings package.
    ;;  (t
    ;;   (let ((lst-lang
    ;; 	     (or (cadr (assq (intern lang) org-e-html-listings-langs)) lang))
    ;; 	    (caption-str
    ;; 	     (when caption
    ;; 	       (let ((main (org-export-secondary-string
    ;; 			    (car caption) 'e-html info)))
    ;; 		 (if (not (cdr caption)) (format "{%s}" main)
    ;; 		   (format
    ;; 		    "{[%s]%s}"
    ;; 		    (org-export-secondary-string (cdr caption) 'e-html info)
    ;; 		    main))))))
    ;; 	(concat (format "\\lstset{%s}\n"
    ;; 			(org-e-html--make-option-string
    ;; 			 (append org-e-html-listings-options
    ;; 				 `(("language" ,lst-lang))
    ;; 				 (when label `(("label" ,label)))
    ;; 				 (when caption-str
    ;; 				   `(("caption" ,caption-str))))))
    ;; 		(format "\\begin{lstlisting}\n%s\\end{lstlisting}" code)))))
    ))


;;;; Statistics Cookie

(defun org-e-html-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-get-property :value statistics-cookie))


;;;; Subscript

(defun org-e-html-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  ;; (format (if (= (length contents) 1) "$_%s$" "$_{\\mathrm{%s}}$") contents)
  (org-e-html-format-fontify contents 'subscript))


;;;; Superscript

(defun org-e-html-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  ;; (format (if (= (length contents) 1) "$^%s$" "$^{\\mathrm{%s}}$") contents)
  (org-e-html-format-fontify contents 'superscript))


;;;; Table

(defun org-e-html-table--format-string (table table-info info)
  "Return an appropriate format string for TABLE.

TABLE-INFO is the plist containing format info about the table,
as returned by `org-export-table-format-info'.  INFO is a plist
used as a communication channel.

The format string leaves one placeholder for the body of the
table."
  (let* ((label (org-element-get-property :name table))
	 (caption (org-e-html--caption/label-string
		   (org-element-get-property :caption table) label info))
	 (attr (mapconcat 'identity
			  (org-element-get-property :attr_html table)
			  " "))
	 ;; Determine alignment string.
	 (alignment (org-e-html-table--align-string attr table-info))
	 ;; Determine environment for the table: longtable, tabular...
	 (table-env (cond
		     ((not attr) org-e-html-default-table-environment)
		     ((string-match "\\<longtable\\>" attr) "longtable")
		     ((string-match "\\<tabular.?\\>" attr)
		      (org-match-string-no-properties 0 attr))
		     (t org-e-html-default-table-environment)))
	 ;; If table is a float, determine environment: table or table*.
	 (float-env (cond
		     ((string= "longtable" table-env) nil)
		     ((and attr
			   (or (string-match (regexp-quote "table*") attr)
			       (string-match "\\<multicolumn\\>" attr)))
		      "table*")
		     ((or (not (string= caption "")) label) "table")))
	 ;; Extract others display options.
	 (width (and attr (string-match "\\<width=\\(\\S-+\\)" attr)
		     (org-match-string-no-properties 1 attr)))
	 (placement
	  (if (and attr (string-match "\\<placement=\\(\\S-+\\)" attr))
	      (org-match-string-no-properties 1 attr)
	    (format "[%s]" org-e-html-default-figure-position))))
    ;; Prepare the final format string for the table.
    (cond
     ;; Longtable.
     ((string= "longtable" table-env)
      (format
       "\\begin{longtable}{%s}\n%s\n%%s\n%s\\end{longtable}"
       alignment
       (if (or (not org-e-html-table-caption-above) (string= "" caption)) ""
	 (concat (org-trim caption) "\\\\"))
       (if (or org-e-html-table-caption-above (string= "" caption)) ""
	 (concat (org-trim caption) "\\\\\n"))))
     ;; Others.
     (t (concat (when float-env
		  (concat
		   (format "\\begin{%s}%s\n" float-env placement)
		   (if org-e-html-table-caption-above caption "")))
		(when org-e-html-tables-centered "\\begin{center}\n")
		(format "\\begin{%s}%s{%s}\n%%s\n\\end{%s}"
			table-env
			(if width (format "{%s}" width) "") alignment table-env)
		(when org-e-html-tables-centered "\n\\end{center}")
		(when float-env
		  (concat (if org-e-html-table-caption-above "" caption)
			  (format "\n\\end{%s}" float-env))))))))

(defun org-e-html-table--align-string (attr table-info)
  "Return an appropriate HTML alignment string.
ATTR is a string containing table's HTML specific attributes.
TABLE-INFO is the plist containing format info about the table,
as returned by `org-export-table-format-info'."
  (or (and attr
	   (string-match "\\<align=\\(\\S-+\\)" attr)
	   (match-string 1 attr))
      (let* ((align (copy-sequence (plist-get table-info :alignment)))
	     (colgroups (copy-sequence (plist-get table-info :column-groups)))
	     (cols (length align))
	     (separators (make-vector (1+ cols) "")))
	;; Ignore the first column if it's special.
	(when (plist-get table-info :special-column-p)
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
	;; Build the HTML specific alignment string.
	(loop for al across align
	      for sep across separators
	      concat (concat sep al) into output
	      finally return (concat output (aref separators cols))))))


;; tables

(defun org-e-html-begin-table (caption label attributes)
  (let* ((html-table-tag (or (plist-get info :html-table-tag) ; FIXME
			     org-export-e-html-table-tag))
	 (html-table-tag
	  (org-export-splice-attributes html-table-tag attributes)))
    (when label
      (setq html-table-tag
	    (org-export-splice-attributes
	     html-table-tag
	     (format "id=\"%s\"" (org-solidify-link-text label)))))
    (concat "\n" html-table-tag
	    (format "\n<caption>%s</caption>" (or caption "")))))

(defun org-e-html-end-table ()
  (when org-lparse-table-is-styled
    ;; column groups
    ;; (unless (car org-table-colgroup-info)
    ;;   (setq org-table-colgroup-info
    ;; 	    (cons :start (cdr org-table-colgroup-info))))

    ;; column alignment
    (let ((c -1))
      ;; (mapc
      ;;  (lambda (x)
      ;; 	 (incf c)
      ;; 	 (setf (aref org-lparse-table-colalign-vector c)
      ;; 	       (or (aref org-lparse-table-colalign-vector c)
      ;; 		   (if (> (/ (float x) (1+ org-lparse-table-rownum))
      ;; 			  org-table-number-fraction)
      ;; 		       "right" "left"))))
      ;;  org-lparse-table-num-numeric-items-per-column)
      ))

  ;; html specific stuff starts here
  ;; (org-e-html-end-table)

  "</table>\n")

(defun org-e-html-format-table-cell (text r c horiz-span)
  (let ((cell-style-cookie
	 (if org-export-e-html-table-align-individual-fields
	     (format (if (and (boundp 'org-e-html-format-table-no-css)
			      org-e-html-format-table-no-css)
			 " align=\"%s\"" " class=\"%s\"")
		     (or (aref (plist-get table-info :alignment) c) "left")) ""))) ;; FIXME
    (cond
     (org-lparse-table-cur-rowgrp-is-hdr
      (concat
       (format (car org-export-table-header-tags) "col" cell-style-cookie)
       text (cdr org-export-table-header-tags)))
     ((and (= c 0) org-export-e-html-table-use-header-tags-for-first-column)
      (concat
       (format (car org-export-table-header-tags) "row" cell-style-cookie)
       text (cdr org-export-table-header-tags)))
     (t
      (concat
       (format (car org-export-table-data-tags) cell-style-cookie)
       text (cdr org-export-table-data-tags))))))

(defun org-e-html-format-table-row (row)
  (concat (eval (car org-export-table-row-tags)) row
	  (eval (cdr org-export-table-row-tags))))

(defun org-e-html-table-row (fields &optional text-for-empty-fields)
  (if org-lparse-table-ncols
      ;; second and subsequent rows of the table
      ;; (when (and org-lparse-list-table-p
      ;; 		 (> (length fields) org-lparse-table-ncols))
      ;; 	(error "Table row has %d columns but header row claims %d columns"
      ;; 	       (length fields) org-lparse-table-ncols))
    ;; first row of the table
    (setq org-lparse-table-ncols (length fields))
    ;; (when org-lparse-table-is-styled
    ;;   (setq org-lparse-table-num-numeric-items-per-column
    ;; 	    (make-vector org-lparse-table-ncols 0)))
    )
  (incf org-lparse-table-rownum)
  (let ((i -1))
    (org-e-html-format-table-row
     (mapconcat
      (lambda (x)
	(when (and (string= x "") text-for-empty-fields)
	  (setq x text-for-empty-fields))
	(incf i)
	(let (col-cookie horiz-span)
	  (when org-lparse-table-is-styled
	    ;; (when (and (< i org-lparse-table-ncols)
	    ;; 	       (string-match org-table-number-regexp x))
	    ;;   (incf (aref org-lparse-table-num-numeric-items-per-column i)))
	    (setq col-cookie (cdr (assoc (1+ i) org-lparse-table-colalign-info))
		  horiz-span (nth 1 col-cookie)))
	  (org-e-html-format-table-cell
	   x org-lparse-table-rownum i (or horiz-span 0))))
      fields "\n"))))

(defun org-e-html-end-table-rowgroup ()
  (when org-lparse-table-rowgrp-open
    (setq org-lparse-table-rowgrp-open nil)
    (if org-lparse-table-cur-rowgrp-is-hdr "</thead>" "</tbody>")))

(defun org-e-html-begin-table-rowgroup (&optional is-header-row)
  (concat
   (when org-lparse-table-rowgrp-open
     (org-e-html-end-table-rowgroup))
   (progn
     (setq org-lparse-table-rowgrp-open t)
     (setq org-lparse-table-cur-rowgrp-is-hdr is-header-row)
     (if is-header-row "<thead>" "<tbody>"))))

(defun org-e-html-table-preamble ()
  (let ((colgroup-vector (plist-get table-info :column-groups)) ;; FIXME
	c gr colgropen preamble)
    (unless (aref colgroup-vector 0)
      (setf (aref colgroup-vector 0) 'start))
    (dotimes (c columns-number preamble)
      (setq gr (aref colgroup-vector c))
      (setq preamble
	    (concat
	     preamble
	     (when (memq gr '(start start-end))
	       (prog1 (if colgropen "</colgroup>\n<colgroup>" "\n<colgroup>")
		 (setq colgropen t)))
	     (let* ((colalign-vector (plist-get table-info :alignment)) ;; FIXME
		    (align (cdr (assoc (aref colalign-vector c)
				       '(("l" . "left")
					 ("r" . "right")
					 ("c" . "center")))))
		    (alignspec (if (and (boundp 'org-e-html-format-table-no-css)
					org-e-html-format-table-no-css)
				   " align=\"%s\"" " class=\"%s\""))
		    (extra (format alignspec  align)))
	       (format "<col%s />" extra))
	     (when (memq gr '(end start-end))
	       (setq colgropen nil)
	       "</colgroup>"))))
    (concat preamble (if colgropen "</colgroup>"))))

(defun org-e-html-list-table (lines &optional splice
				  caption label attributes head
				  org-lparse-table-colalign-info)
  (or (featurep 'org-table)		; required for
      (require 'org-table))		; `org-table-number-regexp'
  (let* ((org-lparse-table-rownum -1)
	 (org-lparse-table-ncols (length (plist-get info :alignment)))
	 i (cnt 0)
	 tbopen fields line
	 org-lparse-table-cur-rowgrp-is-hdr
	 org-lparse-table-rowgrp-open
	 ;; org-lparse-table-num-numeric-items-per-column
	 org-lparse-table-colalign-vector n
	 org-lparse-table-rowgrp-info
	 (org-lparse-table-style 'org-table)
	 org-lparse-table-is-styled)
    (cond
     (splice
      (setq org-lparse-table-is-styled nil)
      (mapconcat 'org-e-html-table-row lines "\n"))
     (t
      (setq org-lparse-table-is-styled t)

      (concat
       (org-e-html-begin-table caption label attributes)
       (org-e-html-table-preamble)
       (progn (push (cons (1+ org-lparse-table-rownum) :start)
		    org-lparse-table-rowgrp-info)
	      (org-e-html-begin-table-rowgroup head))

       (mapconcat
	(lambda (line)
	  (cond
	   ((equal line :hrule)
	    (push (cons (1+ org-lparse-table-rownum) :start)
		  org-lparse-table-rowgrp-info)
	    (org-e-html-begin-table-rowgroup))
	   (t
	    (org-e-html-table-row line))))
	lines "\n")

       (org-e-html-end-table-rowgroup)
       (org-e-html-end-table))))))

(defun org-e-html-org-table-to-list-table (lines &optional splice)
  "Convert org-table to list-table.
LINES is a list of the form (ROW1 ROW2 ROW3 ...) where each
element is a `string' representing a single row of org-table.
Thus each ROW has vertical separators \"|\" separating the table
fields.  A ROW could also be a row-group separator of the form
\"|---...|\".  Return a list of the form (ROW1 ROW2 ROW3
...). ROW could either be symbol `:hrule' or a list of the
form (FIELD1 FIELD2 FIELD3 ...) as appropriate."
  (let (line lines-1)
    (cond
     (splice
      (while (setq line (pop lines))
	(unless (string-match "^[ \t]*|-" line)
	  (push (org-split-string line "[ \t]*|[ \t]*") lines-1))))
     (t
      (while (setq line (pop lines))
	(cond
	 ((string-match "^[ \t]*|-" line)
	  (when lines
	    (push :hrule lines-1)))
	 (t
	  (push (org-split-string line "[ \t]*|[ \t]*") lines-1))))))
    (nreverse lines-1)))

(defun org-e-html-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* (
	 ;; FIXME
	 ;; see `org-e-html-table--format-string'
	 (label (org-element-get-property :name table))
	 (caption (org-e-html--caption/label-string
		   (org-element-get-property :caption table) label info))
	 ;; FIXME

	 (attr (mapconcat #'identity
			  (org-element-get-property :attr_html table)
			  " "))
	 (raw-table (org-element-get-property :raw-table table)))
    (cond
     ;; Case 1: verbatim table.
     ((or org-e-html-tables-verbatim
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
      (with-current-buffer (get-buffer-create "*org-export-table*")
	(erase-buffer))
      (let ((output (with-temp-buffer
		      (insert raw-table)
		      (goto-char 1)
		      (re-search-forward "^[ \t]*|[^|]" nil t)
		      (table-generate-source 'html "*org-export-table*")
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
	      (unless (= n 2)
		(setq output (replace-match "" nil nil output))))))
	;; (if (not org-e-html-tables-centered) output
	;;   (format "\\begin{center}\n%s\n\\end{center}" output))
	output))
     ;; Case 3: Standard table.
     (t
      (let* ((table-info (org-export-table-format-info raw-table))
	     (columns-number (length (plist-get table-info :alignment)))
	     (longtablep (and attr (string-match "\\<longtable\\>" attr)))
	     (booktabsp
	      (or (and attr (string-match "\\<booktabs=\\(yes\\|t\\)\\>" attr))
		  org-e-html-tables-booktabs))
	     ;; CLEAN-TABLE is a table turned into a list, much like
	     ;; `org-table-to-lisp', with special column and
	     ;; formatting cookies removed, and cells already
	     ;; transcoded.
	     (lines (org-split-string
		     (org-export-clean-table
		      raw-table (plist-get table-info :special-column-p)) "\n"))

	     ;; (clean-table
	     ;;  (mapcar
	     ;;   (lambda (row)
	     ;; 	 (if (string-match org-table-hline-regexp row) 'hline
	     ;; 	   (mapcar
	     ;; 	    (lambda (cell)
	     ;; 	      (org-export-secondary-string
	     ;; 	       (org-element-parse-secondary-string
	     ;; 		cell
	     ;; 		(cdr (assq 'table org-element-string-restrictions)))
	     ;; 	       'e-html info))
	     ;; 	    (org-split-string row "[ \t]*|[ \t]*"))))

	     ;;   lines))



	     )

	(let ((splice nil) head)
	  (setq lines (org-e-html-org-table-to-list-table lines splice))
	  (org-e-html-list-table lines splice caption label attr head nil))
	;; If BOOKTABSP is non-nil, remove any rule at the beginning
	;; and the end of the table, since booktabs' special rules
	;; will be inserted instead.
	;; (when booktabsp
	;;   (when (eq (car clean-table) 'hline)
	;;     (setq clean-table (cdr clean-table)))
	;;   (when (eq (car (last clean-table)) 'hline)
	;;     (setq clean-table (butlast clean-table))))
	;; Convert ROWS to send them to `orgtbl-to-latex'.  In
	;; particular, send each cell to
	;; `org-element-parse-secondary-string' to expand any Org
	;; object within.  Eventually, flesh the format string out
	;; with the table.
	;; 	(format
	;; 	 (org-e-html-table--format-string table table-info info)
	;; 	 (orgtbl-to-latex
	;; 	  clean-table
	;; 	  ;; Parameters passed to `orgtbl-to-latex'.
	;; 	  `(:tstart ,(and booktabsp "\\toprule")
	;; 		    :tend ,(and booktabsp "\\bottomrule")
	;; 		    :hline ,(if booktabsp "\\midrule" "\\hline")
	;; 		    ;; Longtable environment requires specific header
	;; 		    ;; lines end string.
	;; 		    :hlend ,(and longtablep
	;; 				 (format "\\\\
	;; %s
	;; \\endhead
	;; %s\\multicolumn{%d}{r}{Continued on next page}\\\\
	;; \\endfoot
	;; \\endlastfoot"
	;; 					 (if booktabsp "\\midrule" "\\hline")
	;; 					 (if booktabsp "\\midrule" "\\hline")
	;; 					 columns-number)))))
	)))))


;;;; Target

(defun org-e-html-target (target text info)
  "Transcode a TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  ;; (format "\\label{%s}%s"
  ;; 	  (org-export-solidify-link-text
  ;; 	   (org-element-get-property :raw-value target))
  ;; 	  text)

  (org-e-html-format-anchor
   text (org-export-solidify-link-text
	 (org-element-get-property :raw-value target))))


;;;; Time-stamp

(defun org-e-html-time-stamp (time-stamp contents info)
  "Transcode a TIME-STAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; (let ((value (org-element-get-property :value time-stamp))
  ;; 	(type (org-element-get-property :type time-stamp))
  ;; 	(appt-type (org-element-get-property :appt-type time-stamp)))
  ;;   (concat (cond ((eq appt-type 'scheduled)
  ;; 		   (format "\\textbf{\\textsc{%s}} " org-scheduled-string))
  ;; 		  ((eq appt-type 'deadline)
  ;; 		   (format "\\textbf{\\textsc{%s}} " org-deadline-string))
  ;; 		  ((eq appt-type 'closed)
  ;; 		   (format "\\textbf{\\textsc{%s}} " org-closed-string)))
  ;; 	    (cond ((memq type '(active active-range))
  ;; 		   (format org-e-html-active-timestamp-format value))
  ;; 		  ((memq type '(inactive inactive-range))
  ;; 		   (format org-e-html-inactive-timestamp-format value))
  ;; 		  (t
  ;; 		   (format org-e-html-diary-timestamp-format value)))))
  (let ((value (org-element-get-property :value time-stamp))
        (type (org-element-get-property :type time-stamp))
        (appt-type (org-element-get-property :appt-type time-stamp)))
    (org-e-html-format-fontify
     (concat
      (org-e-html-format-fontify
       (cond ((eq appt-type 'scheduled) org-scheduled-string)
	     ((eq appt-type 'deadline) org-deadline-string)
	     ((eq appt-type 'closed) org-closed-string)) "timestamp-kwd")
      ;; FIXME: (org-translate-time value)
      (org-e-html-format-fontify value "timestamp"))
     "timestamp-wrapper")))


;;;; Verbatim

(defun org-e-html-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-html-emphasis
   verbatim (org-element-get-property :value verbatim) info)

  ;; (let ((fmt (cdr (assoc (org-element-get-property :marker verbatim)
  ;; 			 org-e-html-emphasis-alist)))
  ;; 	(value (org-element-get-property :value verbatim)))
  ;;   (cond
  ;;    ;; Handle the `verb' special case.
  ;;    ((eq 'verb fmt)
  ;;     (let ((separator (org-e-html--find-verb-separator value)))
  ;; 	(concat "\\verb" separator value separator)))
  ;;    ;; Handle the `protectedtexttt' special case.
  ;;    ((eq 'protectedtexttt fmt)
  ;;     (let ((start 0)
  ;; 	    (trans '(("\\" . "\\textbackslash{}")
  ;; 		     ("~" . "\\textasciitilde{}")
  ;; 		     ("^" . "\\textasciicircum{}")))
  ;; 	    (rtn "")
  ;; 	    char)
  ;; 	(while (string-match "[\\{}$%&_#~^]" value)
  ;; 	  (setq char (match-string 0 value))
  ;; 	  (if (> (match-beginning 0) 0)
  ;; 	      (setq rtn (concat rtn (substring value 0 (match-beginning 0)))))
  ;; 	  (setq value (substring value (1+ (match-beginning 0))))
  ;; 	  (setq char (or (cdr (assoc char trans)) (concat "\\" char))
  ;; 		rtn (concat rtn char)))
  ;; 	(setq value (concat rtn value)
  ;; 	      fmt "\\texttt{%s}")
  ;; 	(while (string-match "--" value)
  ;; 	  (setq value (replace-match "-{}-" t t value)))
  ;; 	(format fmt value)))
  ;;    ;; Else use format string.
  ;;    (t (format fmt value))))

  )


;;;; Verse Block

(defun org-e-html-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-html--wrap-label
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
			'e-html info)))))
     (while (string-match "^[ \t]+" contents)
       (let ((new-str (format "\\hspace*{%dem}"
			      (length (match-string 0 contents)))))
	 (setq contents (replace-match new-str nil t contents))))
     (format "\\begin{verse}\n%s\\end{verse}" contents)))


  (org-e-html--wrap-label
   verse-block
   ;; In a verse environment, add a line break to each newline
   ;; character and change each white space at beginning of a line
   ;; into a space of 1 em.  Also change each blank line with
   ;; a vertical space of 1 em.
   (progn
     (setq contents (replace-regexp-in-string
		     "^ *\\\\\\\\$" "<br/>\n"
		     (replace-regexp-in-string
		      "\\(\\\\\\\\\\)?[ \t]*\n" " <br/>\n"
		      (org-remove-indentation
		       (org-export-secondary-string
			(org-element-get-property :value verse-block)
			'e-html info)))))
     (while (string-match "^[ \t]+" contents)
       (let ((new-str (format "&nbsp;"
			      (length (match-string 0 contents)))))
	 (setq contents (replace-match new-str nil t contents))))
     (format "<p class=\"verse\">\n%s</p>" contents)))

  )



;;; Interactive functions

(setq org-e-html-pp t)

(defun org-e-html-export-to-html
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a HTML file.

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

  (setq org-e-html-footnotes-alist nil)

  ;; FIXME
  (with-current-buffer (get-buffer-create "*debug*")
    (erase-buffer))

  (let ((outfile (org-export-output-file-name ".html" subtreep pub-dir)))
    (org-export-to-file
     'e-html outfile subtreep visible-only body-only ext-plist)))

(defun org-e-html-export-to-pdf
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to HTML then process through to PDF.

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

Return PDF file's name."
  (interactive)
  (org-e-html-compile
   (org-e-html-export-to-html
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-e-html-compile (texfile)
  "Compile a TeX file.

TEXFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-e-html-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (let* ((wconfig (current-window-configuration))
	 (texfile (file-truename texfile))
	 (base (file-name-sans-extension texfile))
	 errors)
    (message (format "Processing HTML file %s ..." texfile))
    (unwind-protect
	(progn
	  (cond
	   ;; A function is provided: Apply it.
	   ((functionp org-latex-to-pdf-process)
	    (funcall org-latex-to-pdf-process (shell-quote-argument texfile)))
	   ;; A list is provided: Replace %b, %f and %o with appropriate
	   ;; values in each command before applying it.  Output is
	   ;; redirected to "*Org PDF HTML Output*" buffer.
	   ((consp org-e-html-pdf-process)
	    (let* ((out-dir (or (file-name-directory texfile) "./"))
		   (outbuf (get-buffer-create "*Org PDF HTML Output*")))
	      (mapc
	       (lambda (command)
		 (shell-command
		  (replace-regexp-in-string
		   "%b" (shell-quote-argument base)
		   (replace-regexp-in-string
		    "%f" (shell-quote-argument texfile)
		    (replace-regexp-in-string
		     "%o" (shell-quote-argument out-dir) command)))
		  outbuf))
	       org-e-html-pdf-process)
	      ;; Collect standard errors from output buffer.
	      (setq errors (org-e-html-collect-errors outbuf))))
	   (t (error "No valid command to process to PDF")))
	  (let ((pdffile (concat base ".pdf")))
	    ;; Check for process failure.  Provide collected errors if
	    ;; possible.
	    (if (not (file-exists-p pdffile))
		(error (concat (format "PDF file %s wasn't produced" pdffile)
			       (when errors (concat ": " errors))))
	      ;; Else remove log files, when specified, and signal end of
	      ;; process to user, along with any error encountered.
	      (when org-e-html-remove-logfiles
		(dolist (ext org-e-html-logfiles-extensions)
		  (let ((file (concat base "." ext)))
		    (when (file-exists-p file) (delete-file file)))))
	      (message (concat "Process completed"
			       (if (not errors) "."
				 (concat " with errors: " errors)))))
	    ;; Return output file name.
	    pdffile))
      (set-window-configuration wconfig))))

(defun org-e-html-collect-errors (buffer)
  "Collect some kind of errors from \"pdflatex\" command output.

BUFFER is the buffer containing output.

Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final "pdflatex" run.
      (when (re-search-backward "^[ \t]*This is pdf.*?TeX.*?Version" nil t)
	(let ((case-fold-search t)
	      (errors ""))
	  (when (save-excursion
		  (re-search-forward "Reference.*?undefined" nil t))
	    (setq errors (concat errors " [undefined reference]")))
	  (when (save-excursion
		  (re-search-forward "Citation.*?undefined" nil t))
	    (setq errors (concat errors " [undefined citation]")))
	  (when (save-excursion
		  (re-search-forward "Undefined control sequence" nil t))
	    (setq errors (concat errors " [undefined control sequence]")))
	  (when (save-excursion
		  (re-search-forward "^! HTML.*?Error" nil t))
	    (setq errors (concat errors " [HTML error]")))
	  (when (save-excursion
		  (re-search-forward "^! Package.*?Error" nil t))
	    (setq errors (concat errors " [package error]")))
	  (and (org-string-nw-p errors) (org-trim errors)))))))


(provide 'org-e-html)
;;; org-e-html.el ends here
