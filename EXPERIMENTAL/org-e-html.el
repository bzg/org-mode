;;; org-e-html.el --- HTML Back-End For Org Export Engine

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Jambunathan K <kjambunathan at gmail dot com>
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

;;; Code:

;;; org-e-html.el
;;; Dependencies

(require 'format-spec)
(eval-when-compile (require 'cl) (require 'table))



;;; Hooks

(defvar org-e-html-after-blockquotes-hook nil
  "Hook run during HTML export, after blockquote, verse, center are done.")

(defvar org-e-html-final-hook nil
  "Hook run at the end of HTML export, in the new buffer.")

;; FIXME: it already exists in org-e-html.el
;;; Function Declarations

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

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))




;;; Internal Variables

(defconst org-e-html-option-alist
  '((:agenda-style nil nil org-agenda-export-html-style)
    (:convert-org-links nil nil org-e-html-link-org-files-as-html)
    ;; FIXME Use (org-xml-encode-org-text-skip-links s) ??
    ;; (:expand-quoted-html nil "@" org-e-html-expand)
    (:inline-images nil nil org-e-html-inline-images)
    ;; (:link-home nil nil org-e-html-link-home) FIXME
    ;; (:link-up nil nil org-e-html-link-up) FIXME
    (:style nil nil org-e-html-style)
    (:style-extra nil nil org-e-html-style-extra)
    (:style-include-default nil nil org-e-html-style-include-default)
    (:style-include-scripts nil nil org-e-html-style-include-scripts)
    ;; (:timestamp nil nil org-e-html-with-timestamp)
    (:html-extension nil nil org-e-html-extension)
    (:html-postamble nil nil org-e-html-postamble)
    (:html-preamble nil nil org-e-html-preamble)
    (:html-table-tag nil nil org-e-html-table-tag)
    (:xml-declaration nil nil org-e-html-xml-declaration)
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

(defvar html-table-tag nil) ; dynamically scoped into this.

;; FIXME: it already exists in org-e-html.el
(defconst org-e-html-cvt-link-fn
   nil
   "Function to convert link URLs to exportable URLs.
Takes two arguments, TYPE and PATH.
Returns exportable url as (TYPE PATH), or nil to signal that it
didn't handle this case.
Intended to be locally bound around a call to `org-export-as-html'." )




(defvar org-e-html-format-table-no-css)
(defvar htmlize-buffer-places)  ; from htmlize.el
(defvar body-only) ; dynamically scoped into this.

(defvar org-e-html-table-rowgrp-open)
(defvar org-e-html-table-rownum)
(defvar org-e-html-table-cur-rowgrp-is-hdr)
(defvar org-lparse-table-is-styled)


(defvar org-e-html-headline-formatter
  (lambda (level snumber todo todo-type priority
		 title tags target extra-targets extra-class)
    (concat snumber " " title)))



;;; User Configuration Variables

(defgroup org-export-e-html nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)

;;;; Debugging

(defcustom org-e-html-pretty-output t
  "Enable this to generate pretty HTML."
  :group 'org-export-e-html
  :type 'boolean)


;;;; Document

(defcustom org-e-html-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-xml-declaration
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

(defcustom org-e-html-coding-system nil
  "Coding system for HTML export, defaults to `buffer-file-coding-system'."
  :group 'org-export-e-html
  :type 'coding-system)

(defvar org-e-html-content-div "content"
  "The name of the container DIV that holds all the page contents.

This variable is obsolete since Org version 7.7.
Please set `org-e-html-divs' instead.")

(defcustom org-e-html-divs '("preamble" "content" "postamble")
  "The name of the main divs for HTML export.
This is a list of three strings, the first one for the preamble
DIV, the second one for the content DIV and the third one for the
postamble DIV."
  :group 'org-export-e-html
  :type '(list
	  (string :tag " Div for the preamble:")
	  (string :tag "  Div for the content:")
	  (string :tag "Div for the postamble:")))


;;;; Document Header (Styles)

(defconst org-e-html-style-default
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
Please use the variables `org-e-html-style' and
`org-e-html-style-extra' to add to this style.  If you wish to not
have the default style included, customize the variable
`org-e-html-style-include-default'.")

(defcustom org-e-html-style-include-default t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-e-html-style-default' and should
not be modified.  Use the variables `org-e-html-style' to add
your own style information."
  :group 'org-export-e-html
  :type 'boolean)
;;;###autoload
(put 'org-e-html-style-include-default 'safe-local-variable 'booleanp)

(defcustom org-e-html-style ""
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
See also the variable `org-e-html-style-extra'."
  :group 'org-export-e-html
  :type 'string)
;;;###autoload
(put 'org-e-html-style 'safe-local-variable 'stringp)

(defcustom org-e-html-style-extra ""
  "Additional style information for HTML export.
The value of this variable is inserted into the HTML buffer right after
the value of `org-e-html-style'.  Use this variable for per-file
settings of style information, and do not forget to surround the style
settings with <style>...</style> tags."
  :group 'org-export-e-html
  :type 'string)
;;;###autoload
(put 'org-e-html-style-extra 'safe-local-variable 'stringp)

(defcustom org-e-html-mathjax-options
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


;;;; Document Header (Scripts)

(defcustom org-e-html-style-include-scripts t
  "Non-nil means include the JavaScript snippets in exported HTML files.
The actual script is defined in `org-e-html-scripts' and should
not be modified."
  :group 'org-export-e-html
  :type 'boolean)

(defconst org-e-html-scripts
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


;;;; Document Header (Mathjax)

(defcustom org-e-html-mathjax-template
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


;;;; Preamble

(defcustom org-e-html-preamble t
  "Non-nil means insert a preamble in HTML export.

When `t', insert a string as defined by one of the formatting
strings in `org-e-html-preamble-format'.  When set to a
string, this string overrides `org-e-html-preamble-format'.
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

(defcustom org-e-html-preamble-format '(("en" ""))
  "The format for the HTML preamble.

%t stands for the title.
%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-home/up-format
  "<div id=\"org-div-home-and-up\" style=\"text-align:right;font-size:70%%;white-space:nowrap;\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-e-html-link-up' and
`org-e-html-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-e-html
  :type 'string)

;;;; Postamble

(defcustom org-e-html-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When `t', insert a string as defined by the formatting string in
`org-e-html-postamble-format'.  When set to a string, this
string overrides `org-e-html-postamble-format'.  When set to
'auto, discard `org-e-html-postamble-format' and honor
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

(defcustom org-e-html-postamble-format
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
%v will be replaced by `org-e-html-validation-link'.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"
  "Link to HTML validation service."
  :group 'org-export-e-html
  :type 'string)

;; FIXME Obsolete since Org 7.7
;; Use the :timestamp option or `org-export-time-stamp-file' instead
;;;; Emphasis

(defcustom org-e-html-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-e-html-protect'."
  :group 'org-export-e-html
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "HTML equivalent"))))

(defconst org-e-html-special-string-regexps
  '(("\\\\-" . "&shy;")
    ("---\\([^-]\\)" . "&mdash;\\1")
    ("--\\([^-]\\)" . "&ndash;\\1")
    ("\\.\\.\\." . "&hellip;"))
  "Regular expressions for special string conversion.")


;;;; Todos

(defcustom org-e-html-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-e-html
  :type 'string)


;;;; Tags

(defcustom org-e-html-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-e-html
  :type 'string)

;;;; Time-stamps
;;;; Statistics Cookie
;;;; Subscript
;;;; Superscript

;;;; Inline images

(defcustom org-e-html-inline-images 'maybe
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-e-html
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

(defcustom org-e-html-inline-image-extensions
  '("png" "jpeg" "jpg" "gif" "svg")
  "Extensions of image files that can be inlined into HTML."
  :group 'org-export-e-html
  :type '(repeat (string :tag "Extension")))


;;;; Block
;;;; Comment
;;;; Comment Block
;;;; Drawer
;;;; Dynamic Block
;;;; Emphasis
;;;; Entity
;;;; Example Block
;;;; Export Snippet
;;;; Export Block
;;;; Fixed Width
;;;; Footnotes

(defcustom org-e-html-footnotes-section "<div id=\"footnotes\">
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

(defcustom org-e-html-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-e-html
  :type 'string)


;;;; Headline
;;;; Horizontal Rule
;;;; Inline Babel Call
;;;; Inline Src Block
;;;; Inlinetask
;;;; Item
;;;; Keyword
;;;; Latex Environment
;;;; Latex Fragment
;;;; Line Break
;;;; Link
;;;; Babel Call
;;;; Macro
;;;; Paragraph
;;;; Plain List
;;;; Plain Text
;;;; Property Drawer
;;;; Quote Block
;;;; Quote Section
;;;; Section
;;;; Radio Target
;;;; Special Block
;;;; Src Block

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


;;;; Table

(defcustom org-e-html-table-tag
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
See also the variable `org-e-html-table-use-header-tags-for-first-column'.
See also the variable `org-e-html-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-data-tags '("<td%s>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-e-html-table-align-individual-fields'."
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

(defcustom org-e-html-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-e-html-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-tables
  :type 'boolean)


;;;; Target
;;;; Time-stamp

;;;; Verbatim
;;;; Verse Block
;;;; Headline

(defcustom org-e-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-e-html
  :type 'string)


;;;; Links
;;;; Drawers
;;;; Inlinetasks
;;;; Publishing

(defcustom org-e-html-link-org-files-as-html t
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


;;;; Compilation



;;; User Configurable Variables (MAYBE)

;;;; Preamble

(defcustom org-e-html-date-format
  "\\today"
  "Format string for \\date{...}."
  :group 'org-export-e-html
  :type 'boolean)

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



;;; Internal Functions (HTML)

(defun org-e-html-cvt-org-as-html (opt-plist type path)
   "Convert an org filename to an equivalent html filename.
If TYPE is not file, just return `nil'.
See variable `org-e-html-link-org-files-as-html'"

   (save-match-data
      (and
	 org-e-html-link-org-files-as-html
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

;;;; Bibliography

(defun org-e-html-bibliography ()
  "Find bibliography, cut it out and return it."
  (catch 'exit
    (let (beg end (cnt 1) bib)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward
	       "^[ \t]*<div \\(id\\|class\\)=\"bibliography\"" nil t)
	  (setq beg (match-beginning 0))
	  (while (re-search-forward "</?div\\>" nil t)
	    (setq cnt (+ cnt (if (string= (match-string 0) "<div") +1 -1)))
	    (when (= cnt 0)
	      (and (looking-at ">") (forward-char 1))
	      (setq bib (buffer-substring beg (point)))
	      (delete-region beg (point))
	    (throw 'exit bib))))
	nil))))

;;;; Table

(defun org-e-html-format-table (lines olines)
  (let ((org-e-html-format-table-no-css nil))
    (org-lparse-format-table lines olines)))

(defun org-e-html-splice-attributes (tag attributes)
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

(defun org-export-splice-style (style extra)
  "Splice EXTRA into STYLE, just before \"</style>\"."
  (if (and (stringp extra)
	   (string-match "\\S-" extra)
	   (string-match "</style>" style))
      (concat (substring style 0 (match-beginning 0))
	      "\n" extra "\n"
	      (substring style (match-beginning 0)))
    style))

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

(defun org-e-html-format-toc-entry (snumber todo headline tags href)
  (setq headline (concat
		  ;; section number
		  (and org-export-with-section-numbers (concat snumber " "))
		  ;; headline
		  headline
		  ;; tags
		  (and tags (concat
			     (org-e-html-format-spaces 3)
			     (org-e-html-format-fontify tags "tag")))))
  ;; fontify headline based on TODO keyword
  (when todo (setq headline (org-e-html-format-fontify headline "todo")))
  (org-e-html-format-link headline (concat  "#" href)))

(defun org-e-html-toc-entry-formatter
  (level snumber todo todo-type priority
	 headline tags target extra-targets extra-class)
  (org-e-html-format-toc-entry snumber todo headline tags target))

(defun org-e-html-make-string (n string)
  (let (out) (dotimes (i n out) (setq out (concat string out)))))

(defun org-e-html-toc-text (toc-entries)
  (let* ((prev-level (1- (nth 1 (car toc-entries))))
	 (start-level prev-level))
    (concat
     (mapconcat
      (lambda (entry)
	(let ((headline (nth 0 entry))
	      (level (nth 1 entry)))
	  (concat
	   (let* ((cnt (- level prev-level))
		  (times (if (> cnt 0) (1- cnt) (- cnt)))
		  rtn)
	     (setq prev-level level)
	     (concat
	      (org-e-html-make-string
	       times (cond ((> cnt 0) "<ul>\n<li>\n")
			   ((< cnt 0) "</li>\n</ul>\n")))
	      (if (> cnt 0) "<ul>\n<li>\n" "</li>\n<li>\n")))
	   headline)))
      toc-entries "")
     (org-e-html-make-string
      (- prev-level start-level) "</li>\n</ul>\n"))))

(defun org-e-html-toc (depth info)
  (assert (wholenump depth))
  (let* ((headlines (org-export-collect-headlines info depth))
	 (toc-entries
	  (loop for headline in headlines collect
		(list (org-e-html-headline-text
		       headline info 'org-e-html-toc-entry-formatter)
		      (org-export-get-relative-level headline info)))))
    (when toc-entries
      (let* ((lang-specific-heading "Table of Contents")) ; FIXME
	(concat
	 "<div id=\"table-of-contents\">\n"
	 (org-e-html-format-heading lang-specific-heading
				    (or org-e-html-toplevel-hlevel 1))
	 "<div id=\"text-table-of-contents\">"
	 (org-e-html-toc-text toc-entries)
	 "</div>\n"
	 "</div>\n")))))

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

(defun org-e-html-format-spaces (n)
  (let (out) (dotimes (i n out) (setq out (concat out "&nbsp;")))))

(defun org-e-html-format-tabs (&optional n)
  (ignore))

(defun org-e-html-format-line-break ()
  "<br/>\n")

(defun org-e-html-format-horizontal-line ()
  "<hr/>\n")

;; (defun org-e-html-format-line (line)
;;   (case org-lparse-dyn-current-environment
;;     ((quote fixedwidth) (concat (org-e-html-encode-plain-text line) "\n"))
;;     (t (concat line "\n"))))

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
    (concat (format "<h%d%s>" level extra) text (format "</h%d>\n" level))))

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

(defun org-e-html-format-org-tags (tags)
  (if (not tags) ""
    (org-e-html-format-fontify
     (mapconcat
      (lambda (x)
	(org-e-html-format-fontify
	 x (concat org-e-html-tag-class-prefix
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
    (format org-e-html-footnote-format
	    (format
	     "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>"
	     n extra n n))))

(defun org-e-html-format-footnotes-section (section-name definitions)
  (if (not definitions) ""
    (format org-e-html-footnotes-section section-name definitions)))

(defun org-e-html-format-footnote-definition (fn)
  (let ((n (car fn)) (def (cdr fn)))
    (format
     "<tr>\n<td>%s</td>\n<td>%s</td>\n</tr>\n"
     (format
      (format org-e-html-footnote-format
	      "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>")
      n n n) def)))

(defun org-e-html-footnote-section (info)
  (let* ((fn-alist (org-export-collect-footnote-definitions
		    (plist-get info :parse-tree) info))

	 (fn-alist
	  (loop for (n type raw) in fn-alist collect
		(cons n (if (equal (org-element-type raw) 'org-data)
			    (org-trim (org-export-data raw 'e-html info))
			  (format "<p>%s</p>"
				  (org-trim (org-export-secondary-string
					     raw 'e-html info))))))))
    (when fn-alist
      (org-e-html-format-footnotes-section
       (nth 4 (or (assoc (plist-get info :language)
			 org-export-language-setup)
		  (assoc "en" org-export-language-setup)))

       (format
	"<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">\n%s\n</table>\n"
	(mapconcat 'org-e-html-format-footnote-definition fn-alist "\n"))))))

(defun org-e-html-format-org-entity (wd)
  (org-entity-get-representation wd 'html))

(defun org-e-html-get-coding-system-for-write ()
  (or org-e-html-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

(defun org-e-html-get-coding-system-for-save ()
  (or org-e-html-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

(defun org-e-html-format-date (info)
  (let ((date (plist-get info :date)))
    (cond
     ((and date (string-match "%" date))
      (format-time-string date))
     (date date)
     (t (format-time-string "%Y-%m-%d %T %Z")))))



;;; Internal Functions (Ngz)

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
  ;; (let ((label (org-element-property :name element)))
  ;;   (if (or (not output) (not label) (string= output "") (string= label ""))
  ;; 	output
  ;;     (concat (format "\\label{%s}\n" label) output)))
  output)



;;; Template

(defun org-e-html-meta-info (info)
  (let* ((title (org-export-secondary-string
		 (plist-get info :title) 'e-html info))
	 (author (and (plist-get info :with-author)
		      (let ((auth (plist-get info :author)))
			(and auth (org-export-secondary-string
				   auth 'e-html info)))))
	 (description (plist-get info :description))
	 (keywords (plist-get info :keywords)))
    (concat
     (format "<title>%s</title>\n" title)
     (format
      "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>\n"
      (and coding-system-for-write
	   (fboundp 'coding-system-get)
	   (coding-system-get coding-system-for-write
			      'mime-charset)))
     (format "<meta name=\"title\" content=\"%s\"/>\n" title)
     (format "<meta name=\"generator\" content=\"Org-mode\"/>")
     (format "<meta name=\"generated\" content=\"%s\"/>\n"
	     (org-e-html-format-date info))
     (format "<meta name=\"author\" content=\"%s\"/>\n" author)
     (format "<meta name=\"description\" content=\"%s\"/>\n" description)
     (format " <meta name=\"keywords\" content=\"%s\"/>\n" keywords))))

(defun org-e-html-style (info)
  (concat
   (when (plist-get info :style-include-default)
     org-e-html-style-default)
   (plist-get info :style)
   (plist-get info :style-extra)
   "\n"
   (when (plist-get info :style-include-scripts)
     org-e-html-scripts)))

(defun org-e-html-mathjax-config (template options in-buffer)
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

(defun org-e-html-mathjax (info)
  (when (or (eq (plist-get info :HTML-fragments) 'mathjax)
	    (and org-export-have-math
		 (eq (plist-get info :HTML-fragments) t)))
    (org-e-html-mathjax-config
     org-e-html-mathjax-template
     org-e-html-mathjax-options
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
				org-e-html-preamble-format))
		   (cadr (assoc "en" org-e-html-preamble-format)))
	       `((?t . ,title) (?a . ,author)
		 (?d . ,date) (?e . ,email)))))))
      (when (not (equal html-pre-real-contents ""))
	(concat
	 (format "
<div id=\"%s\"> "  (nth 0 org-e-html-divs))
	 "
"
	 html-pre-real-contents
	 "
</div>")))))

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
	    (html-validation-link (or org-e-html-validation-link ""))
	    (creator-info
	     (concat "Org version " org-version " with Emacs version "
		     (number-to-string emacs-major-version))))
       (concat
	;; begin postamble
	"
<div id=\"" (nth 2 org-e-html-divs) "\">"
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
			    org-e-html-postamble-format))
	       (cadr (assoc "en" org-e-html-postamble-format)))
	   `((?a . ,author) (?e . ,email)
	     (?d . ,date)   (?c . ,creator-info)
	     (?v . ,html-validation-link)))))
	"
</div>")))
   ;; org-e-html-html-helper-timestamp
   ))

(defun org-e-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."
  (concat
   (format
    (or (and (stringp org-e-html-xml-declaration)
	     org-e-html-xml-declaration)
	(cdr (assoc (plist-get info :html-extension)
		    org-e-html-xml-declaration))
	(cdr (assoc "html" org-e-html-xml-declaration))

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
       (format org-e-html-home/up-format
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; preamble
   (org-e-html-preamble info)
   ;; begin content
   (format "
<div id=\"%s\">" (or org-e-html-content-div
		     (nth 1 org-e-html-divs)))
   ;; document title
   (format "
<h1 class=\"title\"> %s </h1>\n" (plist-get info :title))
   ;; table of contents
   (let ((depth (plist-get info :with-toc)))
     (when (wholenump depth) (org-e-html-toc depth info)))
   ;; document contents
   contents
   ;; footnotes section
   (org-e-html-footnote-section info)
   ;; bibliography
   (org-e-html-bibliography)
   ;; end content
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
  (let* ((name (org-element-property :drawer-name drawer))
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
  ;; (format (cdr (assoc (org-element-property :marker emphasis)
  ;; 		      org-e-html-emphasis-alist))
  ;; 	  contents)
  (org-e-html-format-fontify
   contents (cadr (assoc
		   (org-element-property :marker emphasis)
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
  ;; (let ((ent (org-element-property :latex entity)))
  ;;   (if (org-element-property :latex-math-p entity)
  ;; 	(format "$%s$" ent)
  ;;     ent))
  (org-element-property :html entity))


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
  (let* ((options (or (org-element-property :options example-block) ""))
	 (value (org-export-handle-code example-block info)))
    ;; (org-e-html--wrap-label
    ;;  example-block (format "\\begin{verbatim}\n%s\\end{verbatim}" value))
    (org-e-html--wrap-label
     example-block (org-e-html-format-source-code-or-example nil value))))


;;;; Export Snippet

(defun org-e-html-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value export-snippet))


;;;; Export Block

(defun org-e-html-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "latex")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-e-html-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((value (org-element-normalize-string
		 (replace-regexp-in-string
		  "^[ \t]*: ?" ""
		  (org-element-property :value fixed-width)))))
    (org-e-html--wrap-label
     fixed-width (org-e-html-format-source-code-or-example nil value))))


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
   (cond
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     (org-e-html-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "IGNORED" 100))
    ;; Inline definitions are secondary strings.
    ((eq (org-element-property :type footnote-reference) 'inline)
     (org-e-html-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "IGNORED" 1))
    ;; Non-inline footnotes definitions are full Org data.
    (t (org-e-html-format-footnote-reference
	(org-export-get-footnote-number footnote-reference info)
	"IGNORED" 1)))))


;;;; Headline

(defun org-e-html-todo (todo)
  (when todo
    (org-e-html-format-fontify
     (concat
      org-e-html-todo-kwd-class-prefix
      (org-e-html-fix-class-name todo))
     (list (if (member todo org-done-keywords) "done" "todo")
	   todo))))

(defun org-e-html-headline-text (headline info &optional formatter)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((numberedp (plist-get info :section-numbers))
	 (level (org-export-get-relative-level headline info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property
				 :todo-keyword headline)))
		      (and todo
			   (org-export-secondary-string todo 'e-html info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-secondary-string
		(org-element-property :title headline) 'e-html info))
	 (tags (and (plist-get info :with-tags)
		    (org-element-property :tags headline)))

	 (headline-no (org-export-get-headline-number headline info))
	 (headline-label
	  (format "sec-%s" (mapconcat 'number-to-string headline-no "-")))
	 (headline-labels (list headline-label))
	 (headline-no (org-export-get-headline-number headline info))
	 (section-no (mapconcat 'number-to-string headline-no "."))
	 (primary-target (car (last headline-labels)))
	 (secondary-targets (butlast headline-labels))
	 (extra-class nil)
	 (formatter (or (and (functionp formatter) formatter)
			org-e-html-headline-formatter)))
    (funcall formatter level section-no todo todo-type priority
	     text tags primary-target secondary-targets extra-class)))

(defun org-e-html-headline (headline contents info)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :latex-class))
	 (numberedp (plist-get info :section-numbers))
	 ;; Get level relative to current parsed data.
	 (level (org-export-get-relative-level headline info))
	 ;; (class-sectionning (assoc class org-e-html-classes))
	 ;; Section formatting will set two placeholders: one for the
	 ;; title and the other for the contents.
	 ;; (section-fmt
	 ;;  (let ((sec (if (and (symbolp (nth 2 class-sectionning))
	 ;; 		      (fboundp (nth 2 class-sectionning)))
	 ;; 		 (funcall (nth 2 class-sectionning) level numberedp)
	 ;; 	       (nth (1+ level) class-sectionning))))
	 ;;    (cond
	 ;;     ;; No section available for that LEVEL.
	 ;;     ((not sec) nil)
	 ;;     ;; Section format directly returned by a function.
	 ;;     ((stringp sec) sec)
	 ;;     ;; (numbered-section . unnumbered-section)
	 ;;     ((not (consp (cdr sec)))
	 ;;      (concat (funcall (if numberedp #'car #'cdr) sec) "\n%s"))
	 ;;     ;; (numbered-open numbered-close)
	 ;;     ((= (length sec) 2)
	 ;;      (when numberedp (concat (car sec) "\n%s" (nth 1 sec))))
	 ;;     ;; (num-in num-out no-num-in no-num-out)
	 ;;     ((= (length sec) 4)
	 ;;      (if numberedp
	 ;; 	  (concat (car sec) "\n%s" (nth 1 sec))
	 ;; 	(concat (nth 2 sec) "\n%s" (nth 3 sec)))))))
	 (text (org-export-secondary-string
		(org-element-property :title headline) 'e-html info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property
				 :todo-keyword headline)))
		      (and todo
			   (org-export-secondary-string todo 'e-html info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-element-property :tags headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
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
		      (org-element-property :pre-blank headline) 10)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info) ; FIXME (or (not section-fmt))
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'unordered 'unordered)) ; FIXME
	     (itemized-body (org-e-html-format-list-item
			     contents type nil nil full-text)))
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (org-e-html-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-e-html-end-plain-list type)))))
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
			 (org-element-property :attr_html horizontal-rule)
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
  (let* ((org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block))
	 (separator (org-e-html--find-verb-separator code)))
    (error "FIXME")))


;;;; Inlinetask

(defun org-e-html-format-section (text class &optional id)
  (let ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<div class=\"%s\"%s>\n" class extra) text "</div>\n")))

(defun org-e-html-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-secondary-string
	       (org-element-property :title inlinetask) 'e-html info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-property
				:todo-keyword inlinetask)))
		     (and todo
			  (org-export-secondary-string todo 'e-html info)))))
	(todo-type (org-element-property :todo-type inlinetask))
	(tags (and (plist-get info :with-tags)
		   (org-element-property :tags inlinetask)))
	(priority (and (plist-get info :with-priority)
		       (org-element-property :priority inlinetask))))
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

(defun org-e-html-format-list-item (contents type checkbox
					     &optional term-counter-id
					     headline)
  (when checkbox
    (setq checkbox
	  (org-e-html-format-fontify (case checkbox
				       (on "[X]")
				       (off "[&nbsp;]")
				       (trans "[-]")) 'code)))
  (concat
   (case type
     (ordered
      (let* ((counter term-counter-id)
	     (extra (if counter (format " value=\"%s\"" counter) "")))
	(format "<li%s>" extra)))
     (unordered
      (let* ((id term-counter-id)
	     (extra (if id (format " id=\"%s\"" id) "")))
	(concat
	 (format "<li%s>" extra)
	 (when headline (concat headline "<br/>")))))
     (descriptive
      (let* ((term term-counter-id))
	(setq term (or term "(no term)"))
	(concat (format "<dt> %s </dt>" term) "<dd>"))))
   checkbox (and checkbox " ") contents
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
	 (type (org-element-property :type plain-list))
	 (level (org-element-property
		 :level (car (plist-get info :genealogy))))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-secondary-string tag 'e-html info)))))
    (org-e-html-format-list-item
     contents type checkbox (or tag counter))))


;;;; Keyword

(defun org-e-html-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (downcase (org-element-property :key keyword)))
	(value (org-element-property :value keyword)))
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
	    (when (wholenump depth) (org-e-html-toc depth info))))
	 ((string= "tables" value) "\\listoftables")
	 ((string= "figures" value) "\\listoffigures")
	 ((string= "listings" value)
	  (cond
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
  (org-e-html--wrap-label
   latex-environment
   (let ((latex-frag
	  (org-remove-indentation
	   (org-element-property :value latex-environment)))
	 (processing-type (plist-get info :LaTeX-fragments)))
     (cond
      ((member processing-type '(t mathjax))
       (org-e-html-format-latex latex-frag 'mathjax))
      ((equal processing-type 'dvipng)
       (let* ((formula-link (org-e-html-format-latex
			     latex-frag processing-type)))
	 (when (and formula-link
		    (string-match "file:\\([^]]*\\)" formula-link))
	   (org-e-html-format-inline-image (match-string 1 formula-link)))))
      (t
       latex-frag)))))


;;;; Latex Fragment

(defun org-e-html-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; (org-element-property :value latex-fragment)
  (let* ((latex-frag (org-element-property :value latex-fragment)))
    (cond
     ((string-match "\\\\ref{\\([^{}\n]+\\)}" latex-frag)
      (let* ((label (match-string 1 latex-frag))
	     (href (and label (org-export-solidify-link-text label)))
	     (text (if (string-match "\\`[a-z]\\{1,10\\}:\\(.+\\)" label)
		       (substring label (match-beginning 1))
		     label)))
	(org-e-html-format-internal-link text href)))
     (t (let ((processing-type (plist-get info :LaTeX-fragments)))
	  (cond
	   ((member processing-type '(t mathjax))
	    (org-e-html-format-latex latex-frag 'mathjax))
	   ((equal processing-type 'dvipng)
	    (let* ((formula-link (org-e-html-format-latex
				  latex-frag processing-type)))
	      (when (and formula-link
			 (string-match "file:\\([^]]*\\)" formula-link))
		(org-e-html-format-inline-image
		 (match-string 1 formula-link)))))
	   (t latex-frag)))))))


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
	 (path (let ((raw-path (org-element-property :path link)))
		 (if (not (file-name-absolute-p raw-path)) raw-path
		   (expand-file-name raw-path))))
	 (caption (org-e-html--caption/label-string
		   (org-element-property :caption parent)
		   (org-element-property :name parent)
		   info))
	 (label (org-element-property :name parent))
	 ;; Retrieve latex attributes from the element around.
	 (attr (let ((raw-attr
		      (mapconcat #'identity
				 (org-element-property :attr_html parent)
				 " ")))
		 (unless (string= raw-attr "") raw-attr))))
    ;; Now clear ATTR from any special keyword and set a default
    ;; value if nothing is left.
    (setq attr (if (not attr) "" (org-trim attr)))
    ;; Return proper string, depending on DISPOSITION.
    (let ((href (and label (org-export-solidify-link-text label))))
      (org-e-html-format-inline-image path caption href attr))))

(defun org-e-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
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
	(case (org-element-type destination)
	  (target
	   (org-e-html-format-internal-link
	    (or desc
		(org-export-secondary-string
		 (org-element-property :raw-link link)
		 'e-html info))
	    (org-export-solidify-link-text
	     (org-element-property :raw-value destination))))
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
	       (org-e-html-format-internal-link
		(or desc
		    (org-export-secondary-string
		     (org-element-property :title destination)
		     'e-html info)) label))))
	  ;; Fuzzy link points nowhere.
	  (otherwise
	   (org-e-html-format-fontify
	    (or desc
		(org-export-secondary-string
		 (org-element-property :raw-link link)
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
     ((and path desc) (org-e-html-format-link desc path))
     ;; External link without a description part.
     (path (org-e-html-format-link path path))
     ;; No path, only description.  Try to do something useful.
     (t (org-e-html-format-fontify desc 'emphasis)))))


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
     ((and (equal (car parent) 'item)
	   (= (org-element-property :begin paragraph)
	      (org-element-property :contents-begin parent)))
      ;; leading paragraph in a list item have no tags
      contents)
     (t (concat (format "<p%s> " extra) contents "</p>")))))


;;;; Plain List

(defun org-e-html-begin-plain-list (type &optional arg1)
  (case type
    (ordered
     (format "<ol%s>" (if arg1		; FIXME
			  (format " start=\"%d\"" arg1)
			"")))
    (unordered "<ul>")
    (descriptive "<dl>")))

(defun org-e-html-end-plain-list (type)
  (case type
    (ordered "</ol>")
    (unordered "</ul>")
    (descriptive "</dl>")))

(defun org-e-html-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* (arg1 ;; FIXME
	 (type (org-element-property :type plain-list))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_html plain-list)
			  " ")))
    (org-e-html--wrap-label
     plain-list (format "%s\n%s%s"
			(org-e-html-begin-plain-list type)
			contents (org-e-html-end-plain-list type)))))

;;;; Plain Text

(defun org-e-html-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-e-html-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(setq string (replace-match rpl t nil string))))
    string))

(defun org-e-html-encode-plain-text (s)
  "Convert plain text characters to HTML equivalent.
Possible conversions are set in `org-export-html-protect-char-alist'."
  (let ((cl org-e-html-protect-char-alist) c)
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
  (org-e-html--wrap-label
   quote-block (format "<blockquote>\n%s</blockquote>" contents)))


;;;; Quote Section

(defun org-e-html-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format "<pre>\n%s</pre>" value))))


;;;; Section

(defun org-e-html-section (section contents info) ; FIXME
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section info)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let ((class-num (org-export-get-relative-level parent info))
            (id-num
             (mapconcat
              'number-to-string
	      (org-export-get-headline-number parent info) "-")))
        ;; Build return value.
        (format "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>"
                class-num id-num contents)))))

;;;; Radio Target

(defun org-e-html-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (org-e-html-format-anchor
   text (org-export-solidify-link-text
	 (org-element-property :raw-value radio-target))))


;;;; Special Block

(defun org-e-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-e-html--wrap-label
     special-block
     (format "\\begin{%s}\n%s\\end{%s}" type contents type))))


;;;; Src Block

(defun org-e-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
	 (code (org-export-handle-code src-block info))
	 (caption (org-element-property :caption src-block))
	 (label (org-element-property :name src-block)))
    ;; FIXME: Handle caption

    ;; caption-str (when caption)
    ;; (main (org-export-secondary-string (car caption) 'e-html info))
    ;; (secondary (org-export-secondary-string (cdr caption) 'e-html info))
    ;; (caption-str (org-e-html--caption/label-string caption label info))
    (org-e-html-format-source-code-or-example lang code)))


;;;; Statistics Cookie

(defun org-e-html-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (org-e-html-format-fontify cookie-value 'code)))


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

(defun org-e-html-begin-table (caption label attributes)
  (let* ((html-table-tag (or (plist-get info :html-table-tag) ; FIXME
			     org-e-html-table-tag))
	 (html-table-tag
	  (org-e-html-splice-attributes html-table-tag attributes)))
    (when label
      (setq html-table-tag
	    (org-e-html-splice-attributes
	     html-table-tag
	     (format "id=\"%s\"" (org-solidify-link-text label)))))
    (concat "\n" html-table-tag
	    (format "\n<caption>%s</caption>" (or caption "")))))

(defun org-e-html-end-table ()
  "</table>\n")

(defun org-e-html-format-table-cell (text r c horiz-span)
  (let ((cell-style-cookie
	 (if org-e-html-table-align-individual-fields
	     (format (if (and (boundp 'org-e-html-format-table-no-css)
			      org-e-html-format-table-no-css)
			 " align=\"%s\"" " class=\"%s\"")
		     (or (aref (plist-get table-info :alignment) c) "left")) ""))) ;; FIXME
    (cond
     (org-e-html-table-cur-rowgrp-is-hdr
      (concat
       (format (car org-export-table-header-tags) "col" cell-style-cookie)
       text (cdr org-export-table-header-tags)))
     ((and (= c 0) org-e-html-table-use-header-tags-for-first-column)
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
  (incf org-e-html-table-rownum)
  (let ((i -1))
    (org-e-html-format-table-row
     (mapconcat
      (lambda (x)
	(when (and (string= x "") text-for-empty-fields)
	  (setq x text-for-empty-fields))
	(incf i)
	(let (horiz-span)
	  (org-e-html-format-table-cell
	   x org-e-html-table-rownum i (or horiz-span 0))))
      fields "\n"))))

(defun org-e-html-end-table-rowgroup ()
  (when org-e-html-table-rowgrp-open
    (setq org-e-html-table-rowgrp-open nil)
    (if org-e-html-table-cur-rowgrp-is-hdr "</thead>" "</tbody>")))

(defun org-e-html-begin-table-rowgroup (&optional is-header-row)
  (concat
   (when org-e-html-table-rowgrp-open
     (org-e-html-end-table-rowgroup))
   (progn
     (setq org-e-html-table-rowgrp-open t)
     (setq org-e-html-table-cur-rowgrp-is-hdr is-header-row)
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

(defun org-e-html-list-table (lines caption label attributes)
  (setq lines (org-e-html-org-table-to-list-table lines))
  (let* ((splice nil) head
	 (org-e-html-table-rownum -1)
	 i (cnt 0)
	 fields line
	 org-e-html-table-cur-rowgrp-is-hdr
	 org-e-html-table-rowgrp-open
	 n
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
       (org-e-html-begin-table-rowgroup head)

       (mapconcat
	(lambda (line)
	  (cond
	   ((equal line 'hline) (org-e-html-begin-table-rowgroup))
	   (t (org-e-html-table-row line))))
	lines "\n")

       (org-e-html-end-table-rowgroup)
       (org-e-html-end-table))))))

(defun org-e-html-transcode-table-row (row)
  (if (string-match org-table-hline-regexp row) 'hline
    (mapcar
     (lambda (cell)
       (org-export-secondary-string
	(let ((cell (org-element-parse-secondary-string
		     cell
		     (cdr (assq 'table org-element-string-restrictions)))))
	  cell)
	'e-html info))
     (org-split-string row "[ \t]*|[ \t]*"))))

(defun org-e-html-org-table-to-list-table (lines &optional splice)
  "Convert org-table to list-table.
LINES is a list of the form (ROW1 ROW2 ROW3 ...) where each
element is a `string' representing a single row of org-table.
Thus each ROW has vertical separators \"|\" separating the table
fields.  A ROW could also be a row-group separator of the form
\"|---...|\".  Return a list of the form (ROW1 ROW2 ROW3
...). ROW could either be symbol `'hline' or a list of the
form (FIELD1 FIELD2 FIELD3 ...) as appropriate."
  (let (line lines-1)
    (cond
     (splice
      (while (setq line (pop lines))
	(unless (string-match "^[ \t]*|-" line)
	  (push (org-e-html-transcode-table-row line) lines-1))))
     (t (while (setq line (pop lines))
	  (cond
	   ((string-match "^[ \t]*|-" line)
	    (when lines (push 'hline lines-1)))
	   (t (push (org-e-html-transcode-table-row line) lines-1))))))
    (nreverse lines-1)))

(defun org-e-html-table-table (raw-table)
  (require 'table)
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
    output))

(defun org-e-html-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((label (org-element-property :name table))
	 (caption (org-e-html--caption/label-string
		   (org-element-property :caption table) label info))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_html table)
			  " "))
	 (raw-table (org-element-property :raw-table table))
	 (table-type (org-element-property :type table)))
    (case table-type
      (table.el
       (org-e-html-table-table raw-table))
      (t
       (let* ((table-info (org-export-table-format-info raw-table))
	      (columns-number (length (plist-get table-info :alignment)))
	      (lines (org-split-string
		      (org-export-clean-table
		       raw-table (plist-get table-info :special-column-p)) "\n")))
	 (org-e-html-list-table lines caption label attr))))))


;;;; Target

(defun org-e-html-target (target text info)
  "Transcode a TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (org-e-html-format-anchor
   text (org-export-solidify-link-text
	 (org-element-property :raw-value target))))


;;;; Time-stamp

(defun org-e-html-time-stamp (time-stamp contents info)
  "Transcode a TIME-STAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; (let ((value (org-element-property :value time-stamp))
  ;; 	(type (org-element-property :type time-stamp))
  ;; 	(appt-type (org-element-property :appt-type time-stamp)))
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
  (let ((value (org-element-property :value time-stamp))
        (type (org-element-property :type time-stamp))
        (appt-type (org-element-property :appt-type time-stamp)))
    (setq value (org-export-secondary-string value 'e-html info))
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
   verbatim (org-element-property :value verbatim) info))


;;;; Verse Block

(defun org-e-html-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Replace each newline character with line break.  Also replace
  ;; each blank line with a line break.
  (setq contents (replace-regexp-in-string
		  "^ *\\\\\\\\$" "<br/>\n"
		  (replace-regexp-in-string
		   "\\(\\\\\\\\\\)?[ \t]*\n" " <br/>\n"
		   (org-remove-indentation
		    (org-export-secondary-string
		     (org-element-property :value verse-block)
		     'e-html info)))))

  ;; Replace each white space at beginning of a line with a
  ;; non-breaking space.
  (while (string-match "^[ \t]+" contents)
    (let ((new-str (org-e-html-format-spaces
		    (length (match-string 0 contents)))))
      (setq contents (replace-match new-str nil t contents))))

  (org-e-html--wrap-label
   verse-block (format "<p class=\"verse\">\n%s</p>" contents)))




;;; Filter Functions

;;;; Filter Settings

(defconst org-e-html-filters-alist
  '((:filter-final-output . org-e-html-final-function))
  "Alist between filters keywords and back-end specific filters.
See `org-export-filters-alist' for more information.")


;;;; Filters

(defun org-e-html-final-function (contents backend info)
  (if (not org-e-html-pretty-output) contents
    (with-temp-buffer
      (nxml-mode)
      (insert contents)
      (indent-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))


;;; Interactive functions

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

  ;; FIXME
  (with-current-buffer (get-buffer-create "*debug*")
    (erase-buffer))

  (let ((outfile (org-export-output-file-name ".html" subtreep pub-dir)))
    (org-export-to-file
     'e-html outfile subtreep visible-only body-only ext-plist)))



;;; FIXMES, TODOS, FOR REVIEW etc

;;;; org-format-table-html
;;;; org-format-org-table-html
;;;; org-format-table-table-html
;;;; org-table-number-fraction
;;;; org-table-number-regexp
;;;; org-e-html-table-caption-above

;;;; org-whitespace
;;;; "<span style=\"visibility:hidden;\">%s</span>"
;;;; Remove display properties
;;;; org-e-html-final-hook

;;;; org-e-html-with-timestamp
;;;; org-e-html-html-helper-timestamp

;;;; org-export-as-html-and-open
;;;; org-export-as-html-batch
;;;; org-export-as-html-to-buffer
;;;; org-replace-region-by-html
;;;; org-export-region-as-html
;;;; org-export-as-html

;;;; (org-export-directory :html opt-plist)
;;;; (plist-get opt-plist :html-extension)
;;;; org-e-html-toplevel-hlevel
;;;; org-e-html-special-string-regexps
;;;; org-e-html-coding-system
;;;; org-e-html-coding-system
;;;; org-e-html-inline-images
;;;; org-e-html-inline-image-extensions
;;;; org-e-html-protect-char-alist
;;;; org-e-html-table-use-header-tags-for-first-column
;;;; org-e-html-todo-kwd-class-prefix
;;;; org-e-html-tag-class-prefix
;;;; org-e-html-footnote-separator

(provide 'org-e-html)
;;; org-e-html.el ends here
