;;; org-e-beamer.el --- Beamer Back-End for Org Export Engine

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik AT gmail DOT com>
;;         Nicolas Goaziou <n.goaziou AT gmail DOT com>
;; Keywords: org, wp, tex

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements both a Beamer back-end, derived from the
;; LaTeX one and a minor mode easing structure edition of the
;; document.
;;
;; Depending on the desired output format, three commands are provided
;; for export: `org-e-beamer-export-as-latex' (temporary buffer),
;; `org-e-beamer-export-to-latex' ("tex" file) and
;; `org-e-beamer-export-to-pdf' ("pdf" file).
;;
;; On top of buffer keywords supported by `e-latex' back-end (see
;; `org-e-latex-options-alist'), this back-end introduces the
;; following keywords: "BEAMER_THEME", "BEAMER_COLOR_THEME",
;; "BEAMER_FONT_THEME", "BEAMER_INNER_THEME" and "BEAMER_OUTER_THEME".
;; All accept options in square brackets.
;;
;; Moreover, headlines now fall into three categories: sectioning
;; elements, frames and blocks.
;;
;; - Like `e-latex' back-end sectioning elements are still set through
;;   `org-e-latex-classes' variable.
;;
;; - Headlines become frames when their level is equal to
;;   `org-e-beamer-frame-level' (or "H" value in the OPTIONS line).
;;   Though, if an headline in the current tree has a "BEAMER_env"
;;   (see below) property set to "frame", its level overrides the
;;   variable.
;;
;; - All frames' children become block environments.  Special block
;;   types can be enforced by setting headline's "BEAMER_env" property
;;   to an appropriate value (see `org-e-beamer-environments-default'
;;   for supported value and `org-e-beamer-environments-extra' for
;;   adding more).
;;
;; - As a special case, if the "BEAMER_env" property is set to either
;;   "appendix", "note" or "noteNH", the headline will become,
;;   respectively, an appendix, a note (within frame or between frame,
;;   depending on its level) and a note with its title ignored.
;;
;;   Also, an headline with an "ignoreheading" value will have its
;;   contents only inserted in the output.  This special value is
;;   useful to have data between frames, or to properly close
;;   a "column" environment.
;;
;; Along with "BEAMER_env", headlines also support "BEAMER_act" and
;; "BEAMER_opt" properties.  The former is translated as an
;; overlay/action specification (or a default overlay specification
;; when enclosed within square brackets) whereas the latter specifies
;; options for the current frame ("fragile" option is added
;; automatically, though).
;;
;; Every plain list has support for `:overlay' attribute (through
;; ATTR_BEAMER affiliated keyword).  Also, ordered (resp. description)
;; lists make use of `:template' (resp. `:long-text') attribute.
;;
;; Eventually, an export snippet with a value enclosed within angular
;; brackets put at the beginning of an element or object whose type is
;; among `bold', `item', `link', `radio-target' and `target' will
;; control its overlay specifications.
;;
;; On the minor mode side, `org-e-beamer-select-environment' (bound by
;; default to "C-c C-b") and `org-e-beamer-insert-options-template'
;; are the two entry points.

;;; Code:

(require 'org-e-latex)



;;; User-Configurable Variables

(defgroup org-export-e-beamer nil
  "Options specific for using the beamer class in LaTeX export."
  :tag "Org Beamer"
  :group 'org-export
  :version "24.2")

(defcustom org-e-beamer-frame-level 1
  "The level at which headlines become frames.

Headlines at a lower level will be translated into a sectioning
structure.  At a higher level, they will be translated into
blocks.

If an headline with a \"BEAMER_env\" property set to \"frame\" is
found within a tree, its level locally overrides this number.

This variable has no effect on headlines with the \"BEAMER_env\"
property set to either \"ignoreheading\", \"appendix\", or
\"note\", which will respectively, be invisible, become an
appendix or a note.

This integer is relative to the minimal level of an headline
within the parse tree, defined as 1."
  :group 'org-export-e-beamer
  :type 'integer)

(defcustom org-e-beamer-frame-default-options ""
  "Default options string to use for frames.
For example, it could be set to \"allowframebreaks\"."
  :group 'org-export-e-beamer
  :type '(string :tag "[options]"))

(defcustom org-e-beamer-column-view-format
  "%45ITEM %10BEAMER_env(Env) %10BEAMER_act(Act) %4BEAMER_col(Col) %8BEAMER_opt(Opt)"
  "Column view format that should be used to fill the template."
  :group 'org-export-e-beamer
  :type '(choice
	  (const  :tag "Do not insert Beamer column view format" nil)
	  (string :tag "Beamer column view format")))

(defcustom org-e-beamer-theme "default"
  "Default theme used in Beamer presentations."
  :group 'org-export-e-beamer
  :type '(choice
	  (const :tag "Do not insert a Beamer theme" nil)
	  (string :tag "Beamer theme")))

(defcustom org-e-beamer-environments-extra nil
  "Environments triggered by tags in Beamer export.
Each entry has 4 elements:

name    Name of the environment
key     Selection key for `org-e-beamer-select-environment'
open    The opening template for the environment, with the following escapes
        %a   the action/overlay specification
        %A   the default action/overlay specification
        %o   the options argument of the template
        %h   the headline text
        %H   if there is headline text, that text in {} braces
        %U   if there is headline text, that text in [] brackets
close   The closing string of the environment."
  :group 'org-export-e-beamer
  :type '(repeat
	  (list
	   (string :tag "Environment")
	   (string :tag "Selection key")
	   (string :tag "Begin")
	   (string :tag "End"))))

(defcustom org-e-beamer-outline-frame-title "Outline"
  "Default title of a frame containing an outline."
  :group 'org-export-e-beamer
  :type '(string :tag "Outline frame title"))

(defcustom org-e-beamer-outline-frame-options ""
  "Outline frame options appended after \\begin{frame}.
You might want to put e.g. \"allowframebreaks=0.9\" here."
  :group 'org-export-e-beamer
  :type '(string :tag "Outline frame options"))



;;; Internal Variables

(defconst org-e-beamer-column-widths
  "0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.0 :ETC"
"The column widths that should be installed as allowed property values.")

(defconst org-e-beamer-environments-special
  '(("appendix"       "x")
    ("column"         "c")
    ("frame"          "f")
    ("ignoreheading"  "i")
    ("note"           "n")
    ("noteNH"         "N"))
  "Alist of environments treated in a special way by the back-end.
Keys are environment names, as strings, values are bindings used
in `org-e-beamer-select-environment'.  Environments listed here,
along with their binding, are hard coded and cannot be modified
through `org-e-beamer-environments-extra' variable.")

(defconst org-e-beamer-environments-default
  '(("block"          "b" "\\begin{block}%a{%h}"          "\\end{block}")
    ("alertblock"     "a" "\\begin{alertblock}%a{%h}"     "\\end{alertblock}")
    ("verse"          "v" "\\begin{verse}%a %% %h"        "\\end{verse}")
    ("quotation"      "q" "\\begin{quotation}%a %% %h"    "\\end{quotation}")
    ("quote"          "Q" "\\begin{quote}%a %% %h"        "\\end{quote}")
    ("structureenv"   "s" "\\begin{structureenv}%a %% %h" "\\end{structureenv}")
    ("theorem"        "t" "\\begin{theorem}%a%U"          "\\end{theorem}")
    ("definition"     "d" "\\begin{definition}%a%U"       "\\end{definition}")
    ("example"        "e" "\\begin{example}%a%U"          "\\end{example}")
    ("exampleblock"   "E" "\\begin{exampleblock}%a{%h}"   "\\end{exampleblock}")
    ("proof"          "p" "\\begin{proof}%a%U"            "\\end{proof}")
    ("beamercolorbox" "o" "\\begin{beamercolorbox}%o{%h}" "\\end{beamercolorbox}"))
  "Environments triggered by properties in Beamer export.
These are the defaults - for user definitions, see
`org-e-beamer-environments-extra'.")

(defconst org-e-beamer-verbatim-elements
  '(code example-block fixed-width inline-src-block src-block verbatim)
  "List of element or object types producing verbatim text.
This is used internally to determine when a frame should have the
\"fragile\" option.")



;;; Internal functions

(defun org-e-beamer--normalize-argument (argument type)
  "Return ARGUMENT string with proper boundaries.

TYPE is a symbol among the following:
`action'    Return ARGUMENT within angular brackets.
`defaction' Return ARGUMENT within both square and angular brackets.
`option'    Return ARGUMENT within square brackets."
  (if (not (string-match "\\S-" argument)) ""
    (case type
      (action (if (string-match "\\`<.*>\\'" argument) argument
		(format "<%s>" argument)))
      (defaction (cond
		  ((string-match "\\`\\[<.*>\\]\\'" argument) argument)
		  ((string-match "\\`<.*>\\'" argument)
		   (format "[%s]" argument))
		  ((string-match "\\`\\[\\(.*\\)\\]\\'" argument)
		   (format "[<%s>]" (match-string 1 argument)))
		  (t (format "[<%s>]" argument))))
      (option (if (string-match "\\`\\[.*\\]\\'" argument) argument
		(format "[%s]" argument)))
      (otherwise argument))))

(defun org-e-beamer--element-has-overlay-p (element)
  "Non-nil when ELEMENT has an overlay specified.
An element has an overlay specification when it starts with an
`e-beamer' export-snippet whose value is between angular
brackets.  Return overlay specification, as a string, or nil."
  (let ((first-object (car (org-element-contents element))))
    (when (eq (org-element-type first-object) 'export-snippet)
      (let ((value (org-element-property :value first-object)))
	(and (string-match "\\`<.*>\\'" value) value)))))



;;; Define Back-End

(org-export-define-derived-backend e-beamer e-latex
  :export-block "BEAMER"
  :options-alist
  ((:beamer-theme "BEAMER_THEME" nil org-e-beamer-theme)
   (:beamer-color-theme "BEAMER_COLOR_THEME" nil nil t)
   (:beamer-font-theme "BEAMER_FONT_THEME" nil nil t)
   (:beamer-inner-theme "BEAMER_INNER_THEME" nil nil t)
   (:beamer-outer-theme "BEAMER_OUTER_THEME" nil nil t)
   (:headline-levels nil "H" org-e-beamer-frame-level))
  :translate-alist ((bold . org-e-beamer-bold)
		    (export-block . org-e-beamer-export-block)
		    (export-snippet . org-e-beamer-export-snippet)
		    (headline . org-e-beamer-headline)
		    (item . org-e-beamer-item)
		    (keyword . org-e-beamer-keyword)
		    (link . org-e-beamer-link)
		    (plain-list . org-e-beamer-plain-list)
		    (radio-target . org-e-beamer-radio-target)
		    (target . org-e-beamer-target)
		    (template . org-e-beamer-template)))



;;; Transcode Functions

;;;; Bold

(defun org-e-beamer-bold (bold contents info)
  "Transcode BLOCK object into Beamer code.
CONTENTS is the text being bold.  INFO is a plist used as
a communication channel."
  (format "\\alert%s{%s}"
	  (or (org-e-beamer--element-has-overlay-p bold) "")
	  contents))


;;;; Export Block

(defun org-e-beamer-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element into Beamer code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (member (org-element-property :type export-block) '("BEAMER" "LATEX"))
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Export Snippet

(defun org-e-beamer-export-snippet (export-snippet contents info)
  "Transcode an EXPORT-SNIPPET object into Beamer code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((backend (org-export-snippet-backend export-snippet))
	(value (org-element-property :value export-snippet)))
    ;; Only "e-latex" and "e-beamer" snippets are retained.
    (cond ((eq backend 'e-latex) value)
	  ;; Ignore "e-beamer" snippets specifying overlays.
	  ((and (eq backend 'e-beamer)
		(or (org-export-get-previous-element export-snippet info)
		    (not (string-match "\\`<.*>\\'" value))))
	   value))))


;;;; Headline
;;
;; The main function to translate an headline is
;; `org-e-beamer-headline'.
;;
;; Depending on the level at which an headline is considered as
;; a frame (given by `org-e-beamer--frame-level'), the headline is
;; either a section (`org-e-beamer--format-section'), a frame
;; (`org-e-beamer--format-frame') or a block
;; (`org-e-beamer--format-block').
;;
;; `org-e-beamer-headline' also takes care of special environments
;; like "ignoreheading", "note", "noteNH" and "appendix".

(defun org-e-beamer--frame-level (headline info)
  "Return frame level in subtree containing HEADLINE.
INFO is a plist used as a communication channel."
  (or
   ;; 1. Look for "frame" environment in parents, starting from the
   ;;    farthest.
   (catch 'exit
     (mapc (lambda (parent)
	     (when (equal (org-element-property :beamer-env parent) "frame")
	       (throw 'exit (org-export-get-relative-level parent info))))
	   (reverse (org-export-get-genealogy headline)))
     nil)
   ;; 2. Look for "frame" environment in HEADLINE.
   (and (equal (org-element-property :beamer-env headline) "frame")
	(org-export-get-relative-level headline info))
   ;; 3. Look for "frame" environment in sub-tree.
   (org-element-map
    headline 'headline
    (lambda (hl)
      (when (equal (org-element-property :beamer-env hl) "frame")
	(org-export-get-relative-level hl info)))
    info 'first-match)
   ;; 4. No "frame" environment in tree: use default value.
   (plist-get info :headline-levels)))

(defun org-e-beamer--format-section (headline contents info)
  "Format HEADLINE as a sectioning part.
CONTENTS holds the contents of the headline.  INFO is a plist
used as a communication channel."
  ;; Use `e-latex' back-end output, inserting overlay specifications
  ;; if possible.
  (let ((latex-headline
	 (funcall (cdr (assq 'headline org-e-latex-translate-alist))
		  headline contents info))
	(mode-specs (org-element-property :beamer-act headline)))
    (if (and mode-specs
	     (string-match "\\`\\\\\\(.*?\\)\\(?:\\*\\|\\[.*\\]\\)?{"
			   latex-headline))
	(replace-match (concat (match-string 1 latex-headline)
			       (format "<%s>" mode-specs))
		       nil nil latex-headline 1)
      latex-headline)))

(defun org-e-beamer--format-frame (headline contents info)
  "Format HEADLINE as a frame.
CONTENTS holds the contents of the headline.  INFO is a plist
used as a communication channel."
  (let ((fragilep
	 ;; FRAGILEP is non-nil when HEADLINE contains an element
	 ;; among `org-e-beamer-verbatim-elements'.
	 (org-element-map headline org-e-beamer-verbatim-elements 'identity
			  info 'first-match)))
    (concat "\\begin{frame}"
	    ;; Overlay specification, if any. If is surrounded by square
	    ;; brackets, consider it as a default specification.
	    (let ((action (org-element-property :beamer-act headline)))
	      (cond
	       ((not action) "")
	       ((string-match "\\`\\[.*\\]\\'" action )
		(org-e-beamer--normalize-argument action 'defaction))
	       (t (org-e-beamer--normalize-argument action 'action))))
	    ;; Options, if any.
	    (let ((options
		   ;; Collect options from default value and headline's
		   ;; properties.  Also add a label for links.
		   (append
		    (org-split-string org-e-beamer-frame-default-options
				      ",")
		    (let ((opt (org-element-property :beamer-opt headline)))
		      (and opt (org-split-string
				;; Remove square brackets if user
				;; provided them.
				(and (string-match "^\\[?\\(.*\\)\\]?$" opt)
				     (match-string 1 opt))
				",")))
		    (list
		     (format "label=sec-%s"
			     (mapconcat
			      'number-to-string
			      (org-export-get-headline-number headline info)
			      "-"))))))
	      ;; Change options list into a string.
	      (org-e-beamer--normalize-argument
	       (mapconcat
		'identity
		(if (or (not fragilep) (member "fragile" options)) options
		  (cons "fragile" options))
		",")
	       'option))
	    ;; Title.
	    (format "{%s}"
		    (org-export-data (org-element-property :title headline)
				     info))
	    "\n"
	    ;; The following workaround is required in fragile frames
	    ;; as Beamer will append "\par" to the beginning of the
	    ;; contents.  So we need to make sure the command is
	    ;; separated from the contents by at least one space.  If
	    ;; it isn't, it will create "\parfirst-word" command and
	    ;; remove the first word from the contents in the PDF
	    ;; output.
	    (if (not fragilep) contents
	      (replace-regexp-in-string "\\`\n*" "\\& " contents))
	    "\\end{frame}")))

(defun org-e-beamer--format-block (headline contents info)
  "Format HEADLINE as a block.
CONTENTS holds the contents of the headline.  INFO is a plist
used as a communication channel."
  (let* ((column-width (org-element-property :beamer-col headline))
	 ;; Environment defaults to "block" if none is specified and
	 ;; there is no column specification.  If there is a column
	 ;; specified but still no explicit environment, ENVIRONMENT
	 ;; is nil.
	 (environment (let ((env (org-element-property :beamer-env headline)))
			(cond
			 ;; "block" is the fallback environment.
			 ((and (not env) (not column-width)) "block")
			 ;; "column" only.
			 ((not env) nil)
			 ;; Use specified environment.
			 (t (downcase env)))))
	 (env-format (when environment
		       (assoc environment
			      (append org-e-beamer-environments-special
				      org-e-beamer-environments-extra
				      org-e-beamer-environments-default))))
	 (title (org-export-data (org-element-property :title headline) info))
	 ;; Start a columns environment when there is no previous
	 ;; headline or the previous headline do not have
	 ;; a BEAMER_column property.
	 (start-columns-p
	  (and column-width
	       (or (org-export-first-sibling-p headline info)
		   (not (org-element-property
			 :beamer-col
			 (org-export-get-previous-element headline info))))))
	 ;; Ends a columns environment when there is no next headline
	 ;; or the next headline do not have a BEAMER_column property.
	 (end-columns-p
	  (and column-width
	       (or (org-export-last-sibling-p headline info)
		   (not (org-element-property
			 :beamer-col
			 (org-export-get-next-element headline info)))))))
    (concat
     (when start-columns-p "\\begin{columns}\n")
     (when column-width
       (format "\\begin{column}%s{%s}\n"
	       ;; One can specify placement for column only when
	       ;; HEADLINE stands for a column on its own.
	       (if (not environment) ""
		 (let ((options (org-element-property :beamer-opt headline)))
		   (if (not options) ""
		     (org-e-beamer--normalize-argument options 'option))))
	       (format "%s\\textwidth" column-width)))
     ;; Block's opening string.
     (when env-format
       (concat
	(org-fill-template
	 (nth 2 env-format)
	 (nconc
	  ;; If BEAMER_act property has its value enclosed in square
	  ;; brackets, it is a default overlay specification and
	  ;; overlay specification is empty.  Otherwise, it is an
	  ;; overlay specification and the default one is nil.
	  (let ((action (org-element-property :beamer-act headline)))
	    (cond
	     ((not action) (list (cons "a" "") (cons "A" "")))
	     ((string-match "\\`\\[.*\\]\\'" action)
	      (list
	       (cons "A"
		     (org-e-beamer--normalize-argument action 'defaction))
	       (cons "a" "")))
	     (t
	      (list
	       (cons "a"
		     (org-e-beamer--normalize-argument action 'action))
	       (cons "A" "")))))
	  (list (cons "o"
		      (let ((options
			     (org-element-property :beamer-opt headline)))
			(if (not options) ""
			  (org-e-beamer--normalize-argument options 'option))))
		(cons "h" title)
		(cons "H" (if (equal title "") "" (format "{%s}" title)))
		(cons "U" (if (equal title "") "" (format "[%s]" title))))))
	"\n"))
     contents
     ;; Block's closing string.
     (when environment (concat (nth 3 env-format) "\n"))
     (when column-width "\\end{column}\n")
     (when end-columns-p "\\end{columns}"))))

(defun org-e-beamer-headline (headline contents info)
  "Transcode HEADLINE element into Beamer code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((level (org-export-get-relative-level headline info))
	  (frame-level (org-e-beamer--frame-level headline info))
	  (environment (let ((env (org-element-property :beamer-env headline)))
			 (if (stringp env) (downcase env) "block"))))
      (cond
       ;; Creation of an appendix is requested.
       ((equal environment "appendix")
	(concat "\\appendix"
		(org-element-property :beamer-act headline)
		"\n"
		(make-string (org-element-property :pre-blank headline) ?\n)
		contents))
       ((equal environment "ignoreheading")
	(concat (make-string (org-element-property :pre-blank headline) ?\n)
		contents))
       ;; HEADLINE is a note.
       ((member environment '("note" "noteNH"))
	(format "\\note{%s}"
		(concat (and (equal environment "note")
			     (concat
			      (org-export-data
			       (org-element-property :title headline) info)
			      "\n"))
			(org-trim contents))))
       ;; HEADLINE is a frame.
       ((or (equal environment "frame") (= level frame-level))
	(org-e-beamer--format-frame headline contents info))
       ;; Regular section, extracted from `org-e-latex-classes'.
       ((< level frame-level)
	(org-e-beamer--format-section headline contents info))
       ;; Otherwise, HEADLINE is a block.
       (t (org-e-beamer--format-block headline contents info))))))


;;;; Item

(defun org-e-beamer-item (item contents info)
  "Transcode an ITEM element into Beamer code.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((action (let ((first-element (car (org-element-contents item))))
		  (and (eq (org-element-type first-element) 'paragraph)
		       (org-e-beamer--element-has-overlay-p first-element))))
	(output (funcall (cdr (assq 'item org-e-latex-translate-alist))
			 item contents info)))
    (if (not action) output
      ;; If the item starts with a paragraph and that paragraph starts
      ;; with an export snippet specifying an overlay, insert it after
      ;; \item command.
      (replace-regexp-in-string "\\\\item" (concat "\\\\item" action) output))))


;;;; Keyword

(defun org-e-beamer-keyword (keyword contents info)
  "Transcode a KEYWORD element into Beamer code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    ;; Handle specifically BEAMER and TOC (headlines only) keywords.
    ;; Otherwise, fallback to `e-latex' back-end.
    (cond
     ((equal key "BEAMER") value)
     ((and (equal key "TOC") (string-match "\\<headlines\\>" value))
      (let ((depth (or (and (string-match "[0-9]+" value)
			    (string-to-number (match-string 0 value)))
		       (plist-get info :with-toc)))
	    (options (and (string-match "\\[.*?\\]" value)
			  (match-string 0 value))))
	(concat
	 "\\begin{frame}"
	 (when (wholenump depth) (format "\\setcounter{tocdepth}{%s}\n" depth))
	 "\\tableofcontents" options "\n"
	 "\\end{frame}")))
     (t (funcall (cdr (assq 'keyword org-e-latex-translate-alist))
		 keyword contents info)))))


;;;; Link

(defun org-e-beamer-link (link contents info)
  "Transcode a LINK object into Beamer code.
CONTENTS is the description part of the link.  INFO is a plist
used as a communication channel."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    ;; Use \hyperlink command for all internal links.
    (cond
     ((equal type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (format "\\hyperlink%s{%s}{%s}"
		  (or (org-e-beamer--element-has-overlay-p link) "")
		  (org-export-solidify-link-text path)
		  (org-export-data (org-element-contents destination) info)))))
     ((and (member type '("custom-id" "fuzzy" "id"))
	   (let ((destination (if (string= type "fuzzy")
				  (org-export-resolve-fuzzy-link link info)
				(org-export-resolve-id-link link info))))
	     (case (org-element-type destination)
	       (headline
		(let ((label
		       (format "sec-%s"
			       (mapconcat
				'number-to-string
				(org-export-get-headline-number
				 destination info)
				"-"))))
		  (if (and (plist-get info :section-numbers) (not contents))
		      (format "\\ref{%s}" label)
		    (format "\\hyperlink%s{%s}{%s}"
			    (or (org-e-beamer--element-has-overlay-p link) "")
			    label
			    contents))))
	       (target
		(let ((path (org-export-solidify-link-text path)))
		  (if (not contents) (format "\\ref{%s}" path)
		    (format "\\hyperlink%s{%s}{%s}"
			    (or (org-e-beamer--element-has-overlay-p link) "")
			    path
			    contents))))))))
     ;; Otherwise, use `e-latex' back-end.
     (t (funcall (cdr (assq 'link org-e-latex-translate-alist))
		 link contents info)))))


;;;; Plain List
;;
;; Plain lists support `:overlay' (for any type), `:template' (for
;; ordered lists only) and `:long-text' (for description lists only)
;; attributes.

(defun org-e-beamer-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element into Beamer code.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (attributes (org-export-read-attribute :attr_beamer plain-list))
	 (latex-type (cond ((eq type 'ordered) "enumerate")
			   ((eq type 'descriptive) "description")
			   (t "itemize"))))
    (org-e-latex--wrap-label
     plain-list
     (format "\\begin{%s}%s%s\n%s\\end{%s}"
	     latex-type
	     ;; Default overlay specification, if any.
	     (let ((overlay (plist-get attributes :overlay)))
	       (if (not overlay) ""
		 (org-e-beamer--normalize-argument overlay 'defaction)))
	     ;; Second optional argument depends on the list type.
	     (case type
	       (ordered
		(let ((template (plist-get attributes :template)))
		  (if (not template) ""
		    (org-e-beamer--normalize-argument template 'option))))
	       (descriptive
		(let ((long-text (plist-get attributes :long-text)))
		  (if (not long-text) ""
		    (org-e-beamer--normalize-argument long-text 'option))))
	       ;; There's no second argument for un-ordered lists.
	       (otherwise ""))
	     ;; Eventually insert contents and close environment.
	     contents
	     latex-type))))


;;;; Radio Target

(defun org-e-beamer-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object into Beamer code.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\hypertarget%s{%s}{%s}"
	  (or (org-e-beamer--element-has-overlay-p radio-target) "")
	  (org-export-solidify-link-text
	   (org-element-property :value radio-target))
	  text))


;;;; Target

(defun org-e-beamer-target (target contents info)
  "Transcode a TARGET object into Beamer code.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\hypertarget{%s}{}"
	  (org-export-solidify-link-text (org-element-property :value target))))


;;;; Template
;;
;; Template used is similar to the one used in `e-latex' back-end,
;; excepted for the table of contents and Beamer themes.

(defun org-e-beamer-template (contents info)
  "Return complete document string after Beamer conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info)))
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
	  (when document-class-string
	    (org-e-latex--guess-babel-language
	     (org-e-latex--guess-inputenc
	      (org-splice-latex-header
	       document-class-string
	       org-export-latex-default-packages-alist ; defined in org.el
	       org-export-latex-packages-alist nil ; defined in org.el
	       (plist-get info :latex-header-extra)))
	     info)))))
     ;; 3. Insert themes.
     (let ((format-theme
	    (function
	     (lambda (prop command)
	       (let ((theme (plist-get info prop)))
		 (when theme
		   (concat command
			   (if (not (string-match "\\[.*\\]" theme))
			       (format "{%s}\n" theme)
			     (format "%s{%s}\n"
				     (match-string 0 theme)
				     (org-trim
				      (replace-match "" nil nil theme)))))))))))
       (mapconcat (lambda (args) (apply format-theme args))
		  '((:beamer-theme "\\usetheme")
		    (:beamer-color-theme "\\usecolortheme")
		    (:beamer-font-theme "\\usefonttheme")
		    (:beamer-inner-theme "\\useinnertheme")
		    (:beamer-outer-theme "\\useoutertheme"))
		  ""))
     ;; 4. Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; 5. Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     (author (format "\\author{%s}\n" author))
	     (t "\\author{}\n")))
     ;; 6. Date.
     (format "\\date{%s}\n" (org-export-data (plist-get info :date) info))
     ;; 7. Title
     (format "\\title{%s}\n" title)
     ;; 8. Hyperref options.
     (format "\\hypersetup{\n  pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}}\n"
	     (or (plist-get info :keywords) "")
	     (or (plist-get info :description) "")
	     (if (not (plist-get info :with-creator)) ""
	       (plist-get info :creator)))
     ;; 9. Document start.
     "\\begin{document}\n\n"
     ;; 10. Title command.
     (org-element-normalize-string
      (cond ((string= "" title) nil)
	    ((not (stringp org-e-latex-title-command)) nil)
	    ((string-match "\\(?:[^%]\\|^\\)%s"
			   org-e-latex-title-command)
	     (format org-e-latex-title-command title))
	    (t org-e-latex-title-command)))
     ;; 11. Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat
	  (format "\\begin{frame}%s{%s}\n"
		  (org-e-beamer--normalize-argument
		   org-e-beamer-outline-frame-options 'option)
		  org-e-beamer-outline-frame-title)
	  (when (wholenump depth)
	    (format "\\setcounter{tocdepth}{%d}\n" depth))
	  "\\tableofcontents\n"
	  "\\end{frame}\n\n")))
     ;; 12. Document's body.
     contents
     ;; 13. Creator.
     (let ((creator-info (plist-get info :with-creator)))
       (cond
	((not creator-info) "")
	((eq creator-info 'comment)
	 (format "%% %s\n" (plist-get info :creator)))
	(t (concat (plist-get info :creator) "\n"))))
     ;; 14. Document end.
     "\\end{document}")))



;;; Minor Mode


(defvar org-e-beamer-mode-map (make-sparse-keymap)
  "The keymap for `org-e-beamer-mode'.")
(define-key org-e-beamer-mode-map "\C-c\C-b" 'org-e-beamer-select-environment)

;;;###autoload
(define-minor-mode org-e-beamer-mode
  "Support for editing Beamer oriented Org mode files."
  nil " Bm" 'org-e-beamer-mode-map)

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'org-mode
   '((":\\(B_[a-z]+\\|BMCOL\\):" 1 'org-e-beamer-tag prepend))
   'prepend))

(defface org-e-beamer-tag '((t (:box (:line-width 1 :color grey40))))
  "The special face for beamer tags."
  :group 'org-export-e-beamer)

(defun org-e-beamer-property-changed (property value)
  "Track the BEAMER_env property with tags.
PROPERTY is the name of the modified property.  VALUE is its new
value."
  (cond
   ((equal property "BEAMER_env")
    (save-excursion
      (org-back-to-heading t)
      (let ((tags (org-get-tags)))
	(setq tags (delq nil (mapcar (lambda (x)
				       (if (string-match "^B_" x) nil x))
				     tags)))
	(org-set-tags-to tags))
      (when (org-string-nw-p value) (org-toggle-tag (concat "B_" value) 'on))))
   ((equal property "BEAMER_col")
    (org-toggle-tag "BMCOL" (if (org-string-nw-p value) 'on 'off)))))

(add-hook 'org-property-changed-functions 'org-e-beamer-property-changed)

(defun org-e-beamer-allowed-property-values (property)
  "Supply allowed values for PROPERTY."
  (cond
   ((and (equal property "BEAMER_env")
	 (not (org-entry-get nil (concat property "_ALL") 'inherit)))
    ;; If no allowed values for BEAMER_env have been defined,
    ;; supply all defined environments
    (mapcar 'car (append org-e-beamer-environments-special
			 org-e-beamer-environments-extra
			 org-e-beamer-environments-default)))
   ((and (equal property "BEAMER_col")
	 (not (org-entry-get nil (concat property "_ALL") 'inherit)))
    ;; If no allowed values for BEAMER_col have been defined,
    ;; supply some
    (org-split-string org-e-beamer-column-widths " "))))

(add-hook 'org-property-allowed-value-functions
	  'org-e-beamer-allowed-property-values)



;;; Commands

;;;###autoload
(defun org-e-beamer-export-as-latex
  (&optional subtreep visible-only body-only ext-plist)
  "Export current buffer as a Beamer buffer.

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

Export is done in a buffer named \"*Org E-BEAMER Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (let ((outbuf (org-export-to-buffer
		 'e-beamer "*Org E-BEAMER Export*"
		 subtreep visible-only body-only ext-plist)))
    (with-current-buffer outbuf (LaTeX-mode))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf))))

;;;###autoload
(defun org-e-beamer-export-to-latex
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer as a Beamer presentation (tex).

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
  (let ((outfile (org-export-output-file-name ".tex" subtreep pub-dir)))
    (org-export-to-file
     'e-beamer outfile subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-e-beamer-export-to-pdf
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer as a Beamer presentation (PDF).

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
  (org-e-latex-compile
   (org-e-beamer-export-to-latex
    subtreep visible-only body-only ext-plist pub-dir)))

;;;###autoload
(defun org-e-beamer-select-environment ()
  "Select the environment to be used by beamer for this entry.
While this uses (for convenience) a tag selection interface, the
result of this command will be that the BEAMER_env *property* of
the entry is set.

In addition to this, the command will also set a tag as a visual
aid, but the tag does not have any semantic meaning."
  (interactive)
  ;; Make sure `org-e-beamer-environments-special' has a higher
  ;; priority than `org-e-beamer-environments-extra'.
  (let* ((envs (append org-e-beamer-environments-special
		       org-e-beamer-environments-extra
		       org-e-beamer-environments-default))
	 (org-tag-alist
	  (append '((:startgroup))
		  (mapcar (lambda (e) (cons (concat "B_" (car e))
				       (string-to-char (nth 1 e))))
			  envs)
		  '((:endgroup))
		  '(("BMCOL" . ?|))))
	 (org-fast-tag-selection-single-key t))
    (org-set-tags)
    (let ((tags (or (ignore-errors (org-get-tags-string)) "")))
      (cond
       ((eq org-last-tag-selection-key ?|)
	(if (string-match ":BMCOL:" tags)
	    (org-set-property "BEAMER_col" (read-string "Column width: "))
	  (org-delete-property "BEAMER_col")))
       ((string-match (concat ":B_\\("
			      (mapconcat 'car envs "\\|")
			      "\\):")
		      tags)
	(org-entry-put nil "BEAMER_env" (match-string 1 tags)))
       (t (org-entry-delete nil "BEAMER_env"))))))

;;;###autoload
(defun org-e-beamer-insert-options-template (&optional kind)
  "Insert a settings template, to make sure users do this right."
  (interactive (progn
		 (message "Current [s]ubtree or [g]lobal?")
		 (if (eq (read-char-exclusive) ?g) (list 'global)
		   (list 'subtree))))
  (if (eq kind 'subtree)
      (progn
	(org-back-to-heading t)
	(org-reveal)
	(org-entry-put nil "EXPORT_LaTeX_CLASS" "beamer")
	(org-entry-put nil "EXPORT_LaTeX_CLASS_OPTIONS" "[presentation]")
	(org-entry-put nil "EXPORT_FILE_NAME" "presentation.pdf")
	(when org-e-beamer-column-view-format
	  (org-entry-put nil "COLUMNS" org-e-beamer-column-view-format))
	(org-entry-put nil "BEAMER_col_ALL" org-e-beamer-column-widths))
    (insert "#+LaTeX_CLASS: beamer\n")
    (insert "#+LaTeX_CLASS_OPTIONS: [presentation]\n")
    (when org-e-beamer-theme
      (insert "#+BEAMER_THEME: " org-e-beamer-theme "\n"))
    (when org-e-beamer-column-view-format
      (insert "#+COLUMNS: " org-e-beamer-column-view-format "\n"))
    (insert "#+PROPERTY: BEAMER_col_ALL " org-e-beamer-column-widths "\n")))


(provide 'org-e-beamer)
;;; org-e-beamer.el ends here
