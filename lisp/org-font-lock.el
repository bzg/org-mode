;;; org-font-lock.el --- Org font-lock support                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library implements Org mode fontification.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-keys)
(require 'ol)
(require 'org-fold)
(require 'org-faces)
(require 'org-element-timestamp)
(require 'org-table)
(require 'org-footnote)
(require 'org-src)

(defvar org-ellipsis)
(defvar org-table-may-need-update)

(defvar org-todo-regexp)
(defvar org-not-done-keywords)
(defvar org-done-keywords)
(defvar org-group-tags)
(defvar org-tag-groups-alist)

(defvar org-drawer-regexp)
(defvar org-match-substring-with-braces-regexp)
(defvar org-archive-tag)
(defvar org-closed-string)
(defvar org-scheduled-string)
(defvar org-deadline-string)
(defvar org-heading-keyword-regexp-format)

(declare-function outline-next-heading "outline")
(declare-function org-get-property-block "org")
(declare-function org-fix-decoded-time "org")

(defgroup org-appearance nil
  "Settings for Org mode appearance."
  :tag "Org Appearance"
  :group 'org)

(defcustom org-level-color-stars-only nil
  "Non-nil means fontify only the stars in each headline.
When nil, the entire headline is fontified.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hide-leading-stars nil
  "Non-nil means hide the first N-1 stars in a headline.
This works by using the face `org-hide' for these stars.  This
face is white for a light background, and black for a dark
background.  You may have to customize the face `org-hide' to
make this work.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: hidestars
   #+STARTUP: showstars"
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hidden-keywords nil
  "List of symbols corresponding to keywords to be hidden in the Org buffer.
For example, a value (title) for this list makes the document's title
appear in the buffer without the initial \"#+TITLE:\" part."
  :group 'org-appearance
  :package-version '(Org . "9.5")
  :type '(set (const :tag "#+AUTHOR" author)
	      (const :tag "#+DATE" date)
	      (const :tag "#+EMAIL" email)
	      (const :tag "#+SUBTITLE" subtitle)
	      (const :tag "#+TITLE" title)))

(defcustom org-fontify-todo-headline nil
  "Non-nil means change the face of a headline if it is marked as TODO.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-todo' as an additional indication."
  :group 'org-appearance
  :package-version '(Org . "9.4")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-fontify-done-headline t
  "Non-nil means change the face of a headline if it is marked DONE.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-done' as an additional indication."
  :group 'org-appearance
  :package-version '(Org . "9.4")
  :type 'boolean)

(defcustom org-fontify-emphasized-text t
  "Non-nil means fontify *bold*, /italic/ and _underlined_ text.
Changing this variable requires a restart of Emacs to take effect."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-use-sub-superscripts t
  "Non-nil means interpret \"_\" and \"^\" for display.

If you want to control how Org exports those characters, see
`org-export-with-sub-superscripts'.

When this option is turned on, you can use TeX-like syntax for
sub- and superscripts within the buffer.  Several characters after
\"_\" or \"^\" will be considered as a single item - so grouping
with {} is normally not needed.  For example, the following things
will be parsed as single sub- or superscripts:

 10^24   or   10^tau     several digits will be considered 1 item.
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x^(2 - i)               expression inside round braces, including the
                         braces is read as a sub/superscript.
 x_{i^2}                 curly braces do grouping; braces are not
                         considered a part of the sub/superscript.

Still, ambiguity is possible.  So when in doubt, use {} to enclose
the sub/superscript.  If you set this variable to the symbol `{}',
the curly braces are *required* in order to trigger interpretations as
sub/superscript.  This can be helpful in documents that need \"_\"
frequently in plain text.

Setting this variable does not change Org mode markup.  Org mode will
still parse the matching text as sub/superscript internally.  It is
only the visual appearance that will be changed."
  :group 'org-startup
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Always interpret" t)
	  (const :tag "Only with curly braces" {})
	  (const :tag "Never interpret" nil)))

(defcustom org-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
org-level-* faces."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-fontify-whole-block-delimiter-line t
  "Non-nil means fontify the whole line for begin/end lines of blocks.
This is useful when setting a background color for the
org-block-begin-line and org-block-end-line faces."
  :group 'org-appearance
  :type 'boolean)

(defvaralias 'org-activate-links 'org-highlight-links)
(defcustom org-highlight-links '(bracket angle plain radio tag date footnote)
  "Types of links that should be highlighted in Org files.

This is a list of symbols, each one of them leading to the
highlighting of a certain link type.

You can still open links that are not highlighted.

In principle, it does not hurt to turn on highlighting for all
link types.  There may be a small gain when turning off unused
link types.  The types are:

bracket   The recommended [[link][description]] or [[link]] links with hiding.
angle     Links in angular brackets that may contain whitespace like
          <bbdb:Carsten Dominik>.
plain     Plain links in normal text, no whitespace, like https://gnu.org.
radio     Text that is matched by a radio target, see manual for details.
tag       Tag settings in a headline (link to tag search).
date      Time stamps (link to calendar).
footnote  Footnote labels.

If you set this variable during an Emacs session, use `org-mode-restart'
in the Org buffer so that the change takes effect."
  :group 'org-appearance
  :type '(set :greedy t
	      (const :tag "Double bracket links" bracket)
	      (const :tag "Angular bracket links" angle)
	      (const :tag "Plain text links" plain)
	      (const :tag "Radio target matches" radio)
	      (const :tag "Tags" tag)
	      (const :tag "Timestamps" date)
	      (const :tag "Footnotes" footnote)))

(defcustom org-highlight-latex-and-related nil
  "Non-nil means highlight LaTeX related syntax in the buffer.
When non-nil, the value should be a list containing any of the
following symbols:
  `native'   Highlight LaTeX snippets and environments natively.
  `latex'    Highlight LaTeX snippets and environments.
  `script'   Highlight subscript and superscript.
  `entities' Highlight entities."
  :group 'org-appearance
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "No highlighting" nil)
	  (set :greedy t :tag "Highlight"
	       (const :tag "LaTeX snippets and environments (native)" native)
	       (const :tag "LaTeX snippets and environments" latex)
	       (const :tag "Subscript and superscript" script)
	       (const :tag "Entities" entities))))

(defcustom org-highlight-latex-matchers '("begin" "$1" "$" "$$" "\\(" "\\[")
  "A list indicating which matchers should be used to
find LaTeX fragments to highlight.  Valid members of this list are:

\\\"begin\\\" find environments
\\\"$1\\\"    find single characters surrounded by $.$
\\\"$\\\"     find math expressions surrounded by $...$
\\\"$$\\\"    find math expressions surrounded by $$...$$
\"\\(\"      find math expressions surrounded by \\(...\\)
\"\\=\\[\"      find math expressions surrounded by \\=\\[...\\]"
  :group 'org-appearance
  :package-version '(Org . "10.0")
  :type '(list string))

(defcustom org-hide-emphasis-markers nil
  "Non-nil means font-lock should hide the emphasis marker characters."
  :group 'org-appearance
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hide-macro-markers nil
  "Non-nil means font-lock should hide the brackets marking macro calls."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-pretty-entities nil
  "Non-nil means show entities as UTF8 characters.
When nil, the \\name form remains in the buffer."
  :group 'org-appearance
  :version "24.1"
  :type 'boolean)

(defcustom org-pretty-entities-include-sub-superscripts t
  "Non-nil means pretty entity display includes formatting sub/superscripts."
  :group 'org-appearance
  :version "24.1"
  :type 'boolean)

(defvar org-emph-re nil
  "Regular expression for matching emphasis.
After a match, the match groups contain these elements:
0  The match of the full regular expression, including the characters
   before and after the proper match
1  The character before the proper match, or empty at beginning of line
2  The proper match, including the leading and trailing markers
3  The leading marker like * or /, indicating the type of highlighting
4  The text between the emphasis markers, not including the markers
5  The character after the match, empty at the end of a line")

(defvar org-verbatim-re nil
  "Regular expression for matching verbatim text.")

(defvar org-emphasis-regexp-components) ; defined just below
(defvar org-emphasis-alist) ; defined just below
(defun org-set-emph-re (var val)
  "Set variable and compute the emphasis regular expression."
  (set-default-toplevel-value var val)
  (when (and (boundp 'org-emphasis-alist)
	     (boundp 'org-emphasis-regexp-components)
	     org-emphasis-alist org-emphasis-regexp-components)
    (pcase-let*
	((`(,pre ,post ,border ,body ,nl) org-emphasis-regexp-components)
	 (body (if (<= nl 0) body
		 (format "%s*?\\(?:\n%s*?\\)\\{0,%d\\}" body body nl)))
	 (template
	  (format (concat "\\([%s]\\|^\\)" ;before markers
			  "\\(\\([%%s]\\)\\([^%s]\\|[^%s]%s[^%s]\\)\\3\\)"
			  "\\([%s]\\|$\\)") ;after markers
		  pre border border body border post)))
      (setq org-emph-re (format template "*/_+"))
      (setq org-verbatim-re (format template "=~")))))

;; This used to be a defcustom (Org <8.0) but allowing the users to
;; set this option proved cumbersome.  See this message/thread:
;; https://orgmode.org/list/B72CDC2B-72F6-43A8-AC70-E6E6295766EC@gmail.com
(defvar org-emphasis-regexp-components
  '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
  "Components used to build the regular expression for FONTIFYING emphasis.
WARNING: This variable only affects visual fontification, but does not
change Org markup.  For example, it does not affect how emphasis markup
is interpreted on export.

This is a list with five entries.  Terminology:  In an emphasis string
like \" *strong word* \", we call the initial space PREMATCH, the final
space POSTMATCH, the stars MARKERS, \"s\" and \"d\" are BORDER characters
and \"trong wor\" is the body.  The different components in this variable
specify what is allowed/forbidden in each part:

pre          Chars allowed as prematch.  Beginning of line will be allowed too.
post         Chars allowed as postmatch.  End of line will be allowed too.
border       The chars *forbidden* as border characters.
body-regexp  A regexp like \".\" to match a body character.  Don't use
             non-shy groups here, and don't allow newline here.
newline      The maximum number of newlines allowed in an emphasis exp.

You need to reload Org or to restart Emacs after setting this.")

(defcustom org-emphasis-alist
  '(("*" bold)
    ("/" italic)
    ("_" underline)
    ("=" org-verbatim verbatim)
    ("~" org-code verbatim)
    ("+" (:strike-through t)))
  "Alist of characters and faces to emphasize text.
Text starting and ending with a special character will be emphasized,
for example *bold*, _underlined_ and /italic/.  This variable sets the
face to be used by font-lock for highlighting in Org buffers.
Marker characters must be one of */_=~+.

You need to reload Org or to restart Emacs after customizing this."
  :group 'org-appearance
  :set 'org-set-emph-re
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(repeat
	  (list
           (choice
	    (const :tag "Bold" "*")
            (const :tag "Italic" "/")
            (const :tag "Underline" "_")
            (const :tag "Verbatim" "=")
            (const :tag "Code" "~")
            (const :tag "Strike through" "+"))
	   (choice
	    (face :tag "Font-lock-face")
	    (plist :tag "Face property list"))
	   (option (const verbatim)))))

(defvar org-protecting-blocks '("src" "example" "export")
  "Blocks that contain text that is quoted, i.e. not processed as Org syntax.
This is needed for font-lock setup.")

(defcustom org-custom-properties nil
  "List of properties (as strings) with a special meaning.
The default use of these custom properties is to let the user
hide them with `org-toggle-custom-properties-visibility'."
  :group 'org-properties
  :group 'org-appearance
  :version "24.3"
  :type '(repeat (string :tag "Property Name")))

(defvar org-emph-face nil)

(defconst org-nonsticky-props
  '(mouse-face highlight keymap invisible intangible help-echo org-linked-text htmlize-link))

(defsubst org-rear-nonsticky-at (pos)
  (add-text-properties (1- pos) pos (list 'rear-nonsticky org-nonsticky-props)))

(defvar org-hidden-text-functions nil
  "Abnormal hook called when hiding certain text during fontification.

Each function on the hook should take five arguments:

  TYPE: the type of text being hidden
  BEG, END: the starting and ending buffer positions of the text
  VBEG, VEND: the start and end of the visible portion of the text

Currently supported types (symbols) are:

  `emphasis': emphasized text with hidden markers, with non-nil
              `org-hide-emphasis-markers'.

  `link': bracket links, with non-nil `org-link-descriptive'.

  `raise': sub/superscripts.")

(defun org-do-emphasis-faces (limit)
  "Run through the buffer and emphasize strings."
  (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
			  (car org-emphasis-regexp-components))))
    (catch :exit
      (while (re-search-forward quick-re limit t)
	(let* ((marker (match-string 2))
	       (verbatim? (member marker '("~" "="))))
	  (when (save-excursion
		  (goto-char (match-beginning 0))
		  (and
		   ;; Do not match table hlines.
		   (not (and (equal marker "+")
			   (org-match-line
			    "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
		   ;; Do not match headline stars.  Do not consider
		   ;; stars of a headline as closing marker for bold
		   ;; markup either.
		   (not (and (equal marker "*")
			   (save-excursion
			     (forward-char)
			     (skip-chars-backward "*")
			     (looking-at-p org-outline-regexp-bol))))
		   ;; Match full emphasis markup regexp.
		   (looking-at (if verbatim? org-verbatim-re org-emph-re))
		   ;; Do not span over paragraph boundaries.
		   (not (string-match-p org-element-paragraph-separate
				      (match-string 2)))
		   ;; Do not span over cells in table rows.
		   (not (and (save-match-data (org-match-line "[ \t]*|"))
			   (string-match-p "|" (match-string 4))))))
	    (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
			(m (if org-hide-emphasis-markers 4 2)))
	      (font-lock-prepend-text-property
	       (match-beginning m) (match-end m) 'face face)
	      (when verbatim?
		(org-remove-flyspell-overlays-in
		 (match-beginning 0) (match-end 0))
		(remove-text-properties (match-beginning 2) (match-end 2)
					'(display t invisible t intangible t)))
	      (add-text-properties (match-beginning 2) (match-end 2)
				   '(font-lock-multiline t org-emphasis t))
	      (when (and org-hide-emphasis-markers
			 (not (org-at-comment-p)))
		(add-text-properties (match-end 4) (match-beginning 5)
				     '(invisible org-emphasis))
                ;; https://orgmode.org/list/8b691a7f-6b62-d573-e5a8-80fac3dc9bc6@vodafonemail.de
                (org-rear-nonsticky-at (match-beginning 5))
		(add-text-properties (match-beginning 3) (match-end 3)
				     '(invisible org-emphasis))
                ;; FIXME: This would break current behavior with point
                ;; being adjusted before hidden emphasis marker when
                ;; using M-b.  A proper fix would require custom
                ;; syntax function that will mark emphasis markers as
                ;; word constituents where appropriate.
                ;; https://orgmode.org/list/87edl41jf0.fsf@localhost
                ;; (org-rear-nonsticky-at (match-end 3))
                (run-hook-with-args 'org-hidden-text-functions 'emphasis
                                    (match-beginning 2) (match-end 2)
                                    (match-beginning 4) (match-end 4)))
	      (throw :exit t))))))))

(defun org-activate-links (limit)
  "Add link properties to links.
This includes angle, plain, and bracket links."
  (catch :exit
    (while (re-search-forward org-link-any-re limit t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (visible-start (or (match-beginning 3) (match-beginning 2)))
	     (visible-end (or (match-end 3) (match-end 2)))
	     (style (cond ((eq ?< (char-after start)) 'angle)
			  ((eq ?\[ (char-after (1+ start))) 'bracket)
			  (t 'plain))))
	(if (and (memq style org-highlight-links)
		 ;; Do not span over paragraph boundaries.
		 (not (string-match-p org-element-paragraph-separate
				    (match-string 0)))
		 ;; Do not confuse plain links with tags.
		 (not (and (eq style 'plain)
			 (let ((face (get-text-property
				      (max (1- start) (point-min)) 'face)))
			   (if (consp face) (memq 'org-tag face)
			     (eq 'org-tag face))))))
	    (let* ((link-object (save-excursion
				  (goto-char start)
				  (save-match-data (org-element-link-parser))))
		   (link (org-element-property :raw-link link-object))
		   (type (org-element-property :type link-object))
		   (path (org-element-property :path link-object))
                   (face-property (pcase (org-link-get-parameter type :face)
				    ((and (pred functionp) face) (funcall face path))
				    ((and (pred facep) face) face)
				    ((and (pred consp) face) face) ;anonymous
				    (_ 'org-link)))
		   (properties		;for link's visible part
		    (list 'mouse-face (or (org-link-get-parameter type :mouse-face)
					  'highlight)
			  'keymap (or (org-link-get-parameter type :keymap)
				      org-mouse-map)
			  'help-echo (pcase (org-link-get-parameter type :help-echo)
				       ((and (pred stringp) echo) echo)
				       ((and (pred functionp) echo) echo)
				       (_ (concat "LINK: " link)))
			  'htmlize-link (pcase (org-link-get-parameter type
								       :htmlize-link)
					  ((and (pred functionp) f) (funcall f))
					  (_ `(:uri ,link)))
			  'font-lock-multiline t)))
	      (org-remove-flyspell-overlays-in start end)
	      (org-rear-nonsticky-at end)
	      (if (not (eq 'bracket style))
		  (progn
                    (add-face-text-property start end face-property)
		    (add-text-properties start end properties))
	        ;; Handle invisible parts in bracket links.
	        (remove-text-properties start end '(invisible nil))
	        (let ((hidden
                       (if org-link-descriptive
		           (append `(invisible
			             ,(or (org-link-get-parameter type :display)
				          'org-link))
			           properties)
                         properties)))
		  (add-text-properties start visible-start hidden)
                  (add-face-text-property start end face-property)
		  (add-text-properties visible-start visible-end properties)
		  (add-text-properties visible-end end hidden)
		  (org-rear-nonsticky-at visible-start)
		  (org-rear-nonsticky-at visible-end))
                (when org-link-descriptive
                  (run-hook-with-args 'org-hidden-text-functions 'link
                                      start end visible-start visible-end)))
	      (let ((f (org-link-get-parameter type :activate-func)))
	        (when (functionp f)
		  (funcall f start end path (eq style 'bracket))))
	      (throw :exit t))		;signal success
          ;; Not a real link, move forward one char and repeat the search.
          (goto-char (1+ (match-beginning 0))))))
    nil))

(defun org-activate-code (limit)
  (when (re-search-forward "^[ \t]*\\(:\\(?: .*\\|$\\)\n?\\)" limit t)
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    (remove-text-properties (match-beginning 0) (match-end 0)
			    '(display t invisible t intangible t))
    t))

(defcustom org-src-fontify-natively t
  "When non-nil, fontify code in code blocks.
See also the `org-block' face."
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "8.3")
  :group 'org-appearance
  :group 'org-babel)

(defun org-fontify-meta-lines-and-blocks (limit)
  (condition-case nil
      (org-fontify-meta-lines-and-blocks-1 limit)
    (error (message "Org mode fontification error in %S at %d"
		    (current-buffer)
		    (line-number-at-pos)))))

(defun org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks."
  (let ((case-fold-search t))
    (when (re-search-forward
	   (rx bol (group (zero-or-more (any " \t")) "#"
			  (group (group (or (seq "+" (one-or-more (any "a-zA-Z")) (optional ":"))
					    (any " \t")
					    eol))
				 (optional (group "_" (group (one-or-more (any "a-zA-Z"))))))
			  (zero-or-more (any " \t"))
			  (group (group (zero-or-more (not (any " \t\n"))))
				 (zero-or-more (any " \t"))
				 (group (zero-or-more nonl)))))
	   limit t)
      (let ((beg (match-beginning 0))
	    (end-of-beginline (match-end 0))
	    (lang (match-string 7)) ; The language, if it is a source block.
	    (bol-after-beginline (line-beginning-position 2))
	    (dc1 (downcase (match-string 2)))
	    (dc3 (downcase (match-string 3)))
	    (whole-blockline org-fontify-whole-block-delimiter-line)
	    beg-of-endline end-of-endline nl-before-endline quoting block-type)
	(cond
	 ((and (match-end 4) (equal dc3 "+begin"))
	  ;; Truly a block
	  (setq block-type (downcase (match-string 5))
		;; Src, example, export, maybe more.
		quoting (member block-type org-protecting-blocks))
	  (when (re-search-forward
		 (rx-to-string `(group bol (or (seq (one-or-more "*") space)
					       (seq (zero-or-more (any " \t"))
						    "#+end"
						    ,(match-string 4)
						    word-end
						    (zero-or-more nonl)))))
		 ;; We look further than LIMIT on purpose.
		 nil t)
	    ;; We do have a matching #+end line.
	    (setq beg-of-endline (match-beginning 0)
		  end-of-endline (match-end 0)
		  nl-before-endline (1- (match-beginning 0)))
	    (when quoting
	      (org-remove-flyspell-overlays-in bol-after-beginline nl-before-endline)
	      (remove-text-properties beg end-of-endline
				      '(display t invisible t intangible t)))
	    (add-text-properties
	     beg end-of-endline '(font-lock-fontified t font-lock-multiline t))
	    (org-remove-flyspell-overlays-in beg bol-after-beginline)
	    (org-remove-flyspell-overlays-in nl-before-endline end-of-endline)
            (cond
	     ((and org-src-fontify-natively
                   ;; Skip fontification of empty source-blocks
                   (< bol-after-beginline beg-of-endline)
                   ;; Technically, according to the
                   ;; `org-src-fontify-natively' docstring, we should
                   ;; only fontify src blocks.  However, it is common
                   ;; to use undocumented fontification of export and
                   ;; example blocks. (The latter which do not support a
                   ;; language specifier.) Keep this undocumented feature
                   ;; for user convenience.
                   (member block-type '("src" "export" "example")))
	      (save-match-data
                (org-src-font-lock-fontify-block (or lang "") bol-after-beginline beg-of-endline))
	      (add-text-properties bol-after-beginline beg-of-endline '(src-block t)))
	     (quoting
	      (add-text-properties
	       bol-after-beginline beg-of-endline
	       (list 'face
		     (list :inherit
			   (let ((face-name
				  (intern (format "org-block-%s" lang))))
			     (append (and (facep face-name) (list face-name))
				     '(org-block)))))))
	     ((not org-fontify-quote-and-verse-blocks))
	     ((string= block-type "quote")
	      (add-face-text-property
	       bol-after-beginline beg-of-endline 'org-quote t))
	     ((string= block-type "verse")
	      (add-face-text-property
	       bol-after-beginline beg-of-endline 'org-verse t)))
	    ;; Fontify the #+begin and #+end lines of the blocks
	    (add-text-properties
	     beg (if whole-blockline bol-after-beginline end-of-beginline)
	     '(face org-block-begin-line))
	    (unless (eq (char-after beg-of-endline) ?*)
	      (add-text-properties
	       beg-of-endline
	       (if whole-blockline
		   (let ((beg-of-next-line (1+ end-of-endline)))
		     (min (point-max) beg-of-next-line))
		 (min (point-max) end-of-endline))
	       '(face org-block-end-line)))
	    t))
	 ((member dc1 '("+title:" "+subtitle:" "+author:" "+email:" "+date:"))
	  (org-remove-flyspell-overlays-in
	   (match-beginning 0)
	   (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
	  (add-text-properties
	   beg (match-end 3)
	   (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
	       '(font-lock-fontified t invisible t)
	     '(font-lock-fontified t face org-document-info-keyword)))
	  (add-text-properties
	   (match-beginning 6) (min (point-max) (1+ (match-end 6)))
	   (if (string-equal dc1 "+title:")
	       '(font-lock-fontified t face org-document-title)
	     '(font-lock-fontified t face org-document-info))))
	 ((string-prefix-p "+caption" dc1)
	  (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  ;; Handle short captions
	  (save-excursion
	    (forward-line 0)
	    (looking-at (rx (group (zero-or-more (any " \t"))
				   "#+caption"
				   (optional "[" (zero-or-more nonl) "]")
				   ":")
			    (zero-or-more (any " \t")))))
	  (add-text-properties (line-beginning-position) (match-end 1)
			       '(font-lock-fontified t face org-meta-line))
	  (add-text-properties (match-end 0) (line-end-position)
			       '(font-lock-fontified t face org-block))
	  t)
	 ((member dc3 '(" " ""))
	  ;; Just a comment, the plus was not there
	  (org-remove-flyspell-overlays-in beg (match-end 0))
	  (add-text-properties
	   beg (match-end 0)
	   '(font-lock-fontified t face font-lock-comment-face)))
	 (t ;; Just any other in-buffer setting, but not indented
	  (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  (add-text-properties beg (match-end 0)
			       '(font-lock-fontified t face org-meta-line))
	  t))))))

(defun org-fontify-drawers (limit)
  "Fontify drawers."
  (when (re-search-forward org-drawer-regexp limit t)
    (add-text-properties (1- (match-beginning 1)) (1+ (match-end 1))
			 '(font-lock-fontified t face org-drawer))
    (org-remove-flyspell-overlays-in
     (line-beginning-position) (line-beginning-position 2))
    t))

(defun org-fontify-macros (limit)
  "Fontify macros."
  (when (re-search-forward "{{{\\([a-zA-Z][-a-zA-Z0-9_]*\\)" limit t)
    (let ((begin (match-beginning 0))
	  (opening-end (match-beginning 1)))
      (when (and (re-search-forward "\n[ \t]*\n\\|\\(}}}\\)" limit t)
		 (match-string 1))
	(let ((end (match-end 1))
	      (closing-start (match-beginning 1)))
          (add-face-text-property begin end 'org-macro)
	  (add-text-properties
	   begin end
	   '(font-lock-multiline t font-lock-fontified t))
	  (org-remove-flyspell-overlays-in begin end)
	  (when org-hide-macro-markers
	    (add-text-properties begin opening-end '(invisible t))
	    (add-text-properties closing-start end '(invisible t)))
	  t)))))

(defun org-fontify-extend-region (beg end _old-len)
  (let ((end (if (progn (goto-char end) (looking-at-p "^[*#]"))
                 (min (point-max) (1+ end))
               ;; See `font-lock-extend-jit-lock-region-after-change' and bug#68849.
               (min (point-max) (1+ end))))
        (begin-re "\\(\\\\\\[\\|\\(#\\+begin_\\|\\\\begin{\\)\\S-+\\)")
	(end-re "\\(\\\\\\]\\|\\(#\\+end_\\|\\\\end{\\)\\S-+\\)")
	(extend
         (lambda (r1 r2 dir)
	   (let ((re (replace-regexp-in-string
                      "\\(begin\\|end\\)" r1
		      (replace-regexp-in-string
                       "[][]" r2
		       (match-string-no-properties 0)))))
	     (re-search-forward (regexp-quote re) nil t dir)))))
    (goto-char beg)
    (back-to-indentation)
    (save-match-data
      (cond ((looking-at end-re)
	     (cons (or (funcall extend "begin" "[" -1) beg) end))
	    ((looking-at begin-re)
	     (cons beg (or (funcall extend "end" "]" 1) end)))
	    (t (cons beg end))))))

(defun org-activate-footnote-links (limit)
  "Add text properties for footnotes."
  (let ((fn (org-footnote-next-reference-or-definition limit)))
    (when fn
      (let* ((beg (nth 1 fn))
	     (end (nth 2 fn))
	     (label (car fn))
	     (referencep (/= (line-beginning-position) beg)))
	(when (and referencep (nth 3 fn))
	  (save-excursion
	    (goto-char beg)
	    (search-forward (or label "fn:"))
	    (org-remove-flyspell-overlays-in beg (match-end 0))))
        (when (null referencep) ; No flyspell inside [fn:X] definitions.
          (org-remove-flyspell-overlays-in beg end))
        (add-face-text-property beg end 'org-footnote)
	(add-text-properties beg end
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map
				   'help-echo
				   (if referencep "Footnote reference"
				     "Footnote definition")
				   'font-lock-fontified t
				   'font-lock-multiline t))))))

(defalias 'org-toggle-time-stamp-overlays #'org-toggle-timestamp-overlays)
(defun org-toggle-timestamp-overlays ()
  "Toggle the use of custom time stamp formats."
  (interactive nil org-mode)
  (setq org-display-custom-times (not org-display-custom-times))
  (unless org-display-custom-times
    (let ((p (point-min)) (bmp (buffer-modified-p)))
      (while (setq p (next-single-property-change p 'org-custom-date))
	(when (get-text-property p 'org-custom-date)
	  (remove-text-properties
	   p (setq p (next-single-property-change p 'org-custom-date))
	   '(display t org-custom-date t))))
      (set-buffer-modified-p bmp)))
  (org-restart-font-lock)
  (setq org-table-may-need-update t)
  (if org-display-custom-times
      (message "Time stamps are overlaid with custom format")
    (message "Time stamp overlays removed")))

(defun org-display-custom-time (beg end)
  "Overlay modified time stamp format over timestamp between BEG and END."
  (let* ((ts (buffer-substring beg end))
	 t1 with-hm tf time str (off 0))
    (save-match-data
      (setq t1 (org-parse-time-string ts t))
      (when (string-match "\\(-[0-9]+:[0-9]+\\)?\\( [.+]?\\+[0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)?\\'" ts)
	(setq off (- (match-end 0) (match-beginning 0)))))
    (setq end (- end off))
    (setq with-hm (and (decoded-time-minute t1) (decoded-time-hour t1))
	  tf (org-time-stamp-format with-hm 'no-brackets 'custom)
	  time (org-fix-decoded-time t1)
	  str (org-add-props
		  (format-time-string tf (org-encode-time time))
		  nil 'mouse-face 'highlight))
    (put-text-property beg end 'display str)
    (put-text-property beg end 'org-custom-date t)))

(defun org-activate-dates (limit)
  "Add text properties for dates."
  (when (and (re-search-forward org-tsr-regexp-both limit t)
	     (not (equal (char-before (match-beginning 0)) 91)))
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    (add-text-properties (match-beginning 0) (match-end 0)
			 (list 'mouse-face 'highlight
			       'keymap org-mouse-map))
    (org-rear-nonsticky-at (match-end 0))
    (when org-display-custom-times
      ;; If it's a date range, activate custom time for second date.
      (when (match-end 3)
	(org-display-custom-time (match-beginning 3) (match-end 3)))
      (org-display-custom-time (match-beginning 1) (match-end 1)))
    t))

(defun org-activate-target-links (limit)
  "Add text properties for target matches."
  (when org-target-link-regexp
    (let ((case-fold-search t))
      ;; `org-target-link-regexp' matches one character before the
      ;; actual target.
      (unless (bolp) (forward-char -1))
      (when (if org-target-link-regexps
                (org--re-list-search-forward org-target-link-regexps limit t)
              (re-search-forward org-target-link-regexp limit t))
	(org-remove-flyspell-overlays-in (match-beginning 1) (match-end 1))
	(add-text-properties (match-beginning 1) (match-end 1)
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map
				   'help-echo "Radio target link"
				   'org-linked-text t))
	(org-rear-nonsticky-at (match-end 1))
	t))))

(defvar org-latex-and-related-regexp nil
  "Regular expression for highlighting LaTeX, entities and sub/superscript.")

(defconst org-latex-regexps
  '(("begin" "^[ \t]*\\(\\\\begin{\\([a-zA-Z0-9\\*]+\\)[^z-a]+?\\\\end{\\2}[ \t]*\n?\\)" 1 t)
    ;; ("$" "\\([ \t(]\\|^\\)\\(\\(\\([$]\\)\\([^ \t\n,.$].*?\\(\n.*?\\)\\{0,5\\}[^ \t\n,.$]\\)\\4\\)\\)\\([ \t.,?;:'\")]\\|$\\)" 2 nil)
    ("$1" "\\([^$]\\|^\\)\\(\\$[^ \t\r\n,;.$]\\$\\)\\(\\s.\\|\\s-\\|\\s(\\|\\s)\\|\\s\"\\|'\\|$\\)" 2 nil)
    ("$"  "\\([^$]\\|^\\)\\(\\(\\$\\([^ \t\n,;.$][^$\n\r]*?\\(\n[^$\n\r]*?\\)\\{0,2\\}[^ \t\n,.$]\\)\\$\\)\\)\\(\\s.\\|\\s-\\|\\s(\\|\\s)\\|\\s\"\\|'\\|$\\)" 2 nil)
    ("\\(" "\\\\([^z-a]*?\\\\)" 0 nil)
    ("\\[" "\\\\\\[[^z-a]*?\\\\\\]" 0 nil)
    ("$$" "\\$\\$[^z-a]*?\\$\\$" 0 nil))
  "Regular expressions for matching embedded LaTeX.")

(defvar org-match-substring-regexp)
(defun org-compute-latex-and-related-regexp ()
  "Compute regular expression for LaTeX, entities and sub/superscript.
Result depends on variable `org-highlight-latex-and-related'."
  (let ((re-sub
	 (cond ((not (memq 'script org-highlight-latex-and-related)) nil)
	       ((eq org-use-sub-superscripts '{})
		(list org-match-substring-with-braces-regexp))
	       (org-use-sub-superscripts (list org-match-substring-regexp))))
	(re-latex
	 (when (or (memq 'latex org-highlight-latex-and-related)
		   (memq 'native org-highlight-latex-and-related))
           (defvar org-latex-preview-appearance-options) ; org-latex-preview.el
           (let ((matchers (or org-highlight-latex-matchers
                               ;; FIXME: Remove after deleting the
                               ;; obsolete `org-format-latex-options'
                               (plist-get org-latex-preview-appearance-options :matchers))))
	     (delq nil
		   (mapcar (lambda (x)
			     (and (member (car x) matchers) (nth 1 x)))
			   org-latex-regexps)))))
	(re-entities
	 (when (memq 'entities org-highlight-latex-and-related)
	   (list "\\\\\\(there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\
\\($\\|{}\\|[^[:alpha:]]\\)"))))
    (setq-local org-latex-and-related-regexp
		(mapconcat #'identity
			   (append re-latex re-entities re-sub)
			   "\\|"))))

(defvar org-latex-preview-appearance-options) ; Defined in org-latex-preview.el.
(declare-function org-latex-preview--face-around "org-latex-preview" (start end))

(defun org-do-latex-and-related (limit)
  "Highlight LaTeX snippets and environments, entities and sub/superscript.
Stop at first highlighted object, if any.  Return t if some
highlighting was done, nil otherwise."
  (when (org-string-nw-p org-latex-and-related-regexp)
    (let ((latex-prefix-re (rx (or "$" "\\(" "\\[")))
	  (blank-line-re (rx (and "\n" (zero-or-more (or " " "\t")) "\n"))))
      (catch 'found
	(while (and (< (point) limit)
		    (re-search-forward org-latex-and-related-regexp nil t))
	  (cond
           ((>= (match-beginning 0) limit)
	    (throw 'found nil))
	   ((cl-some (lambda (f)
		       (memq f '(org-code org-verbatim underline
					  org-special-keyword)))
		     (save-excursion
		       (goto-char (1+ (match-beginning 0)))
		       (face-at-point nil t))))
	   ;; Try to limit false positives.  In this case, ignore
	   ;; $$...$$, \(...\), and \[...\] LaTeX constructs if they
	   ;; contain an empty line.
	   ((save-excursion
	      (goto-char (match-beginning 0))
	      (and (looking-at-p latex-prefix-re)
		   (save-match-data
		     (re-search-forward blank-line-re (1- (match-end 0)) t)))))
	   (t
	    (let* ((offset (if (memq (char-after (1+ (match-beginning 0)))
				     '(?_ ?^))
			       1
			     0))
		   (start (+ offset (match-beginning 0)))
		   (end (save-excursion
                          (goto-char (match-end 0))
                          ;; Do not fontify trailing whitespace.
                          (skip-chars-backward " \t") (point))))
	      (if (memq 'native org-highlight-latex-and-related)
		  (org-src-font-lock-fontify-block "latex" start end)
		(font-lock-prepend-text-property start end
						 'face 'org-latex-and-related))
	      (add-text-properties (+ offset (match-beginning 0)) (match-end 0)
				   '(font-lock-multiline t))
              ;; Refresh the face of LaTeX previews (when applicable).
              (when (eq (plist-get org-latex-preview-appearance-options :foreground)
                        'auto)
                (dolist (ov (overlays-at start))
                  (when (and (eq (overlay-get ov 'org-overlay-type)
                                 'org-latex-overlay)
                             (overlay-get ov 'face)
                             (not (eq (overlay-get ov 'face) 'error)))
                    (overlay-put ov 'face
                                 (org-latex-preview--face-around
                                  (overlay-start ov) (overlay-end ov))))))
	      (throw 'found t)))))
	nil))))

(defun org-restart-font-lock ()
  "Restart `font-lock-mode', to force refontification."
  (when font-lock-mode
    (font-lock-mode -1)
    (font-lock-mode 1)))

(defun org-activate-tags (limit)
  (when (re-search-forward org-tag-line-re limit t)
    (org-remove-flyspell-overlays-in (match-beginning 1) (match-end 1))
    (add-text-properties (match-beginning 1) (match-end 1)
			 (list 'mouse-face 'highlight
			       'keymap org-mouse-map))
    (org-rear-nonsticky-at (match-end 1))
    t))

(defun org-activate-folds (limit)
  "Arrange trailing newlines after folds to inherit face before the fold."
  (let ((next-unfolded-newline (search-forward "\n" limit 'move)))
    (while (and next-unfolded-newline (org-fold-folded-p) (not (eobp)))
      (goto-char (org-fold-core-next-visibility-change nil limit))
      (setq next-unfolded-newline (search-forward "\n" limit 'move)))
    (when next-unfolded-newline
      (org-with-wide-buffer
       (when (and (> (match-beginning 0) (point-min))
                  (org-fold-folded-p (1- (match-beginning 0))))
         (put-text-property
          (match-beginning 0) (match-end 0)
          'face
          (get-text-property
           (org-fold-previous-visibility-change
            (1- (match-beginning 0)))
           'face)))
       t))))

(defvar org-font-lock-keywords nil)

(defvar org-font-lock-hook nil
  "Functions to be called for special font lock stuff.")

(defvar org-font-lock-extra-keywords nil) ;Dynamically scoped.

(defvar org-font-lock-set-keywords-hook nil
  "Functions that can manipulate `org-font-lock-extra-keywords'.
This is called after `org-font-lock-extra-keywords' is defined, but before
it is installed to be used by font lock.  This can be useful if something
needs to be inserted at a specific position in the font-lock sequence.")

(defun org-font-lock-hook (limit)
  "Run `org-font-lock-hook' within LIMIT."
  (run-hook-with-args 'org-font-lock-hook limit))

(defun org-set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (let ((org-font-lock-extra-keywords
         ;; As a general rule, we apply the element (container) faces
         ;; first and then prepend the object faces on top.
	 (list
	  ;; Call the hook
	  '(org-font-lock-hook)
	  ;; Headlines
	  `(,(if org-fontify-whole-heading-line
		 "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
	       "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
	    (1 (org-get-level-face 1))
	    (2 (org-get-level-face 2))
	    (3 (org-get-level-face 3)))
	  ;; Table lines
	  '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)\n?"
            (0 'org-table-row t)
	    (1 'org-table t))
	  ;; Table internals
	  '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
	  '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
	  '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
	  '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
	  ;; Properties
	  (list org-property-re
		'(1 'org-special-keyword t)
		'(3 'org-property-value t))
	  ;; Drawer boundaries.
	  '(org-fontify-drawers)
	  ;; Diary sexps.
	  '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
	  ;; Link related fontification.
	  '(org-activate-links) ; `org-activate-links' prepends faces
	  (when (memq 'tag org-highlight-links) '(org-activate-tags (1 'org-tag prepend)))
	  (when (memq 'radio org-highlight-links) '(org-activate-target-links (1 'org-link prepend)))
	  (when (memq 'date org-highlight-links) '(org-activate-dates (0 'org-date prepend)))
          ;; `org-activate-footnote-links' prepends faces
	  (when (memq 'footnote org-highlight-links) '(org-activate-footnote-links))
          ;; Targets.
          (list org-radio-target-regexp '(0 'org-target prepend))
	  (list org-target-regexp '(0 'org-target prepend))
	  ;; Macro
	  '(org-fontify-macros) ; `org-fontify-macro' prepends faces
	  ;; TODO keyword
	  (list (format org-heading-keyword-regexp-format
			org-todo-regexp)
		'(2 (org-get-todo-face 2) prepend))
	  ;; TODO
	  (when org-fontify-todo-headline
	    (list (format org-heading-keyword-regexp-format
			  (concat
			   "\\(?:"
			   (mapconcat #'regexp-quote org-not-done-keywords "\\|")
			   "\\)"))
		  '(2 'org-headline-todo prepend)))
	  ;; DONE
	  (when org-fontify-done-headline
	    (list (format org-heading-keyword-regexp-format
			  (concat
			   "\\(?:"
			   (mapconcat #'regexp-quote org-done-keywords "\\|")
			   "\\)"))
		  '(2 'org-headline-done prepend)))
	  ;; Priorities
          ;; `org-font-lock-add-priority-faces' prepends faces
	  '(org-font-lock-add-priority-faces)
	  ;; Tags
          ;; `org-font-lock-add-tag-faces' prepends faces
	  '(org-font-lock-add-tag-faces)
	  ;; Tags groups
	  (when (and org-group-tags org-tag-groups-alist)
	    (list (concat org-outline-regexp-bol ".+\\(:"
			  (regexp-opt (mapcar #'car org-tag-groups-alist))
			  ":\\).*$")
		  '(1 'org-tag-group prepend)))
	  ;; Special keywords (as a part of planning)
	  (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
	  (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
	  (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
	  (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
	  ;; Emphasis
          ;; `org-do-emphasis-faces' prepends faces
	  (when org-fontify-emphasized-text '(org-do-emphasis-faces))
	  ;; Description list items
          '("\\(?:^[ \t]*[-+]\\|^[ \t]+[*]\\)[ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
	    1 'org-list-dt prepend)
	  ;; Checkboxes
	  `(,org-list-full-item-re 3 'org-checkbox prepend lax)
	  (when (cdr (assq 'checkbox org-list-automatic-rules))
	    '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
	      (0 (org-get-checkbox-statistics-face) prepend)))
          ;; Inline export snippets
          '("\\(@@\\)\\([a-z-]+:\\).*?\\(@@\\)"
            (1 'font-lock-comment-face prepend)
            (2 'org-tag prepend)
            (3 'font-lock-comment-face prepend))
	  ;; ARCHIVEd headings
	  (list (concat
		 org-outline-regexp-bol
		 "\\(.*:" org-archive-tag ":.*\\)")
		'(1 'org-archived prepend))
	  ;; Specials
	  '(org-do-latex-and-related) ; prepends faces
	  '(org-fontify-entities) ; applies composition
	  '(org-raise-scripts) ; applies display
	  ;; Code
	  '(org-activate-code (1 'org-code prepend))
	  ;; Blocks and meta lines
          ;; Their face is an override - keywords, affiliated
          ;; keywords, blocks, and block boundaries are all
          ;; containers or part of container-only markup.
	  '(org-fontify-meta-lines-and-blocks)
          ;; `org-fontify-inline-src-blocks' prepends object boundary
          ;; faces and overrides native faces.
          '(org-fontify-inline-src-blocks)
          ;; Citations.  When an activate processor is specified, if
          ;; specified, try loading it beforehand.
          (progn
            (unless (null org-cite-activate-processor)
              (org-cite-try-load-processor org-cite-activate-processor))
            ;; prepends faces
            '(org-cite-activate))
	  ;; COMMENT
          ;; Apply this last, after all the markup is highlighted, so
          ;; that even "bright" markup will become dim.
	  (list (format
		 "^\\*+\\(?: +%s\\)?\\(?: +%s\\(?:%s\\)%s\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
		 org-todo-regexp
                 (regexp-quote org-priority-prefix)
                 org-priority-value-regexp
                 (regexp-quote org-priority-suffix)
		 org-comment-string)
		'(9 'org-special-keyword prepend))
          '(org-activate-folds))))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    (run-hooks 'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (setq-local org-font-lock-keywords org-font-lock-extra-keywords)
    (setq-local font-lock-defaults
		'(org-font-lock-keywords t nil nil backward-paragraph))
    (setq-local font-lock-extend-after-change-region-function
		#'org-fontify-extend-region)
    nil))

(defun org-toggle-pretty-entities ()
  "Toggle the composition display of entities as UTF8 characters."
  (interactive nil org-mode)
  (setq-local org-pretty-entities (not org-pretty-entities))
  (org-restart-font-lock)
  (if org-pretty-entities
      (message "Entities are now displayed as UTF8 characters")
    (save-restriction
      (widen)
      (decompose-region (point-min) (point-max))
      (message "Entities are now displayed as plain text"))))

(defvar-local org-custom-properties-overlays nil
  "List of overlays used for custom properties.")
;; Preserve when switching modes or when restarting Org.
(put 'org-custom-properties-overlays 'permanent-local t)

(defun org-toggle-custom-properties-visibility ()
  "Display or hide properties in `org-custom-properties'."
  (interactive nil org-mode)
  (if org-custom-properties-overlays
      (progn (mapc #'delete-overlay org-custom-properties-overlays)
	     (setq org-custom-properties-overlays nil))
    (when org-custom-properties
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((regexp (org-re-property (regexp-opt org-custom-properties) t t)))
	 (while (re-search-forward regexp nil t)
	   (let ((end (cdr (save-match-data (org-get-property-block)))))
	     (when (and end (< (point) end))
	       ;; Hide first custom property in current drawer.
	       (let ((o (make-overlay (match-beginning 0) (1+ (match-end 0)))))
		 (overlay-put o 'invisible t)
		 (overlay-put o 'org-custom-property t)
		 (push o org-custom-properties-overlays))
	       ;; Hide additional custom properties in the same drawer.
	       (while (re-search-forward regexp end t)
		 (let ((o (make-overlay (match-beginning 0) (1+ (match-end 0)))))
		   (overlay-put o 'invisible t)
		   (overlay-put o 'org-custom-property t)
		   (push o org-custom-properties-overlays)))))
	   ;; Each entry is limited to a single property drawer.
	   (outline-next-heading)))))))

(defun org-fontify-entities (limit)
  "Find an entity to fontify."
  (let (ee)
    (when org-pretty-entities
      (catch 'match
	;; "\_ "-family is left out on purpose.  Only the first one,
	;; i.e., "\_ ", could be fontified anyway, and it would be
	;; confusing when adding a second white space character.
	(while (re-search-forward
		"\\\\\\(there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\\($\\|{}\\|[^[:alpha:]\n]\\)"
		limit t)
	  (when (and (not (org-at-comment-p))
		     (setq ee (org-entity-get (match-string 1)))
		     (= (length (nth 6 ee)) 1))
	    (let* ((end (if (equal (match-string 2) "{}")
			    (match-end 2)
			  (match-end 1))))
	      (add-text-properties
	       (match-beginning 0) end
	       (list 'font-lock-fontified t))
	      (compose-region (match-beginning 0) end
			      (nth 6 ee) nil)
	      (backward-char 1)
	      (throw 'match t))))
	nil))))

(defun org-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org mode."
  (with-temp-buffer
    (insert s)
    (let ((org-odd-levels-only odd-levels))
      (org-mode)
      (font-lock-ensure)
      (if org-link-descriptive
          (org-link-display-format
           (buffer-string))
        (buffer-string)))))

(defun org-get-level-face (n)
  "Get the right face for match N in font-lock matching of headlines."
  (let* ((org-l0 (- (match-end 2) (match-beginning 1) 1))
	 (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
	 (org-f (if org-cycle-level-faces
		    (nth (% (1- org-l) org-n-level-faces) org-level-faces)
		  (nth (1- (min org-l org-n-level-faces)) org-level-faces))))
    (cond
     ((eq n 1) (if org-hide-leading-stars 'org-hide org-f))
     ((eq n 2) org-f)
     (t (unless org-level-color-stars-only org-f)))))

(defun org-face-from-face-or-color (context inherit face-or-color)
  "Create a face list that inherits INHERIT, but sets the foreground color.
When FACE-OR-COLOR is not a string, just return it."
  (if (stringp face-or-color)
      (list :inherit inherit
	    (cdr (assoc context org-faces-easy-properties))
	    face-or-color)
    face-or-color))

(defun org-get-todo-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (when (numberp kwd) (setq kwd (match-string kwd)))
  (or (org-face-from-face-or-color
       'todo 'org-todo (cdr (assoc kwd org-todo-keyword-faces)))
      (and (member kwd org-done-keywords) 'org-done)
      'org-todo))

(defun org-get-priority-face (priority)
  "Get the right face for PRIORITY.
PRIORITY is a number from 0-64 or a character value from ?A to ?Z."
  (or (org-face-from-face-or-color
       'priority 'org-priority (cdr (assq priority org-priority-faces)))
      'org-priority))

(defun org-get-tag-face (tag)
  "Get the right face for TAG.
If TAG is a number, get the corresponding match group."
  (let ((tag (if (wholenump tag) (match-string tag) tag)))
    (or (org-face-from-face-or-color
	 'tag 'org-tag (cdr (assoc tag org-tag-faces)))
	'org-tag)))

(defun org-font-lock-add-priority-faces (limit)
  "Add the special priority faces."
  (while (re-search-forward (concat "^\\*+" org-priority-regexp) limit t)
    (let ((beg (match-beginning 1))
	  (end (1+ (match-end 2))))
      (add-face-text-property
       beg end
       (org-get-priority-face (org-priority-to-value (match-string 2))))
      (add-text-properties
       beg end
       (list 'font-lock-fontified t)))))

(defun org-font-lock-add-tag-faces (limit)
  "Add the special tag faces."
  (when (and org-tag-faces org-tags-special-faces-re)
    (while (re-search-forward org-tags-special-faces-re limit t)
      (add-face-text-property
       (match-beginning 1)
       (match-end 1)
       (org-get-tag-face 1))
      (add-text-properties (match-beginning 1) (match-end 1)
			   (list 'font-lock-fontified t))
      (backward-char 1))))

(defvar org--extra-unfontify-properties nil
  "Extra properties to unfontify.
Specify as `(PROP1 PROP2 ...)'.")

(defun org-unfontify-region (beg end &optional _maybe_loudly)
  "Remove fontification and activation overlays from links."
  (font-lock-default-unfontify-region beg end)
  (with-silent-modifications
    (decompose-region beg end)
    (remove-text-properties
     beg end
     `( mouse-face t keymap t org-linked-text t
	invisible t intangible t
	org-emphasis t
        syntax-table t
        ,@(mapcan (lambda (p) (list p t)) org--extra-unfontify-properties)))
    (org-fold-core-update-optimization beg end)
    (org-remove-font-lock-display-properties beg end)))

(defconst org-script-display  '(((raise -0.3) (height 0.7))
				((raise 0.3)  (height 0.7))
				((raise -0.5))
				((raise 0.5)))
  "Display properties for showing superscripts and subscripts.")

(defun org-remove-font-lock-display-properties (beg end)
  "Remove specific display properties that have been added by font lock.
The will remove the raise properties that are used to show superscripts
and subscripts."
  (let (next prop)
    (while (< beg end)
      (setq next (next-single-property-change beg 'display nil end)
	    prop (get-text-property beg 'display))
      (when (member prop org-script-display)
	(put-text-property beg next 'display nil))
      (setq beg next))))

(defun org-raise-scripts (limit)
  "Add raise properties to sub/superscripts."
  (when (and org-pretty-entities org-pretty-entities-include-sub-superscripts
	     (re-search-forward
	      (if (eq org-use-sub-superscripts t)
		  org-match-substring-regexp
		org-match-substring-with-braces-regexp)
	      limit t))
    (let* ((pos (point)) table-p comment-p
	   (vbeg (match-beginning 3)) (vend (match-end 3))
	   (emph-p (get-text-property vbeg 'org-emphasis))
	   (link-p (get-text-property vbeg 'mouse-face))
	   (keyw-p (eq 'org-special-keyword (get-text-property vbeg 'face)))
           (props '(invisible org-raise rear-nonsticky (invisible))))
      (goto-char (line-beginning-position))
      (setq table-p (looking-at-p org-table-dataline-regexp)
	    comment-p (looking-at-p "^[ \t]*#[ +]"))
      (goto-char pos)
      ;; Handle a_b^c
      (when (member (char-after) '(?_ ?^)) (goto-char (1- pos)))
      (unless (or comment-p emph-p link-p keyw-p)
	(put-text-property (match-beginning 2) vend 'org-emphasis t)
        (add-text-properties (match-beginning 2) (match-end 2) props)
	(when (and (eq (char-after vbeg) ?{)
		   (eq (char-before vend) ?}))
	  (add-text-properties vbeg (1+ vbeg) props)
	  (add-text-properties (1- vend) vend props)
          (setq vbeg (1+ vbeg) vend (1- vend)))
	(put-text-property vbeg vend 'display
			   (if (equal (char-after (match-beginning 2)) ?^)
			       (nth (if table-p 3 1) org-script-display)
			     (nth (if table-p 2 0) org-script-display)))
        (run-hook-with-args 'org-hidden-text-functions 'raise
                            (match-beginning 0) (match-end 0) vbeg vend))
      t)))

(defun org-remove-empty-overlays-at (pos)
  "Remove outline overlays that do not contain non-white stuff."
  (dolist (o (overlays-at pos))
    (and (eq 'outline (overlay-get o 'invisible))
	 (not (string-match-p
             "\\S-" (buffer-substring (overlay-start o)
				     (overlay-end o))))
	 (delete-overlay o))))

(provide 'org-font-lock)

;;; org-font-lock.el ends here
