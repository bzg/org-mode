;; org-e-man.el --- Man Back-End For Org Export Engine

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Author: Luis R Anaya <papoanaya aroba hot mail punto com>
;; Keywords: outlines, hypermedia, calendar, wp
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
;; This library implements a Man back-end for Org generic exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-man "*Test e-Man*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the Man
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.
;;
;; It introduces one new buffer keywords:
;; "MAN_CLASS_OPTIONS".

;;;; Code:

(require 'org-export)

(eval-when-compile (require 'cl))

(defvar org-export-man-default-packages-alist)
(defvar org-export-man-packages-alist)






;;;; Define Back-End

(defvar org-e-man-translate-alist
  '((babel-call . org-e-man-babel-call)
    (bold . org-e-man-bold)
    (center-block . org-e-man-center-block)
    (clock . org-e-man-clock)
    (code . org-e-man-code)
    (comment . org-e-man-comment)
    (comment-block . org-e-man-comment-block)
    (drawer . org-e-man-drawer)
    (dynamic-block . org-e-man-dynamic-block)
    (entity . org-e-man-entity)
    (example-block . org-e-man-example-block)
    (export-block . org-e-man-export-block)
    (export-snippet . org-e-man-export-snippet)
    (fixed-width . org-e-man-fixed-width)
    (footnote-definition . org-e-man-footnote-definition)
    (footnote-reference . org-e-man-footnote-reference)
    (headline . org-e-man-headline)
    (horizontal-rule . org-e-man-horizontal-rule)
    (inline-babel-call . org-e-man-inline-babel-call)
    (inline-src-block . org-e-man-inline-src-block)
    (inlinetask . org-e-man-inlinetask)
    (italic . org-e-man-italic)
    (item . org-e-man-item)
    (keyword . org-e-man-keyword)
    (man-environment . org-e-man-man-environment)
    (man-fragment . org-e-man-man-fragment)
    (line-break . org-e-man-line-break)
    (link . org-e-man-link)
    (macro . org-e-man-macro)
    (paragraph . org-e-man-paragraph)
    (plain-list . org-e-man-plain-list)
    (plain-text . org-e-man-plain-text)
    (planning . org-e-man-planning)
    (property-drawer . org-e-man-property-drawer)
    (quote-block . org-e-man-quote-block)
    (quote-section . org-e-man-quote-section)
    (radio-target . org-e-man-radio-target)
    (section . org-e-man-section)
    (special-block . org-e-man-special-block)
    (src-block . org-e-man-src-block)
    (statistics-cookie . org-e-man-statistics-cookie)
    (strike-through . org-e-man-strike-through)
    (subscript . org-e-man-subscript)
    (superscript . org-e-man-superscript)
    (table . org-e-man-table)
    (table-cell . org-e-man-table-cell)
    (table-row . org-e-man-table-row)
    (target . org-e-man-target)
    (template . org-e-man-template)
    (timestamp . org-e-man-timestamp)
    (underline . org-e-man-underline)
    (verbatim . org-e-man-verbatim)
    (verse-block . org-e-man-verse-block))
  "Alist between element or object types and translators.")

(defconst org-e-man-options-alist
  '((:date "DATE" nil nil t)
    (:man-class "MAN_CLASS" nil nil t)
    (:man-class-options "MAN_CLASS_OPTIONS" nil nil t)
    (:man-header-extra "MAN_HEADER" nil nil newline))
  "Alist between Man export properties and ways to set them.
See `org-export-options-alist' for more information on the
structure of the values.")




;;; User Configurable Variables


(defgroup org-export-e-man nil
  "Options for exporting Org mode files to Man."
  :tag "Org Export Man"
  :group 'org-export)


;;;; Tables


(defcustom org-e-man-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-e-man
  :type 'boolean)

(defcustom org-e-man-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-e-man
  :type 'boolean)

(defcustom org-e-man-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-e-man
  :type '(choice
          (string :tag "Format string")
          (const :tag "No formatting")))


;;;; Inlinetasks


;; Src blocks

(defcustom org-e-man-source-highlight nil
  "Use GNU source highlight to embellish source blocks "
  :group 'org-export-e-man
  :type 'boolean)

(defcustom org-e-man-source-highlight-langs
  '((emacs-lisp "lisp") (lisp "lisp") (clojure "lisp")
    (scheme "scheme")
    (c "c") (cc "cpp") (csharp "csharp") (d "d")
    (fortran "fortran") (cobol "cobol") (pascal "pascal")
    (ada "ada") (asm "asm")
    (perl "perl") (cperl "perl")
    (python "python") (ruby "ruby") (tcl "tcl") (lua "lua")
    (java "java") (javascript "javascript")
    (tex "latex")
    (shell-script "sh") (awk "awk") (diff "diff") (m4 "m4")
    (ocaml "caml") (caml "caml")
    (sql "sql") (sqlite "sql")
    (html "html") (css "css") (xml "xml")
    (bat "bat") (bison "bison") (clipper "clipper")
    (ldap "ldap") (opa "opa")
    (php "php") (postscript "postscript") (prolog "prolog")
    (properties "properties") (makefile "makefile")
    (tml "tml") (vala "vala") (vbscript "vbscript") (xorg "xorg"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-e-man
  :type '(repeat
          (list
           (symbol :tag "Major mode       ")
           (string :tag "Listings language"))))


(defvar org-e-man-custom-lang-environments nil
  "Alist mapping languages to language-specific Man environments.

It is used during export of src blocks by the listings and
man packages.  For example,

  \(setq org-e-man-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during man export."
)


;;;; Plain text

(defcustom org-e-man-quotes
  '(("fr"
     ("\\(\\s-\\|[[(]\\|^\\)\"" . "«~")
     ("\\(\\S-\\)\"" . "~»")
     ("\\(\\s-\\|(\\|^\\)'" . "'"))
    ("en"
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
  :group 'org-export-e-man
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

(defcustom org-e-man-pdf-process
  '("tbl %f | eqn | groff -man | ps2pdf - > %b.pdf"
    "tbl %f | eqn | groff -man | ps2pdf - > %b.pdf"
    "tbl %f | eqn | groff -man | ps2pdf - > %b.pdf")

  "Commands to process a Man file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name \(i.e. without
extension) and %o by the base directory of the file.


By default, Org uses 3 runs of to do the processing.

Alternatively, this may be a Lisp function that does the
processing.  This function should accept the file name as
its single argument."
  :group 'org-export-pdf
  :type '(choice
          (repeat :tag "Shell command sequence"
                  (string :tag "Shell command"))
          (const :tag "2 runs of pdfgroff"
                 ("tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (const :tag "3 runs of pdfgroff"
                 ("tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (function)))

(defcustom org-e-man-logfiles-extensions
  '("log" "out" "toc")
  "The list of file extensions to consider as Man logfiles."
  :group 'org-export-e-man
  :type '(repeat (string :tag "Extension")))

(defcustom org-e-man-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-e-man
  :type 'boolean)



;; Preamble


;; Adding MAN as a block parser to make sure that its contents
;; does not execute

(add-to-list 'org-element-block-name-alist
             '("MAN" . org-element-export-block-parser))





;;; Internal Functions

(defun org-e-man--caption/label-string (caption label info)
  "Return caption and label Man string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-man--wrap-label'."
  (let ((label-str ""))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\fI%s\\fP" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "\\fR%s\\fP - \\fI%s\\P - %s\n"
              (org-export-data (cdr caption) info)
              label-str
              (org-export-data (car caption) info)))
     ;; Standard caption format.
     (t (format "\\fR%s\\fP"
                (org-export-data (car caption) info))))))

(defun org-e-man--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
          (let ((start 0))
            (while (setq start (string-match (car l) text start))
              (let ((new-quote (concat (match-string 1 text) (cdr l))))
                (setq text (replace-match new-quote  t t text))))))
        (cdr (or (assoc (plist-get info :language) org-e-man-quotes)
                 ;; Falls back on English.
                 (assoc "en" org-e-man-quotes))))
  text)

(defun org-e-man--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-man--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
        output
      (concat (format "%s\n.br\n" label) output))))




;;; Template

(defun org-e-man-template (contents info)
  "Return complete document string after Man conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (org-export-data (plist-get info :title) info))
        (attr
         (read (format "(%s)"
           (mapconcat
            #'identity
            (list (plist-get info :man-class-options))
            " "))))
    (section-item (plist-get attr :section-id)))

    (concat
     (cond
      ((and title (stringp section-item))
       (format ".TH \"%s\" \"%s\" \n" title section-item))
      ((and (string= "" title) (stringp section-item))
       (format ".TH \"%s\" \"%s\" \n" " " section-item))
      (title
       (format ".TH \"%s\" \"1\" \n" title))
      (t
       ".TH \" \" \"1\" "))
     contents)))




;;; Transcode Functions

;;;; Babel Call

;; Babel Calls are ignored.


;;;; Bold

(defun org-e-man-bold (bold contents info)
  "Transcode BOLD from Org to Man.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "\\fB%s\\fP" contents))


;;;; Center Block

(defun org-e-man-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to Man.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-e-man--wrap-label
   center-block
   (format ".ce %d\n.nf\n%s\n.fi"
           (- (length (split-string contents "\n")) 1)
           contents)))


;;;; Clock

(defun org-e-man-clock (clock contents info)
  "Transcode a CLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  "")


;;;; Code

(defun org-e-man-code (code contents info)
  "Transcode a CODE object from Org to Man.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "\\fC%s\\fP" code))


;;;; Comment
;; Comments are ignored.


;;;; Comment Block
;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-man-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Man.
   DRAWER holds the drawer information
   CONTENTS holds the contents of the block.
   INFO is a plist holding contextual information. "
  contents)


;;;; Dynamic Block

(defun org-e-man-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-e-man--wrap-label dynamic-block contents))


;;;; Entity

(defun org-e-man-entity (entity contents info)
  "Transcode an ENTITY object from Org to Man.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :utf8 entity))) ent))


;;;; Example Block

(defun org-e-man-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-e-man--wrap-label
   example-block
   (format ".RS\n.nf\n%s\n.fi\n.RE"
           (org-export-format-code-default example-block info))))

;;;; Export Block

(defun org-e-man-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "MAN")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Export Snippet

(defun org-e-man-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-man)
    (org-element-property :value export-snippet)))


;;;; Fixed Width

(defun org-e-man-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-man--wrap-label
   fixed-width
   (format "\\fC\n%s\\fP"
           (org-remove-indentation
            (org-element-property :value fixed-width)))))


;;;; Footnote Definition
;; Footnote Definitions are ignored.

;;;; Footnote References
;; Footnote References are Ignored


;;;; Headline

(defun org-e-man-headline (headline contents info)
  "Transcode an HEADLINE element from Org to Man.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (org-export-get-relative-level headline info))
		 (numberedp (org-export-numbered-headline-p headline info))
		 ;; Section formatting will set two placeholders: one for the
		 ;; title and the other for the contents.
		 (section-fmt
		  (case level
			(1 ".SH \"%s\"\n%s")
			(2 ".SS \"%s\"\n%s")
			(3 ".SS \"%s\"\n%s")
			(t nil)))
		 (text (org-export-data (org-element-property :title headline) info)))

    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)

     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt) (org-export-low-level-p headline info))
      ;; Build the real contents of the sub-tree.
      (let ((low-level-body
			 (concat
			  ;; If the headline is the first sibling, start a list.
			  (when (org-export-first-sibling-p headline info)
				(format "%s\n" ".RS"))
			  ;; Itemize headline
			  ".TP\n.ft I\n" text "\n.ft\n"
			  contents ".RE")))
		;; If headline is not the last sibling simply return
		;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
		;; blank line.
		(if (not (org-export-last-sibling-p headline info)) low-level-body
		  (replace-regexp-in-string
		   "[ \t\n]*\\'" ""
		   low-level-body))))

     ;; Case 3. Standard headline.  Export it as a section.
     (t (format section-fmt text contents)))))


;;;; Horizontal Rule
;; Not supported


;;;; Inline Babel Call
;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-man-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block)))
    (cond
     (org-e-man-source-highlight
      (let* ((tmpdir (if (featurep 'xemacs)
                         temp-directory
                       temporary-file-directory))
             (in-file  (make-temp-name
                        (expand-file-name "srchilite" tmpdir)))
             (out-file (make-temp-name
                        (expand-file-name "reshilite" tmpdir)))
             (org-lang (org-element-property :language inline-src-block))
             (lst-lang (cadr (assq (intern org-lang)
                                   org-e-man-source-highlight-langs)))

             (cmd (concat (expand-file-name "source-highlight")
                          " -s " lst-lang
                          " -f groff_man"
                          " -i " in-file
                          " -o " out-file)))

        (if lst-lang
            (let ((code-block ""))
              (with-temp-file in-file (insert code))
              (shell-command cmd)
              (setq code-block  (org-file-contents out-file))
              (delete-file in-file)
              (delete-file out-file)
              code-block)
          (format ".RS\n.nf\n\\fC\\m[black]%s\\m[]\\fP\n.fi\n.RE\n"
                  code))))

     ;; Do not use a special package: transcode it verbatim.
     (t
      (concat ".RS\n.nf\n" "\\fC" "\n" code "\n"
              "\\fP\n.fi\n.RE\n")))))


;;;; Inlinetask
;;;; Italic

(defun org-e-man-italic (italic contents info)
  "Transcode ITALIC from Org to Man.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "\\fI%s\\fP" contents))


;;;; Item

(defun org-e-man-item (item contents info)

  "Transcode an ITEM element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."

  (let* ((bullet (org-element-property :bullet item))
         (type (org-element-property :type (org-element-property :parent item)))
         (checkbox (case (org-element-property :checkbox item)
                     (on "\\o'\\(sq\\(mu'")			;;
                     (off "\\(sq ")					;;
                     (trans "\\o'\\(sq\\(mi'"))) ;;

         (tag (let ((tag (org-element-property :tag item)))
                ;; Check-boxes must belong to the tag.
                (and tag (format "\\fB%s\\fP"
                                 (concat checkbox
                                         (org-export-data tag info)))))))

    (if (and (null tag)
			 (null checkbox))
		(let* ((bullet (org-trim bullet))
			   (marker (cond  ((string= "-" bullet) "\\(em")
							  ((string= "*" bullet) "\\(bu")
							  ((eq type 'ordered)
							   (format "%s " (org-trim bullet)))
							  (t "\\(dg"))))
		  (concat ".IP " marker " 4\n"
				  (org-trim (or contents " "))))
                                        ; else
      (concat ".TP\n" (or tag (concat " " checkbox)) "\n"
              (org-trim (or contents " "))))))


;;;; Keyword

(defun org-e-man-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "MAN") value)
     ((string= key "INDEX") nil)
     ;; Invisible targets.
     ((string= key "TARGET") nil)
     ((string= key "TOC") nil))))


;;;; Man Environment

(defun org-e-man-man-environment (man-environment contents info)
  "Transcode a MAN-ENVIRONMENT element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((label (org-element-property :name man-environment))
        (value (org-remove-indentation
                (org-element-property :value man-environment))))
    (if (not (org-string-nw-p label)) value
      ;; Environment is labelled: label must be within the environment
      ;; (otherwise, a reference pointing to that element will count
      ;; the section instead).
      (with-temp-buffer
        (insert value)
        (goto-char (point-min))
        (forward-line)
        (insert (format "%s\n" label))
        (buffer-string)))))


;;;; Man Fragment

(defun org-e-man-man-fragment (man-fragment contents info)
  "Transcode a MAN-FRAGMENT object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value man-fragment))


;;;; Line Break

(defun org-e-man-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ".br\n")


;;;; Link

(defun org-e-man-link (link desc info)
  "Transcode a LINK object from Org to Man.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."

  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (and (not (string= desc "")) desc))

         (path (cond
                ((member type '("http" "https" "ftp" "mailto"))
                 (concat type ":" raw-path))
                ((string= type "file")
                 (when (string-match "\\(.+\\)::.+" raw-path)
                   (setq raw-path (match-string 1 raw-path)))
                 (if (file-name-absolute-p raw-path)
                     (concat "file://" (expand-file-name raw-path))
                   (concat "file://" raw-path)))
                (t raw-path)))
         protocol)
    (cond
     ;; External link with a description part.
     ((and path desc) (format "%s \\fBat\\fP \\fI%s\\fP" path desc))
     ;; External link without a description part.
     (path (format "\\fI%s\\fP" path))
     ;; No path, only description.  Try to do something useful.
     (t (format "\\fI%s\\fP" desc)))))


;;;; Macro

(defun org-e-man-macro (macro contents info)
  "Transcode a MACRO element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-man-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Man.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((parent (plist-get (nth 1 paragraph) :parent)))
    (when parent
      (let ((parent-type (car parent))
            (fixed-paragraph ""))
        (cond ((and (eq parent-type 'item)
                    (plist-get (nth 1 parent) :bullet))
               (setq fixed-paragraph (concat "" contents)))
              ((eq parent-type 'section)
               (setq fixed-paragraph (concat ".PP\n" contents)))
              ((eq parent-type 'footnote-definition)
               (setq fixed-paragraph contents))
              (t (setq fixed-paragraph (concat "" contents))))
        fixed-paragraph))))


;;;; Plain List

(defun org-e-man-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Man.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  contents)


;;;; Plain Text

(defun org-e-man-plain-text (text info)
  "Transcode a TEXT string from Org to Man.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect
  (setq text (replace-regexp-in-string
              "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
              "$\\" text nil t 1))

  ;; Handle quotation marks
  (setq text (org-e-man--quotation-marks text info))

  ;; Handle break preservation if required.

  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
                                         text)))
  ;; Return value.
  text)


;;;; Planning

;;;; Property Drawer


;;;; Quote Block

(defun org-e-man-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-man--wrap-label
   quote-block
   (format ".RS\n%s\n.RE" contents)))


;;;; Quote Section

(defun org-e-man-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
                (org-element-property :value quote-section))))
    (when value (format ".RS\\fI%s\\fP\n.RE\n" value))))


;;;; Radio Target

(defun org-e-man-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Man.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  text)


;;;; Section

(defun org-e-man-section (section contents info)
  "Transcode a SECTION element from Org to Man.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;;; Special Block

(defun org-e-man-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-e-man--wrap-label
     special-block
     (format "%s\n" contents))))


;;;; Src Block

(defun org-e-man-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."

  (let* ((lang (org-element-property :language src-block))
         (caption (org-element-property :caption src-block))
         (label (org-element-property :name src-block))
         (code (org-element-property :value src-block))
         (custom-env (and lang
                          (cadr (assq (intern lang)
                                      org-e-man-custom-lang-environments))))
         (num-start (case (org-element-property :number-lines src-block)
                      (continued (org-export-get-loc src-block info))
                      (new 0)))
         (retain-labels (org-element-property :retain-labels src-block)))
    (cond
     ;; Case 1.  No source fontification.
     ((not org-e-man-source-highlight)
      (let ((caption-str (org-e-man--caption/label-string caption label info)))
        (concat
          (format ".RS\n.nf\n\\fC%s\\fP\n.fi\n.RE\n\n"
                  (org-export-format-code-default src-block info)))))
     ((and org-e-man-source-highlight)
       (let* ((tmpdir (if (featurep 'xemacs)
                          temp-directory
                        temporary-file-directory))

              (in-file  (make-temp-name
                         (expand-file-name "srchilite" tmpdir)))
              (out-file (make-temp-name
                         (expand-file-name "reshilite" tmpdir)))

              (org-lang (org-element-property :language src-block))
              (lst-lang (cadr (assq (intern org-lang)
                                    org-e-man-source-highlight-langs)))

              (cmd (concat "source-highlight"
                           " -s " lst-lang
                           " -f groff_man "
                           " -i " in-file
                           " -o " out-file)))

         (if lst-lang
             (let ((code-block ""))
               (with-temp-file in-file (insert code))
               (shell-command cmd)
               (setq code-block  (org-file-contents out-file))
               (delete-file in-file)
               (delete-file out-file)
               code-block)
           (format ".RS\n.nf\n\\fC\\m[black]%s\\m[]\\fP\n.fi\n.RE"
                   code)))))))


;;;; Statistics Cookie

(defun org-e-man-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Strike-Through

(defun org-e-man-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Man.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "\\fI%s\\fP" contents))


;;;; Subscript

(defun org-e-man-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\d\\s-2%s\\s+2\\u" contents))


;;;; Superscript "^_%s$

(defun org-e-man-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\u\\s-2%s\\s+2\\d" contents))


;;;; Table
;;
;; `org-e-man-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to either `org-e-man-table--table.el-table' or
;; `org-e-man-table--org-table' functions, depending of the type of
;; the table.
;;
;; `org-e-man-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-e-man-table (table contents info)
  "Transcode a TABLE element from Org to Man.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or org-e-man-tables-verbatim
        (let ((attr
               (read
                (format
                 "(%s)"
                 (mapconcat
                  #'identity
                  (org-element-property :attr_man table)
                  " ")))))

          (and attr (plist-get attr :verbatim))))

    (format ".nf\n\\fC%s\\fP\n.fi"
            ;; Re-create table, without affiliated keywords.
            (org-trim
             (org-element-interpret-data
              `(table nil ,@(org-element-contents table))))))
   ;; Case 2: Standard table.
   (t (org-e-man-table--org-table table contents info))))

(defun org-e-man-table--align-string (divider table info)
  "Return an appropriate Man alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel."
(let (alignment)
      ;; Extract column groups and alignment from first (non-rule)
      ;; row.
      (org-element-map
       (org-element-map
        table 'table-row
        (lambda (row)
          (and (eq (org-element-property :type row) 'standard) row))
        info 'first-match)
       'table-cell
       (lambda (cell)
         (let* ((borders (org-export-table-cell-borders cell info))
                (raw-width (org-export-table-cell-width cell info))
                (width-cm (when raw-width (/ raw-width 5)))
                (width (if raw-width (format "w(%dc)"
                                             (if (< width-cm 1) 1 width-cm)) "")))
           ;; Check left border for the first cell only.
           (when (and (memq 'left borders) (not alignment))
             (push "|" alignment))
           (push
            (case (org-export-table-cell-alignment cell info)
                  (left (concat "l" width divider))
                  (right (concat "r" width divider))
                  (center (concat "c" width divider)))
            alignment)
           (when (memq 'right borders) (push "|" alignment))))
       info)
      (apply 'concat (reverse alignment))))

(defun org-e-man-table--org-table (table contents info)
  "Return appropriate Man code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' attribute."
  (let* ((label (org-element-property :name table))
         (caption (org-e-man--caption/label-string
                   (org-element-property :caption table) label info))
         (attr
          (read
           (format
            "(%s)"
            (mapconcat
             #'identity
             (org-element-property :attr_man table)
             " "))))

         (divider (if (plist-get attr :divider)
                      "|"
                    " "))

         ;; Determine alignment string.
         (alignment (org-e-man-table--align-string divider table info))
         ;; Extract others display options.
         (lines (org-split-string contents "\n"))

         (attr-list
          (let ((result-list '()))
            (dolist (attr-item
                     (list
                      (if (plist-get attr :expand)
                          "expand"
                        nil)

                      (case (plist-get attr :placement)
                        ('center "center")
                        ('left nil)
                        (t
                         (if org-e-man-tables-centered
                             "center" "")))

                      (case (plist-get attr :boxtype)
                        ('box "box")
                        ('doublebox "doublebox")
                        ('allbox "allbox")
                        ('none nil)
                        (t "box"))))

              (if attr-item
                  (add-to-list 'result-list attr-item)))
            result-list))


    (title-line  (plist-get attr :title-line))

    (table-format
     (concat
      (format "%s"
              (or (car attr-list) ""))
      (or
       (let ((output-list '()))
         (when (cdr attr-list)
           (dolist (attr-item (cdr attr-list))
             (setq output-list (concat output-list  (format ",%s" attr-item)))))
         output-list)
       "")))

    (first-line
      (when lines (org-split-string (car lines) "\t"))))
    ;; Prepare the final format string for the table.

    (cond
     ;; Others.
     (lines (concat ".TS\n " table-format ";\n"

                    (format "%s.\n"
                            (let ((final-line ""))

                              (when title-line
                                (dotimes (i (length first-line))
                                  (setq final-line (concat final-line "cb" divider))))

                              (setq final-line (concat final-line "\n"))
                              (if alignment
                                  (setq final-line (concat final-line alignment))
                                (dotimes (i (length first-line))
                                  (setq final-line (concat final-line "c" divider))))
                              final-line))

                    (format "%s.TE"
                            (let ((final-line ""))
                              (dolist (line-item lines)
                                (cond
                                 (t
                                  (setq lines (org-split-string contents "\n"))

                                  (setq final-line (concat final-line
                                                           (car (org-split-string line-item "\\\\")) "\n")))))
                              final-line)))))))


;;;; Table Cell

(defun org-e-man-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Man
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat (if (and contents
                   org-e-man-table-scientific-notation
                   (string-match orgtbl-exp-regexp contents))
              ;; Use appropriate format string for scientific
              ;; notation.
              (format org-e-man-table-scientific-notation
                      (match-string 1 contents)
                      (match-string 2 contents))
            contents)
          (when (org-export-get-next-element table-cell info) " \t ")))


;;;; Table Row

(defun org-e-man-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Man
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((attr (mapconcat 'identity
                            (org-element-property
                             :attr_man (org-export-get-parent table-row))
                            " "))
           ;; TABLE-ROW's borders are extracted from its first cell.
           (borders
            (org-export-table-cell-borders
             (car (org-element-contents table-row)) info)))
      (concat
       ;; Mark "hline" for horizontal lines.
       (cond  ((and (memq 'top borders) (memq 'above borders)) "_\n"))
       contents "\\\\\n"
       (cond
        ;; When BOOKTABS are activated enforce bottom rule even when
        ;; no hline was specifically marked.
        ((and (memq 'bottom borders) (memq 'below borders)) "_\n")
        ((memq 'below borders) "_"))))))


;;;; Target

(defun org-e-man-target (target contents info)
  "Transcode a TARGET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\fI%s\\fP"
          (org-export-solidify-link-text (org-element-property :value target))))


;;;; Timestamp

(defun org-e-man-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Man.
  CONTENTS is nil.  INFO is a plist holding contextual
  information."
  "")


;;;; Underline

(defun org-e-man-underline (underline contents info)
  "Transcode UNDERLINE from Org to Man.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "\\fI%s\\fP" contents))


;;;; Verbatim

(defun org-e-man-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Man.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format ".nf\n%s\n.fi" contents))


;;;; Verse Block

(defun org-e-man-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Man.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".RS\n.ft I\n%s\n.ft\n.RE" contents))



;;; Interactive functions

(defun org-e-man-export-to-man
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a Man file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only the body
without any markers.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".man"  subtreep pub-dir)))
    (org-export-to-file
     'e-man outfile subtreep visible-only body-only ext-plist)))

(defun org-e-man-export-to-pdf
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to Groff then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write between
markers.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return PDF file's name."
  (interactive)
  (org-e-man-compile
   (org-e-man-export-to-man
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-e-man-compile (grofffile)
  "Compile a Groff file.

GROFFFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-e-man-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (let* ((wconfig (current-window-configuration))
         (grofffile (file-truename grofffile))
         (base (file-name-sans-extension grofffile))
         errors)
    (message (format "Processing Groff file %s ..." grofffile))
    (unwind-protect
        (progn
          (cond
           ;; A function is provided: Apply it.
           ((functionp org-e-man-pdf-process)
            (funcall org-e-man-pdf-process (shell-quote-argument grofffile)))
           ;; A list is provided: Replace %b, %f and %o with appropriate
           ;; values in each command before applying it.  Output is
           ;; redirected to "*Org PDF Groff Output*" buffer.
           ((consp org-e-man-pdf-process)
            (let* ((out-dir (or (file-name-directory grofffile) "./"))
                   (outbuf (get-buffer-create "*Org PDF Groff Output*")))
              (mapc
               (lambda (command)
                 (shell-command
                  (replace-regexp-in-string
                   "%b" (shell-quote-argument base)
                   (replace-regexp-in-string
                    "%f" (shell-quote-argument grofffile)
                    (replace-regexp-in-string
                     "%o" (shell-quote-argument out-dir) command t t) t t) t t)
                  outbuf))
               org-e-man-pdf-process)
              ;; Collect standard errors from output buffer.
              (setq errors (org-e-man-collect-errors outbuf))))
           (t (error "No valid command to process to PDF")))
          (let ((pdffile (concat base ".pdf")))
            ;; Check for process failure.  Provide collected errors if
            ;; possible.
            (if (not (file-exists-p pdffile))
                (error (concat (format "PDF file %s wasn't produced" pdffile)
                               (when errors (concat ": " errors))))
              ;; Else remove log files, when specified, and signal end of
              ;; process to user, along with any error encountered.
              (when org-e-man-remove-logfiles
                (dolist (ext org-e-man-logfiles-extensions)
                  (let ((file (concat base "." ext)))
                    (when (file-exists-p file) (delete-file file)))))
              (message (concat "Process completed"
                               (if (not errors) "."
                                 (concat " with errors: " errors)))))
            ;; Return output file name.
            pdffile))
      (set-window-configuration wconfig))))

(defun org-e-man-collect-errors (buffer)
  "Collect some kind of errors from \"groff\" output
BUFFER is the buffer containing output.
Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final run
      nil)))


(provide 'org-e-man)
;;; org-e-man.el ends here
