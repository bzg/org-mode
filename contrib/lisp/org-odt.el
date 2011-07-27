;;; org-odt.el --- OpenDocumentText export for Org-mode

;; Copyright (C) 2010, 2011
;;   Jambunathan <kjambunathan at gmail dot com>

;; Author: Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.8

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;;; Code:
(eval-when-compile (require 'cl))
(require 'org-lparse)

(defun org-odt-end-export ()
  ;; remove empty paragraphs
  (goto-char (point-min))
  (while (re-search-forward
	  "<text:p\\( text:style-name=\"Text_20_body\"\\)?>[ \r\n\t]*</text:p>"
	  nil t)
    (replace-match ""))
  (goto-char (point-min))

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
  (remove-text-properties (point-min) (point-max) '(display t)))

(defvar org-odt-suppress-xref nil)
(defconst org-export-odt-special-string-regexps
  '(("\\\\-" . "&#x00ad;\\1")		; shy
    ("---\\([^-]\\)" . "&#x2014;\\1")	; mdash
    ("--\\([^-]\\)" . "&#x2013;\\1")	; ndash
    ("\\.\\.\\." . "&#x2026;"))		; hellip
  "Regular expressions for special string conversion.")

(defconst org-odt-lib-dir (file-name-directory load-file-name))
(defconst org-odt-data-dir
  (let ((dir1 (expand-file-name "../odt" org-odt-lib-dir))	   ; git
	(dir2 (expand-file-name "./contrib/odt" org-odt-lib-dir))) ; elpa
    (cond
     ((file-directory-p dir1) dir1)
     ((file-directory-p dir2) dir2)
     (t (error "Cannot find factory styles file. Check package dir layout"))))
  "Directory that holds auxiliary files used by the ODT exporter.

The 'styles' subdir contains the following xml files -
 'OrgOdtStyles.xml' and 'OrgOdtAutomaticStyles.xml' - which are
 used as factory settings of `org-export-odt-styles-file' and
 `org-export-odt-automatic-styles-file'.

The 'etc/schema' subdir contains rnc files for validating of
OpenDocument xml files.")

(defvar org-odt-file-extensions
  '(("odt" . "OpenDocument Text")
    ("ott" . "OpenDocument Text Template")
    ("odm" . "OpenDocument Master Document")
    ("ods" . "OpenDocument Spreadsheet")
    ("ots" . "OpenDocument Spreadsheet Template")
    ("odg" . "OpenDocument Drawing (Graphics)")
    ("otg" . "OpenDocument Drawing Template")
    ("odp" . "OpenDocument Presentation")
    ("otp" . "OpenDocument Presentation Template")
    ("odi" . "OpenDocument Image")
    ("odf" . "OpenDocument Formula")
    ("odc" . "OpenDocument Chart")
    ("doc" . "Microsoft Text")
    ("docx" . "Microsoft Text")
    ("xls" . "Microsoft Spreadsheet")
    ("xlsx" . "Microsoft Spreadsheet")
    ("ppt" . "Microsoft Presentation")
    ("pptx" . "Microsoft Presentation")))

(defvar org-odt-ms-file-extensions
  '(("doc" . "Microsoft Text")
    ("docx" . "Microsoft Text")
    ("xls" . "Microsoft Spreadsheet")
    ("xlsx" . "Microsoft Spreadsheet")
    ("ppt" . "Microsoft Presentation")
    ("pptx" . "Microsoft Presentation")))

;; RelaxNG validation of OpenDocument xml files
(eval-after-load 'rng-nxml
  '(setq rng-nxml-auto-validate-flag t))

(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files
		(expand-file-name "etc/schema/schemas.xml" org-odt-data-dir)))

(mapc
 (lambda (desc)
   ;; Let Org open all OpenDocument files using system-registered app
   (add-to-list 'org-file-apps
		(cons (concat  "\\." (car desc) "\\'") 'system))
   ;; Let Emacs open all OpenDocument files in archive mode
   (add-to-list 'auto-mode-alist
		(cons (concat  "\\." (car desc) "\\'") 'archive-mode)))
 org-odt-file-extensions)

(mapc
 (lambda (desc)
   ;; Let Org open all Microsoft files using system-registered app
   (add-to-list 'org-file-apps
		(cons (concat  "\\." (car desc) "\\'") 'system)))
 org-odt-ms-file-extensions)

;; register the odt exporter with the pre-processor
(add-to-list 'org-export-backends 'odt)

;; register the odt exporter with org-lparse library
(org-lparse-register-backend 'odt)

(defcustom org-export-odt-automatic-styles-file nil
  "Automatic styles for use with ODT exporter.
If unspecified, the file under `org-odt-data-dir' is used."
  :type 'file
  :group 'org-export-odt)

(defcustom org-export-odt-styles-file nil
  "Default styles file for use with ODT export.
Valid values are one of:
1. nil
2. path to a styles.xml file
3. path to a *.odt or a *.ott file
4. list of the form (ODT-OR-OTT-FILE (FILE-MEMBER-1 FILE-MEMBER-2
...))

In case of option 1, an in-built styles.xml is used. See
`org-odt-data-dir' for more information.

In case of option 3, the specified file is unzipped and the
styles.xml embedded therein is used.

In case of option 4, the specified ODT-OR-OTT-FILE is unzipped
and FILE-MEMBER-1, FILE-MEMBER-2 etc are copied in to the
generated odt file.  Use relative path for specifying the
FILE-MEMBERS.  styles.xml must be specified as one of the
FILE-MEMBERS.

Use options 1, 2 or 3 only if styles.xml alone suffices for
achieving the desired formatting.  Use option 4, if the styles.xml
references additional files like header and footer images for
achieving the desired formattting."
  :group 'org-export-odt
  :type
  '(choice
    (const :tag "Factory settings" nil)
    (file :must-match t :tag "styles.xml")
    (file :must-match t :tag "ODT or OTT file")
    (list :tag "ODT or OTT file + Members"
	  (file :must-match t :tag "ODF Text or Text Template file")
	  (cons :tag "Members"
		(file :tag "	Member" "styles.xml")
		(repeat (file :tag "Member"))))))

(defconst org-export-odt-tmpdir-prefix "odt-")
(defconst org-export-odt-bookmark-prefix "OrgXref.")
(defcustom org-export-odt-use-bookmarks-for-internal-links t
  "Export Internal links as bookmarks?."
  :type 'boolean
  :group 'org-export-odt)

(defcustom org-export-odt-embed-images t
  "Should the images be copied in to the odt file or just linked?"
  :type 'boolean
  :group 'org-export-odt)

(defcustom org-odt-export-inline-images 'maybe
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-odt-export
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

(defcustom org-odt-export-inline-image-extensions
  '("png" "jpeg" "jpg" "gif")
  "Extensions of image files that can be inlined into HTML."
  :type '(repeat (string :tag "Extension"))
  :group 'org-odt-export)

(defcustom org-export-odt-pixels-per-inch display-pixels-per-inch
  ;; FIXME add docstring
  ""
  :type 'float
  :group 'org-export-odt)

(defvar org-export-odt-default-org-styles-alist
  '((paragraph . ((default . "Text_20_body")
		  (fixedwidth . "OrgSourceBlock")
		  (verse . "OrgVerse")
		  (quote . "Quotations")
		  (blockquote . "Quotations")
		  (center . "OrgCenter")
		  (left . "OrgLeft")
		  (right . "OrgRight")
		  (title . "Heading_20_1.title")
		  (footnote . "Footnote")
		  (src . "OrgSourceBlock")
		  (illustration . "Illustration")
		  (table . "Table")
		  (definition-term . "Text_20_body_20_bold")
		  (horizontal-line . "Horizontal_20_Line")))
    (character . ((bold . "Bold")
		  (emphasis . "Emphasis")
		  (code . "OrgCode")
		  (verbatim . "OrgCode")
		  (strike . "Strikethrough")
		  (underline . "Underline")
		  (subscript . "OrgSubscript")
		  (superscript . "OrgSuperscript")))
    (list . ((ordered . "OrgNumberedList")
	     (unordered . "OrgBulletedList")
	     (description . "OrgDescriptionList"))))
  "Default styles for various entities.")

(defvar org-export-odt-org-styles-alist org-export-odt-default-org-styles-alist)
(defun org-odt-get-style-name-for-entity (category &optional entity)
  (let ((entity (or entity 'default)))
    (or
     (cdr (assoc entity (cdr (assoc category
				    org-export-odt-org-styles-alist))))
     (cdr (assoc entity (cdr (assoc category
				    org-export-odt-default-org-styles-alist))))
     (error "Cannot determine style name for entity %s of type %s"
	    entity category))))

;;;###autoload
(defun org-export-as-odt-and-open (arg)
  "Export the outline as ODT and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-lparse-and-open "odt" "odt" arg))

;;;###autoload
(defun org-export-as-odt-batch ()
  "Call the function `org-lparse-batch'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-odt-batch"
  (org-lparse-batch "odt"))

;;;###autoload
(defun org-export-as-odt-to-buffer (arg)
  "Call `org-lparse-odt` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-lparse-to-buffer'."
  (interactive "P")
  (org-lparse-to-buffer "odt" arg))

;;;###autoload
(defun org-replace-region-by-odt (beg end)
  "Assume the current region has org-mode syntax, and convert it to ODT.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an ODT buffer and then use this
command to convert it."
  (interactive "r")
  (org-replace-region-by "odt" beg end))

;;;###autoload
(defun org-export-region-as-odt (beg end &optional body-only buffer)
  "Convert region from BEG to END in org-mode buffer to ODT.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted ODT.  If BUFFER is the symbol `string', return the
produced ODT as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq odt (org-export-region-as-odt beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (org-lparse-region "odt" beg end body-only buffer))

;;; org-export-as-odt
;;;###autoload
(defun org-export-as-odt (arg &optional hidden ext-plist
			      to-buffer body-only pub-dir)
  "Export the outline as a OpenDocumentText file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting XML as a string.  When BODY-ONLY is set, don't produce
the file header and footer, simply return the content of
<body>...</body>, without even the body tags themselves.  When
PUB-DIR is set, use this as the publishing directory."
  (interactive "P")
  (org-lparse "odt" "odt" arg hidden ext-plist to-buffer body-only pub-dir))

(defvar org-odt-entity-control-callbacks-alist
  `((EXPORT
     . (org-odt-begin-export org-odt-end-export))
    (DOCUMENT-CONTENT
     . (org-odt-begin-document-content org-odt-end-document-content))
    (DOCUMENT-BODY
     . (org-odt-begin-document-body org-odt-end-document-body))
    (TOC
     . (org-odt-begin-toc org-odt-end-toc))
    (ENVIRONMENT
     . (org-odt-begin-environment org-odt-end-environment))
    (FOOTNOTE-DEFINITION
     . (org-odt-begin-footnote-definition org-odt-end-footnote-definition))
    (TABLE
     . (org-odt-begin-table org-odt-end-table))
    (TABLE-ROWGROUP
     . (org-odt-begin-table-rowgroup org-odt-end-table-rowgroup))
    (LIST
     . (org-odt-begin-list org-odt-end-list))
    (LIST-ITEM
     . (org-odt-begin-list-item org-odt-end-list-item))
    (OUTLINE
     . (org-odt-begin-outline org-odt-end-outline))
    (OUTLINE-TEXT
     . (org-odt-begin-outline-text org-odt-end-outline-text))
    (PARAGRAPH
     . (org-odt-begin-paragraph org-odt-end-paragraph)))
  "")

(defvar org-odt-entity-format-callbacks-alist
  `((EXTRA-TARGETS . org-lparse-format-extra-targets)
    (ORG-TAGS . org-lparse-format-org-tags)
    (SECTION-NUMBER . org-lparse-format-section-number)
    (HEADLINE . org-odt-format-headline)
    (TOC-ENTRY . org-odt-format-toc-entry)
    (TOC-ITEM . org-odt-format-toc-item)
    (TAGS . org-odt-format-tags)
    (SPACES . org-odt-format-spaces)
    (TABS . org-odt-format-tabs)
    (LINE-BREAK . org-odt-format-line-break)
    (FONTIFY . org-odt-format-fontify)
    (TODO . org-lparse-format-todo)
    (LINK . org-odt-format-link)
    (INLINE-IMAGE . org-odt-format-inline-image)
    (ORG-LINK . org-odt-format-org-link)
    (HEADING . org-odt-format-heading)
    (ANCHOR . org-odt-format-anchor)
    (TABLE . org-lparse-format-table)
    (TABLE-ROW . org-odt-format-table-row)
    (TABLE-CELL . org-odt-format-table-cell)
    (FOOTNOTES-SECTION . ignore)
    (FOOTNOTE-REFERENCE . org-odt-format-footnote-reference)
    (HORIZONTAL-LINE . org-odt-format-horizontal-line)
    (COMMENT . org-odt-format-comment)
    (LINE . org-odt-format-line)
    (ORG-ENTITY . org-odt-format-org-entity))
  "")

;;;_. callbacks
;;;_. control callbacks
;;;_ , document body
(defun org-odt-begin-office-body ()
  (insert "
  <office:body>
    <office:text>
      <text:sequence-decls>
	<text:sequence-decl text:display-outline-level=\"0\" text:name=\"Illustration\"/>
	<text:sequence-decl text:display-outline-level=\"0\" text:name=\"Table\"/>
	<text:sequence-decl text:display-outline-level=\"0\" text:name=\"Text\"/>
	<text:sequence-decl text:display-outline-level=\"0\" text:name=\"Drawing\"/>
      </text:sequence-decls>"))

;; Following variable is let bound when `org-do-lparse' is in
;; progress. See org-html.el.
(defvar org-lparse-toc)
(defun org-odt-begin-document-body (opt-plist)
  (org-odt-begin-office-body)
  (let ((title (plist-get opt-plist :title)))
    (when title
      (insert
       (org-odt-format-stylized-paragraph 'title title))))

  ;; insert toc
  (when org-lparse-toc
    (insert "\n" org-lparse-toc "\n")))

(defvar org-lparse-body-only)		; let bound during org-do-lparse
(defvar org-lparse-to-buffer)		; let bound during org-do-lparse
(defun org-odt-end-document-body (opt-plist)
  (unless org-lparse-body-only
    (org-lparse-insert-tag "</office:text>")
    (org-lparse-insert-tag "</office:body>")))

(defconst org-odt-document-content-header
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<office:document-content
    xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\"
    xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\"
    xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\"
    xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\"
    xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\"
    xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\"
    xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
    xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\"
    xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\"
    xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\"
    xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\"
    xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\"
    xmlns:math=\"http://www.w3.org/1998/Math/MathML\"
    xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\"
    xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\"
    xmlns:ooo=\"http://openoffice.org/2004/office\"
    xmlns:ooow=\"http://openoffice.org/2004/writer\"
    xmlns:oooc=\"http://openoffice.org/2004/calc\"
    xmlns:dom=\"http://www.w3.org/2001/xml-events\"
    xmlns:xforms=\"http://www.w3.org/2002/xforms\"
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
    xmlns:rpt=\"http://openoffice.org/2005/report\"
    xmlns:of=\"urn:oasis:names:tc:opendocument:xmlns:of:1.2\"
    xmlns:xodt=\"http://www.w3.org/1999/xodt\"
    xmlns:field=\"urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0\"    office:version=\"1.2\">
")

(defun org-odt-begin-document-content (opt-plist)
  ;; document header
  (insert org-odt-document-content-header)

  ;; automatic styles
  (insert-file-contents
   (or org-export-odt-automatic-styles-file
       (expand-file-name "styles/OrgOdtAutomaticStyles.xml"
			 org-odt-data-dir)))
   (goto-char (point-max)))

(defun org-odt-end-document-content ()
  (org-lparse-insert-tag "</office:document-content>"))

(defun org-odt-begin-outline (level1 snumber title tags
				     target extra-targets class)
  (org-lparse-insert
   'HEADING (org-lparse-format
	     'HEADLINE title extra-targets tags snumber level1)
   level1 target))

(defun org-odt-end-outline ()
  (ignore))

(defun org-odt-begin-outline-text (level1 snumber class)
  (ignore))

(defun org-odt-end-outline-text ()
  (ignore))

(defun org-odt-begin-paragraph (&optional style)
  (org-lparse-insert-tag
   "<text:p%s>" (org-odt-get-extra-attrs-for-paragraph-style style)))

(defun org-odt-end-paragraph ()
  (org-lparse-insert-tag "</text:p>"))

(defun org-odt-get-extra-attrs-for-paragraph-style (style)
  (let (style-name)
    (setq style-name
	  (cond
	   ((stringp style) style)
	   ((symbolp style) (org-odt-get-style-name-for-entity
			     'paragraph style))))
    (unless style-name
      (error "Don't know how to handle paragraph style %s" style))
    (format " text:style-name=\"%s\"" style-name)))

(defun org-odt-format-stylized-paragraph (style text)
  (org-odt-format-tags
   '("<text:p%s>" . "</text:p>") text
   (org-odt-get-extra-attrs-for-paragraph-style style)))

(defun org-odt-begin-environment (style)
  (case style
    ((blockquote verse center quote)
     (org-lparse-begin-paragraph style)
     (list))
    ((fixedwidth native)
     (org-lparse-end-paragraph)
     (list))
    (t (error "Unknown environment %s" style))))

(defun org-odt-end-environment (style)
  (case style
    ((blockquote verse center quote)
     (org-lparse-end-paragraph)
     (list))
    ((fixedwidth native)
     (org-lparse-begin-paragraph)
     (list))
    (t (error "Unknown environment %s" style))))

(defun org-odt-begin-list (ltype &optional arg1)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (let* ((style-name (org-odt-get-style-name-for-entity 'list ltype))
	 (extra (if style-name
		    (format " text:style-name=\"%s\""  style-name) "")))

    ;; FIXME: Handle arg1 incase of ordered lists.
    (case ltype
      ((ordered unordered description)
       (org-lparse-end-paragraph)
       (org-lparse-insert-tag "<text:list%s>" extra))
      (t (error "Unknown list type: %s"  ltype)))))

(defun org-odt-end-list (ltype)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (if ltype
      (org-lparse-insert-tag "</text:list>")
    (error "Unknown list type: %s" ltype)))

(defun org-odt-begin-list-item (ltype &optional arg headline)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered
     (assert (not headline) t)
     (let* ((counter arg) (extra ""))
       (org-lparse-insert-tag "<text:list-item>")
       (org-lparse-begin-paragraph)))
    (unordered
     (let* ((id arg) (extra ""))
       (org-lparse-insert-tag "<text:list-item>")
       (org-lparse-begin-paragraph)
       (insert (if headline (org-odt-format-target headline id)
		 (org-odt-format-bookmark "" id)))))
    (description
     (assert (not headline) t)
     (let ((term (or arg "(no term)")))
       (insert
	(org-odt-format-tags
	 '("<text:list-item>" . "</text:list-item>")
	 (org-odt-format-stylized-paragraph 'definition-term term)))
       (org-lparse-begin 'LIST-ITEM 'unordered)
       (org-lparse-begin 'LIST 'description)
       (org-lparse-begin 'LIST-ITEM 'unordered)))
    (t (error "Unknown list type"))))

(defun org-odt-end-list-item (ltype)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    ((ordered unordered)
     (org-lparse-insert-tag "</text:list-item>"))
    (description
     (org-lparse-end-list-item)
     (org-lparse-end 'LIST 'description)
     (org-lparse-end-list-item))
    (t (error "Unknown list type"))))

;; Following variables are let bound when table emission is in
;; progress. See org-lparse.el.
(defvar org-lparse-table-begin-marker)
(defvar org-lparse-table-ncols)
(defvar org-lparse-table-rowgrp-open)
(defvar org-lparse-table-rownum)
(defvar org-lparse-table-cur-rowgrp-is-hdr)
(defvar org-lparse-table-is-styled)
(defvar org-lparse-table-rowgrp-info)
(defvar org-lparse-table-colalign-vector)
(defun org-odt-begin-table (caption label attributes)
  (when label
    (insert
     (org-odt-format-stylized-paragraph
      'table (org-odt-format-entity-caption label caption "Table"))))

  (org-lparse-insert-tag
   "<table:table table:name=\"%s\" table:style-name=\"%s\">"
   (or label "") "OrgTable")
  (setq org-lparse-table-begin-marker (point)))

(defun org-odt-end-table ()
  (goto-char org-lparse-table-begin-marker)
  (loop for level from 0 below org-lparse-table-ncols
	do (insert (org-odt-format-tags "<table:table-column/>"  "")))

  ;; fill style attributes for table cells
  (when org-lparse-table-is-styled
    (while (re-search-forward "@@\\(table-cell:p\\|table-cell:style-name\\)@@\\([0-9]+\\)@@\\([0-9]+\\)@@" nil t)
      (let ((spec (match-string 1))
	    (r (string-to-number (match-string 2)))
	    (c (string-to-number (match-string 3))))
	(cond
	 ((equal spec "table-cell:p")
	  (let ((style-name (org-odt-get-paragraph-style-for-table-cell r c)))
	    (replace-match style-name t t)))
	 ((equal spec "table-cell:style-name")
	  (let ((style-name (org-odt-get-style-name-for-table-cell r c)))
	    (replace-match style-name t t)))))))

  (goto-char (point-max))
  (org-lparse-insert-tag "</table:table>"))

(defun org-odt-begin-table-rowgroup (&optional is-header-row)
  (when org-lparse-table-rowgrp-open
    (org-lparse-end 'TABLE-ROWGROUP))
  (org-lparse-insert-tag (if is-header-row
			   "<table:table-header-rows>"
			 "<table:table-rows>"))
  (setq org-lparse-table-rowgrp-open t)
  (setq org-lparse-table-cur-rowgrp-is-hdr is-header-row))

(defun org-odt-end-table-rowgroup ()
  (when org-lparse-table-rowgrp-open
    (setq org-lparse-table-rowgrp-open nil)
    (org-lparse-insert-tag
     (if org-lparse-table-cur-rowgrp-is-hdr
	 "</table:table-header-rows>" "</table:table-rows>"))))

(defun org-odt-format-table-row (row)
  (org-odt-format-tags
   '("<table:table-row>" . "</table:table-row>") row))

(defun org-odt-get-style-name-for-table-cell (r c)
  (concat
   "OrgTblCell"
   (cond
    ((= r 0) "T")
    ((eq (cdr (assoc r org-lparse-table-rowgrp-info))  :start) "T")
    (t ""))
   (when (= r org-lparse-table-rownum) "B")
   (cond
    ((= c 0) "")
    ((or (memq (nth c org-table-colgroup-info) '(:start :startend))
	 (memq (nth (1- c) org-table-colgroup-info) '(:end :startend))) "L")
    (t ""))))

(defun org-odt-get-paragraph-style-for-table-cell (r c)
  (capitalize (aref org-lparse-table-colalign-vector c)))

(defun org-odt-format-table-cell (data r c)
  (if (not org-lparse-table-is-styled)
      (org-odt-format-tags
       '("<table:table-cell>" . "</table:table-cell>")
       (org-odt-format-stylized-paragraph
	(cond
	 (org-lparse-table-cur-rowgrp-is-hdr "OrgTableHeading")
	 ((and (= c 0) (org-lparse-get 'TABLE-FIRST-COLUMN-AS-LABELS))
	  "OrgTableHeading")
	 (t "OrgTableContents"))
	data))
    (let* ((style-name-cookie
	    (format "@@table-cell:style-name@@%03d@@%03d@@" r c))
	   (paragraph-style-cookie
	    (concat
	     (cond
	      (org-lparse-table-cur-rowgrp-is-hdr "OrgTableHeading")
	      ((and (= c 0) (org-lparse-get 'TABLE-FIRST-COLUMN-AS-LABELS))
	       "OrgTableHeading")
	      (t "OrgTableContents"))
	     (format "@@table-cell:p@@%03d@@%03d@@" r c))))
      (org-odt-format-tags
       '("<table:table-cell table:style-name=\"%s\">" .
	 "</table:table-cell>")
       (org-odt-format-stylized-paragraph paragraph-style-cookie data)
       style-name-cookie))))

(defun org-odt-begin-footnote-definition (n)
  (org-lparse-begin-paragraph 'footnote))

(defun org-odt-end-footnote-definition (n)
  (org-lparse-end-paragraph))

(defun org-odt-begin-toc (lang-specific-heading)
  (insert
   (format "
    <text:table-of-content text:style-name=\"Sect2\" text:protected=\"true\" text:name=\"Table of Contents1\">
     <text:table-of-content-source text:outline-level=\"10\">
      <text:index-title-template text:style-name=\"Contents_20_Heading\">%s</text:index-title-template>
" lang-specific-heading))

  (loop for level from 1 upto 10
	do (insert (format
		    "
      <text:table-of-content-entry-template text:outline-level=\"%d\" text:style-name=\"Contents_20_%d\">
       <text:index-entry-link-start text:style-name=\"Internet_20_link\"/>
       <text:index-entry-chapter/>
       <text:index-entry-text/>
       <text:index-entry-link-end/>
      </text:table-of-content-entry-template>
" level level)))

  (insert
   (format  "
     </text:table-of-content-source>

     <text:index-body>
      <text:index-title text:style-name=\"Sect1\" text:name=\"Table of Contents1_Head\">
       <text:p text:style-name=\"Contents_20_Heading\">%s</text:p>
      </text:index-title>
" lang-specific-heading)))

(defun org-odt-end-toc ()
  (insert "
     </text:index-body>
    </text:table-of-content>
"))

(defun org-odt-format-toc-entry (snumber todo headline tags href)
  (setq headline (concat
		  (and org-export-with-section-numbers
		       (concat snumber ". "))
		  headline
		  (and tags
		    (concat
		     (org-lparse-format 'SPACES 3)
		     (org-lparse-format 'FONTIFY tags "tag")))))
  (when todo
    (setq headline (org-lparse-format 'FONTIFY headline "todo")))

  (let ((org-odt-suppress-xref t))
    (org-odt-format-link headline (concat  "#" href))))

(defun org-odt-format-toc-item (toc-entry level org-last-level)
  (let ((style (format "Contents_20_%d"
		       (+ level (or (org-lparse-get 'TOPLEVEL-HLEVEL) 1) -1))))
    (insert "\n" (org-odt-format-stylized-paragraph style toc-entry) "\n")))

;; Following variable is let bound during 'ORG-LINK callback. See
;; org-html.el
(defvar org-lparse-link-description-is-image nil)
(defun org-odt-format-link (desc href &optional attr)
  (cond
   ((and (= (string-to-char href) ?#) (not org-odt-suppress-xref))
    (setq href (concat org-export-odt-bookmark-prefix (substring href 1)))
    (org-odt-format-tags
     '("<text:bookmark-ref text:reference-format=\"text\" text:ref-name=\"%s\">" .
       "</text:bookmark-ref>")
     desc href))
   (org-lparse-link-description-is-image
    (org-odt-format-tags
     '("<draw:a xlink:type=\"simple\" xlink:href=\"%s\" %s>" . "</draw:a>")
     desc href (or attr "")))
   (t
    (org-odt-format-tags
     '("<text:a xlink:type=\"simple\" xlink:href=\"%s\" %s>" . "</text:a>")
     desc href (or attr "")))))

(defun org-odt-format-spaces (n)
  (org-odt-format-tags "<text:s text:c=\"%d\"/>" "" n))

(defun org-odt-format-tabs (&optional n)
  (let ((tab "<text:tab/>")
	(n (or n 1)))
    (insert tab)))

(defun org-odt-format-line-break ()
  (org-odt-format-tags "<text:line-break/>" ""))

(defun org-odt-format-horizontal-line ()
  (org-odt-format-stylized-paragraph 'horizontal-line ""))

(defun org-odt-format-line (line)
  (case org-lparse-dyn-current-environment
    (fixedwidth (concat (org-odt-format-source-code-or-example-line
			 (org-xml-encode-plain-text line)) "\n"))
    (t (concat line "\n"))))

(defun org-odt-format-comment (fmt &rest args)
  (let ((comment (apply 'format fmt args)))
    (format "\n<!-- %s  -->\n" comment)))

(defun org-odt-format-org-entity (wd)
  (org-entity-get-representation wd 'utf8))

(defun org-odt-fill-tabs-and-spaces (line)
  (replace-regexp-in-string
   "\\([\t]\\|\\([ ]+\\)\\)" (lambda (s)
	    (cond
	     ((string= s "\t") (org-odt-format-tabs))
	     ((> (length s) 1)
	      (org-odt-format-spaces (length s)))
	     (t " "))) line))

(defun org-odt-format-source-code-or-example-line (line)
  (org-odt-format-stylized-paragraph 'src (org-odt-fill-tabs-and-spaces line)))

(defun org-odt-format-example (lines)
  (mapconcat
   (lambda (line)
     (org-odt-format-source-code-or-example-line line))
   (org-split-string lines "[\r\n]") "\n"))

(defun org-odt-format-source-code-or-example (lines lang caption textareap
						    cols rows num cont
						    rpllbl fmt)
  (org-odt-format-example (org-export-number-lines
			   (org-xml-encode-plain-text-lines lines)
			   0 0 num cont rpllbl fmt)))

(defun org-xml-encode-plain-text-lines (rtn)
  (mapconcat 'org-xml-encode-plain-text (org-split-string rtn "[\r\n]") "\n"))

(defun org-odt-remap-stylenames (style-name)
  (or
   (cdr (assoc style-name '(("timestamp-wrapper" . "OrgTimestampWrapper")
			    ("timestamp" . "OrgTimestamp")
			    ("timestamp-kwd" . "OrgTimestampKeyword")
			    ("tag" . "OrgTag")
			    ("todo" . "OrgTodo")
			    ("done" . "OrgDone")
			    ("target" . "OrgTarget"))))
   style-name))

(defun org-odt-format-fontify (text style &optional id)
  (let* ((style-name
	  (cond
	   ((stringp style)
	    (org-odt-remap-stylenames style))
	   ((symbolp style)
	    (org-odt-get-style-name-for-entity 'character style))
	   ((listp style)
	    (assert (< 1 (length style)))
	    (let ((parent-style (pop style)))
	      (mapconcat (lambda (s)
			   ;; (assert (stringp s) t)
			   (org-odt-remap-stylenames s)) style "")
	      (org-odt-remap-stylenames parent-style)))
	   (t (error "Don't how to handle style %s"  style)))))
    (org-odt-format-tags
     '("<text:span text:style-name=\"%s\">" . "</text:span>")
     text style-name)))

(defun org-odt-relocate-relative-path (path dir)
  (if (file-name-absolute-p path) path
    (file-relative-name (expand-file-name path dir)
			(expand-file-name "eyecandy" dir))))

(defun org-odt-format-inline-image (thefile)
  (let* ((thelink (if (file-name-absolute-p thefile) thefile
		    (org-xml-format-href
		     (org-odt-relocate-relative-path
		      thefile org-current-export-file))))
	 (href
	  (org-odt-format-tags
	   "<draw:image xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>" ""
	   (if org-export-odt-embed-images
	       (org-odt-copy-image-file thefile) thelink))))
    (org-export-odt-format-image thefile href)))

(defun org-odt-format-org-link (opt-plist type-1 path fragment desc attr
					  descp)
  "Make an HTML link.
OPT-PLIST is an options list.
TYPE is the device-type of the link (THIS://foo.html)
PATH is the path of the link (http://THIS#locationx)
FRAGMENT is the fragment part of the link, if any (foo.html#THIS)
DESC is the link description, if any.
ATTR is a string of other attributes of the a element.
MAY-INLINE-P allows inlining it as an image."

  (declare (special org-lparse-par-open))
  (save-match-data
    (let* ((may-inline-p
	    (and (member type-1 '("http" "https" "file"))
		 (org-lparse-should-inline-p path descp)
		 (not fragment)))
	   (type (if (equal type-1 "id") "file" type-1))
	   (filename path)
	   (thefile path))

      (cond
       ;; check for inlined images
       ((and (member type '("file"))
	     (not fragment)
	     (org-file-image-p
	      filename org-odt-export-inline-image-extensions)
	     (or (eq t org-odt-export-inline-images)
		 (and org-odt-export-inline-images (not descp))))

	;; (when (and (string= type "file") (file-name-absolute-p path))
	;;   (setq thefile (concat "file://" (expand-file-name path))))
	;; (setq thefile (org-xml-format-href thefile))
	;; (org-export-html-format-image thefile)
	(org-odt-format-inline-image thefile))
       (t
	(when (string= type "file")
	  (setq thefile
		(cond
		 ((file-name-absolute-p path)
		  (concat "file://" (expand-file-name path)))
		 (t (org-odt-relocate-relative-path
		     thefile org-current-export-file)))))

	(when (and (member type '("" "http" "https" "file" "coderef"))
		   fragment)
	  (setq thefile (concat thefile "#" fragment)))

	(setq thefile (org-xml-format-href thefile))

	(when (not (member type '("" "file" "coderef")))
	  (setq thefile (concat type ":" thefile)))

	(let ((org-odt-suppress-xref (string= type "coderef")))
	  (org-odt-format-link
	   (org-xml-format-desc desc) thefile attr)))))))

(defun org-odt-format-heading (text level &optional id)
  (let* ((text (if id (org-odt-format-target text id) text)))
    (org-odt-format-tags
     '("<text:h text:style-name=\"Heading_20_%s\" text:outline-level=\"%s\">" .
       "</text:h>") text level level)))

(defun org-odt-format-headline (title extra-targets tags
					    &optional snumber level)
  (concat
   (org-lparse-format 'EXTRA-TARGETS extra-targets)

   ;; No need to generate section numbers. They are auto-generated by
   ;; the application

   ;; (concat (org-lparse-format 'SECTION-NUMBER snumber level) " ")
   title
   (and tags (concat (org-lparse-format 'SPACES 3)
		     (org-lparse-format 'ORG-TAGS tags)))))

(defun org-odt-format-anchor (text name &optional class)
  (org-odt-format-target text name))

(defun org-odt-format-bookmark (text id)
  (if id
      (org-odt-format-tags "<text:bookmark text:name=\"%s\"/>" text id)
    text))

(defun org-odt-format-target (text id)
  (let ((name (concat org-export-odt-bookmark-prefix id)))
    (concat
     (and id (org-odt-format-tags
	      "<text:bookmark-start text:name=\"%s\"/>" "" name))
     (org-odt-format-bookmark text id)
     (and id (org-odt-format-tags
	      "<text:bookmark-end text:name=\"%s\"/>" "" name)))))

(defun org-odt-format-footnote (n def)
  (let ((id (concat  "fn" n))
	(note-class "footnote")
	(par-style "Footnote"))
    (org-odt-format-tags
     '("<text:note text:id=\"%s\" text:note-class=\"%s\">" .
       "</text:note>")
     (concat
      (org-odt-format-tags
       '("<text:note-citation>" . "</text:note-citation>")
       n)
      (org-odt-format-tags
       '("<text:note-body>" . "</text:note-body>")
       def))
     id note-class)))

(defun org-odt-format-footnote-reference (n def refcnt)
  (if (= refcnt 1)
      (org-odt-format-footnote n def)
    (org-odt-format-footnote-ref n)))

(defun org-odt-format-footnote-ref (n)
  (let ((note-class "footnote")
	(ref-format "text")
	(ref-name (concat "fn" n)))
    (org-odt-format-tags
     '("<text:span text:style-name=\"%s\">" . "</text:span>")
     (org-odt-format-tags
      '("<text:note-ref text:note-class=\"%s\" text:reference-format=\"%s\" text:ref-name=\"%s\">" . "</text:note-ref>")
      n note-class ref-format ref-name)
     "OrgSuperscript")))

(defun org-odt-get-image-name (file-name)
  (require 'sha1)
  (file-relative-name
   (expand-file-name
    (concat (sha1 file-name) "." (file-name-extension file-name)) "Pictures")))

(defun org-export-odt-format-image (src href
					;; par-open
					)
  "Create image tag with source and attributes."
  (save-match-data

    (let (embed-as caption attr label attr-plist size width height)

      (cond
       ((string-match "^ltxpng/" src)
	;; FIXME: Anyway the latex src can be embedded as an
	;; annotation

	;; (org-find-text-property-in-string 'org-latex-src src)
	(setq caption nil attr nil label nil embed-as 'character))

       (t
	(setq caption (org-find-text-property-in-string 'org-caption src)
	      caption (and caption (org-xml-format-desc caption))
	      attr (org-find-text-property-in-string 'org-attributes src)
	      label (org-find-text-property-in-string 'org-label src)
	      embed-as 'paragraph)))

      (setq attr-plist (when attr (read  attr)))
      (setq size (org-odt-image-size-from-file
		  src (plist-get attr-plist :width)
		  (plist-get attr-plist :height)
		  (plist-get attr-plist :scale) nil embed-as))

      (org-export-odt-do-format-image embed-as caption attr label
				       size href))))

(defun org-export-odt-do-format-image (embed-as caption attr label
						size href)
  "Create image tag with source and attributes."
  (save-match-data
    (let ((width (car size)) (height (cdr size))
	  (draw-frame-pair
	   '("<draw:frame draw:style-name=\"%s\"
              text:anchor-type=\"%s\"
              draw:z-index=\"%d\" %s>" . "</draw:frame>")))
      (cond
       ((and (not caption) (not label))
	(let (style-name anchor-type)
	  (cond
	   ((eq embed-as 'paragraph)
	    (setq style-name  "OrgGraphicsParagraph" anchor-type "paragraph"))
	   ((eq embed-as 'character)
	    (setq style-name  "OrgGraphicsBaseline" anchor-type "as-char")))
	  (org-odt-format-tags
	   draw-frame-pair href style-name anchor-type 0
	   (org-odt-image-attrs-from-size width height))))

       (t
	(concat
	 ;; (when par-open (org-odt-close-par))
	 (org-odt-format-tags
	  draw-frame-pair
	  (org-odt-format-tags
	   '("<draw:text-box fo:min-height=\"%dcm\">" . "</draw:text-box>")
	   (org-odt-format-stylized-paragraph
	    'illustration
	    (concat
	     (let ((extra " style:rel-width=\"100%\" style:rel-height=\"scale\""))
	       (org-odt-format-tags
		draw-frame-pair href "OrgGraphicsParagraphContent" "paragraph" 2
		(concat (org-odt-image-attrs-from-size width height) extra)))
	     (org-odt-format-entity-caption label caption)))
	   height)
	  "OrgFrame" "paragraph" 1
	  (org-odt-image-attrs-from-size width))

	 ;; (when par-open (org-odt-open-par))
	 ))))))

;; xml files generated on-the-fly
(defconst org-export-odt-save-list
  '("mimetype" "META-INF/manifest.xml" "content.xml" "meta.xml" "styles.xml"))

;; xml files that are copied
(defconst org-export-odt-nosave-list '())

;; xml files that contribute to the final odt file
(defvar org-export-odt-file-list nil)

(defconst org-export-odt-manifest-lines
  '(("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
     "<manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\" manifest:version=\"1.2\">"
     "<manifest:file-entry manifest:media-type=\"application/vnd.oasis.opendocument.text\" manifest:version=\"1.2\" manifest:full-path=\"/\"/>"
     "<manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"content.xml\"/>"
     "<manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"styles.xml\"/>"
     "<manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"meta.xml\"/>"
     "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Pictures/\"/>") . ("</manifest:manifest>")))

(defconst org-export-odt-meta-lines
  '(("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
     "<office:document-meta"
     "    xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\""
     "    xmlns:xlink=\"http://www.w3.org/1999/xlink\""
     "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
     "    xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\""
     "    xmlns:ooo=\"http://openoffice.org/2004/office\" "
     "    office:version=\"1.2\">"
     "  <office:meta>") . ("  </office:meta>" "</office:document-meta>")))

(defun org-odt-copy-image-file (path &optional target-file)
  "Returns the internal name of the file"
  (let* ((image-type (file-name-extension path))
	 (media-type (format "image/%s" image-type))
	 (src-file (expand-file-name
		    path (file-name-directory org-current-export-file)))
	 (target-file (or target-file (org-odt-get-image-name src-file)))
	 ;; FIXME
	 (body-only nil))

    (when (not org-lparse-to-buffer)
      (message "Embedding %s as %s ..."
	       (substring-no-properties path) target-file)
      (copy-file src-file target-file 'overwrite)
      (org-odt-update-manifest-file media-type target-file)
      (push target-file org-export-odt-file-list)) target-file))

(defun org-odt-image-attrs-from-size (&optional width height)
  (concat
   (when width (format "svg:width=\"%0.2fcm\""  width))
   " "
   (when height (format "svg:height=\"%0.2fcm\""  height))))

(defvar org-export-odt-image-size-probe-method
  '(emacs imagemagick force)
  "Ordered list of methods by for determining size of an embedded
  image.")

(defvar org-export-odt-default-image-sizes-alist
  '(("character" . (5 . 0.4))
    ("paragraph" . (5 . 5)))
  "Hardcoded image dimensions one for each of the anchor
  methods.")

(defun org-odt-do-image-size (probe-method file &optional dpi anchor-type)
  (setq dpi (or dpi org-export-odt-pixels-per-inch))
  (setq anchor-type (or anchor-type "paragraph"))
  (flet ((size-in-cms (size-in-pixels)
		      (flet ((pixels-to-cms (pixels)
					    (let* ((cms-per-inch 2.54)
						   (inches (/ pixels dpi)))
					      (* cms-per-inch inches))))
			(and size-in-pixels
			     (cons (pixels-to-cms (car size-in-pixels))
				   (pixels-to-cms (cdr size-in-pixels)))))))
    (case probe-method
      (emacs
       (size-in-cms (ignore-errors (image-size (create-image file) 'pixels))))
      (imagemagick
       (size-in-cms
	(let ((dim (shell-command-to-string
		    (format "identify -format \"%%w:%%h\" \"%s\"" file))))
	  (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" dim)
	    (cons (string-to-number (match-string 1 dim))
		  (string-to-number (match-string 2 dim)))))))
      (t
       (cdr (assoc-string anchor-type
			  org-export-odt-default-image-sizes-alist))))))

(defun org-odt-image-size-from-file (file &optional user-width
					  user-height scale dpi embed-as)
  (unless (file-name-absolute-p file)
    (setq file (expand-file-name
		file (file-name-directory org-current-export-file))))
  (let* (size width height)
    (unless (and user-height user-width)
      (loop for probe-method in org-export-odt-image-size-probe-method
	    until size
	    do (setq size (org-odt-do-image-size
			   probe-method file dpi embed-as)))
      (or size (error "Cannot determine Image size. Aborting ..."))
      (setq width (car size) height (cdr size)))
    (cond
     (scale
      (setq width (* width scale) height (* height scale)))
     ((and user-height user-width)
      (setq width user-width height user-height))
     (user-height
      (setq width (* user-height (/ width height)) height user-height))
     (user-width
      (setq height (* user-width (/ height width)) width user-width))
     (t (ignore)))
    (cons width height)))

(defvar org-odt-default-entity "Illustration")
(defun org-odt-format-entity-caption (label caption &optional default-entity)
  (if (not label) (or caption "")
    (let* ((label-components (org-odt-parse-label label))
	   (entity (car label-components))
	   (seqno (cdr label-components))
	   (caption (and caption (concat ": " caption))))
      (unless seqno
	(setq seqno label
	      entity (or default-entity org-odt-default-entity)))
      (concat
       entity " "
       (org-odt-format-tags
	'("<text:sequence text:ref-name=\"%s\" text:name=\"%s\" text:formula=\"ooow:%s+1\" style:num-format=\"1\">" . "</text:sequence>")
	seqno label entity entity)
       caption))))

(defun org-odt-format-tags (tag text &rest args)
  (let ((prefix (when org-lparse-encode-pending "@"))
	(suffix (when org-lparse-encode-pending "@")))
    (apply 'org-lparse-format-tags tag text prefix suffix args)))

(defun org-odt-init-outfile (filename)
  (unless (executable-find "zip")
    ;; Not at all OSes ship with zip by default
    (error "Executable \"zip\" needed for creating OpenDocument files. Aborting."))

  (let* ((outdir (make-temp-file org-export-odt-tmpdir-prefix t))
	 (mimetype-file (expand-file-name "mimetype" outdir))
	 (content-file (expand-file-name "content.xml" outdir))
	 (manifest-file (expand-file-name "META-INF/manifest.xml" outdir))
	 (meta-file (expand-file-name "meta.xml" outdir))
	 (styles-file (expand-file-name "styles.xml" outdir))
	 (pictures-dir (expand-file-name "Pictures" outdir))
	 (body-only nil))

    ;; content file
    (with-current-buffer (find-file-noselect content-file t)
      (erase-buffer))

    ;; FIXME: How to factor in body-only here
    (unless body-only
      ;; manifest file
      (make-directory (file-name-directory manifest-file))
      (with-current-buffer (find-file-noselect manifest-file t)
	(erase-buffer)
	(insert (mapconcat 'identity (car org-export-odt-manifest-lines) "\n"))
	(insert "\n")
	(save-excursion
	  (insert (mapconcat 'identity (cdr org-export-odt-manifest-lines) "\n"))))

    ;; meta file
    (with-current-buffer (find-file-noselect meta-file t)
      (erase-buffer)
      (insert (mapconcat 'identity (car org-export-odt-meta-lines) "\n"))
      (insert "\n")
      (save-excursion
	(insert (mapconcat 'identity (cdr org-export-odt-meta-lines) "\n"))))

    ;; mimetype
    (with-current-buffer (find-file-noselect mimetype-file t)
      (insert "application/vnd.oasis.opendocument.text"))

    ;; styles file
    ;; (copy-file org-export-odt-styles-file styles-file t)

    ;; Pictures dir
    (make-directory pictures-dir)

    ;; initialize list of files that contribute to the odt file
    (setq org-export-odt-file-list
	  (append org-export-odt-save-list org-export-odt-nosave-list)))
    content-file))

(defconst org-odt-manifest-file-entry-tag
  "<manifest:file-entry manifest:media-type=\"%s\" manifest:full-path=\"%s\"/>")

(defun org-odt-save-as-outfile (target opt-plist)
  ;; write meta file
  (org-odt-update-meta-file opt-plist)

  ;; write styles file
  (org-odt-copy-styles-file)

  ;; Update styles.xml - take care of outline numbering
  (with-current-buffer
      (find-file-noselect (expand-file-name "styles.xml") t)
    ;; Don't make automatic backup of styles.xml file. This setting
    ;; prevents the backedup styles.xml file from being zipped in to
    ;; odt file. This is more of a hackish fix. Better alternative
    ;; would be to fix the zip command so that the output odt file
    ;; includes only the needed files and excludes any auto-generated
    ;; extra files like backups and auto-saves etc etc. Note that
    ;; currently the zip command zips up the entire temp directory so
    ;; that any auto-generated files created under the hood ends up in
    ;; the resulting odt file.
    (set (make-local-variable 'backup-inhibited) t)

    ;; Import local setting of `org-export-with-section-numbers'
    (org-lparse-bind-local-variables opt-plist)
    (org-odt-configure-outline-numbering
     (if org-export-with-section-numbers org-export-headline-levels 0)))

  (let ((zipdir default-directory))
    (message "Switching to directory %s" (expand-file-name zipdir))

    ;; save all xml files
    (mapc (lambda (file)
	    (with-current-buffer
		(find-file-noselect (expand-file-name file) t)
	      ;; prettify output
	      (indent-region (point-min) (point-max))
	      (save-buffer)))
	  org-export-odt-save-list)

    (let* ((target-name (file-name-nondirectory target))
	   (target-dir (file-name-directory target))
	   (cmds `(("zip" "-mX0" ,target-name "mimetype")
		   ("zip" "-rmTq" ,target-name "."))))
      (when (file-exists-p target)
	;; FIXME: If the file is locked this throws a cryptic error
	(delete-file target))

      (let ((coding-system-for-write 'no-conversion) exitcode)
	(message "Creating odt file...")
	(mapc
	 (lambda (cmd)
	   (message "Running %s" (mapconcat 'identity cmd " "))
	   (setq exitcode
		 (apply 'call-process (car cmd) nil nil nil (cdr cmd)))
	   (or (zerop exitcode)
	       (error "Unable to create odt file (%S)" exitcode)))
	 cmds))

      ;; move the file from outdir to target-dir
      (rename-file target-name target-dir)

      ;; kill all xml buffers
      (mapc (lambda (file)
	      (kill-buffer
	       (find-file-noselect (expand-file-name file zipdir) t)))
	    org-export-odt-save-list)

      (delete-directory zipdir)))

  (message "Created %s" target)
  (set-buffer (find-file-noselect target t)))

(defun org-odt-format-date (date)
  (let ((warning-msg
	 "OpenDocument files require that dates be in ISO-8601 format. Please review your DATE options for compatibility."))
    ;; If the user is not careful with the date specification, an
    ;; invalid meta.xml will be emitted.

    ;; For now honor user's diktat and let him off with a warning
    ;; message. This is OK as LibreOffice (and possibly other
    ;; apps) doesn't deem this deviation as critical and continue
    ;; to load the file.

    ;; FIXME: Surely there a better way to handle this. Revisit this
    ;; later.
    (cond
     ((and date (string-match "%" date))
      ;; Honor user's diktat. See comments above
      (org-lparse-warn warning-msg)
      (format-time-string date))
     (date
      ;; Honor user's diktat. See comments above
      (org-lparse-warn warning-msg)
      date)
     (t
      ;; ISO 8601 format
      (format-time-string "%Y-%m-%dT%T%:z")))))

(defun org-odt-update-meta-file (opt-plist)
  (with-current-buffer
      (find-file-noselect (expand-file-name "meta.xml") t)
    (let ((date (org-odt-format-date (plist-get opt-plist :date)))
	  (author (or (plist-get opt-plist :author) ""))
	  (email (plist-get opt-plist :email))
	  (keywords (plist-get opt-plist :keywords))
	  (description (plist-get opt-plist :description))
	  (title (plist-get opt-plist :title)))

      (insert
       "\n"
       (org-odt-format-tags '("<dc:creator>" . "</dc:creator>") author)
       (org-odt-format-tags
	'("\n<meta:initial-creator>" . "</meta:initial-creator>") author)
       (org-odt-format-tags '("\n<dc:date>" . "</dc:date>") date)
       (org-odt-format-tags
	'("\n<meta:creation-date>" . "</meta:creation-date>") date)
       (org-odt-format-tags '("\n<meta:generator>" . "</meta:generator>")
			     (when org-export-creator-info
			       (format "Org-%s/Emacs-%s"
				       org-version emacs-version)))
       (org-odt-format-tags '("\n<meta:keyword>" . "</meta:keyword>") keywords)
       (org-odt-format-tags '("\n<dc:subject>" . "</dc:subject>") description)
       (org-odt-format-tags '("\n<dc:title>" . "</dc:title>") title)
       "\n"))))

(defun org-odt-update-manifest-file (media-type full-path)
  (with-current-buffer
      (find-file-noselect (expand-file-name "META-INF/manifest.xml") t)
    (insert (format org-odt-manifest-file-entry-tag media-type full-path))))

(defun org-odt-finalize-outfile ()
  (message "org-newodt: Finalizing outfile")
  (org-odt-delete-empty-paragraphs))

(defun org-odt-delete-empty-paragraphs ()
  (goto-char (point-min))
  (let ((open "<text:p[^>]*>")
	(close "</text:p>"))
    (while (re-search-forward (format "%s[ \r\n\t]*%s" open close) nil t)
      (replace-match ""))))

(defun org-odt-get (what &optional opt-plist)
  (case what
    (BACKEND 'odt)
    (EXPORT-DIR (org-export-directory :html opt-plist))
    (FILE-NAME-EXTENSION "odt")
    (EXPORT-BUFFER-NAME "*Org ODT Export*")
    (ENTITY-CONTROL org-odt-entity-control-callbacks-alist)
    (ENTITY-FORMAT org-odt-entity-format-callbacks-alist)
    (INIT-METHOD 'org-odt-init-outfile)
    (FINAL-METHOD 'org-odt-finalize-outfile)
    (SAVE-METHOD 'org-odt-save-as-outfile)
    (OTHER-BACKENDS
     '("bib" "doc" "doc6" "doc95" "html" "xhtml" "latex" "odt" "ott" "pdf" "rtf"
       "sdw" "sdw3" "sdw4" "stw " "sxw" "mediawiki" "text" "txt" "uot" "vor"
       "vor3" "vor4" "docbook" "ooxml" "ppt" "odp"))
    (CONVERT-METHOD org-lparse-convert-process)
    (TOPLEVEL-HLEVEL 1)
    (SPECIAL-STRING-REGEXPS org-export-odt-special-string-regexps)
    (INLINE-IMAGES 'maybe)
    (INLINE-IMAGE-EXTENSIONS '("png" "jpeg" "jpg" "gif" "svg"))
    (PLAIN-TEXT-MAP '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")))
    (TABLE-FIRST-COLUMN-AS-LABELS nil)
    (FOOTNOTE-SEPARATOR (org-lparse-format 'FONTIFY "," 'superscript))
    (CODING-SYSTEM-FOR-WRITE 'utf-8)
    (CODING-SYSTEM-FOR-SAVE 'utf-8)
    (t (error "Unknown property: %s"  what))))

(defun org-odt-parse-label (label)
  (save-match-data
    (if (not (string-match "\\`[a-zA-Z]+:\\(.+\\)" label))
	(cons label nil)
      (cons
       (capitalize (substring label 0 (1- (match-beginning 1))))
       (substring label (match-beginning 1))))))

(defvar org-lparse-latex-fragment-fallback) ; set by org-do-lparse
(defun org-export-odt-preprocess (parameters)
  "Convert LaTeX fragments to images."
  (when (and org-current-export-file
	     (plist-get parameters :LaTeX-fragments))
    (org-format-latex
     (concat "ltxpng/" (file-name-sans-extension
			(file-name-nondirectory
			 org-current-export-file)))
     org-current-export-dir nil "Creating LaTeX image %s"
     nil nil
     (cond
      ((eq (plist-get parameters :LaTeX-fragments) 'verbatim) 'verbatim)
      ;; Investigate MathToWeb for converting TeX equations to MathML
      ;; See http://lists.gnu.org/archive/html/emacs-orgmode/2011-03/msg01755.html
      ((or (eq (plist-get parameters :LaTeX-fragments) 'mathjax )
	   (eq (plist-get parameters :LaTeX-fragments) t        ))
       (org-lparse-warn
	(concat
	 "Use of MathJax is incompatible with ODT exporter. "
	 (format "Using %S instead."  org-lparse-latex-fragment-fallback)))
	 org-lparse-latex-fragment-fallback)
      ((eq (plist-get parameters :LaTeX-fragments) 'dvipng  ) 'dvipng)
      (t nil))))
  (goto-char (point-min))
  (let (label label-components category value pretty-label)
    (while (re-search-forward "\\\\ref{\\([^{}\n]+\\)}" nil t)
      (org-if-unprotected-at (match-beginning 1)
	(setq label (match-string 1)
	      label-components (org-odt-parse-label label)
	      category (car label-components)
	      value (cdr label-components)
	      pretty-label (if value (concat category " " value) label))
	(replace-match
	 (let ((org-lparse-encode-pending t))
	   (org-odt-format-tags
	    '("<text:sequence-ref text:reference-format=\"category-and-value\" text:ref-name=\"%s\">"
	      . "</text:sequence-ref>") pretty-label label)) t t)))))

(declare-function archive-zip-extract "arc-mode.el" (archive name))
(defun org-odt-zip-extract-one (archive member &optional target)
  (require 'arc-mode)
  (let* ((target (or target default-directory))
	 (archive (expand-file-name archive))
	 (archive-zip-extract
	  (list "unzip" "-qq" "-o" "-d" target))
	 exit-code command-output)
    (setq command-output
	  (with-temp-buffer
	    (setq exit-code (archive-zip-extract archive member))
	    (buffer-string)))
    (unless (zerop exit-code)
      (message command-output)
      (error "Extraction failed"))))

(defun org-odt-zip-extract (archive members &optional target)
  (when (atom members) (setq members (list members)))
  (mapc (lambda (member)
	  (org-odt-zip-extract-one archive member target))
	members))

(defun org-odt-copy-styles-file (&optional styles-file)
  ;; Non-availability of styles.xml is not a critical error. For now
  ;; throw an error purely for aesthetic reasons.
  (setq styles-file (or styles-file
			org-export-odt-styles-file
			(expand-file-name "styles/OrgOdtStyles.xml"
					  org-odt-data-dir)
			(error "org-odt: Missing styles file?")))
  (cond
   ((listp styles-file)
    (let ((archive (nth 0 styles-file))
	  (members (nth 1 styles-file)))
      (org-odt-zip-extract archive members)
      (mapc
       (lambda (member)
	 (when (org-file-image-p member)
	   (let* ((image-type (file-name-extension member))
		  (media-type (format "image/%s" image-type)))
	     (org-odt-update-manifest-file media-type member))))
       members)))
   ((and (stringp styles-file) (file-exists-p styles-file))
    (let ((styles-file-type (file-name-extension styles-file)))
      (cond
       ((string= styles-file-type "xml")
	(copy-file styles-file "styles.xml" t))
       ((member styles-file-type '("odt" "ott"))
	(org-odt-zip-extract styles-file "styles.xml")))))
   (t
    (error (format "Invalid specification of styles.xml file: %S"
		   org-export-odt-styles-file)))))

(defvar org-export-odt-factory-settings
  "d4328fb9d1b6cb211d4320ff546829f26700dc5e"
  "SHA1 hash of OrgOdtStyles.xml.")

(defun org-odt-configure-outline-numbering (level)
  "Outline numbering is retained only upto LEVEL.
To disable outline numbering pass a LEVEL of 0."
  (if (not (string= org-export-odt-factory-settings (sha1 (current-buffer))))
      (org-lparse-warn
       "org-odt: Using custom styles file? Consider tweaking styles.xml for better output. To suppress this warning update `org-export-odt-factory-settings'")
    (goto-char (point-min))
    (let ((regex
	   "<text:outline-level-style\\(.*\\)text:level=\"\\(.*\\)\"\\(.*\\)>")
	  (replacement
	   "<text:outline-level-style\\1text:level=\"\\2\" style:num-format=\"\">"))
      (while (re-search-forward regex nil t)
	(when (> (string-to-number (match-string 1)) level)
	  (replace-match replacement t nil))))
    (save-buffer 0)))

(provide 'org-odt)

;;; org-odt.el ends here
