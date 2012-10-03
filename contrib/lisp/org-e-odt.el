;;; org-e-odt.el --- OpenDocument Text exporter for Org-mode

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org

;; This file is not part of GNU Emacs.

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

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'table))
(require 'format-spec)
(require 'org-export)

;;; Define Back-End

(org-export-define-backend e-odt
  ((bold . org-e-odt-bold)
   (center-block . org-e-odt-center-block)
   (clock . org-e-odt-clock)
   (code . org-e-odt-code)
   (drawer . org-e-odt-drawer)
   (dynamic-block . org-e-odt-dynamic-block)
   (entity . org-e-odt-entity)
   (example-block . org-e-odt-example-block)
   (export-block . org-e-odt-export-block)
   (export-snippet . org-e-odt-export-snippet)
   (fixed-width . org-e-odt-fixed-width)
   (footnote-definition . org-e-odt-footnote-definition)
   (footnote-reference . org-e-odt-footnote-reference)
   (headline . org-e-odt-headline)
   (horizontal-rule . org-e-odt-horizontal-rule)
   (inline-src-block . org-e-odt-inline-src-block)
   (inlinetask . org-e-odt-inlinetask)
   (italic . org-e-odt-italic)
   (item . org-e-odt-item)
   (keyword . org-e-odt-keyword)
   (latex-environment . org-e-odt-latex-environment)
   (latex-fragment . org-e-odt-latex-fragment)
   (line-break . org-e-odt-line-break)
   (link . org-e-odt-link)
   (macro . org-e-odt-macro)
   (paragraph . org-e-odt-paragraph)
   (plain-list . org-e-odt-plain-list)
   (plain-text . org-e-odt-plain-text)
   (planning . org-e-odt-planning)
   (property-drawer . org-e-odt-property-drawer)
   (quote-block . org-e-odt-quote-block)
   (quote-section . org-e-odt-quote-section)
   (radio-target . org-e-odt-radio-target)
   (section . org-e-odt-section)
   (special-block . org-e-odt-special-block)
   (src-block . org-e-odt-src-block)
   (statistics-cookie . org-e-odt-statistics-cookie)
   (strike-through . org-e-odt-strike-through)
   (subscript . org-e-odt-subscript)
   (superscript . org-e-odt-superscript)
   (table . org-e-odt-table)
   (table-cell . org-e-odt-table-cell)
   (table-row . org-e-odt-table-row)
   (target . org-e-odt-target)
   (template . org-e-odt-template)
   (timestamp . org-e-odt-timestamp)
   (underline . org-e-odt-underline)
   (verbatim . org-e-odt-verbatim)
   (verse-block . org-e-odt-verse-block))
  :export-block "ODT"
  :options-alist
  ((:odt-styles-file "ODT_STYLES_FILE" nil nil t)
   (:LaTeX-fragments nil "LaTeX" org-export-with-LaTeX-fragments)))


;;; Dependencies

;;; Hooks

;;; Function Declarations

(declare-function org-id-find-id-file "org-id" (id))
(declare-function hfy-face-to-style "htmlfontify" (fn))
(declare-function hfy-face-or-def-to-name "htmlfontify" (fn))
(declare-function archive-zip-extract "arc-mode.el" (archive name))
(declare-function org-create-math-formula "org" (latex-frag &optional mathml-file))
(declare-function browse-url-file-url "browse-url" (file))




;;; Internal Variables

(defconst org-e-odt-lib-dir
  (file-name-directory load-file-name)
  "Location of ODT exporter.
Use this to infer values of `org-e-odt-styles-dir' and
`org-e-odt-schema-dir'.")

(defvar org-e-odt-data-dir
  (expand-file-name "../../etc/" org-e-odt-lib-dir)
  "Data directory for ODT exporter.
Use this to infer values of `org-e-odt-styles-dir' and
`org-e-odt-schema-dir'.")

(defconst org-e-odt-special-string-regexps
  '(("\\\\-" . "&#x00ad;\\1")		; shy
    ("---\\([^-]\\)" . "&#x2014;\\1")	; mdash
    ("--\\([^-]\\)" . "&#x2013;\\1")	; ndash
    ("\\.\\.\\." . "&#x2026;"))		; hellip
  "Regular expressions for special string conversion.")

(defconst org-e-odt-schema-dir-list
  (list
   (and org-e-odt-data-dir
	(expand-file-name "./schema/" org-e-odt-data-dir)) ; bail out
   (eval-when-compile
     (and (boundp 'org-e-odt-data-dir) org-e-odt-data-dir ; see make install
	  (expand-file-name "./schema/" org-e-odt-data-dir))))
  "List of directories to search for OpenDocument schema files.
Use this list to set the default value of
`org-e-odt-schema-dir'.  The entries in this list are
populated heuristically based on the values of `org-e-odt-lib-dir'
and `org-e-odt-data-dir'.")

(defconst org-e-odt-styles-dir-list
  (list
   (and org-e-odt-data-dir
	(expand-file-name "./styles/" org-e-odt-data-dir)) ; bail out
   (eval-when-compile
     (and (boundp 'org-e-odt-data-dir) org-e-odt-data-dir ; see make install
	  (expand-file-name "./styles/" org-e-odt-data-dir)))
   (expand-file-name "../../etc/styles/" org-e-odt-lib-dir) ; git
   (expand-file-name "./etc/styles/" org-e-odt-lib-dir)  ; elpa
   (expand-file-name "./org/" data-directory)	       ; system
   )
  "List of directories to search for OpenDocument styles files.
See `org-e-odt-styles-dir'.  The entries in this list are populated
heuristically based on the values of `org-e-odt-lib-dir' and
`org-e-odt-data-dir'.")

(defconst org-e-odt-styles-dir
  (let* ((styles-dir
	  (catch 'styles-dir
	    (message "Debug (org-e-odt): Searching for OpenDocument styles files...")
	    (mapc (lambda (styles-dir)
		    (when styles-dir
		      (message "Debug (org-e-odt): Trying %s..." styles-dir)
		      (when (and (file-readable-p
				  (expand-file-name
				   "OrgOdtContentTemplate.xml" styles-dir))
				 (file-readable-p
				  (expand-file-name
				   "OrgOdtStyles.xml" styles-dir)))
			(message "Debug (org-e-odt): Using styles under %s"
				 styles-dir)
			(throw 'styles-dir styles-dir))))
		  org-e-odt-styles-dir-list)
	    nil)))
    (unless styles-dir
      (error "Error (org-e-odt): Cannot find factory styles files, aborting"))
    styles-dir)
  "Directory that holds auxiliary XML files used by the ODT exporter.

This directory contains the following XML files -
 \"OrgOdtStyles.xml\" and \"OrgOdtContentTemplate.xml\".  These
 XML files are used as the default values of
 `org-e-odt-styles-file' and
 `org-e-odt-content-template-file'.

The default value of this variable varies depending on the
version of org in use and is initialized from
`org-e-odt-styles-dir-list'.  Note that the user could be using org
from one of: org's own private git repository, GNU ELPA tar or
standard Emacs.")

(defconst org-e-odt-bookmark-prefix "OrgXref.")

(defconst org-e-odt-manifest-file-entry-tag
  "\n<manifest:file-entry manifest:media-type=\"%s\" manifest:full-path=\"%s\"%s/>")

(defconst org-e-odt-file-extensions
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
    ("odc" . "OpenDocument Chart")))

(defvar org-e-odt-table-style-format
  "
<style:style style:name=\"%s\" style:family=\"table\">
  <style:table-properties style:rel-width=\"%d%%\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0.20cm\" table:align=\"center\"/>
</style:style>
"
  "Template for auto-generated Table styles.")

(defvar org-e-odt-automatic-styles '()
  "Registry of automatic styles for various OBJECT-TYPEs.
The variable has the following form:
\(\(OBJECT-TYPE-A
  \(\(OBJECT-NAME-A.1 OBJECT-PROPS-A.1\)
   \(OBJECT-NAME-A.2 OBJECT-PROPS-A.2\) ...\)\)
 \(OBJECT-TYPE-B
  \(\(OBJECT-NAME-B.1 OBJECT-PROPS-B.1\)
   \(OBJECT-NAME-B.2 OBJECT-PROPS-B.2\) ...\)\)
 ...\).

OBJECT-TYPEs could be \"Section\", \"Table\", \"Figure\" etc.
OBJECT-PROPS is (typically) a plist created by passing
\"#+ATTR_ODT: \" option to `org-e-odt-parse-block-attributes'.

Use `org-e-odt-add-automatic-style' to add update this variable.'")

(defvar org-e-odt-object-counters nil
  "Running counters for various OBJECT-TYPEs.
Use this to generate automatic names and style-names. See
`org-e-odt-add-automatic-style'.")

(defvar org-e-odt-src-block-paragraph-format
  "<style:style style:name=\"OrgSrcBlock\" style:family=\"paragraph\" style:parent-style-name=\"Preformatted_20_Text\">
   <style:paragraph-properties fo:background-color=\"%s\" fo:padding=\"0.049cm\" fo:border=\"0.51pt solid #000000\" style:shadow=\"none\">
    <style:background-image/>
   </style:paragraph-properties>
   <style:text-properties fo:color=\"%s\"/>
  </style:style>"
  "Custom paragraph style for colorized source and example blocks.
This style is much the same as that of \"OrgFixedWidthBlock\"
except that the foreground and background colors are set
according to the default face identified by the `htmlfontify'.")

(defvar hfy-optimisations)
(defvar org-e-odt-embedded-formulas-count 0)
(defvar org-e-odt-entity-frame-styles
  '(("As-CharImage" "__Figure__" ("OrgInlineImage" nil "as-char"))
    ("ParagraphImage" "__Figure__" ("OrgDisplayImage" nil "paragraph"))
    ("PageImage" "__Figure__" ("OrgPageImage" nil "page"))
    ("CaptionedAs-CharImage" "__Figure__"
     ("OrgCaptionedImage"
      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
     ("OrgInlineImage" nil "as-char"))
    ("CaptionedParagraphImage" "__Figure__"
     ("OrgCaptionedImage"
      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
     ("OrgImageCaptionFrame" nil "paragraph"))
    ("CaptionedPageImage" "__Figure__"
     ("OrgCaptionedImage"
      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
     ("OrgPageImageCaptionFrame" nil "page"))
    ("InlineFormula" "__MathFormula__" ("OrgInlineFormula" nil "as-char"))
    ("DisplayFormula" "__MathFormula__" ("OrgDisplayFormula" nil "as-char"))
    ("CaptionedDisplayFormula" "__MathFormula__"
     ("OrgCaptionedFormula" nil "paragraph")
     ("OrgFormulaCaptionFrame" nil "as-char"))))

(defvar org-e-odt-embedded-images-count 0)
(defvar org-e-odt-image-size-probe-method
  (append (and (executable-find "identify") '(imagemagick)) ; See Bug#10675
	  '(emacs fixed))
  "Ordered list of methods for determining image sizes.")

(defvar org-e-odt-default-image-sizes-alist
  '(("as-char" . (5 . 0.4))
    ("paragraph" . (5 . 5)))
  "Hardcoded image dimensions one for each of the anchor
  methods.")

;; A4 page size is 21.0 by 29.7 cms
;; The default page settings has 2cm margin on each of the sides. So
;; the effective text area is 17.0 by 25.7 cm
(defvar org-e-odt-max-image-size '(17.0 . 20.0)
  "Limiting dimensions for an embedded image.")

(defvar org-e-odt-label-styles
  '(("math-formula" "%c" "text" "(%n)")
    ("math-label" "(%n)" "text" "(%n)")
    ("category-and-value" "%e %n: %c" "category-and-value" "%e %n")
    ("value" "%e %n: %c" "value" "%n"))
  "Specify how labels are applied and referenced.
This is an alist where each element is of the
form (LABEL-STYLE-NAME LABEL-ATTACH-FMT LABEL-REF-MODE
LABEL-REF-FMT).

LABEL-ATTACH-FMT controls how labels and captions are attached to
an entity.  It may contain following specifiers - %e, %n and %c.
%e is replaced with the CATEGORY-NAME.  %n is replaced with
\"<text:sequence ...> SEQNO </text:sequence>\".  %c is replaced
with CAPTION. See `org-e-odt-format-label-definition'.

LABEL-REF-MODE and LABEL-REF-FMT controls how label references
are generated.  The following XML is generated for a label
reference - \"<text:sequence-ref
text:reference-format=\"LABEL-REF-MODE\" ...> LABEL-REF-FMT
</text:sequence-ref>\".  LABEL-REF-FMT may contain following
specifiers - %e and %n.  %e is replaced with the CATEGORY-NAME.
%n is replaced with SEQNO. See
`org-e-odt-format-label-reference'.")

(defvar org-e-odt-category-map-alist
  '(("__Table__" "Table" "value" "Table")
    ("__Figure__" "Illustration" "value" "Figure")
    ("__MathFormula__" "Text" "math-formula" "Equation")
    ("__DvipngImage__" "Equation" "value" "Equation")
    ("__Listing__" "Listing" "value" "Listing")
    ;; ("__Table__" "Table" "category-and-value")
    ;; ("__Figure__" "Figure" "category-and-value")
    ;; ("__DvipngImage__" "Equation" "category-and-value")
    )
  "Map a CATEGORY-HANDLE to OD-VARIABLE and LABEL-STYLE.
This is a list where each entry is of the form \\(CATEGORY-HANDLE
OD-VARIABLE LABEL-STYLE CATEGORY-NAME\\).  CATEGORY_HANDLE
identifies the captionable entity in question.  OD-VARIABLE is
the OpenDocument sequence counter associated with the entity.
These counters are declared within
\"<text:sequence-decls>...</text:sequence-decls>\" block of
`org-e-odt-content-template-file'.  LABEL-STYLE is a key into
`org-e-odt-label-styles' and specifies how a given entity should
be captioned and referenced.  CATEGORY-NAME is used for
qualifying captions on export.  You can modify the CATEGORY-NAME
used in the exported document by modifying
`org-export-dictionary'.  For example, an embedded image in an
English document is captioned as \"Figure 1: Orgmode Logo\", by
default.  If you want the image to be captioned as \"Illustration
1: Orgmode Logo\" instead, install an entry in
`org-export-dictionary' which translates \"Figure\" to
\"Illustration\" when the language is \"en\" and encoding is
`:utf-8'.")

(defvar org-e-odt-manifest-file-entries nil)
(defvar hfy-user-sheet-assoc)

(defvar org-e-odt-zip-dir nil
  "Temporary work directory for OpenDocument exporter.")



;;; User Configuration Variables

(defgroup org-export-e-odt nil
  "Options for exporting Org mode files to ODT."
  :tag "Org Export ODT"
  :group 'org-export)


;;;; Debugging

(defcustom org-e-odt-prettify-xml nil
  "Specify whether or not the xml output should be prettified.
When this option is turned on, `indent-region' is run on all
component xml buffers before they are saved.  Turn this off for
regular use.  Turn this on if you need to examine the xml
visually."
  :group 'org-export-e-odt
  :version "24.1"
  :type 'boolean)


;;;; Document schema

(defcustom org-e-odt-schema-dir
  (let* ((schema-dir
	  (catch 'schema-dir
	    (message "Debug (org-e-odt): Searching for OpenDocument schema files...")
	    (mapc
	     (lambda (schema-dir)
	       (when schema-dir
		 (message "Debug (org-e-odt): Trying %s..." schema-dir)
		 (when (and (file-readable-p
			     (expand-file-name "od-manifest-schema-v1.2-cs01.rnc"
					       schema-dir))
			    (file-readable-p
			     (expand-file-name "od-schema-v1.2-cs01.rnc"
					       schema-dir))
			    (file-readable-p
			     (expand-file-name "schemas.xml" schema-dir)))
		   (message "Debug (org-e-odt): Using schema files under %s"
			    schema-dir)
		   (throw 'schema-dir schema-dir))))
	     org-e-odt-schema-dir-list)
	    (message "Debug (org-e-odt): No OpenDocument schema files installed")
	    nil)))
    schema-dir)
  "Directory that contains OpenDocument schema files.

This directory contains:
1. rnc files for OpenDocument schema
2. a \"schemas.xml\" file that specifies locating rules needed
   for auto validation of OpenDocument XML files.

Use the customize interface to set this variable.  This ensures
that `rng-schema-locating-files' is updated and auto-validation
of OpenDocument XML takes place based on the value
`rng-nxml-auto-validate-flag'.

The default value of this variable varies depending on the
version of org in use and is initialized from
`org-e-odt-schema-dir-list'.  The OASIS schema files are available
only in the org's private git repository.  It is *not* bundled
with GNU ELPA tar or standard Emacs distribution."
  :type '(choice
	  (const :tag "Not set" nil)
	  (directory :tag "Schema directory"))
  :group 'org-export-e-odt
  :version "24.1"
  :set
  (lambda (var value)
    "Set `org-e-odt-schema-dir'.
Also add it to `rng-schema-locating-files'."
    (let ((schema-dir value))
      (set var
	   (if (and
		(file-readable-p
		 (expand-file-name "od-manifest-schema-v1.2-cs01.rnc" schema-dir))
		(file-readable-p
		 (expand-file-name "od-schema-v1.2-cs01.rnc" schema-dir))
		(file-readable-p
		 (expand-file-name "schemas.xml" schema-dir)))
	       schema-dir
	     (when value
	       (message "Error (org-e-odt): %s has no OpenDocument schema files"
			value))
	     nil)))
    (when org-e-odt-schema-dir
      (eval-after-load 'rng-loc
	'(add-to-list 'rng-schema-locating-files
		      (expand-file-name "schemas.xml"
					org-e-odt-schema-dir))))))


;;;; Document styles

(defcustom org-e-odt-content-template-file nil
  "Template file for \"content.xml\".
The exporter embeds the exported content just before
\"</office:text>\" element.

If unspecified, the file named \"OrgOdtContentTemplate.xml\"
under `org-e-odt-styles-dir' is used."
  :type 'file
  :group 'org-export-e-odt
  :version "24.1")

(defcustom org-e-odt-styles-file nil
  "Default styles file for use with ODT export.
Valid values are one of:
1. nil
2. path to a styles.xml file
3. path to a *.odt or a *.ott file
4. list of the form (ODT-OR-OTT-FILE (FILE-MEMBER-1 FILE-MEMBER-2
...))

In case of option 1, an in-built styles.xml is used. See
`org-e-odt-styles-dir' for more information.

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
achieving the desired formatting.

Use \"#+ODT_STYLES_FILE: ...\" directive to set this variable on
a per-file basis.  For example,

#+ODT_STYLES_FILE: \"/path/to/styles.xml\" or
#+ODT_STYLES_FILE: (\"/path/to/file.ott\" (\"styles.xml\" \"image/hdr.png\"))."
  :group 'org-export-e-odt
  :version "24.1"
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

(defcustom org-e-odt-display-outline-level 2
  "Outline levels considered for enumerating captioned entities."
  :group 'org-export-e-odt
  :version "24.2"
  :type 'integer)

;;;; Document conversion

(defcustom org-e-odt-convert-processes
  '(("LibreOffice"
     "soffice --headless --convert-to %f%x --outdir %d %i")
    ("unoconv"
     "unoconv -f %f -o %d %i"))
  "Specify a list of document converters and their usage.
The converters in this list are offered as choices while
customizing `org-e-odt-convert-process'.

This variable is a list where each element is of the
form (CONVERTER-NAME CONVERTER-CMD).  CONVERTER-NAME is the name
of the converter.  CONVERTER-CMD is the shell command for the
converter and can contain format specifiers.  These format
specifiers are interpreted as below:

%i input file name in full
%I input file name as a URL
%f format of the output file
%o output file name in full
%O output file name as a URL
%d output dir in full
%D output dir as a URL.
%x extra options as set in `org-e-odt-convert-capabilities'."
  :group 'org-export-e-odt
  :version "24.1"
  :type
  '(choice
    (const :tag "None" nil)
    (alist :tag "Converters"
	   :key-type (string :tag "Converter Name")
	   :value-type (group (string :tag "Command line")))))

(defcustom org-e-odt-convert-process "LibreOffice"
  "Use this converter to convert from \"odt\" format to other formats.
During customization, the list of converter names are populated
from `org-e-odt-convert-processes'."
  :group 'org-export-e-odt
  :version "24.1"
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,(car c) ,(car c)))
			     org-e-odt-convert-processes))))

(defcustom org-e-odt-convert-capabilities
  '(("Text"
     ("odt" "ott" "doc" "rtf" "docx")
     (("pdf" "pdf") ("odt" "odt") ("rtf" "rtf") ("ott" "ott")
      ("doc" "doc" ":\"MS Word 97\"") ("docx" "docx") ("html" "html")))
    ("Web"
     ("html")
     (("pdf" "pdf") ("odt" "odt") ("html" "html")))
    ("Spreadsheet"
     ("ods" "ots" "xls" "csv" "xlsx")
     (("pdf" "pdf") ("ots" "ots") ("html" "html") ("csv" "csv") ("ods" "ods")
      ("xls" "xls") ("xlsx" "xlsx")))
    ("Presentation"
     ("odp" "otp" "ppt" "pptx")
     (("pdf" "pdf") ("swf" "swf") ("odp" "odp") ("otp" "otp") ("ppt" "ppt")
      ("pptx" "pptx") ("odg" "odg"))))
  "Specify input and output formats of `org-e-odt-convert-process'.
More correctly, specify the set of input and output formats that
the user is actually interested in.

This variable is an alist where each element is of the
form (DOCUMENT-CLASS INPUT-FMT-LIST OUTPUT-FMT-ALIST).
INPUT-FMT-LIST is a list of INPUT-FMTs.  OUTPUT-FMT-ALIST is an
alist where each element is of the form (OUTPUT-FMT
OUTPUT-FILE-EXTENSION EXTRA-OPTIONS).

The variable is interpreted as follows:
`org-e-odt-convert-process' can take any document that is in
INPUT-FMT-LIST and produce any document that is in the
OUTPUT-FMT-LIST.  A document converted to OUTPUT-FMT will have
OUTPUT-FILE-EXTENSION as the file name extension.  OUTPUT-FMT
serves dual purposes:
- It is used for populating completion candidates during
  `org-e-odt-convert' commands.
- It is used as the value of \"%f\" specifier in
  `org-e-odt-convert-process'.

EXTRA-OPTIONS is used as the value of \"%x\" specifier in
`org-e-odt-convert-process'.

DOCUMENT-CLASS is used to group a set of file formats in
INPUT-FMT-LIST in to a single class.

Note that this variable inherently captures how LibreOffice based
converters work.  LibreOffice maps documents of various formats
to classes like Text, Web, Spreadsheet, Presentation etc and
allow document of a given class (irrespective of it's source
format) to be converted to any of the export formats associated
with that class.

See default setting of this variable for an typical
configuration."
  :group 'org-export-e-odt
  :version "24.1"
  :type
  '(choice
    (const :tag "None" nil)
    (alist :tag "Capabilities"
	   :key-type (string :tag "Document Class")
	   :value-type
	   (group (repeat :tag "Input formats" (string :tag "Input format"))
		  (alist :tag "Output formats"
			 :key-type (string :tag "Output format")
			 :value-type
			 (group (string :tag "Output file extension")
				(choice
				 (const :tag "None" nil)
				 (string :tag "Extra options"))))))))

(defcustom org-e-odt-preferred-output-format nil
  "Automatically post-process to this format after exporting to \"odt\".
Interactive commands `org-export-as-e-odt' and
`org-export-as-e-odt-and-open' export first to \"odt\" format and
then use `org-e-odt-convert-process' to convert the
resulting document to this format.  During customization of this
variable, the list of valid values are populated based on
`org-e-odt-convert-capabilities'."
  :group 'org-export-e-odt
  :version "24.1"
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,c ,c))
			     (org-e-odt-reachable-formats "odt")))))


;;;; Drawers

(defcustom org-e-odt-format-drawer-function nil
  "Function called to format a drawer in HTML code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-odt-format-drawer-default \(name contents\)
  \"Format a drawer element for HTML export.\"
  contents\)"
  :group 'org-export-e-odt
  :type 'function)


;;;; Headline

(defcustom org-e-odt-format-headline-function nil
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

\(defun org-e-odt-format-headline \(todo todo-type priority text tags\)
  \"Default format function for an headline.\"
  \(concat \(when todo
            \(format \"\\\\textbf{\\\\textsc{\\\\textsf{%s}}} \" todo\)\)
	  \(when priority
            \(format \"\\\\framebox{\\\\#%c} \" priority\)\)
	  text
	  \(when tags \(format \"\\\\hfill{}\\\\textsc{%s}\" tags\)\)\)\)"
  :group 'org-export-e-odt
  :type 'function)


;;;; Inlinetasks

(defcustom org-e-odt-format-inlinetask-function nil
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

\(defun org-e-odt-format-inlinetask \(todo type priority name tags contents\)
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
  :group 'org-export-e-odt
  :type 'function)


;;;; Links

(defcustom org-e-odt-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\)\\'"))
  "Rules characterizing image files that can be inlined into HTML.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extension *actually* allowed
depend on the way the HTML file is processed.  When used with
pdflatex, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-e-odt
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

(defcustom org-e-odt-pixels-per-inch display-pixels-per-inch
  "Scaling factor for converting images pixels to inches.
Use this for sizing of embedded images.  See Info node `(org)
Images in ODT export' for more information."
  :type 'float
  :group 'org-export-e-odt
  :version "24.1")


;;;; Plain text

(defcustom org-e-odt-quotes
  '(("fr"
     ("\\(\\s-\\|[[(]\\|^\\)\"" . "« ")
     ("\\(\\S-\\)\"" . "» ")
     ("\\(\\s-\\|(\\|^\\)'" . "'"))
    ("en"
     ("\\(\\s-\\|[[(]\\|^\\)\"" . "“")
     ("\\(\\S-\\)\"" . "”")
     ("\\(\\s-\\|(\\|^\\)'" . "‘")
     ("\\(\\S-\\)'" . "’")))
  "Alist for quotes to use when converting english double-quotes.

The CAR of each item in this alist is the language code.
The CDR of each item in this alist is a list of three CONS:
- the first CONS defines the opening quote;
- the second CONS defines the closing quote;
- the last CONS defines single quotes.

For each item in a CONS, the first string is a regexp
for allowed characters before/after the quote, the second
string defines the replacement string for this quote."
  :group 'org-export-e-odt
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


;;;; Src Block

(defcustom org-e-odt-create-custom-styles-for-srcblocks t
  "Whether custom styles for colorized source blocks be automatically created.
When this option is turned on, the exporter creates custom styles
for source blocks based on the advice of `htmlfontify'.  Creation
of custom styles happen as part of `org-e-odt-hfy-face-to-css'.

When this option is turned off exporter does not create such
styles.

Use the latter option if you do not want the custom styles to be
based on your current display settings.  It is necessary that the
styles.xml already contains needed styles for colorizing to work.

This variable is effective only if
`org-e-odt-fontify-srcblocks' is turned on."
  :group 'org-export-e-odt
  :version "24.1"
  :type 'boolean)

(defcustom org-e-odt-fontify-srcblocks t
  "Specify whether or not source blocks need to be fontified.
Turn this option on if you want to colorize the source code
blocks in the exported file.  For colorization to work, you need
to make available an enhanced version of `htmlfontify' library."
  :type 'boolean
  :group 'org-export-e-odt
  :version "24.1")


;;;; Table

(defcustom org-e-odt-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-e-odt
  :type 'boolean)

(defcustom org-e-odt-table-styles
  '(("OrgEquation" "OrgEquation"
     ((use-first-column-styles . t)
      (use-last-column-styles . t))))
  "Specify how Table Styles should be derived from a Table Template.
This is a list where each element is of the
form (TABLE-STYLE-NAME TABLE-TEMPLATE-NAME TABLE-CELL-OPTIONS).

TABLE-STYLE-NAME is the style associated with the table through
\"#+ATTR_ODT: :style TABLE-STYLE-NAME\" line.

TABLE-TEMPLATE-NAME is a set of - upto 9 - automatic
TABLE-CELL-STYLE-NAMEs and PARAGRAPH-STYLE-NAMEs (as defined
below) that is included in
`org-e-odt-content-template-file'.

TABLE-CELL-STYLE-NAME := TABLE-TEMPLATE-NAME + TABLE-CELL-TYPE +
                         \"TableCell\"
PARAGRAPH-STYLE-NAME  := TABLE-TEMPLATE-NAME + TABLE-CELL-TYPE +
                         \"TableParagraph\"
TABLE-CELL-TYPE       := \"FirstRow\"   | \"LastColumn\" |
                         \"FirstRow\"   | \"LastRow\"    |
                         \"EvenRow\"    | \"OddRow\"     |
                         \"EvenColumn\" | \"OddColumn\"  | \"\"
where \"+\" above denotes string concatenation.

TABLE-CELL-OPTIONS is an alist where each element is of the
form (TABLE-CELL-STYLE-SELECTOR . ON-OR-OFF).
TABLE-CELL-STYLE-SELECTOR := `use-first-row-styles'       |
                             `use-last-row-styles'        |
                             `use-first-column-styles'    |
                             `use-last-column-styles'     |
                             `use-banding-rows-styles'    |
                             `use-banding-columns-styles' |
                             `use-first-row-styles'
ON-OR-OFF                 := `t' | `nil'

For example, with the following configuration

\(setq org-e-odt-table-styles
      '\(\(\"TableWithHeaderRowsAndColumns\" \"Custom\"
         \(\(use-first-row-styles . t\)
          \(use-first-column-styles . t\)\)\)
        \(\"TableWithHeaderColumns\" \"Custom\"
         \(\(use-first-column-styles . t\)\)\)\)\)

1. A table associated with \"TableWithHeaderRowsAndColumns\"
   style will use the following table-cell styles -
   \"CustomFirstRowTableCell\", \"CustomFirstColumnTableCell\",
   \"CustomTableCell\" and the following paragraph styles
   \"CustomFirstRowTableParagraph\",
   \"CustomFirstColumnTableParagraph\", \"CustomTableParagraph\"
   as appropriate.

2. A table associated with \"TableWithHeaderColumns\" style will
   use the following table-cell styles -
   \"CustomFirstColumnTableCell\", \"CustomTableCell\" and the
   following paragraph styles
   \"CustomFirstColumnTableParagraph\", \"CustomTableParagraph\"
   as appropriate..

Note that TABLE-TEMPLATE-NAME corresponds to the
\"<table:table-template>\" elements contained within
\"<office:styles>\".  The entries (TABLE-STYLE-NAME
TABLE-TEMPLATE-NAME TABLE-CELL-OPTIONS) correspond to
\"table:template-name\" and \"table:use-first-row-styles\" etc
attributes of \"<table:table>\" element.  Refer ODF-1.2
specification for more information.  Also consult the
implementation filed under `org-e-odt-get-table-cell-styles'.

The TABLE-STYLE-NAME \"OrgEquation\" is used internally for
formatting of numbered display equations.  Do not delete this
style from the list."
  :group 'org-export-e-odt
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (repeat :tag "Table Styles"
                  (list :tag "Table Style Specification"
			(string :tag "Table Style Name")
			(string  :tag "Table Template Name")
			(alist :options (use-first-row-styles
					 use-last-row-styles
					 use-first-column-styles
					 use-last-column-styles
					 use-banding-rows-styles
					 use-banding-columns-styles)
			       :key-type symbol
			       :value-type (const :tag "True" t))))))



;;; Internal functions

;;;; Date

(defun org-e-odt--date (&optional org-ts fmt)
  (save-match-data
    (let* ((time
	    (and (stringp org-ts)
		 (string-match org-ts-regexp0 org-ts)
		 (apply 'encode-time
			(org-fix-decoded-time
			 (org-parse-time-string (match-string 0 org-ts) t)))))
	   date)
      (cond
       (fmt (format-time-string fmt time))
       (t (setq date (format-time-string "%Y-%m-%dT%H:%M:%S%z" time))
	  (format "%s:%s" (substring date 0 -2) (substring date -2)))))))

;;;; Frame

(defun org-e-odt--frame (text width height style &optional extra
			      anchor-type)
  (let ((frame-attrs
	 (concat
	  (if width (format " svg:width=\"%0.2fcm\"" width) "")
	  (if height (format " svg:height=\"%0.2fcm\"" height) "")
	  extra
	  (format " text:anchor-type=\"%s\"" (or anchor-type "paragraph")))))
    (format
     "\n<draw:frame draw:style-name=\"%s\"%s>\n%s\n</draw:frame>"
     style frame-attrs
     (concat text
	     (let ((title (get-text-property 0 :title text))
		   (desc (get-text-property 0 :description text)))
	       (concat (and title
			    (format "<svg:title>%s</svg:title>"
				    (org-e-odt-encode-plain-text title t)))
		       (and desc
			    (format "<svg:desc>%s</svg:desc>"
				    (org-e-odt-encode-plain-text desc t)))))))))

;;;; Library wrappers

(defun org-e-odt--adopt-elements (parent &rest children)
  (prog1 parent
    (mapc (lambda (child)
	    (let ((parent-1 (org-element-adopt-element parent child nil)))
	      (assert (eq parent-1 parent))))
	  children)))

(defun org-e-odt--zip-extract (archive members target)
  (when (atom members) (setq members (list members)))
  (mapc (lambda (archive member target)
	  (require 'arc-mode)
	  (let* ((--quote-file-name
		  ;; This is shamelessly stolen from `archive-zip-extract'.
		  (lambda (name)
		    (if (or (not (memq system-type '(windows-nt ms-dos)))
			    (and (boundp 'w32-quote-process-args)
				 (null w32-quote-process-args)))
			(shell-quote-argument name)
		      name)))
		 (target (funcall --quote-file-name target))
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
	members))

;;;; Textbox

(defun org-e-odt--textbox (text width height style &optional
				extra anchor-type)
  (org-e-odt--frame
   (format "\n<draw:text-box %s>%s\n</draw:text-box>"
	   (concat (format " fo:min-height=\"%0.2fcm\"" (or height .2))
		   (and (not width)
			(format " fo:min-width=\"%0.2fcm\"" (or width .2))))
	   text)
   width nil style extra anchor-type))



;;;; Table of Contents

(defun org-e-odt-begin-toc (index-title depth)
  (concat
   (format "
    <text:table-of-content text:style-name=\"Sect2\" text:protected=\"true\" text:name=\"Table of Contents1\">
     <text:table-of-content-source text:outline-level=\"%d\">
      <text:index-title-template text:style-name=\"Contents_20_Heading\">%s</text:index-title-template>
" depth index-title)

   (let ((levels (number-sequence 1 10)))
     (mapconcat
      (lambda (level)
	(format
	 "
      <text:table-of-content-entry-template text:outline-level=\"%d\" text:style-name=\"Contents_20_%d\">
       <text:index-entry-link-start text:style-name=\"Internet_20_link\"/>
       <text:index-entry-chapter/>
       <text:index-entry-text/>
       <text:index-entry-link-end/>
      </text:table-of-content-entry-template>
" level level)) levels ""))

   (format  "
     </text:table-of-content-source>

     <text:index-body>
      <text:index-title text:style-name=\"Sect1\" text:name=\"Table of Contents1_Head\">
       <text:p text:style-name=\"Contents_20_Heading\">%s</text:p>
      </text:index-title>
 " index-title)))

(defun org-e-odt-end-toc ()
  (format "
     </text:index-body>
    </text:table-of-content>
"))



(defun* org-e-odt-format-toc-headline
    (todo todo-type priority text tags
	  &key level section-number headline-label &allow-other-keys)
  (setq text (concat
	      (and org-export-with-section-numbers
		   (concat section-number ". "))
	      text
	      (and tags
		   (concat
		    "<text:tab/>"
		    (format "<text:span text:style-name=\"%s\">%s</text:span>"
			    "OrgTag" tags)))))
  (when todo
    (setq text (format "<text:span text:style-name=\"%s\">%s</text:span>"
		       "OrgTodo" text)))
  (org-e-odt-format-link text (concat "#" headline-label) t))

(defun org-e-odt-toc (depth info)
  (assert (wholenump depth))
  (let* ((title (org-export-translate "Table of Contents" :utf-8 info))
	 (headlines (org-export-collect-headlines info depth)))

    (when headlines
      (concat
       (org-e-odt-begin-toc title depth)

       (mapconcat
	(lambda (headline)
	  (let* ((entry (org-e-odt-format-headline--wrap
			 headline info 'org-e-odt-format-toc-headline))
		 (level (org-export-get-relative-level headline info))
		 (style (format "Contents_20_%d" level)))
	    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		    style entry)))
	headlines "\n")

       (org-e-odt-end-toc)))))


;;;; Document styles

(defun org-e-odt-add-automatic-style (object-type &optional object-props)
  "Create an automatic style of type OBJECT-TYPE with param OBJECT-PROPS.
OBJECT-PROPS is (typically) a plist created by passing
\"#+ATTR_ODT: \" option of the object in question to
`org-e-odt-parse-block-attributes'.

Use `org-e-odt-object-counters' to generate an automatic
OBJECT-NAME and STYLE-NAME.  If OBJECT-PROPS is non-nil, add a
new entry in `org-e-odt-automatic-styles'.  Return (OBJECT-NAME
. STYLE-NAME)."
  (assert (stringp object-type))
  (let* ((object (intern object-type))
	 (seqvar object)
	 (seqno (1+ (or (plist-get org-e-odt-object-counters seqvar) 0)))
	 (object-name (format "%s%d" object-type seqno)) style-name)
    (setq org-e-odt-object-counters
	  (plist-put org-e-odt-object-counters seqvar seqno))
    (when object-props
      (setq style-name (format "Org%s" object-name))
      (setq org-e-odt-automatic-styles
	    (plist-put org-e-odt-automatic-styles object
		       (append (list (list style-name object-props))
			       (plist-get org-e-odt-automatic-styles object)))))
    (cons object-name style-name)))


;;;; Caption and Labels


(defun org-e-odt--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-odt--caption/label-string'."
  ;; (let ((label (org-element-property :name element)))
  ;;   (if (or (not output) (not label) (string= output "") (string= label ""))
  ;; 	output
  ;;     (concat (format "\\label{%s}\n" label) output)))
  output)


(defun org-e-odt--caption/label-string (caption label info)
  "Return caption and label HTML string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-odt--wrap-label'."
  (setq label nil) ;; FIXME

  (let ((label-str (if label (format "\\label{%s}" label) "")))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\label{%s}\n" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "\\caption[%s]{%s%s}\n"
	      (org-export-data (cdr caption) info)
	      label-str
	      (org-export-data (car caption) info)))
     ;; Standard caption format.
     ;; (t (format "\\caption{%s%s}\n"
     ;; 		label-str
     ;; 		(org-export-data (car caption) info)))
     (t (org-export-data (car caption) info)))))

;;;; Checkbox

(defun org-e-odt--checkbox (item)
  "Return check-box string associated to ITEM."
  (let ((checkbox (org-element-property :checkbox item)))
    (if (not checkbox) ""
      (format "<text:span text:style-name=\"%s\">%s</text:span>"
	      "OrgCode" (case checkbox
			  (on "[&#x2713;] ") ; CHECK MARK
			  (off "[ ] ")
			  (trans "[-] "))))))



;;; Template

(defun org-e-odt-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."
  ;; Write meta file.
  (let ((title (org-export-data (plist-get info :title) info))
	(author (let ((author (plist-get info :author)))
		  (if (not author) "" (org-export-data author info))))
	(date (org-e-odt--date
	       (org-export-data (plist-get info :date) info)))
	(email (plist-get info :email))
	(keywords (plist-get info :keywords))
	(description (plist-get info :description)))
    (write-region
     (concat
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <office:document-meta
         xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\"
         xmlns:xlink=\"http://www.w3.org/1999/xlink\"
         xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
         xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\"
         xmlns:ooo=\"http://openoffice.org/2004/office\"
         office:version=\"1.2\">
       <office:meta>\n"
      (format "<dc:creator>%s</dc:creator>\n" author)
      (format "<meta:initial-creator>%s</meta:initial-creator>\n" author)
      (format "<dc:date>%s</dc:date>\n" date)
      (format "<meta:creation-date>%s</meta:creation-date>\n" date)
      (format "<meta:generator>%s</meta:generator>\n"
	      (let ((creator-info (plist-get info :with-creator)))
		(if (or (not creator-info) (eq creator-info 'comment)) ""
		  (plist-get info :creator))))
      (format "<meta:keyword>%s</meta:keyword>\n" keywords)
      (format "<dc:subject>%s</dc:subject>\n" description)
      (format "<dc:title>%s</dc:title>\n" title)
      "\n"
      "  </office:meta>\n" "</office:document-meta>")
     nil (concat org-e-odt-zip-dir "meta.xml"))
    ;; Add meta.xml in to manifest.
    (org-e-odt-create-manifest-file-entry "text/xml" "meta.xml"))

  ;; Update styles file.
  ;; Copy styles.xml.  Also dump htmlfontify styles, if there is any.
  ;; Write styles file.
  (let* ((styles-file (plist-get info :odt-styles-file))
	 (styles-file (and styles-file (read (org-trim styles-file))))
	 ;; Non-availability of styles.xml is not a critical
	 ;; error. For now throw an error purely for aesthetic
	 ;; reasons.
	 (styles-file (or styles-file
			  org-e-odt-styles-file
			  (expand-file-name "OrgOdtStyles.xml"
					    org-e-odt-styles-dir)
			  (error "org-e-odt: Missing styles file?"))))
    (cond
     ((listp styles-file)
      (let ((archive (nth 0 styles-file))
	    (members (nth 1 styles-file)))
	(org-e-odt--zip-extract archive members org-e-odt-zip-dir)
	(mapc
	 (lambda (member)
	   (when (org-file-image-p member)
	     (let* ((image-type (file-name-extension member))
		    (media-type (format "image/%s" image-type)))
	       (org-e-odt-create-manifest-file-entry media-type member))))
	 members)))
     ((and (stringp styles-file) (file-exists-p styles-file))
      (let ((styles-file-type (file-name-extension styles-file)))
	(cond
	 ((string= styles-file-type "xml")
	  (copy-file styles-file (concat org-e-odt-zip-dir "styles.xml") t))
	 ((member styles-file-type '("odt" "ott"))
	  (org-e-odt--zip-extract styles-file "styles.xml" org-e-odt-zip-dir)))))
     (t
      (error (format "Invalid specification of styles.xml file: %S"
		     org-e-odt-styles-file))))

    ;; create a manifest entry for styles.xml
    (org-e-odt-create-manifest-file-entry "text/xml" "styles.xml")

    ;; FIXME: Who is opening an empty styles.xml before this point?
    (with-current-buffer
	(find-file-noselect (concat org-e-odt-zip-dir "styles.xml") t)
      (revert-buffer t t)

      ;; Write custom styles for source blocks
      ;; Save STYLES used for colorizing of source blocks.
      ;; Update styles.xml with styles that were collected as part of
      ;; `org-e-odt-hfy-face-to-css' callbacks.
      (let ((styles (mapconcat (lambda (style) (format " %s\n" (cddr style)))
			       hfy-user-sheet-assoc "")))
	(when styles
	  (goto-char (point-min))
	  (when (re-search-forward "</office:styles>" nil t)
	    (goto-char (match-beginning 0))
	    (insert "\n<!-- Org Htmlfontify Styles -->\n" styles "\n"))))

      ;; Update styles.xml - take care of outline numbering

      ;; Don't make automatic backup of styles.xml file. This setting
      ;; prevents the backed-up styles.xml file from being zipped in to
      ;; odt file. This is more of a hackish fix. Better alternative
      ;; would be to fix the zip command so that the output odt file
      ;; includes only the needed files and excludes any auto-generated
      ;; extra files like backups and auto-saves etc etc. Note that
      ;; currently the zip command zips up the entire temp directory so
      ;; that any auto-generated files created under the hood ends up in
      ;; the resulting odt file.
      (set (make-local-variable 'backup-inhibited) t)

      ;; Outline numbering is retained only upto LEVEL.
      ;; To disable outline numbering pass a LEVEL of 0.

      (goto-char (point-min))
      (let ((regex
	     "<text:outline-level-style\\([^>]*\\)text:level=\"\\([^\"]*\\)\"\\([^>]*\\)>")
	    (replacement
	     "<text:outline-level-style\\1text:level=\"\\2\" style:num-format=\"\">"))
	(while (re-search-forward regex nil t)
	  (unless (let ((sec-num (plist-get info :section-numbers))
			(level (string-to-number (match-string 2))))
		    (if (wholenump sec-num) (<= level sec-num) sec-num))
	    (replace-match replacement t nil))))
      (save-buffer 0)))
  ;; Update content.xml.
  (with-temp-buffer
    (insert-file-contents
     (or org-e-odt-content-template-file
	 (expand-file-name "OrgOdtContentTemplate.xml"
			   org-e-odt-styles-dir)))
    ;; Write automatic styles.
    ;; - Position the cursor.
    (goto-char (point-min))
    (re-search-forward "  </office:automatic-styles>" nil t)
    (goto-char (match-beginning 0))
    ;; - Dump automatic table styles
    (loop for (style-name props) in
	  (plist-get org-e-odt-automatic-styles 'Table) do
	  (when (setq props (or (plist-get props :rel-width) 96))
	    (insert (format org-e-odt-table-style-format style-name props))))
    ;; Update display level.
    ;; - Remove existing sequence decls.  Also position the cursor.
    (goto-char (point-min))
    (when (re-search-forward "<text:sequence-decls" nil t)
      (delete-region (match-beginning 0)
		     (re-search-forward "</text:sequence-decls>" nil nil)))
    ;; Update sequence decls according to user preference.
    (insert
     (format
      "\n<text:sequence-decls>\n%s\n</text:sequence-decls>"
      (mapconcat
       (lambda (x)
	 (format
	  "<text:sequence-decl text:display-outline-level=\"%d\" text:name=\"%s\"/>"
	  org-e-odt-display-outline-level (nth 1 x)))
       org-e-odt-category-map-alist "\n")))
    ;; Position the cursor to document body.
    (goto-char (point-min))
    (re-search-forward "</office:text>" nil nil)
    (goto-char (match-beginning 0))

    ;; Preamble - Title, Author, Date etc.
    (insert
     (let* ((title (org-export-data (plist-get info :title) info))
	    (author (and (plist-get info :with-author)
			 (let ((auth (plist-get info :author)))
			   (and auth (org-export-data auth info)))))
	    (date (org-export-data (plist-get info :date) info))
	    (iso-date (org-e-odt--date date))
	    (date (org-e-odt--date date "%d %b %Y"))
	    (email (plist-get info :email))
	    ;; switch on or off above vars based on user settings
	    (author (and (plist-get info :with-author) (or author email)))
	    ;; (date (and (plist-get info :time-stamp-file) date))
	    (email (and (plist-get info :with-email) email)))
       (concat
	;; title
	(when title
	  (concat
	   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		   "OrgTitle" (format "\n<text:title>%s</text:title>" title))
	   ;; separator
	   "\n<text:p text:style-name=\"OrgTitle\"/>"))
	(cond
	 ((and author (not email))
	  ;; author only
	  (concat
	   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		   "OrgSubtitle"
		   (format "<text:initial-creator>%s</text:initial-creator>" author))
	   ;; separator
	   "\n<text:p text:style-name=\"OrgSubtitle\"/>"))
	 ((and author email)
	  ;; author and email
	  (concat
	   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		   "OrgSubtitle"
		   (org-e-odt-format-link
		    (format "<text:initial-creator>%s</text:initial-creator>" author)
		    (concat "mailto:" email)))
	   ;; separator
	   "\n<text:p text:style-name=\"OrgSubtitle\"/>")))
	;; date
	(when date
	  (concat
	   (format
	    "\n<text:p text:style-name=\"%s\">%s</text:p>"
	    "OrgSubtitle"
	    (format
	     "\n<text:date style:data-style-name=\"%s\" text:date-value=\"%s\">%s</text:date>"

	     "N75" iso-date date))
	   ;; separator
	   "<text:p text:style-name=\"OrgSubtitle\"/>")))))

    ;; Table of Contents
    (let ((depth (plist-get info :with-toc)))
      (when (wholenump depth) (insert (org-e-odt-toc depth info))))
    ;; Contents.
    (insert contents)
    ;; Return contents.
    (buffer-substring-no-properties (point-min) (point-max))))



;;; Transcode Functions

;;;; Bold

(defun org-e-odt-bold (bold contents info)
  "Transcode BOLD from Org to ODT.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Bold" contents))


;;;; Center Block

(defun org-e-odt-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to ODT.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-e-odt--wrap-label center-block contents))


;;;; Clock

(defun org-e-odt-clock (clock contents info)
  "Transcode a CLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgTimestampWrapper"
	  (concat
	   (format "<text:span text:style-name=\"%s\">%s</text:span>"
		   "OrgTimestampKeyword" org-clock-string)
	   (format "<text:span text:style-name=\"%s\">%s</text:span>"
		   "OrgTimestamp"
		   (concat (org-translate-time
			    (org-element-property :value clock))
			   (let ((time (org-element-property :time clock)))
			     (and time (format " (%s)" time))))))))


;;;; Code

(defun org-e-odt-code (code contents info)
  "Transcode a CODE object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgCode" (org-element-property :value code)))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-odt-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (if (functionp org-e-odt-format-drawer-function)
		     (funcall org-e-odt-format-drawer-function
			      name contents)
		   ;; If there's no user defined function: simply
		   ;; display contents of the drawer.
		   contents)))
    (org-e-odt--wrap-label drawer output)))


;;;; Dynamic Block

(defun org-e-odt-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-e-odt--wrap-label dynamic-block contents))


;;;; Entity

(defun org-e-odt-entity (entity contents info)
  "Transcode an ENTITY object from Org to ODT.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  ;; (let ((ent (org-element-property :latex entity)))
  ;;   (if (org-element-property :latex-math-p entity)
  ;; 	(format "$%s$" ent)
  ;;     ent))
  (org-element-property :utf-8 entity))


;;;; Example Block

(defun org-e-odt-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-odt--wrap-label
   example-block (org-e-odt-format-code example-block info)))


;;;; Export Snippet

(defun org-e-odt-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-odt)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-e-odt-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "ODT")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-e-odt-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-odt--wrap-label
   fixed-width (org-e-odt-do-format-code
		(org-element-property :value fixed-width))))


;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-e-odt-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((--format-footnote-definition
	 (function
	  (lambda (n def)
	    (setq n (format "%d" n))
	    (let ((id (concat  "fn" n))
		  (note-class "footnote")
		  (par-style "Footnote"))
	      (format
	       "<text:note text:id=\"%s\" text:note-class=\"%s\">%s</text:note>"
	       id note-class
	       (concat
		(format "<text:note-citation>%s</text:note-citation>" n)
		(format "<text:note-body>%s</text:note-body>" def)))))))
	(--format-footnote-reference
	 (function
	  (lambda (n)
	    (setq n (format "%d" n))
	    (let ((note-class "footnote")
		  (ref-format "text")
		  (ref-name (concat "fn" n)))
	      (format
	       "<text:span text:style-name=\"%s\">%s</text:span>"
	       "OrgSuperscript"
	       (format "<text:note-ref text:note-class=\"%s\" text:reference-format=\"%s\" text:ref-name=\"%s\">%s</text:note-ref>"
		       note-class ref-format ref-name n)))))))
    (concat
     ;; Insert separator between two footnotes in a row.
     (let ((prev (org-export-get-previous-element footnote-reference info)))
       (and (eq (org-element-type prev) 'footnote-reference)
	    (format "<text:span text:style-name=\"%s\">%s</text:span>"
		    "OrgSuperscript" ",")))
     ;; Trancode footnote reference.
     (let ((n (org-export-get-footnote-number footnote-reference info)))
       (cond
	((not (org-export-footnote-first-reference-p footnote-reference info))
	 (funcall --format-footnote-reference n))
	;; Inline definitions are secondary strings.
	;; Non-inline footnotes definitions are full Org data.
	(t
	 (let* ((raw (org-export-get-footnote-definition footnote-reference
							 info))
		(def (let ((def (org-trim (org-export-data raw info))))
		       (if (eq (org-element-type raw) 'org-data) def
			 (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
				 "Footnote" def)))))
	   (funcall --format-footnote-definition n def))))))))


;;;; Headline

(defun* org-e-odt-format-headline
    (todo todo-type priority text tags
	  &key level section-number headline-label &allow-other-keys)
  (concat
   ;; Todo.
   (and todo
	(concat
	 (let ((style (if (member todo org-done-keywords) "OrgDone" "OrgTodo")))
	   (format "<text:span text:style-name=\"%s\">%s</text:span>"
		   style todo)) " "))
   ;; Title.
   text
   ;; Tags.
   (and tags
	(concat "<text:tab/>"
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgTag" (mapconcat 'org-trim tags " : "))))))

(defun org-e-odt-format-headline--wrap (headline info
						 &optional format-function
						 &rest extra-keys)
  "Transcode an HEADLINE element from Org to ODT.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (+ (org-export-get-relative-level headline info)))
	 (headline-number (org-export-get-headline-number headline info))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 headline-number ".")))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data (org-element-property :title headline) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (headline-label (concat "sec-" (mapconcat 'number-to-string
						   headline-number "-")))
	 (format-function (cond
			   ((functionp format-function) format-function)
			   ((functionp org-e-odt-format-headline-function)
			    (function*
			     (lambda (todo todo-type priority text tags
					   &allow-other-keys)
			       (funcall org-e-odt-format-headline-function
					todo todo-type priority text tags))))
			   (t 'org-e-odt-format-headline))))
    (apply format-function
    	   todo todo-type  priority text tags
    	   :headline-label headline-label :level level
    	   :section-number section-number extra-keys)))


(defun org-e-odt-begin-plain-list (ltype &optional continue-numbering)
  (unless (member ltype '(ordered unordered descriptive))
    (error "Unknown list type: %s"  ltype))
  (let ((style-name (assoc-default ltype
				   '((ordered . "OrgNumberedList")
				     (unordered . "OrgBulletedList")
				     (descriptive . "OrgDescriptionList")))))
    (format "<text:list text:style-name=\"%s\" text:continue-numbering=\"%s\">"
	    style-name (if continue-numbering "true" "false"))))

(defun org-e-odt-headline (headline contents info)
  "Transcode an HEADLINE element from Org to ODT.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 ;; Get level relative to current parsed data.
	 (level (org-export-get-relative-level headline info))
	 (text (org-export-data (org-element-property :title headline) info))
	 ;; Create the headline text.
	 (full-text (org-e-odt-format-headline--wrap headline info)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ;; FIXME
     ;; ((org-export-low-level-p headline info)
     ;;  ;; Build the real contents of the sub-tree.
     ;;  (let* ((type (if numberedp 'unordered 'unordered)) ; FIXME
     ;; 	     (itemized-body (org-e-odt-format-list-item
     ;; 			     contents type nil nil full-text)))
     ;; 	(concat
     ;; 	 (and (org-export-first-sibling-p headline info)
     ;; 	      (org-e-odt-begin-plain-list type))
     ;; 	 itemized-body
     ;; 	 (and (org-export-last-sibling-p headline info)
     ;; 	      "</text:list>"))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((extra-ids (list (org-element-property :custom-id headline)
			      (org-element-property :id headline)))
	     (extra-ids nil)		; FIXME
	     (id (concat "sec-" (mapconcat 'number-to-string
					   (org-export-get-headline-number
					    headline info) "-"))))
	(concat
	 (format
	  "\n<text:h text:style-name=\"%s\" text:outline-level=\"%s\">%s%s</text:h>"
	  (format "Heading_20_%s" level)
	  level
	  ;; Extra targets.
	  (mapconcat (lambda (x)
		       (when x
			 (let ((x (if (org-uuidgen-p x) (concat "ID-" x) x)))
			   (org-e-odt-format-target
			    "" (org-export-solidify-link-text x)))))
		     extra-ids "")
	  ;; Title.
	  (org-e-odt-format-target full-text id))
	 contents))))))


;;;; Horizontal Rule

(defun org-e-odt-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-odt--wrap-label
   horizontal-rule
   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	   "Horizontal_20_Line" "")))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-odt--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

(defun org-e-odt-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block))
	 (separator (org-e-odt--find-verb-separator code)))
    (error "FIXME")))


;;;; Inlinetask

(defun org-e-odt-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (cond
   ;; If `org-e-odt-format-inlinetask-function' is provided, call it
   ;; with appropriate arguments.
   ((functionp org-e-odt-format-inlinetask-function)
    (let ((format-function
	   (function*
	    (lambda (todo todo-type priority text tags
			  &key contents &allow-other-keys)
	      (funcall org-e-odt-format-inlinetask-function
		       todo todo-type priority text tags contents)))))
      (org-e-odt-format-headline--wrap
       inlinetask info format-function :contents contents)))
   ;; Otherwise, use a default template.
   (t (org-e-odt--wrap-label
       inlinetask
       (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	       "Text_20_body"
	       (org-e-odt--textbox
		(concat
		 (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			 "OrgInlineTaskHeading"
			 (org-e-odt-format-headline--wrap
			  inlinetask info))
		 contents)
		nil nil "OrgInlineTaskFrame" " style:rel-width=\"100%\""))))))

;;;; Italic

(defun org-e-odt-italic (italic contents info)
  "Transcode ITALIC from Org to ODT.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Emphasis" contents))


;;;; Item

(defun org-e-odt-item (item contents info)
  "Transcode an ITEM element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag
		     (concat (org-e-odt--checkbox item)
			     (org-export-data tag info))))))
    (case type
      ((ordered unordered)
       (format "\n<text:list-item>\n%s\n%s"
	       contents
	       (let* ((--element-has-a-table-p
		       (function
			(lambda (element info)
			  (loop for el in (org-element-contents element)
				thereis (eq (org-element-type el) 'table))))))
		 (cond
		  ((funcall --element-has-a-table-p item info)
		   "</text:list-header>")
		  (t "</text:list-item>")))))
      (descriptive
       (concat
	(let ((term (or tag "(no term)")))
	  (concat
	   (format "\n<text:list-item>\n%s\n</text:list-item>"
		   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			   "Text_20_body_20_bold" term))
	   (format
	    "\n<text:list-item>\n%s\n</text:list-item>"
	    (format "\n<text:list text:style-name=\"%s\" %s>\n%s\n</text:list>"
		    "OrgDescriptionList"
		    "text:continue-numbering=\"false\""
		    (format "\n<text:list-item>\n%s\n</text:list-item>"
			    contents)))))))
      (t (error "Unknown list type: %S" type)))))


;;;; Keyword

(defun org-e-odt-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "LATEX") value)
     ((string= key "INDEX") (format "\\index{%s}" value))
     ((string= key "TARGET") nil	; FIXME
      ;; (format "\\label{%s}" (org-export-solidify-link-text value))
      )
     ((string= key "toc")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (when (wholenump depth) (org-e-odt-toc depth info))))
	 ((string= "tables" value) "FIXME")
	 ((string= "figures" value) "FIXME")
	 ((string= "listings" value)
	  (cond
	   ;; At the moment, src blocks with a caption are wrapped
	   ;; into a figure environment.
	   (t "FIXME")))))))))


;;;; Latex Environment


(eval-after-load 'org-odt
  '(ad-deactivate 'org-format-latex-as-mathml))

;; (defadvice org-format-latex-as-mathml	; FIXME
;;   (after org-e-odt-protect-latex-fragment activate)
;;   "Encode LaTeX fragment as XML.
;; Do this when translation to MathML fails."
;;   (when (or (not (> (length ad-return-value) 0))
;; 	    (get-text-property 0 'org-protected ad-return-value))
;;     (setq ad-return-value
;; 	  (org-propertize (org-e-odt-encode-plain-text (ad-get-arg 0))
;; 			  'org-protected t))))

(defun org-e-odt-format-latex (latex-frag processing-type info)
  (let* ((prefix (case processing-type
		   (dvipng "ltxpng/")
		   (mathml "ltxmathml/")))
	 (input-file (plist-get info :input-file))
	 (cache-subdir
	  (concat prefix (file-name-sans-extension
			  (file-name-nondirectory input-file))))
	 (cache-dir (file-name-directory input-file))
	 (display-msg (case processing-type
			(dvipng "Creating LaTeX Image...")
			(mathml "Creating MathML snippet..."))))
    (with-temp-buffer
      (insert latex-frag)
      (org-format-latex cache-subdir cache-dir nil display-msg
			nil nil processing-type)
      (buffer-string))))

(defun org-e-odt-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-odt--wrap-label
   latex-environment
   (let* ((latex-frag
	   (org-remove-indentation
	    (org-element-property :value latex-environment)))
	  (processing-type (plist-get info :LaTeX-fragments))
	  (caption (org-element-property :caption latex-environment))
	  (short-caption (and (cdr caption)
			      (org-export-data (cdr caption) info)))
	  (caption (and (car caption) (org-export-data (car caption) info)))
	  (label (org-element-property :name latex-environment))
	  (attr nil)			; FIXME
	  (label (org-element-property :name latex-environment)))

     (when (memq processing-type '(t mathjax))
       (unless (and (fboundp 'org-format-latex-mathml-available-p)
		    (org-format-latex-mathml-available-p))
	 (message "LaTeX to MathML converter not available.  Trying dvinpng...")
	 (setq processing-type 'dvipng)))

     (when (eq processing-type 'dvipng)
       (unless (and (org-check-external-command "latex" "" t)
		    (org-check-external-command "dvipng" "" t))
	 (message "LaTeX to PNG converter not available.  Using verbatim.")
	 (setq processing-type 'verbatim)))

     (case processing-type
       ((t mathjax)
	(org-e-odt-format-formula latex-environment info))
       (dvipng
	(format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		"Text_20_body"
		(org-e-odt-link--inline-image latex-environment info)))
       (t (org-e-odt-do-format-code latex-frag))))))


;;;; Latex Fragment


;; (when latex-frag			; FIXME
;; 	(setq href (org-propertize href :title "LaTeX Fragment"
;; 				   :description latex-frag)))
;; handle verbatim
;; provide descriptions

(defun org-e-odt-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((latex-frag (org-element-property :value latex-fragment))
	 (processing-type (plist-get info :LaTeX-fragments)))
    (cond
     ((member processing-type '(t mathjax))
      (org-e-odt-format-formula latex-fragment info))
     ((eq processing-type 'dvipng)
      (org-e-odt-link--inline-image latex-fragment info))
     (t (org-e-odt-encode-plain-text latex-frag t)))))


;;;; Line Break

(defun org-e-odt-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "<text:line-break/>\n")


;;;; Link



;;;; Links :: Generic

(defun org-e-odt-format-link (desc href &optional suppress-xref)
  (cond
   ((and (= (string-to-char href) ?#) (not suppress-xref))
    (setq href (substring href 1))
    (let ((xref-format "text"))
      (when (numberp desc)
	(setq desc (format "%d" desc) xref-format "number"))
      (when (listp desc)
	(setq desc (mapconcat 'number-to-string desc ".") xref-format "chapter"))
      (setq href (concat org-e-odt-bookmark-prefix href))
      (format
       "<text:bookmark-ref text:reference-format=\"%s\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
       xref-format href desc)))
   ;; (org-lparse-link-description-is-image
   ;;  (format "\n<draw:a xlink:type=\"simple\" xlink:href=\"%s\">\n%s\n</draw:a>"
   ;; 	    href desc))
   (t (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
	      href desc))))

(defun org-e-odt-format-internal-link (text href)
  (org-e-odt-format-link text (concat "#" href)))

;;;; Links :: Label references

(defun org-e-odt-enumerate-element (element info &optional predicate n)
  (let* ((--numbered-parent-headline-at-<=-n
	  (function
	   (lambda (element n info)
	     (loop for x in (org-export-get-genealogy element)
		   thereis (and (eq (org-element-type x) 'headline)
		   		(<= (org-export-get-relative-level x info) n)
		   		(org-export-numbered-headline-p x info)
		   		x)))))
	 (--enumerate
	  (function
	   (lambda (element scope info &optional predicate)
	     (let ((counter 0))
	       (org-element-map
		(or scope (plist-get info :parse-tree))
		(org-element-type element)
		(lambda (el)
		  (and (or (not predicate) (funcall predicate el info))
		       (incf counter)
		       (eq element el)
		       counter))
		info 'first-match)))))
	 (scope (funcall --numbered-parent-headline-at-<=-n
			 element (or n org-e-odt-display-outline-level) info))
	 (ordinal (funcall --enumerate element scope info predicate))
	 (tag
	  (concat
	   ;; Section number.
	   (and scope
		(mapconcat 'number-to-string
			   (org-export-get-headline-number scope info) "."))
	   ;; Separator.
	   (and scope ".")
	   ;; Ordinal.
	   (number-to-string ordinal))))
    tag))

(defun org-e-odt-format-label (element info op)
  (let* ((caption-from
	  (case (org-element-type element)
	    (link (org-export-get-parent-element element))
	    (t element)))
	 ;; get label and caption.
	 (label (org-element-property :name caption-from))
	 (caption (org-element-property :caption caption-from))
	 (short-caption (cdr caption))
	 ;; transcode captions.
	 (caption (and (car caption) (org-export-data (car caption) info)))
	 (short-caption (and short-caption
			     (org-export-data short-caption info))))
    (when (or label caption)
      (let* ((default-category
	       (cond
		((eq (org-element-type element) 'table)
		 "__Table__")
		((org-e-odt-standalone-image-p element info)
		 "__Figure__")
		((member (org-element-type element)
			 '(latex-environment latex-fragment))
		 (let ((processing-type (plist-get info :LaTeX-fragments)))
		   (cond
		    ((eq processing-type 'dvipng) "__DvipngImage__")
		    ((eq processing-type 'mathjax) "__MathFormula__")
		    ((eq processing-type 't) "__MathFormula__")
		    (t (error "Handle LaTeX:verbatim")))))
		((eq (org-element-type element) 'src-block)
		 "__Listing__")
		(t (error "Handle enumeration of %S" element))))
	     (predicate
	      (cond
	       ((member (org-element-type element)
			'(table latex-environment src-block))
		nil)
	       ((org-e-odt-standalone-image-p element info)
		'org-e-odt-standalone-image-p)
	       (t (error "Handle enumeration of %S" element))))
	     (seqno (org-e-odt-enumerate-element
		     element info predicate)) ; FIXME
	     ;; handle label props.
	     (label-props (assoc default-category org-e-odt-category-map-alist))
	     ;; identify opendocument counter
	     (counter (nth 1 label-props))
	     ;; identify label style
	     (label-style (nth 2 label-props))
	     ;; retrieve localized category sting
	     (category (org-export-translate (nth 3 label-props) :utf-8 info)))
	(case op
	  (definition
	    ;; assign an internal label, if user has not provided one
	    (setq label (or label (format  "%s-%s" default-category seqno)))
	    (setq label (org-export-solidify-link-text label))

	    (cons
	     (format-spec
	      (cadr (assoc-string label-style org-e-odt-label-styles t))
	      `((?e . ,category)
		(?n . ,(format
			"<text:sequence text:ref-name=\"%s\" text:name=\"%s\" text:formula=\"ooow:%s+1\" style:num-format=\"1\">%s</text:sequence>"
			label counter counter seqno))
		(?c . ,(or caption ""))))
	     short-caption))
	  (reference
	   (assert label)
	   (setq label (org-export-solidify-link-text label))
	   (let* ((fmt (cddr (assoc-string label-style org-e-odt-label-styles t)))
		  (fmt1 (car fmt))
		  (fmt2 (cadr fmt)))
	     (format "<text:sequence-ref text:reference-format=\"%s\" text:ref-name=\"%s\">%s</text:sequence-ref>"
		     fmt1 label (format-spec fmt2 `((?e . ,category)
						    (?n . ,seqno))))))
	  (t (error "Unknow %S on label" op)))))))

;;;; Links :: Embedded images

(defun org-e-odt-copy-image-file (path)
  "Returns the internal name of the file"
  (let* ((image-type (file-name-extension path))
	 (media-type (format "image/%s" image-type))
	 (target-dir "Images/")
	 (target-file
	  (format "%s%04d.%s" target-dir
		  (incf org-e-odt-embedded-images-count) image-type)))
    (message "Embedding %s as %s ..."
	     (substring-no-properties path) target-file)

    (when (= 1 org-e-odt-embedded-images-count)
      (make-directory (concat org-e-odt-zip-dir target-dir))
      (org-e-odt-create-manifest-file-entry "" target-dir))

    (copy-file path (concat org-e-odt-zip-dir target-file) 'overwrite)
    (org-e-odt-create-manifest-file-entry media-type target-file)
    target-file))

(defun org-e-odt-image-size-from-file (file &optional user-width
					    user-height scale dpi embed-as)
  (let* ((--pixels-to-cms
	  (function (lambda (pixels dpi)
		      (let ((cms-per-inch 2.54)
			    (inches (/ pixels dpi)))
			(* cms-per-inch inches)))))
	 (--size-in-cms
	  (function
	   (lambda (size-in-pixels dpi)
	     (and size-in-pixels
		  (cons (funcall --pixels-to-cms (car size-in-pixels) dpi)
			(funcall --pixels-to-cms (cdr size-in-pixels) dpi))))))
	 (dpi (or dpi org-e-odt-pixels-per-inch))
	 (anchor-type (or embed-as "paragraph"))
	 (user-width (and (not scale) user-width))
	 (user-height (and (not scale) user-height))
	 (size
	  (and
	   (not (and user-height user-width))
	   (or
	    ;; Use Imagemagick.
	    (and (executable-find "identify")
		 (let ((size-in-pixels
			(let ((dim (shell-command-to-string
				    (format "identify -format \"%%w:%%h\" \"%s\""
					    file))))
			  (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" dim)
			    (cons (string-to-number (match-string 1 dim))
				  (string-to-number (match-string 2 dim)))))))
		   (funcall --size-in-cms size-in-pixels dpi)))
	    ;; Use Emacs.
	    (let ((size-in-pixels
		   (ignore-errors	; Emacs could be in batch mode
		     (clear-image-cache)
		     (image-size (create-image file) 'pixels))))
	      (funcall --size-in-cms size-in-pixels dpi))
	    ;; Use hard-coded values.
	    (cdr (assoc-string anchor-type
			       org-e-odt-default-image-sizes-alist))
	    ;; Error out.
	    (error "Cannot determine image size, aborting"))))
	 (width (car size)) (height (cdr size)))
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
    ;; ensure that an embedded image fits comfortably within a page
    (let ((max-width (car org-e-odt-max-image-size))
	  (max-height (cdr org-e-odt-max-image-size)))
      (when (or (> width max-width) (> height max-height))
	(let* ((scale1 (/ max-width width))
	       (scale2 (/ max-height height))
	       (scale (min scale1 scale2)))
	  (setq width (* scale width) height (* scale height)))))
    (cons width height)))

;;;; Links :: Math formula

(defun org-e-odt-format-formula (element info)
  (let* ((src (cond
	       ((eq (org-element-type element) 'link) ; FIXME
		(let* ((type (org-element-property :type element))
		       (raw-path (org-element-property :path element)))
		  (cond
		   ((file-name-absolute-p raw-path)
		    (expand-file-name raw-path))
		   (t raw-path))))
	       ((member (org-element-type element)
			'(latex-fragment latex-environment))
		(let* ((latex-frag (org-remove-indentation
				    (org-element-property :value element)))
		       (formula-link (org-e-odt-format-latex
				      latex-frag 'mathml info)))
		  (and formula-link
		       (string-match "file:\\([^]]*\\)" formula-link)
		       (match-string 1 formula-link))))
	       (t (error "what is this?"))))
	 (full-src (if (file-name-absolute-p src) src
		     (expand-file-name src (file-name-directory
					    (plist-get info :input-file)))))
	 (caption-from
	  (case (org-element-type element)
	    (link (org-export-get-parent-element element))
	    (t element)))
	 (captions (org-e-odt-format-label caption-from info 'definition))
	 (caption (car captions))
	 (href
	  (format "\n<draw:object %s xlink:href=\"%s\" xlink:type=\"simple\"/>"
		  " xlink:show=\"embed\" xlink:actuate=\"onLoad\""
		  (file-name-directory (org-e-odt-copy-formula-file full-src))))
	 (embed-as (if caption 'paragraph 'character))
	 width height)
    (cond
     ((eq embed-as 'character)
      (org-e-odt-format-entity "InlineFormula" href width height))
     (t
      (let* ((equation (org-e-odt-format-entity
			"CaptionedDisplayFormula" href width height captions))
	     (label
	      (let* ((org-e-odt-category-map-alist
		      '(("__Table__" "Table" "value")
			("__Figure__" "Illustration" "value")
			("__MathFormula__" "Text" "math-label")
			("__DvipngImage__" "Equation" "value")
			("__Listing__" "Listing" "value"))))
		(car (org-e-odt-format-label caption-from info 'definition))))
	     (formula-tree
	      (org-e-odt--adopt-elements
	       `(table (:type org :attr_odt (":style \"OrgEquation\"")))
	       (org-e-odt--adopt-elements
		`(table-row (:type standard))
		`(table-cell nil "<c8>") `(table-cell nil "<c1>"))
	       (org-e-odt--adopt-elements
		`(table-row (:type standard))
		(org-e-odt--adopt-elements
		 `(table-cell nil) `(export-block
				     (:type "ODT" :value ,equation)))
		(org-e-odt--adopt-elements
		 `(table-cell nil) `(export-block
				     (:type "ODT" :value ,label))))))
	     (formula-info
	      (org-export-collect-tree-properties
	       formula-tree (org-export-get-environment 'e-odt))))
	(org-export-data formula-tree formula-info))))))

(defun org-e-odt-copy-formula-file (src-file)
  "Returns the internal name of the file"
  (let* ((target-dir (format "Formula-%04d/"
			     (incf org-e-odt-embedded-formulas-count)))
	 (target-file (concat target-dir "content.xml")))
    ;; Create a directory for holding formula file.  Also enter it in
    ;; to manifest.
    (make-directory (concat org-e-odt-zip-dir target-dir))
    (org-e-odt-create-manifest-file-entry
     "application/vnd.oasis.opendocument.formula" target-dir "1.2")
    ;; Copy over the formula file from user directory to zip
    ;; directory.
    (message "Embedding %s as %s ..." src-file target-file)
    (let ((case-fold-search nil))
      (cond
       ;; Case 1: Mathml.
       ((string-match "\\.\\(mathml\\|mml\\)\\'" src-file)
	(copy-file src-file (concat org-e-odt-zip-dir target-file) 'overwrite))
       ;; Case 2: OpenDocument formula.
       ((string-match "\\.odf\\'" src-file)
	(org-e-odt--zip-extract src-file "content.xml"
				(concat org-e-odt-zip-dir target-dir)))
       (t (error "%s is not a formula file" src-file))))
    ;; Enter the formula file in to manifest.
    (org-e-odt-create-manifest-file-entry "text/xml" target-file)
    target-file))

;;;; Targets

(defun org-e-odt-format-target (text id)
  (let ((name (concat org-e-odt-bookmark-prefix id)))
    (concat
     (and id (format "\n<text:bookmark-start text:name=\"%s\"/>" name))
     (concat (and id (format "\n<text:bookmark text:name=\"%s\"/>" id)) text)
     (and id (format "\n<text:bookmark-end text:name=\"%s\"/>" name)))))

(defun org-e-odt-link--inline-image (element info)
  "Return HTML code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((src (cond
	       ((eq (org-element-type element) 'link)
		(let* ((type (org-element-property :type element))
		       (raw-path (org-element-property :path element)))
		  (cond ((member type '("http" "https"))
			 (concat type ":" raw-path))
			((file-name-absolute-p raw-path)
			 (expand-file-name raw-path))
			(t raw-path))))
	       ((member (org-element-type element)
			'(latex-fragment latex-environment))
		(let* ((latex-frag (org-remove-indentation
				    (org-element-property :value element)))
		       (formula-link (org-e-odt-format-latex
				      latex-frag 'dvipng info)))
		  (and formula-link
		       (string-match "file:\\([^]]*\\)" formula-link)
		       (match-string 1 formula-link))))
	       (t (error "what is this?"))))
	 (src-expanded (if (file-name-absolute-p src) src
			 (expand-file-name src (file-name-directory
						(plist-get info :input-file)))))
	 (href (format
		"\n<draw:image xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>"
		(org-e-odt-copy-image-file src-expanded)))
	 ;; extract attributes from #+ATTR_ODT line.
	 (attr-from (case (org-element-type element)
		      (link (org-export-get-parent-element element))
		      (t element)))
	 ;; convert attributes to a plist.
	 (attr-plist (org-export-read-attribute :attr_odt attr-from))
	 ;; handle `:anchor', `:style' and `:attributes' properties.
	 (user-frame-anchor
	  (car (assoc-string (plist-get attr-plist :anchor)
			     '(("as-char") ("paragraph") ("page")) t)))
	 (user-frame-style
	  (and user-frame-anchor (plist-get attr-plist :style)))
	 (user-frame-attrs
	  (and user-frame-anchor (plist-get attr-plist :attributes)))
	 (user-frame-params
	  (list user-frame-style user-frame-attrs user-frame-anchor))
	 ;; (embed-as (or embed-as user-frame-anchor "paragraph"))
	 ;; extrac
	 ;; handle `:width', `:height' and `:scale' properties.
	 (size (org-e-odt-image-size-from-file
		src-expanded (plist-get attr-plist :width)
		(plist-get attr-plist :height)
		(plist-get attr-plist :scale) nil ;; embed-as
		"paragraph"			  ; FIXME
		))
	 (width (car size)) (height (cdr size))
	 (embed-as
	  (case (org-element-type element)
	    ((org-e-odt-standalone-image-p element info) "paragraph")
	    (latex-fragment "as-char")
	    (latex-environment "paragraph")
	    (t "paragraph")))
	 (captions (org-e-odt-format-label element info 'definition))
	 (caption (car captions)) (short-caption (cdr captions))
	 (entity (concat (and caption "Captioned") embed-as "Image")))
    (org-e-odt-format-entity entity href width height
			     captions user-frame-params )))

(defun org-e-odt-format-entity (entity href width height &optional
				       captions user-frame-params)
  (let* ((caption (car captions)) (short-caption (cdr captions))
	 (entity-style (assoc-string entity org-e-odt-entity-frame-styles t))
	 default-frame-params frame-params
	 (--merge-frame-params
	  (function
	   (lambda (default-frame-params user-frame-params)
	     (if (not user-frame-params) default-frame-params
	       (assert (= (length default-frame-params) 3))
	       (assert (= (length user-frame-params) 3))
	       (loop for user-frame-param in user-frame-params
		     for default-frame-param in default-frame-params
		     collect (or user-frame-param default-frame-param)))))))
    (cond
     ((not caption)
      (setq default-frame-params (nth 2 entity-style))
      (setq frame-params (funcall --merge-frame-params
				  default-frame-params user-frame-params))
      (apply 'org-e-odt--frame href width height frame-params))
     (t
      (setq default-frame-params (nth 3 entity-style))
      (setq frame-params (funcall --merge-frame-params
				  default-frame-params user-frame-params))
      (apply 'org-e-odt--textbox
	     (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		     "Illustration"
		     (concat
		      (apply 'org-e-odt--frame href width height
			     (let ((entity-style-1 (copy-sequence
						    (nth 2 entity-style))))
			       (setcar (cdr entity-style-1)
				       (concat
					(cadr entity-style-1)
					(and short-caption
					     (format " draw:name=\"%s\" "
						     short-caption))))
			       entity-style-1))
		      caption))
	     width height frame-params)))))

(defun org-e-odt-standalone-image-p (element info)
  "Test if ELEMENT is a standalone image for the purpose ODT export.
INFO is a plist holding contextual information.

Return non-nil, if ELEMENT is of type paragraph and it's sole
content, save for whitespaces, is a link that qualifies as an
inline image.

Return non-nil, if ELEMENT is of type link and it's containing
paragraph has no other content save for leading and trailing
whitespaces.

Return nil, otherwise.

Bind `org-e-odt-standalone-image-predicate' to constrain
paragraph further.  For example, to check for only captioned
standalone images, do the following.

  \(setq org-e-odt-standalone-image-predicate
	\(lambda \(paragraph\)
	  \(org-element-property :caption paragraph\)\)\)
"
  (let ((--standalone-image-predicate
	 (function (lambda (paragraph)
		     (or (org-element-property :caption paragraph)
			 (org-element-property :name paragraph)))))
	(paragraph (case (org-element-type element)
		     (paragraph element)
		     (link (and (org-export-inline-image-p
				 element org-e-odt-inline-image-rules)
				(org-export-get-parent element)))
		     (t nil))))
    (when paragraph
      (assert (eq (org-element-type paragraph) 'paragraph))
      (when (funcall --standalone-image-predicate paragraph)
	(let ((contents (org-element-contents paragraph)))
	  (loop for x in contents
		with inline-image-count = 0
		always (cond
			((eq (org-element-type x) 'plain-text)
			 (not (org-string-nw-p x)))
			((eq (org-element-type x) 'link)
			 (when (org-export-inline-image-p
				x org-e-odt-inline-image-rules)
			   (= (incf inline-image-count) 1)))
			(t nil))))))))

(defun org-e-odt-link (link desc info)
  "Transcode a LINK object from Org to ODT.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (imagep (org-export-inline-image-p
		  link org-e-odt-inline-image-rules))
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
     ;; Image file.
     ((and (not desc) (org-export-inline-image-p
		       link org-e-odt-inline-image-rules))
      (org-e-odt-link--inline-image link info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (org-e-odt-format-internal-link
	   (org-export-data (org-element-contents destination) info)
	   (org-export-solidify-link-text path)))))
     ;; Links pointing to an headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; Fuzzy link points nowhere.
	  ('nil
	   (format "<text:span text:style-name=\"%s\">%s</text:span>"
		   "Emphasis" (or desc (org-export-data
					(org-element-property
					 :raw-link link) info))))
	  ;; Fuzzy link points to an invisible target.
	  (keyword nil)
	  ;; LINK points to an headline.  Check if LINK should display
	  ;; section numbers.
	  (headline
	   (let* ((headline-no (org-export-get-headline-number destination info))
		  (label (format "sec-%s" (mapconcat 'number-to-string
						     headline-no "-")))
		  (desc
		   ;; Case 1: Headline is numbered and LINK has no
		   ;; description or LINK's description matches
		   ;; headline's title.  Display section number.
		   (if (and (org-export-numbered-headline-p destination info)
			    (or (not desc)
				(string= desc (org-element-property
					       :raw-value destination))))
		       headline-no
		     ;; Case 2: Either the headline is un-numbered or
		     ;; LINK has a custom description.  Display LINK's
		     ;; description or headline's title.
		     (or desc (org-export-data (org-element-property
						:title destination) info)))))
	     (org-e-odt-format-internal-link desc label)))
	  ;; Fuzzy link points to a target.  Do as above.
	  (otherwise
	   ;; (unless desc
	   ;;   (setq number (cond
	   ;; 		   ((org-e-odt-standalone-image-p destination info)
	   ;; 		    (org-export-get-ordinal
	   ;; 		     (assoc 'link (org-element-contents destination))
	   ;; 		     info 'link 'org-e-odt-standalone-image-p))
	   ;; 		   (t (org-export-get-ordinal destination info))))
	   ;;   (setq desc (when number
	   ;; 		  (if (atom number) (number-to-string number)
	   ;; 		    (mapconcat 'number-to-string number ".")))))

	   (let ((label-reference
		  (org-e-odt-format-label destination info 'reference)))
	     (assert label-reference)
	     label-reference)))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let* ((fmt (org-export-get-coderef-format path desc))
	     (res (org-export-resolve-coderef path info))
	     (href (concat "#coderef-" path)))
	(format fmt (org-e-odt-format-link res href))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'odt))
     ;; External link with a description part.
     ((and path desc) (org-e-odt-format-link desc path))
     ;; External link without a description part.
     (path (org-e-odt-format-link path path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<text:span text:style-name=\"%s\">%s</text:span>"
		"Emphasis" desc)))))


;;;; Babel Call

;; Babel Calls are ignored.


;;;; Macro

(defun org-e-odt-macro (macro contents info)
  "Transcode a MACRO element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-odt-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to ODT.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (style (case parent-type
		  (quote-block "Quotations")
		  (center-block "OrgCenter")
		  (footnote-definition "Footnote")
		  (t "Text_20_body"))))
    ;; If this paragraph is a leading paragraph in a non-descriptive
    ;; item and the item has a checkbox, splice the checkbox and
    ;; paragraph contents together.
    (when (and (eq (org-element-type parent) 'item)
    	       (not  (eq (org-element-property :type
					       (org-export-get-parent parent))
			 'descriptive))
    	       (eq paragraph (car (org-element-contents parent))))
      (setq contents (concat (org-e-odt--checkbox parent) contents)))
    (assert style)
    (format "\n<text:p text:style-name=\"%s\">%s</text:p>" style contents)))


;;;; Plain List

(defun org-e-odt-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to ODT.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (continue-numbering nil))
    (assert (member type '(ordered unordered descriptive)))
    (org-e-odt--wrap-label
     plain-list
     (format "\n<text:list text:style-name=\"%s\" %s>\n%s</text:list>"
	     (assoc-default type '((ordered . "OrgNumberedList")
				   (unordered . "OrgBulletedList")
				   (descriptive . "OrgDescriptionList")))
	     ;; If top-level list, re-start numbering.  Otherwise,
	     ;; continue numbering.
	     (format "text:continue-numbering=\"%s\""
		     (let* ((parent (org-export-get-parent plain-list)))
		       (if (and parent (eq (org-element-type parent) 'item))
			   "true" "false")))
	     contents))))

;;;; Plain Text

(defun org-e-odt-fill-tabs-and-spaces (line)
  (replace-regexp-in-string
   "\\([\t]\\|\\([ ]+\\)\\)"
   (lambda (s)
     (cond
      ((string= s "\t") "<text:tab/>")
      (t (let ((n (length s)))
	   (cond
	    ((= n 1) " ")
	    ((> n 1) (concat " " (format "<text:s text:c=\"%d\"/>" (1- n))))
	    (t ""))))))
   line))

(defun org-e-odt-encode-plain-text (text &optional no-whitespace-filling)
  (mapc
   (lambda (pair)
     (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
   '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")))
  (if no-whitespace-filling text
    (org-e-odt-fill-tabs-and-spaces text)))

(defun org-e-odt--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
	  (let ((start 0))
	    (while (setq start (string-match (car l) text start))
	      (let ((new-quote (concat (match-string 1 text) (cdr l))))
		(setq text (replace-match new-quote  t t text))))))
	(cdr (or (assoc (plist-get info :language) org-e-odt-quotes)
		 ;; Falls back on English.
		 (assoc "en" org-e-odt-quotes))))
  text)

(defun org-e-odt-plain-text (text info)
  "Transcode a TEXT string from Org to ODT.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect &, < and >.
  (setq text (org-e-odt-encode-plain-text text t))
  ;; Handle quotation marks
  (setq text (org-e-odt--quotation-marks text info))
  ;; Convert special strings.
  (when (plist-get info :with-special-strings)
    (mapc
     (lambda (pair)
       (setq text (replace-regexp-in-string (car pair) (cdr pair) text t nil)))
     org-e-odt-special-string-regexps))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string
		"\\(\\\\\\\\\\)?[ \t]*\n" "<text:line-break/>\n" text t)))
  ;; Return value.
  text)


;;;; Planning

(defun org-e-odt-planning (planning contents info)
  "Transcode a PLANNING element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgTimestampWrapper"
	  (concat
	   (let ((closed (org-element-property :closed planning)))
	     (when closed
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgTimestampKeyword" org-closed-string)
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgTimestamp" (org-translate-time closed)))))
	   (let ((deadline (org-element-property :deadline planning)))
	     (when deadline
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgTimestampKeyword" org-deadline-string)
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgTimestamp" (org-translate-time deadline)))))
	   (let ((scheduled (org-element-property :scheduled planning)))
	     (when scheduled
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgTimestampKeyword" org-scheduled-string)
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgTimestamp" (org-translate-time scheduled))))))))


;;;; Property Drawer

(defun org-e-odt-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")


;;;; Quote Block

(defun org-e-odt-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-odt--wrap-label quote-block contents))


;;;; Quote Section

(defun org-e-odt-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (org-e-odt-do-format-code value))))


;;;; Section


(defun org-e-odt-format-section (text style &optional name)
  (let ((default-name (car (org-e-odt-add-automatic-style "Section"))))
    (format "\n<text:section text:style-name=\"%s\" %s>\n%s</text:section>"
	    style
	    (format "text:name=\"%s\"" (or name default-name))
	    text)))


(defun org-e-odt-section (section contents info) ; FIXME
  "Transcode a SECTION element from Org to ODT.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

;;;; Radio Target

(defun org-e-odt-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to ODT.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (org-e-odt-format-target
   text (org-export-solidify-link-text
	 (org-element-property :value radio-target))))


;;;; Special Block

(defun org-e-odt-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block)))
	(attributes (org-export-read-attribute :attr_odt special-block)))
    (org-e-odt--wrap-label
     special-block
     (cond
      ;; Annotation.
      ((string= type "annotation")
       (let ((author (or (plist-get attributes :author)
			 (let ((author (plist-get info :author)))
			   (and author (org-export-data author info)))))
	     (date (or (plist-get attributes :date)
		       (plist-get info :date))))

	 (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		 "Text_20_body"
		 (format "<office:annotation>\n%s\n</office:annotation>"
			 (concat
			  (and author
			       (format "<dc:creator>%s</dc:creator>" author))
			  (and date
			       (format "<dc:date>%s</dc:date>"
				       (org-e-odt--date date)))
			  contents)))))
      ;; Textbox.
      ((string= type "textbox")
       (let ((width (plist-get attributes :width))
	     (height (plist-get attributes :height))
	     (style (plist-get attributes :style))
	     (extra (plist-get attributes :extra))
	     (anchor (plist-get attributes :anchor)))
	 (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		 "Text_20_body" (org-e-odt--textbox contents width height
						    style extra anchor))))
      (t contents)))))


;;;; Src Block


(defun org-e-odt-hfy-face-to-css (fn)
  "Create custom style for face FN.
When FN is the default face, use it's foreground and background
properties to create \"OrgSrcBlock\" paragraph style.  Otherwise
use it's color attribute to create a character style whose name
is obtained from FN.  Currently all attributes of FN other than
color are ignored.

The style name for a face FN is derived using the following
operations on the face name in that order - de-dash, CamelCase
and prefix with \"OrgSrc\".  For example,
`font-lock-function-name-face' is associated with
\"OrgSrcFontLockFunctionNameFace\"."
  (let* ((css-list (hfy-face-to-style fn))
	 (style-name ((lambda (fn)
			(concat "OrgSrc"
				(mapconcat
				 'capitalize (split-string
					      (hfy-face-or-def-to-name fn) "-")
				 ""))) fn))
	 (color-val (cdr (assoc "color" css-list)))
	 (background-color-val (cdr (assoc "background" css-list)))
	 (style (and org-e-odt-create-custom-styles-for-srcblocks
		     (cond
		      ((eq fn 'default)
		       (format org-e-odt-src-block-paragraph-format
			       background-color-val color-val))
		      (t
		       (format
			"
<style:style style:name=\"%s\" style:family=\"text\">
  <style:text-properties fo:color=\"%s\"/>
 </style:style>" style-name color-val))))))
    (cons style-name style)))

(defun org-e-odt-htmlfontify-string (line)
  (let* ((hfy-html-quote-regex "\\([<\"&> 	]\\)")
	 (hfy-html-quote-map '(("\"" "&quot;")
			       ("<" "&lt;")
			       ("&" "&amp;")
			       (">" "&gt;")
			       (" " "<text:s/>")
			       ("	" "<text:tab/>")))
	 (hfy-face-to-css 'org-e-odt-hfy-face-to-css)
	 (hfy-optimisations-1 (copy-seq hfy-optimisations))
	 (hfy-optimisations (add-to-list 'hfy-optimisations-1
					 'body-text-only))
	 (hfy-begin-span-handler
	  (lambda (style text-block text-id text-begins-block-p)
	    (insert (format "<text:span text:style-name=\"%s\">" style))))
	 (hfy-end-span-handler (lambda nil (insert "</text:span>"))))
    (with-no-warnings (htmlfontify-string line))))

(defun org-e-odt-do-format-code
  (code &optional lang refs retain-labels num-start)
  (let* ((lang (or (assoc-default lang org-src-lang-modes) lang))
	 (lang-mode (and lang (intern (format "%s-mode" lang))))
	 (code-lines (org-split-string code "\n"))
	 (code-length (length code-lines))
	 (use-htmlfontify-p (and (functionp lang-mode)
				 org-e-odt-fontify-srcblocks
				 (require 'htmlfontify nil t)
				 (fboundp 'htmlfontify-string)))
	 (code (if (not use-htmlfontify-p) code
		 (with-temp-buffer
		   (insert code)
		   (funcall lang-mode)
		   (font-lock-fontify-buffer)
		   (buffer-string))))
	 (fontifier (if use-htmlfontify-p 'org-e-odt-htmlfontify-string
		      'org-e-odt-encode-plain-text))
	 (par-style (if use-htmlfontify-p "OrgSrcBlock"
		      "OrgFixedWidthBlock"))
	 (i 0))
    (assert (= code-length (length (org-split-string code "\n"))))
    (setq code
	  (org-export-format-code
	   code
	   (lambda (loc line-num ref)
	     (setq par-style
		   (concat par-style (and (= (incf i) code-length) "LastLine")))

	     (setq loc (concat loc (and ref retain-labels (format " (%s)" ref))))
	     (setq loc (funcall fontifier loc))
	     (when ref
	       (setq loc (org-e-odt-format-target loc (concat "coderef-" ref))))
	     (assert par-style)
	     (setq loc (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			       par-style loc))
	     (if (not line-num) loc
	       (format "\n<text:list-item>%s\n</text:list-item>" loc)))
	   num-start refs))
    (cond
     ((not num-start) code)
     ((= num-start 0)
      (format
       "\n<text:list text:style-name=\"OrgSrcBlockNumberedLine\"%s>%s</text:list>"
       " text:continue-numbering=\"false\"" code))
     (t
      (format
       "\n<text:list text:style-name=\"OrgSrcBlockNumberedLine\"%s>%s</text:list>"
       " text:continue-numbering=\"true\"" code)))))

(defun org-e-odt-format-code (element info)
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element))
	 ;; Does it have line numbers?
	 (num-start (case (org-element-property :number-lines element)
		      (continued (org-export-get-loc element info))
		      (new 0))))
    (org-e-odt-do-format-code code lang refs retain-labels num-start)))

(defun org-e-odt-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
	 (caption (org-element-property :caption src-block))
	 (short-caption (and (cdr caption)
			     (org-export-data (cdr caption) info)))
	 (caption (and (car caption) (org-export-data (car caption) info)))
	 (label (org-element-property :name src-block))
	 (attributes (org-export-read-attribute :attr_odt src-block)))
    ;; FIXME: Handle caption
    ;; caption-str (when caption)
    ;; (main (org-export-data (car caption) info))
    ;; (secondary (org-export-data (cdr caption) info))
    ;; (caption-str (org-e-odt--caption/label-string caption label info))
    (let* ((captions (org-e-odt-format-label src-block info 'definition))
	   (caption (car captions)) (short-caption (cdr captions)))
      (concat
       (and caption
	    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		    "Listing" caption))
       (let ((--src-block (org-e-odt-format-code src-block info)))
	 (if (not (plist-get attributes :textbox)) --src-block
	   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		   "Text_20_body"
		   (org-e-odt--textbox --src-block nil nil nil))))))))


;;;; Statistics Cookie

(defun org-e-odt-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (format "<text:span text:style-name=\"%s\">%s</text:span>"
	    "OrgCode" cookie-value)))


;;;; Strike-Through

(defun org-e-odt-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to ODT.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Strikethrough" contents))


;;;; Subscript

(defun org-e-odt-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to ODT.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgSubscript" contents))


;;;; Superscript

(defun org-e-odt-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to ODT.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgSuperscript" contents))


;;;; Table Cell

(defun org-e-odt-table-style-spec (element info)
  (let* ((table (org-export-get-parent-table element))
	 (table-attributes (org-export-read-attribute :attr_odt table))
	 (table-style (plist-get table-attributes :style)))
    (assoc table-style org-e-odt-table-styles)))

(defun org-e-odt-get-table-cell-styles (table-cell info)
  "Retrieve styles applicable to a table cell.
R and C are (zero-based) row and column numbers of the table
cell.  STYLE-SPEC is an entry in `org-e-odt-table-styles'
applicable to the current table.  It is `nil' if the table is not
associated with any style attributes.

Return a cons of (TABLE-CELL-STYLE-NAME . PARAGRAPH-STYLE-NAME).

When STYLE-SPEC is nil, style the table cell the conventional way
- choose cell borders based on row and column groupings and
choose paragraph alignment based on `org-col-cookies' text
property.  See also
`org-e-odt-get-paragraph-style-cookie-for-table-cell'.

When STYLE-SPEC is non-nil, ignore the above cookie and return
styles congruent with the ODF-1.2 specification."
  (let* ((table-cell-address (org-export-table-cell-address table-cell info))
	 (r (car table-cell-address)) (c (cdr table-cell-address))
	 (style-spec (org-e-odt-table-style-spec table-cell info))
	 (table-dimensions (org-export-table-dimensions
			    (org-export-get-parent-table table-cell)
			    info)))
    (when style-spec
      ;; LibreOffice - particularly the Writer - honors neither table
      ;; templates nor custom table-cell styles.  Inorder to retain
      ;; inter-operability with LibreOffice, only automatic styles are
      ;; used for styling of table-cells.  The current implementation is
      ;; congruent with ODF-1.2 specification and hence is
      ;; future-compatible.

      ;; Additional Note: LibreOffice's AutoFormat facility for tables -
      ;; which recognizes as many as 16 different cell types - is much
      ;; richer. Unfortunately it is NOT amenable to easy configuration
      ;; by hand.
      (let* ((template-name (nth 1 style-spec))
	     (cell-style-selectors (nth 2 style-spec))
	     (cell-type
	      (cond
	       ((and (cdr (assoc 'use-first-column-styles cell-style-selectors))
		     (= c 0)) "FirstColumn")
	       ((and (cdr (assoc 'use-last-column-styles cell-style-selectors))
		     (= (1+ c) (cdr table-dimensions)))
		"LastColumn")
	       ((and (cdr (assoc 'use-first-row-styles cell-style-selectors))
		     (= r 0)) "FirstRow")
	       ((and (cdr (assoc 'use-last-row-styles cell-style-selectors))
		     (= (1+ r) (car table-dimensions)))
		"LastRow")
	       ((and (cdr (assoc 'use-banding-rows-styles cell-style-selectors))
		     (= (% r 2) 1)) "EvenRow")
	       ((and (cdr (assoc 'use-banding-rows-styles cell-style-selectors))
		     (= (% r 2) 0)) "OddRow")
	       ((and (cdr (assoc 'use-banding-columns-styles cell-style-selectors))
		     (= (% c 2) 1)) "EvenColumn")
	       ((and (cdr (assoc 'use-banding-columns-styles cell-style-selectors))
		     (= (% c 2) 0)) "OddColumn")
	       (t ""))))
	(concat template-name cell-type)))))

(defun org-e-odt-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-cell-address (org-export-table-cell-address table-cell info))
	 (r (car table-cell-address))
	 (c (cdr table-cell-address))
	 (horiz-span (or (org-export-table-cell-width table-cell info) 0))
	 (table-row (org-export-get-parent table-cell))
	 (custom-style-prefix (org-e-odt-get-table-cell-styles
			       table-cell info))
	 (paragraph-style
	  (or
	   (and custom-style-prefix
		(format "%sTableParagraph" custom-style-prefix))
	   (concat
	    (cond
	     ((and (= 1 (org-export-table-row-group table-row info))
		   (org-export-table-has-header-p
		    (org-export-get-parent-table table-row) info))
	      "OrgTableHeading")
	     ((let* ((table (org-export-get-parent-table table-cell))
		     (table-attrs (org-export-read-attribute :attr_odt table))
		     (table-header-columns (plist-get table-attrs
						      :header-columns)))
		(<= c (cond ((wholenump table-header-columns)
			     (- table-header-columns 1))
			    (table-header-columns 0)
			    (t -1))))
	      "OrgTableHeading")
	     (t "OrgTableContents"))
	    (capitalize (symbol-name (org-export-table-cell-alignment
				      table-cell info))))))
	 (cell-style-name
	  (or
	   (and custom-style-prefix (format "%sTableCell"
					    custom-style-prefix))
	   (concat
	    "OrgTblCell"
	    (when (or (org-export-table-row-starts-rowgroup-p table-row info)
		      (zerop r)) "T")
	    (when (org-export-table-row-ends-rowgroup-p table-row info) "B")
	    (when (and (org-export-table-cell-starts-colgroup-p table-cell info)
		       (not (zerop c)) ) "L"))))
	 (cell-attributes
	  (concat
	   (format " table:style-name=\"%s\"" cell-style-name)
	   (and (> horiz-span 0)
		(format " table:number-columns-spanned=\"%d\""
			(1+ horiz-span))))))
    (unless contents (setq contents ""))
    (concat
     (assert paragraph-style)
     (format "\n<table:table-cell%s>\n%s\n</table:table-cell>"
	     cell-attributes
	     (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		     paragraph-style contents))
     (let (s)
       (dotimes (i horiz-span s)
	 (setq s (concat s "\n<table:covered-table-cell/>"))))
     "\n")))


;;;; Table Row

(defun org-e-odt-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to ODT.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((rowgroup-tags
	    (if (and (= 1 (org-export-table-row-group table-row info))
		     (org-export-table-has-header-p
		      (org-export-get-parent-table table-row) info))
		;; If the row belongs to the first rowgroup and the
		;; table has more than one row groups, then this row
		;; belongs to the header row group.
		'("\n<table:table-header-rows>" . "\n</table:table-header-rows>")
	      ;; Otherwise, it belongs to non-header row group.
	      '("\n<table:table-rows>" . "\n</table:table-rows>"))))
      (concat
       ;; Does this row begin a rowgroup?
       (when (org-export-table-row-starts-rowgroup-p table-row info)
  	 (car rowgroup-tags))
       ;; Actual table row
       (format "\n<table:table-row>\n%s\n</table:table-row>" contents)
       ;; Does this row end a rowgroup?
       (when (org-export-table-row-ends-rowgroup-p table-row info)
  	 (cdr rowgroup-tags))))))


;;;; Table

(defun org-e-odt-table-first-row-data-cells (table info)
  (let ((table-row
	 (org-element-map
	  table 'table-row
	  (lambda (row)
	    (unless (eq (org-element-property :type row) 'rule) row))
	  info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun org-e-odt--table (table contents info)
  "Transcode a TABLE element from Org to ODT.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (case (org-element-property :type table)
    ;; Case 1: table.el doesn't support export to OD format.  Strip
    ;; such tables from export.
    (table.el
     (prog1 nil
       (message
	(concat
	 "(org-e-odt): Found table.el-type table in the source Org file."
	 "  table.el doesn't support export to ODT format."
	 "  Stripping the table from export."))))
    ;; Case 2: Native Org tables.
    (otherwise
     (let* ((captions (org-e-odt-format-label table info 'definition))
	    (caption (car captions)) (short-caption (cdr captions))
	    (attributes (org-export-read-attribute :attr_odt table))
	    (custom-table-style (nth 1 (org-e-odt-table-style-spec table info)))
	    (table-column-specs
	     (function
	      (lambda (table info)
		(let* ((table-style (or custom-table-style "OrgTable"))
		       (column-style (format "%sColumn" table-style)))
		  (mapconcat
		   (lambda (table-cell)
		     (let ((width (1+ (or (org-export-table-cell-width
					   table-cell info) 0)))
			   (s (format
			       "\n<table:table-column table:style-name=\"%s\"/>"
			       column-style))
			   out)
		       (dotimes (i width out) (setq out (concat s out)))))
		   (org-e-odt-table-first-row-data-cells table info) "\n"))))))
       (concat
	;; caption.
	(when caption
	  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		  "Table" caption))
	;; begin table.
	(let* ((automatic-name
		(org-e-odt-add-automatic-style "Table" attributes)))
	  (format
	   "\n<table:table table:name=\"%s\" table:style-name=\"%s\">"
	   (or short-caption (car automatic-name))
	   (or custom-table-style (cdr automatic-name) "OrgTable")))
	;; column specification.
	(funcall table-column-specs table info)
	;; actual contents.
	"\n" contents
	;; end table.
	"</table:table>")))))

(defun org-e-odt-table (table contents info)
  "Transcode a TABLE element from Org to ODT.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let* ((--get-previous-elements
	  (function
	   (lambda (blob info)
	     (let ((parent (org-export-get-parent blob)))
	       (cdr (member blob (reverse (org-element-contents parent))))))))
	 (--element-preceded-by-table-p
	  (function
	   (lambda (element info)
	     (loop for el in (funcall --get-previous-elements element info)
		   thereis (eq (org-element-type el) 'table)))))
	 (--walk-list-genealogy-and-collect-tags
	  (function
	   (lambda (table info)
	     (let* ((genealogy (org-export-get-genealogy table))
		    (list-genealogy
		     (when (eq (org-element-type (car genealogy)) 'item)
		       (loop for el in genealogy
			     when (member (org-element-type el)
					  '(item plain-list))
			     collect el))))
	       (loop for el in list-genealogy
		     with parent-list collect
		     (case (org-element-type el)
		       (plain-list
			(setq parent-list el)
			`("</text:list>"
			  . ,(let ((type (org-element-property :type el)))
			       (format
				"<text:list text:style-name=\"%s\" %s>"
				(assoc-default
				 type '((ordered . "OrgNumberedList")
					(unordered . "OrgBulletedList")
					(descriptive . "OrgDescriptionList")))
				"text:continue-numbering=\"true\""))))
		       (item
			(cond
			 ((not parent-list)
			  (if (funcall --element-preceded-by-table-p table info)
			      '("</text:list-header>" . "<text:list-header>")
			    '("</text:list-item>" . "<text:list-header>")))
			 ((funcall --element-preceded-by-table-p
				   parent-list info)
			  '("</text:list-header>" . "<text:list-header>"))
			 (t '("</text:list-item>" . "<text:list-item>"))))))))))
	 (close-open-tags (funcall --walk-list-genealogy-and-collect-tags
				   table info)))
    ;; OpenDocument schema does not permit table to occur within a
    ;; list item.  So, to typeset an indented table, we make use of
    ;; list continuations.
    (concat "\n"
	    ;; Discontinue the list.
	    (mapconcat 'car close-open-tags "\n")
	    ;; Put the table in an indented section.
	    (let* ((table (org-e-odt--table table contents info))
		   (level (/ (length (mapcar 'car close-open-tags)) 2))
		   (style (format "OrgIndentedSection-Level-%d" level)))
	      (when table (org-e-odt-format-section table style)))
	    ;; Continue the list.
	    (mapconcat 'cdr (nreverse close-open-tags) "\n"))))


;;;; Target

(defun org-e-odt-target (target contents info)
  "Transcode a TARGET object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-e-odt-format-target
   "" (org-export-solidify-link-text (org-element-property :value target))))


;;;; Timestamp

(defun org-e-odt-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((timestamp-1 (org-element-property :value timestamp))
	(timestamp-2 (org-element-property :range-end timestamp)))
    (format "<text:span text:style-name=\"%s\">%s</text:span>"
	    "OrgTimestampWrapper"
	    (concat
	     (format "<text:span text:style-name=\"%s\">%s</text:span>"
		     "OrgTimestamp" (org-translate-time timestamp-1))
	     (and timestamp-2
		  "&#x2013;"
		  (format "<text:span text:style-name=\"%s\">%s</text:span>"
			  "OrgTimestamp" (org-translate-time timestamp-2)))))))


;;;; Underline

(defun org-e-odt-underline (underline contents info)
  "Transcode UNDERLINE from Org to ODT.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Underline" contents))


;;;; Verbatim

(defun org-e-odt-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgCode" (org-element-property :value verbatim)))


;;;; Verse Block

(defun org-e-odt-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to ODT.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Add line breaks to each line of verse.
  (setq contents (replace-regexp-in-string
		  "\\(<text:line-break/>\\)?[ \t]*\n"
		  "<text:line-break/>" contents))
  ;; Replace tabs and spaces.
  (setq contents (org-e-odt-fill-tabs-and-spaces contents))
  ;; Surround it in a verse environment.
  (org-e-odt--wrap-label
   verse-block
   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	   "OrgVerse" contents)))





;;; Interactive functions

(defun org-e-odt-create-manifest-file-entry (&rest args)
  (push args org-e-odt-manifest-file-entries))

(defun org-e-odt-write-manifest-file ()
  (make-directory (concat org-e-odt-zip-dir "META-INF"))
  (let ((manifest-file (concat org-e-odt-zip-dir "META-INF/manifest.xml")))
    (with-current-buffer
	(let ((nxml-auto-insert-xml-declaration-flag nil))
	  (find-file-noselect manifest-file t))
      (insert
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\" manifest:version=\"1.2\">\n")
      (mapc
       (lambda (file-entry)
	 (let* ((version (nth 2 file-entry))
		(extra (if (not version) ""
			 (format " manifest:version=\"%s\"" version))))
	   (insert
	    (format org-e-odt-manifest-file-entry-tag
		    (nth 0 file-entry) (nth 1 file-entry) extra))))
       org-e-odt-manifest-file-entries)
      (insert "\n</manifest:manifest>"))))

(defmacro org-e-odt--export-wrap (out-file &rest body)
  `(let* ((--out-file ,out-file)
	  (out-file-type (file-name-extension --out-file))
	  (org-e-odt-xml-files '("META-INF/manifest.xml" "content.xml"
				 "meta.xml" "styles.xml"))
	  ;; Initialize workarea.  All files that end up in the
	  ;; exported get created here.
	  (org-e-odt-zip-dir (file-name-as-directory
			      (make-temp-file (format "%s-" out-file-type) t)))
	  (org-e-odt-manifest-file-entries nil)
	  (--cleanup-xml-buffers
	   (function
	    (lambda nil
	      ;; Kill all XML buffers.
	      (mapc (lambda (file)
		      (let ((buf (get-file-buffer
				  (concat org-e-odt-zip-dir file))))
			(when buf
			  (set-buffer-modified-p nil)
			  (kill-buffer buf))))
		    org-e-odt-xml-files)
	      ;; Delete temporary directory and also other embedded
	      ;; files that get copied there.
	      (delete-directory org-e-odt-zip-dir t)))))
     (org-condition-case-unless-debug
      err
      (progn
	(unless (executable-find "zip")
	  ;; Not at all OSes ship with zip by default
	  (error "Executable \"zip\" needed for creating OpenDocument files"))
	;; Do export.  This creates a bunch of xml files ready to be
	;; saved and zipped.
	(progn ,@body)
	;; Create a manifest entry for content.xml.
	(org-e-odt-create-manifest-file-entry "text/xml" "content.xml")

	;; Write mimetype file
	(let* ((mimetypes
		'(("odt" . "application/vnd.oasis.opendocument.text")
		  ("odf" .  "application/vnd.oasis.opendocument.formula")))
	       (mimetype (cdr (assoc-string out-file-type mimetypes t))))
	  (unless mimetype
	    (error "Unknown OpenDocument backend %S" out-file-type))
	  (write-region mimetype nil (concat org-e-odt-zip-dir "mimetype"))
	  (org-e-odt-create-manifest-file-entry mimetype "/" "1.2"))
	;; Write out the manifest entries before zipping
	(org-e-odt-write-manifest-file)
	;; Save all XML files.
	(mapc (lambda (file)
		(let ((buf (get-file-buffer (concat org-e-odt-zip-dir file))))
		  (when buf
		    (with-current-buffer buf
		      ;; Prettify output if needed.
		      (when org-e-odt-prettify-xml
			(indent-region (point-min) (point-max)))
		      (save-buffer 0)))))
	      org-e-odt-xml-files)
	;; Run zip.
	(let* ((target --out-file)
	       (target-name (file-name-nondirectory target))
	       (target-dir (file-name-directory target))
	       (cmds `(("zip" "-mX0" ,target-name "mimetype")
		       ("zip" "-rmTq" ,target-name "."))))
	  ;; If a file with same name as the desired output file
	  ;; exists, remove it.
	  (when (file-exists-p target)
	    (delete-file target))
	  ;; Zip up the xml files.
	  (let ((coding-system-for-write 'no-conversion) exitcode err-string)
	    (message "Creating ODT file...")
	    ;; Switch temporarily to content.xml.  This way Zip
	    ;; process will inherit `org-e-odt-zip-dir' as the current
	    ;; directory.
	    (with-current-buffer
		(find-file-noselect (concat org-e-odt-zip-dir "content.xml") t)
	      (mapc
	       (lambda (cmd)
		 (message "Running %s" (mapconcat 'identity cmd " "))
		 (setq err-string
		       (with-output-to-string
			 (setq exitcode
			       (apply 'call-process (car cmd)
				      nil standard-output nil (cdr cmd)))))
		 (or (zerop exitcode)
		     (error (concat "Unable to create OpenDocument file."
				    (format "  Zip failed with error (%s)"
					    err-string)))))
	       cmds)
	      ;; Zip file is now in the rightful place.
	      (rename-file target-name target)))
	  (message "Created %s" target)
	  ;; Cleanup work directory and work files.
	  (funcall --cleanup-xml-buffers)
	  ;; Open the OpenDocument file in archive-mode for
	  ;; examination.
	  (find-file-noselect target t)
	  ;; Return exported file.
	  (cond
	   ;; Case 1: Conversion desired on exported file.  Run the
	   ;; converter on the OpenDocument file.  Return the
	   ;; converted file.
	   (org-e-odt-preferred-output-format
	    (or (org-e-odt-convert target org-e-odt-preferred-output-format)
		target))
	   ;; Case 2: No further conversion.  Return exported
	   ;; OpenDocument file.
	   (t target))))
      ((quit error)
       ;; Cleanup work directory and work files.
       (funcall --cleanup-xml-buffers)
       (message "OpenDocument export failed: %s"
		(error-message-string err))))))



;;;###autoload
(defun org-e-odt-export-as-odf (latex-frag &optional odf-file)
  "Export LATEX-FRAG as OpenDocument formula file ODF-FILE.
Use `org-create-math-formula' to convert LATEX-FRAG first to
MathML.  When invoked as an interactive command, use
`org-latex-regexps' to infer LATEX-FRAG from currently active
region.  If no LaTeX fragments are found, prompt for it.  Push
MathML source to kill ring, if `org-export-copy-to-kill-ring' is
non-nil."
  (interactive
   `(,(let (frag)
	(setq frag (and (setq frag (and (region-active-p)
					(buffer-substring (region-beginning)
							  (region-end))))
			(loop for e in org-latex-regexps
			      thereis (when (string-match (nth 1 e) frag)
					(match-string (nth 2 e) frag)))))
	(read-string "LaTeX Fragment: " frag nil frag))
     ,(let ((odf-filename (expand-file-name
			   (concat
			    (file-name-sans-extension
			     (or (file-name-nondirectory buffer-file-name)))
			    "." "odf")
			   (file-name-directory buffer-file-name))))
	(read-file-name "ODF filename: " nil odf-filename nil
			(file-name-nondirectory odf-filename)))))
  (let ((filename (or odf-file
		      (expand-file-name
		       (concat
			(file-name-sans-extension
			 (or (file-name-nondirectory buffer-file-name)))
			"." "odf")
		       (file-name-directory buffer-file-name)))))
    (org-e-odt--export-wrap
     filename
     (let* ((buffer (progn
		      (require 'nxml-mode)
		      (let ((nxml-auto-insert-xml-declaration-flag nil))
			(find-file-noselect (concat org-e-odt-zip-dir
						    "content.xml") t))))
	    (coding-system-for-write 'utf-8)
	    (save-buffer-coding-system 'utf-8))
       (set-buffer buffer)
       (set-buffer-file-coding-system coding-system-for-write)
       (let ((mathml (org-create-math-formula latex-frag)))
	 (unless mathml (error "No Math formula created"))
	 (insert mathml)
	 ;; Add MathML to kill ring, if needed.
	 (when org-export-copy-to-kill-ring
	   (org-kill-new (buffer-string))))))))

;;;###autoload
(defun org-e-odt-export-as-odf-and-open ()
  "Export LaTeX fragment as OpenDocument formula and immediately open it.
Use `org-e-odt-export-as-odf' to read LaTeX fragment and OpenDocument
formula file."
  (interactive)
  (org-open-file (call-interactively 'org-e-odt-export-as-odf)))

;;;###autoload
(defun org-e-odt-export-to-odt
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
  (org-e-odt--export-wrap
   (org-export-output-file-name ".odt" subtreep pub-dir)
   (let* ((org-e-odt-embedded-images-count 0)
	  (org-e-odt-embedded-formulas-count 0)
	  (org-e-odt-automatic-styles nil)
	  (org-e-odt-object-counters nil)
	  ;; Let `htmlfontify' know that we are interested in collecting
	  ;; styles.
	  (hfy-user-sheet-assoc nil))
     ;; Initialize content.xml and kick-off the export process.
     (let ((out-buf (progn
		      (require 'nxml-mode)
		      (let ((nxml-auto-insert-xml-declaration-flag nil))
			(find-file-noselect
			 (concat org-e-odt-zip-dir "content.xml") t)))))
       (org-export-to-buffer 'e-odt out-buf subtreep visible-only body-only)))))




(defun org-e-odt-reachable-p (in-fmt out-fmt)
  "Return non-nil if IN-FMT can be converted to OUT-FMT."
  (catch 'done
    (let ((reachable-formats (org-e-odt-do-reachable-formats in-fmt)))
      (dolist (e reachable-formats)
	(let ((out-fmt-spec (assoc out-fmt (cdr e))))
	  (when out-fmt-spec
	    (throw 'done (cons (car e) out-fmt-spec))))))))

(defun org-e-odt-do-convert (in-file out-fmt &optional prefix-arg)
  "Workhorse routine for `org-e-odt-convert'."
  (require 'browse-url)
  (let* ((in-file (expand-file-name (or in-file buffer-file-name)))
	 (dummy (or (file-readable-p in-file)
		    (error "Cannot read %s" in-file)))
	 (in-fmt (file-name-extension in-file))
	 (out-fmt (or out-fmt (error "Output format unspecified")))
	 (how (or (org-e-odt-reachable-p in-fmt out-fmt)
		  (error "Cannot convert from %s format to %s format?"
			 in-fmt out-fmt)))
	 (convert-process (car how))
	 (out-file (concat (file-name-sans-extension in-file) "."
			   (nth 1 (or (cdr how) out-fmt))))
	 (extra-options (or (nth 2 (cdr how)) ""))
	 (out-dir (file-name-directory in-file))
	 (cmd (format-spec convert-process
			   `((?i . ,(shell-quote-argument in-file))
			     (?I . ,(browse-url-file-url in-file))
			     (?f . ,out-fmt)
			     (?o . ,out-file)
			     (?O . ,(browse-url-file-url out-file))
			     (?d . , (shell-quote-argument out-dir))
			     (?D . ,(browse-url-file-url out-dir))
			     (?x . ,extra-options)))))
    (when (file-exists-p out-file)
      (delete-file out-file))

    (message "Executing %s" cmd)
    (let ((cmd-output (shell-command-to-string cmd)))
      (message "%s" cmd-output))

    (cond
     ((file-exists-p out-file)
      (message "Exported to %s" out-file)
      (when prefix-arg
	(message "Opening %s..."  out-file)
	(org-open-file out-file))
      out-file)
     (t
      (message "Export to %s failed" out-file)
      nil))))

(defun org-e-odt-do-reachable-formats (in-fmt)
  "Return verbose info about formats to which IN-FMT can be converted.
Return a list where each element is of the
form (CONVERTER-PROCESS . OUTPUT-FMT-ALIST).  See
`org-e-odt-convert-processes' for CONVERTER-PROCESS and see
`org-e-odt-convert-capabilities' for OUTPUT-FMT-ALIST."
  (let* ((converter
	  (and org-e-odt-convert-process
	       (cadr (assoc-string org-e-odt-convert-process
				   org-e-odt-convert-processes t))))
	 (capabilities
	  (and org-e-odt-convert-process
	       (cadr (assoc-string org-e-odt-convert-process
				   org-e-odt-convert-processes t))
	       org-e-odt-convert-capabilities))
	 reachable-formats)
    (when converter
      (dolist (c capabilities)
	(when (member in-fmt (nth 1 c))
	  (push (cons converter (nth 2 c)) reachable-formats))))
    reachable-formats))

(defun org-e-odt-reachable-formats (in-fmt)
  "Return list of formats to which IN-FMT can be converted.
The list of the form (OUTPUT-FMT-1 OUTPUT-FMT-2 ...)."
  (let (l)
    (mapc (lambda (e) (add-to-list 'l e))
	  (apply 'append (mapcar
			  (lambda (e) (mapcar 'car (cdr e)))
			  (org-e-odt-do-reachable-formats in-fmt))))
    l))

(defun org-e-odt-convert-read-params ()
  "Return IN-FILE and OUT-FMT params for `org-e-odt-do-convert'.
This is a helper routine for interactive use."
  (let* ((input (if (featurep 'ido) 'ido-completing-read 'completing-read))
	 (in-file (read-file-name "File to be converted: "
				  nil buffer-file-name t))
	 (in-fmt (file-name-extension in-file))
	 (out-fmt-choices (org-e-odt-reachable-formats in-fmt))
	 (out-fmt
	  (or (and out-fmt-choices
		   (funcall input "Output format:  "
			    out-fmt-choices nil nil nil))
	      (error
	       "No known converter or no known output formats for %s files"
	       in-fmt))))
    (list in-file out-fmt)))

;;;###autoload
(defun org-e-odt-convert (&optional in-file out-fmt prefix-arg)
  "Convert IN-FILE to format OUT-FMT using a command line converter.
IN-FILE is the file to be converted.  If unspecified, it defaults
to variable `buffer-file-name'.  OUT-FMT is the desired output
format.  Use `org-e-odt-convert-process' as the converter.
If PREFIX-ARG is non-nil then the newly converted file is opened
using `org-open-file'."
  (interactive
   (append (org-e-odt-convert-read-params) current-prefix-arg))
  (org-e-odt-do-convert in-file out-fmt prefix-arg))

;;; Library Initializations

(mapc
 (lambda (desc)
   ;; Let Org open all OpenDocument files using system-registered app
   (add-to-list 'org-file-apps
		(cons (concat  "\\." (car desc) "\\'") 'system))
   ;; Let Emacs open all OpenDocument files in archive mode
   (add-to-list 'auto-mode-alist
		(cons (concat  "\\." (car desc) "\\'") 'archive-mode)))
 org-e-odt-file-extensions)

(provide 'org-e-odt)

;;; org-e-odt.el ends here
