;;; org-e-odt.el --- OpenDocument Text exporter for Org-mode

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'cl))

;; FIXMES
;; org-e-odt-preprocess-latex-fragments
;; org-export-as-e-odt-and-open
;; org-export-as-e-odt-batch
;; org-export-as-e-odt

(defun org-e-odt-get-style-name-for-entity (category &optional entity)
  (let ((entity (or entity 'default)))
    (or
     (cdr (assoc entity (cdr (assoc category
				    org-e-odt-org-styles-alist))))
     (cdr (assoc entity (cdr (assoc category
				    org-e-odt-default-org-styles-alist))))
     (error "Cannot determine style name for entity %s of type %s"
	    entity category))))

;; Following variable is let bound when `org-do-lparse' is in
;; progress. See org-html.el.

(defun org-e-odt-format-preamble (info)
  (let* ((title (org-export-data (plist-get info :title) info))
	 (author (and (plist-get info :with-author)
		      (let ((auth (plist-get info :author)))
			(and auth (org-export-data auth info)))))
	 (date (plist-get info :date))
	 (iso-date (org-e-odt-format-date date))
	 (date (org-e-odt-format-date date "%d %b %Y"))
	 (email (plist-get info :email))
	 ;; switch on or off above vars based on user settings
	 (author (and (plist-get info :with-author) (or author email)))
	 ;; (date (and (plist-get info :time-stamp-file) date))
	 (email (and (plist-get info :with-email) email)))
    (concat
     ;; title
     (when title
       (concat
	(org-e-odt-format-stylized-paragraph
	 'title (format "\n<text:title>%s</text:title>" title))
	;; separator
	"\n<text:p text:style-name=\"OrgTitle\"/>"))
     (cond
      ((and author (not email))
       ;; author only
       (concat
	(org-e-odt-format-stylized-paragraph
	 'subtitle
	 (format "<text:initial-creator>%s</text:initial-creator>" author))
	;; separator
	"\n<text:p text:style-name=\"OrgSubtitle\"/>"))
      ((and author email)
       ;; author and email
       (concat
	(org-e-odt-format-stylized-paragraph
	 'subtitle
	 (org-e-odt-format-link
	  (format "<text:initial-creator>%s</text:initial-creator>" author)
	  (concat "mailto:" email)))
	;; separator
	"\n<text:p text:style-name=\"OrgSubtitle\"/>")))
     ;; date
     (when date
       (concat
	(org-e-odt-format-stylized-paragraph
	 'subtitle
	 (org-e-odt-format-tags
	  '("<text:date style:data-style-name=\"%s\" text:date-value=\"%s\">"
	    . "</text:date>")
	  date "N75" iso-date))
	;; separator
	"<text:p text:style-name=\"OrgSubtitle\"/>")))))

(defun org-e-odt-begin-section (style &optional name)
  (let ((default-name (car (org-e-odt-add-automatic-style "Section"))))
    (format "<text:section text:style-name=\"%s\" text:name=\"%s\">"
	    style (or name default-name))))

(defun org-e-odt-end-section ()
  "</text:section>")

(defun org-e-odt-begin-paragraph (&optional style)
  (format "<text:p%s>" (org-e-odt-get-extra-attrs-for-paragraph-style style)))

(defun org-e-odt-end-paragraph ()
  "</text:p>")

(defun org-e-odt-get-extra-attrs-for-paragraph-style (style)
  (let (style-name)
    (setq style-name
	  (cond
	   ((stringp style) style)
	   ((symbolp style) (org-e-odt-get-style-name-for-entity
			     'paragraph style))))
    (unless style-name
      (error "Don't know how to handle paragraph style %s" style))
    (format " text:style-name=\"%s\"" style-name)))

(defun org-e-odt-format-stylized-paragraph (style text)
  (format "\n<text:p%s>%s</text:p>"
	  (org-e-odt-get-extra-attrs-for-paragraph-style style)
	  text))

(defun org-e-odt-format-author (&optional author )
  (when (setq author (or author (plist-get org-lparse-opt-plist :author)))
    (format "<dc:creator>%s</dc:creator>" author)))

(defun org-e-odt-format-date (&optional org-ts fmt)
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

(defun org-e-odt-begin-annotation (&optional author date)
  (concat
   "<office:annotation>\n"
   (and author (org-e-odt-format-author author))
   (org-e-odt-format-tags
    '("<dc:date>" . "</dc:date>")
    (org-e-odt-format-date
     (or date (plist-get org-lparse-opt-plist :date))))
   (org-e-odt-begin-paragraph)))

(defun org-e-odt-end-annotation ()
  "</office:annotation>")

(defun org-e-odt-begin-plain-list (ltype)
  (let* ((style-name (org-e-odt-get-style-name-for-entity 'list ltype))
	 (extra (concat
		 ;; (if (or org-lparse-list-table-p
		 ;; 	 (and (= 1 (length org-lparse-list-stack))
		 ;; 	      (null org-e-odt-list-stack-stashed)))
		 ;;     " text:continue-numbering=\"false\""
		 ;;   " text:continue-numbering=\"true\"")

		 " text:continue-numbering=\"true\""

		 (when style-name
		   (format " text:style-name=\"%s\""  style-name)))))
    (case ltype
      ((ordered unordered descriptive)
       (concat
	;; (org-e-odt-end-paragraph)
	(format "<text:list%s>" extra)))
      (t (error "Unknown list type: %s"  ltype)))))

(defun org-e-odt-end-plain-list (ltype)
  (if ltype "</text:list>"
    (error "Unknown list type: %s" ltype)))

(defun org-e-odt-begin-list-item (ltype &optional arg headline)
  (case ltype
    (ordered
     (assert (not headline) t)
     (let* ((counter arg) (extra ""))
       (concat "<text:list-item>" ;; (org-e-odt-begin-paragraph)
	       )
       ;; (if (= (length org-lparse-list-stack)
       ;;        (length org-e-odt-list-stack-stashed))
       ;;     "<text:list-header>" "<text:list-item>")
       ))
    (unordered
     (let* ((id arg) (extra ""))
       (concat
	"<text:list-item>"
	;; (org-e-odt-begin-paragraph)
	(if headline (org-e-odt-format-target headline id)
	  (org-e-odt-format-bookmark "" id)))
       ;; (if (= (length org-lparse-list-stack)
       ;;        (length org-e-odt-list-stack-stashed))
       ;;     "<text:list-header>" "<text:list-item>")
       ))
    (descriptive
     (assert (not headline) t)
     (let ((term (or arg "(no term)")))
       (concat
	(org-e-odt-format-tags
    	 '("<text:list-item>" . "</text:list-item>")
    	 (org-e-odt-format-stylized-paragraph 'definition-term term))
	(org-e-odt-begin-list-item 'unordered)
	(org-e-odt-begin-plain-list 'descriptive)
	(org-e-odt-begin-list-item 'unordered))))
    (t (error "Unknown list type"))))

(defun org-e-odt-end-list-item (ltype)
  (case ltype
    ((ordered unordered)
     ;; (org-lparse-insert-tag
     ;; (if (= (length org-lparse-list-stack)
     ;; 	     (length org-e-odt-list-stack-stashed))
     ;; 	  (prog1 "</text:list-header>"
     ;; 	    (setq org-e-odt-list-stack-stashed nil))
     ;; 	"</text:list-item>")
     "</text:list-item>"
     ;; )
     )
    (descriptive
     (concat
      (org-e-odt-end-list-item 'unordered)
      (org-e-odt-end-plain-list 'descriptive)
      (org-e-odt-end-list-item 'unordered)
      ))
    (t (error "Unknown list type"))))

(defun org-e-odt-write-automatic-styles ()
  "Write automatic styles to \"content.xml\"."
  (with-current-buffer
      (find-file-noselect (expand-file-name "content.xml") t)
    ;; position the cursor
    (goto-char (point-min))
    (re-search-forward "  </office:automatic-styles>" nil t)
    (goto-char (match-beginning 0))
    ;; write automatic table styles
    (loop for (style-name props) in
	  (plist-get org-e-odt-automatic-styles 'Table) do
	  (when (setq props (or (plist-get props :rel-width) 96))
	    (insert (format org-e-odt-table-style-format style-name props))))))

(defun org-e-odt-update-display-level (&optional level)
  (with-current-buffer
      (find-file-noselect (expand-file-name "content.xml") t)
    ;; position the cursor.
    (goto-char (point-min))
    ;; remove existing sequence decls.
    (when (re-search-forward "<text:sequence-decls" nil t)
      (delete-region (match-beginning 0)
		     (re-search-forward "</text:sequence-decls>" nil nil)))
    ;; insert new ones.
    (insert "
      <text:sequence-decls>")
    (loop for x in org-e-odt-category-map-alist
	  do (insert (format "
	<text:sequence-decl text:display-outline-level=\"%d\" text:name=\"%s\"/>"
			     level (nth 1 x))))
    (insert "
      </text:sequence-decls>")))

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

(defun org-e-odt-begin-toc (lang-specific-heading max-level)
  (concat
   (format "
    <text:table-of-content text:style-name=\"Sect2\" text:protected=\"true\" text:name=\"Table of Contents1\">
     <text:table-of-content-source text:outline-level=\"%d\">
      <text:index-title-template text:style-name=\"Contents_20_Heading\">%s</text:index-title-template>
" max-level lang-specific-heading)

   (let ((entry-templates ""))
     (loop for level from 1 upto 10
	   do (setq entry-templates
		    (concat entry-templates
			    (format
			     "
      <text:table-of-content-entry-template text:outline-level=\"%d\" text:style-name=\"Contents_20_%d\">
       <text:index-entry-link-start text:style-name=\"Internet_20_link\"/>
       <text:index-entry-chapter/>
       <text:index-entry-text/>
       <text:index-entry-link-end/>
      </text:table-of-content-entry-template>
" level level))))
     entry-templates)

   (format  "
     </text:table-of-content-source>

     <text:index-body>
      <text:index-title text:style-name=\"Sect1\" text:name=\"Table of Contents1_Head\">
       <text:p text:style-name=\"Contents_20_Heading\">%s</text:p>
      </text:index-title>
 " lang-specific-heading)))

(defun org-e-odt-end-toc ()
  (format "
     </text:index-body>
    </text:table-of-content>
"))

(defun org-e-odt-format-toc-entry (snumber todo headline tags href)

  ;; FIXME
  (setq headline (concat
  		  (and org-export-with-section-numbers
  		       (concat snumber ". "))
  		  headline
  		  (and tags
  		       (concat
  			(org-e-odt-format-spaces 3)
  			(org-e-odt-format-fontify tags "tag")))))
  (when todo
    (setq headline (org-e-odt-format-fontify headline "todo")))

  (let ((org-e-odt-suppress-xref t))
    (org-e-odt-format-link headline (concat  "#" href))))

(defun org-e-odt-format-toc-item (toc-entry level org-last-level)
  (let ((style (format "Contents_20_%d" level)))
    (concat "\n" (org-e-odt-format-stylized-paragraph style toc-entry) "\n")))

;; Following variable is let bound during 'ORG-LINK callback. See
;; org-html.el

(defun org-e-odt-format-link (desc href &optional attr)
  (cond
   ((and (= (string-to-char href) ?#) (not org-e-odt-suppress-xref))
    (setq href (substring href 1))
    (let ((xref-format "text"))
      (when (numberp desc)
	(setq desc (format "%d" desc) xref-format "number"))
      (when (listp desc)
	(setq desc (mapconcat 'identity desc ".") xref-format "chapter"))
      (setq href (concat org-e-odt-bookmark-prefix href))
      (org-e-odt-format-tags-simple
       '("<text:bookmark-ref text:reference-format=\"%s\" text:ref-name=\"%s\">" .
	 "</text:bookmark-ref>")
       desc xref-format href)))
   (org-lparse-link-description-is-image
    (org-e-odt-format-tags
     '("<draw:a xlink:type=\"simple\" xlink:href=\"%s\" %s>" . "</draw:a>")
     desc href (or attr "")))
   (t
    (org-e-odt-format-tags-simple
     '("<text:a xlink:type=\"simple\" xlink:href=\"%s\" %s>" . "</text:a>")
     desc href (or attr "")))))

(defun org-e-odt-format-spaces (n)
  (cond
   ((= n 1) " ")
   ((> n 1) (concat
	     " " (org-e-odt-format-tags "<text:s text:c=\"%d\"/>" "" (1- n))))
   (t "")))

(defun org-e-odt-format-tabs (&optional n)
  (let ((tab "<text:tab/>")
	(n (or n 1)))
    (insert tab)))

(defun org-e-odt-format-line-break ()
  (org-e-odt-format-tags "<text:line-break/>" ""))

(defun org-e-odt-format-horizontal-line ()
  (org-e-odt-format-stylized-paragraph 'horizontal-line ""))

(defun org-e-odt-encode-plain-text (line &optional no-whitespace-filling)
  (setq line (org-e-html-encode-plain-text line))
  (if no-whitespace-filling line
    (org-e-odt-fill-tabs-and-spaces line)))

(defun org-e-odt-format-line (line)
  (case org-lparse-dyn-current-environment
    (fixedwidth (concat
		 (org-e-odt-format-stylized-paragraph
		  'fixedwidth (org-e-odt-encode-plain-text line)) "\n"))
    (t (concat line "\n"))))

(defun org-e-odt-format-comment (fmt &rest args)
  (let ((comment (apply 'format fmt args)))
    (format "\n<!-- %s  -->\n" comment)))

(defun org-e-odt-format-org-entity (wd)
  (org-entity-get-representation wd 'utf8))

(defun org-e-odt-fill-tabs-and-spaces (line)
  (replace-regexp-in-string
   "\\([\t]\\|\\([ ]+\\)\\)" (lambda (s)
			       (cond
				((string= s "\t") (org-e-odt-format-tabs))
				(t (org-e-odt-format-spaces (length s))))) line))

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
		       (format org-src-block-paragraph-format
			       background-color-val color-val))
		      (t
		       (format
			"
<style:style style:name=\"%s\" style:family=\"text\">
  <style:text-properties fo:color=\"%s\"/>
 </style:style>" style-name color-val))))))
    (cons style-name style)))

(defun org-e-odt-insert-custom-styles-for-srcblocks (styles)
  "Save STYLES used for colorizing of source blocks.
Update styles.xml with styles that were collected as part of
`org-e-odt-hfy-face-to-css' callbacks."
  (when styles
    (with-current-buffer
	(find-file-noselect (expand-file-name "styles.xml") t)
      (goto-char (point-min))
      (when (re-search-forward "</office:styles>" nil t)
	(goto-char (match-beginning 0))
	(insert "\n<!-- Org Htmlfontify Styles -->\n" styles "\n")))))

(defun org-e-odt-remap-stylenames (style-name)
  (or
   (cdr (assoc style-name '(("timestamp-wrapper" . "OrgTimestampWrapper")
			    ("timestamp" . "OrgTimestamp")
			    ("timestamp-kwd" . "OrgTimestampKeyword")
			    ("tag" . "OrgTag")
			    ("todo" . "OrgTodo")
			    ("done" . "OrgDone")
			    ("target" . "OrgTarget"))))
   style-name))

(defun org-e-odt-format-fontify (text style &optional id)
  (let* ((style-name
	  (cond
	   ((stringp style)
	    (org-e-odt-remap-stylenames style))
	   ((symbolp style)
	    (org-e-odt-get-style-name-for-entity 'character style))
	   ((listp style)
	    (assert (< 1 (length style)))
	    (let ((parent-style (pop style)))
	      (mapconcat (lambda (s)
			   ;; (assert (stringp s) t)
			   (org-e-odt-remap-stylenames s)) style "")
	      (org-e-odt-remap-stylenames parent-style)))
	   (t (error "Don't how to handle style %s"  style)))))
    (org-e-odt-format-tags-simple
     '("<text:span text:style-name=\"%s\">" . "</text:span>")
     text style-name)))

(defun org-e-odt-relocate-relative-path (path dir)
  (if (file-name-absolute-p path) path
    (file-relative-name (expand-file-name path dir)
			(expand-file-name "eyecandy" dir))))

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
				    (org-element-property
				     :value element)))
		       (formula-link (org-e-odt-format-latex
				      latex-frag 'mathml)))
		  (and formula-link
		       (string-match "file:\\([^]]*\\)" formula-link)
		       (match-string 1 formula-link))))
	       (t (error "what is this?"))))
	 (caption-from
	  (case (org-element-type element)
	    (link (org-export-get-parent-paragraph element info))
	    (t element)))
	 (captions (org-e-odt-format-label caption-from info 'definition))
	 (caption (car captions))
	 (href
	  (org-e-odt-format-tags
	   "<draw:object xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>" ""
	   (file-name-directory (org-e-odt-copy-formula-file src))))
	 (embed-as (if caption 'paragraph 'character))
	 width height)
    (cond
     ((eq embed-as 'character)
      (org-e-odt-format-entity "InlineFormula" href width height))
     (t
      (let ((table-info nil)
	    (table-info
	     '(:alignment ["c" "c"]
			  :column-groups [nil nil]
			  :row-groups (0)
			  :special-column-p nil :width [8 1]))
	    (org-lparse-table-ncols 2)) ; FIXME
	(org-e-odt-list-table		; FIXME
	 (list
	  (list
	   (org-e-odt-format-entity
	    "CaptionedDisplayFormula" href width height captions)
	   (let* ((org-e-odt-category-map-alist
		   '(("__Table__" "Table" "value")
		     ("__Figure__" "Illustration" "value")
		     ("__MathFormula__" "Text" "math-label")
		     ("__DvipngImage__" "Equation" "value")
		     ("__Listing__" "Listing" "value"))))
	     (car (org-e-odt-format-label caption-from info 'definition)))))
	 '(table (:attr_odt (":style \"OrgEquation\""))) info))))))

(defun org-e-odt-copy-formula-file (path)
  "Returns the internal name of the file"
  (let* ((src-file (expand-file-name
		    path (file-name-directory org-current-export-file)))
	 (target-dir (format "Formula-%04d/"
			     (incf org-e-odt-embedded-formulas-count)))
	 (target-file (concat target-dir "content.xml")))
    (message "Embedding %s as %s ..."
	     (substring-no-properties path) target-file)

    (make-directory target-dir)
    (org-e-odt-create-manifest-file-entry
     "application/vnd.oasis.opendocument.formula" target-dir "1.2")

    (case (org-e-odt-is-formula-link-p src-file)
      (mathml
       (copy-file src-file target-file 'overwrite))
      (odf
       (org-e-odt-zip-extract-one src-file "content.xml" target-dir))
      (t
       (error "%s is not a formula file" src-file)))

    (org-e-odt-create-manifest-file-entry "text/xml" target-file)
    target-file))

(defun org-e-odt-is-formula-link-p (file)
  (let ((case-fold-search nil))
    (cond
     ((string-match "\\.\\(mathml\\|mml\\)\\'" file)
      'mathml)
     ((string-match "\\.odf\\'" file)
      'odf))))

(defun org-e-odt-format-org-link (opt-plist type-1 path fragment desc attr
					    descp)
  "Make a OpenDocument link.
OPT-PLIST is an options list.
TYPE-1 is the device-type of the link (THIS://foo.html).
PATH is the path of the link (http://THIS#location).
FRAGMENT is the fragment part of the link, if any (foo.html#THIS).
DESC is the link description, if any.
ATTR is a string of other attributes of the a element."
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
	      filename org-e-odt-inline-image-extensions)
	     (not descp))
	(org-e-odt-format-inline-image thefile))
       ;; check for embedded formulas
       ((and (member type '("file"))
	     (not fragment)
	     (org-e-odt-is-formula-link-p filename)
	     (or (not descp)))
	(org-e-odt-format-formula thefile))
       ((string= type "coderef")
	(let* ((ref fragment)
	       (lineno-or-ref (cdr (assoc ref org-export-code-refs)))
	       (desc (and descp desc))
	       (org-e-odt-suppress-xref nil)
	       (href (org-xml-format-href (concat "#coderef-" ref))))
	  (cond
	   ((and (numberp lineno-or-ref) (not desc))
	    (org-e-odt-format-link lineno-or-ref href))
	   ((and (numberp lineno-or-ref) desc
		 (string-match (regexp-quote (concat "(" ref ")")) desc))
	    (format (replace-match "%s" t t desc)
		    (org-e-odt-format-link lineno-or-ref href)))
	   (t
	    (setq desc (format
			(if (and desc (string-match
				       (regexp-quote (concat "(" ref ")"))
				       desc))
			    (replace-match "%s" t t desc)
			  (or desc "%s"))
			lineno-or-ref))
	    (org-e-odt-format-link (org-xml-format-desc desc) href)))))
       (t
	(when (string= type "file")
	  (setq thefile
		(cond
		 ((file-name-absolute-p path)
		  (concat "file://" (expand-file-name path)))
		 (t (org-e-odt-relocate-relative-path
		     thefile org-current-export-file)))))

	(when (and (member type '("" "http" "https" "file")) fragment)
	  (setq thefile (concat thefile "#" fragment)))

	(setq thefile (org-xml-format-href thefile))

	(when (not (member type '("" "file")))
	  (setq thefile (concat type ":" thefile)))

	(let ((org-e-odt-suppress-xref nil))
	  (org-e-odt-format-link
	   (org-xml-format-desc desc) thefile attr)))))))

(defun org-e-odt-format-anchor (text name &optional class)
  (org-e-odt-format-target text name))

(defun org-e-odt-format-bookmark (text id)
  (if id
      (org-e-odt-format-tags "<text:bookmark text:name=\"%s\"/>" text id)
    text))

(defun org-e-odt-format-target (text id)
  (let ((name (concat org-e-odt-bookmark-prefix id)))
    (concat
     (and id (org-e-odt-format-tags
	      "<text:bookmark-start text:name=\"%s\"/>" "" name))
     (org-e-odt-format-bookmark text id)
     (and id (org-e-odt-format-tags
	      "<text:bookmark-end text:name=\"%s\"/>" "" name)))))

(defun org-e-odt-format-footnote (n def)
  (setq n (format "%d" n))
  (let ((id (concat  "fn" n))
	(note-class "footnote")
	(par-style "Footnote"))
    (org-e-odt-format-tags-simple
     '("<text:note text:id=\"%s\" text:note-class=\"%s\">" . "</text:note>")
     (concat
      (org-e-odt-format-tags-simple
       '("<text:note-citation>" . "</text:note-citation>") n)
      (org-e-odt-format-tags-simple
       '("<text:note-body>" . "</text:note-body>") def))
     id note-class)))

(defun org-e-odt-format-footnote-reference (n def refcnt)
  (if (= refcnt 1)
      (org-e-odt-format-footnote n def)
    (org-e-odt-format-footnote-ref n)))

(defun org-e-odt-format-footnote-ref (n)
  (setq n (format "%d" n))
  (let ((note-class "footnote")
	(ref-format "text")
	(ref-name (concat "fn" n)))
    (org-e-odt-format-tags-simple
     '("<text:span text:style-name=\"%s\">" . "</text:span>")
     (org-e-odt-format-tags-simple
      '("<text:note-ref text:note-class=\"%s\" text:reference-format=\"%s\" text:ref-name=\"%s\">" . "</text:note-ref>")
      n note-class ref-format ref-name)
     "OrgSuperscript")))

(defun org-e-odt-element-attributes (element info)
  (let* ((raw-attr (org-element-property :attr_odt element))
	 (raw-attr (and raw-attr
			(org-trim (mapconcat #'identity raw-attr " ")))))
    (unless (and raw-attr (string-match "\\`(.*)\\'" raw-attr))
      (setq raw-attr (format "(%s)" raw-attr)))
    (ignore-errors (read raw-attr))))

(defun org-e-odt-format-object-description (title description)
  (concat (and title (org-e-odt-format-tags
		      '("<svg:title>" . "</svg:title>")
		      (org-e-odt-encode-plain-text title t)))
	  (and description (org-e-odt-format-tags
			    '("<svg:desc>" . "</svg:desc>")
			    (org-e-odt-encode-plain-text description t)))))

(defun org-e-odt-format-frame (text width height style &optional
				    extra anchor-type)
  (let ((frame-attrs
	 (concat
	  (if width (format " svg:width=\"%0.2fcm\"" width) "")
	  (if height (format " svg:height=\"%0.2fcm\"" height) "")
	  extra
	  (format " text:anchor-type=\"%s\"" (or anchor-type "paragraph")))))
    (org-e-odt-format-tags
     '("<draw:frame draw:style-name=\"%s\"%s>" . "</draw:frame>")
     (concat text (org-e-odt-format-object-description
		   (get-text-property 0 :title text)
		   (get-text-property 0 :description text)))
     style frame-attrs)))

(defun org-e-odt-format-textbox (text width height style &optional
				      extra anchor-type)
  (org-e-odt-format-frame
   (org-e-odt-format-tags
    '("<draw:text-box %s>" . "</draw:text-box>")
    text (concat (format " fo:min-height=\"%0.2fcm\"" (or height .2))
		 (unless width
		   (format " fo:min-width=\"%0.2fcm\"" (or width .2)))))
   width nil style extra anchor-type))

(defun org-e-odt-merge-frame-params(default-frame-params user-frame-params)
  (if (not user-frame-params) default-frame-params
    (assert (= (length default-frame-params) 3))
    (assert (= (length user-frame-params) 3))
    (loop for user-frame-param in user-frame-params
	  for default-frame-param in default-frame-params
	  collect (or user-frame-param default-frame-param))))

(defun org-e-odt-copy-image-file (path)
  "Returns the internal name of the file"
  (let* ((image-type (file-name-extension path))
	 (media-type (format "image/%s" image-type))
	 (src-file (expand-file-name
		    path (file-name-directory org-current-export-file)))
	 (target-dir "Images/")
	 (target-file
	  (format "%s%04d.%s" target-dir
		  (incf org-e-odt-embedded-images-count) image-type)))
    (message "Embedding %s as %s ..."
	     (substring-no-properties path) target-file)

    (when (= 1 org-e-odt-embedded-images-count)
      (make-directory target-dir)
      (org-e-odt-create-manifest-file-entry "" target-dir))

    (copy-file src-file target-file 'overwrite)
    (org-e-odt-create-manifest-file-entry media-type target-file)
    target-file))

(defun org-e-odt-do-image-size (probe-method file &optional dpi anchor-type)
  (setq dpi (or dpi org-e-odt-pixels-per-inch))
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
       (size-in-cms (ignore-errors	; Emacs could be in batch mode
		      (clear-image-cache)
		      (image-size (create-image file) 'pixels))))
      (imagemagick
       (size-in-cms
	(let ((dim (shell-command-to-string
		    (format "identify -format \"%%w:%%h\" \"%s\"" file))))
	  (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" dim)
	    (cons (string-to-number (match-string 1 dim))
		  (string-to-number (match-string 2 dim)))))))
      (t
       (cdr (assoc-string anchor-type
			  org-e-odt-default-image-sizes-alist))))))

(defun org-e-odt-image-size-from-file (file &optional user-width
					    user-height scale dpi embed-as)
  (unless (file-name-absolute-p file)
    (setq file (expand-file-name
		file (file-name-directory org-current-export-file))))
  (let* (size width height)
    (unless (and user-height user-width)
      (loop for probe-method in org-e-odt-image-size-probe-method
	    until size
	    do (setq size (org-e-odt-do-image-size
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
    ;; ensure that an embedded image fits comfortably within a page
    (let ((max-width (car org-e-odt-max-image-size))
	  (max-height (cdr org-e-odt-max-image-size)))
      (when (or (> width max-width) (> height max-height))
	(let* ((scale1 (/ max-width width))
	       (scale2 (/ max-height height))
	       (scale (min scale1 scale2)))
	  (setq width (* scale width) height (* scale height)))))
    (cons width height)))

(defun org-e-odt-format-label (element info op)
  (let* ((caption-from
	  (case (org-element-type element)
	    (link (org-export-get-parent-paragraph element info))
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
	     ;; grok language setting
	     (en-strings (assoc-default "en" org-e-odt-category-strings))
	     (lang (plist-get info :language)) ; FIXME
	     (lang-strings (assoc-default lang org-e-odt-category-strings))
	     ;; retrieve localized category sting
	     (pos (- (length org-e-odt-category-map-alist)
		     (length (memq label-props org-e-odt-category-map-alist))))
	     (category (or (nth pos lang-strings) (nth pos en-strings))))
	(case op
	  (definition
	    ;; assign an internal label, if user has not provided one
	    (setq label (or label (format  "%s-%s" default-category seqno)))
	    (setq label (org-solidify-link-text label))

	    (cons
	     (format-spec
	      (cadr (assoc-string label-style org-e-odt-label-styles t))
	      `((?e . ,category)
		(?n . ,(org-e-odt-format-tags-simple
			'("<text:sequence text:ref-name=\"%s\" text:name=\"%s\" text:formula=\"ooow:%s+1\" style:num-format=\"1\">" . "</text:sequence>")
			seqno label counter counter))
		(?c . ,(or caption ""))))
	     short-caption))
	  (reference
	   (assert label)
	   (setq label (org-solidify-link-text label))
	   (let* ((fmt (cddr (assoc-string label-style org-e-odt-label-styles t)))
		  (fmt1 (car fmt))
		  (fmt2 (cadr fmt)))
	     (org-e-odt-format-tags-simple
	      '("<text:sequence-ref text:reference-format=\"%s\" text:ref-name=\"%s\">"
		. "</text:sequence-ref>")
	      (format-spec fmt2 `((?e . ,category)
				  (?n . ,seqno))) fmt1 label)))
	  (t (error "Unknow %S on label" op)))))))

(defun org-e-odt-format-tags-1 (tag text prefix suffix &rest args)
  (cond
   ((consp tag)
    (concat prefix (apply 'format (car tag) args) text suffix
	    (format (cdr tag))))
   ((stringp tag)			; singleton tag
    (concat prefix (apply 'format tag args) text))))

(defun org-e-odt-format-tags (tag text &rest args)
  (apply 'org-e-odt-format-tags-1 tag text "\n" "\n" args))

(defun org-e-odt-format-tags-simple (tag text &rest args)
  (apply 'org-e-odt-format-tags-1 tag text nil nil args))

(defun org-e-odt-init-outfile ()
  (unless (executable-find "zip")
    ;; Not at all OSes ship with zip by default
    (error "Executable \"zip\" needed for creating OpenDocument files"))

  (let* ((outdir (make-temp-file
		  (format org-e-odt-tmpdir-prefix 'odt) t)) ; FIXME
	 (content-file (expand-file-name "content.xml" outdir)))

    ;; reset variables
    (setq org-e-odt-manifest-file-entries nil
	  org-e-odt-embedded-images-count 0
	  org-e-odt-embedded-formulas-count 0
	  org-e-odt-section-count 0
	  org-e-odt-entity-labels-alist nil
	  org-e-odt-list-stack-stashed nil
	  org-e-odt-automatic-styles nil
	  org-e-odt-object-counters nil
	  org-e-odt-entity-counts-plist nil)

    ;; let `htmlfontify' know that we are interested in collecting
    ;; styles - FIXME

    (setq hfy-user-sheet-assoc nil)

    ;; init conten.xml
    (with-current-buffer
	(find-file-noselect content-file t)
      (current-buffer))))

(defun org-e-odt-save-as-outfile (target opt-plist)
  ;; write automatic styles
  (org-e-odt-write-automatic-styles)

  ;; update display levels
  (org-e-odt-update-display-level org-e-odt-display-outline-level)

  ;; write styles file
  ;; (when (equal org-lparse-backend 'odt) FIXME
  ;;   )

  ;; (org-e-odt-update-styles-file opt-plist)

  ;; create mimetype file
  (let ((mimetype (org-e-odt-write-mimetype-file ;; org-lparse-backend FIXME
		   'odt)))
    (org-e-odt-create-manifest-file-entry mimetype "/" "1.2"))

  ;; create a manifest entry for content.xml
  (org-e-odt-create-manifest-file-entry "text/xml" "content.xml")

  ;; write out the manifest entries before zipping
  (org-e-odt-write-manifest-file)

  (let ((xml-files '("mimetype" "META-INF/manifest.xml" "content.xml"
		     "meta.xml"))
	(zipdir default-directory))
    (when (or t (equal org-lparse-backend 'odt)) ; FIXME
      (push "styles.xml" xml-files))
    (message "Switching to directory %s" (expand-file-name zipdir))

    ;; save all xml files
    (mapc (lambda (file)
	    (with-current-buffer
		(find-file-noselect (expand-file-name file) t)
	      ;; prettify output if needed
	      (when org-e-odt-prettify-xml
		(indent-region (point-min) (point-max)))
	      (save-buffer 0)))
	  xml-files)

    (let* ((target-name (file-name-nondirectory target))
	   (target-dir (file-name-directory target))
	   (cmds `(("zip" "-mX0" ,target-name "mimetype")
		   ("zip" "-rmTq" ,target-name "."))))
      (when (file-exists-p target)
	;; FIXME: If the file is locked this throws a cryptic error
	(delete-file target))

      (let ((coding-system-for-write 'no-conversion) exitcode err-string)
	(message "Creating odt file...")
	(mapc
	 (lambda (cmd)
	   (message "Running %s" (mapconcat 'identity cmd " "))
	   (setq err-string
		 (with-output-to-string
		   (setq exitcode
			 (apply 'call-process (car cmd)
				nil standard-output nil (cdr cmd)))))
	   (or (zerop exitcode)
	       (ignore (message "%s" err-string))
	       (error "Unable to create odt file (%S)" exitcode)))
	 cmds))

      ;; move the file from outdir to target-dir
      (rename-file target-name target-dir)

      ;; kill all xml buffers
      (mapc (lambda (file)
	      (kill-buffer
	       (find-file-noselect (expand-file-name file zipdir) t)))
	    xml-files)

      (delete-directory zipdir)))
  (message "Created %s" target)
  (set-buffer (find-file-noselect target t)))


(defun org-e-odt-create-manifest-file-entry (&rest args)
  (push args org-e-odt-manifest-file-entries))

(defun org-e-odt-write-manifest-file ()
  (make-directory "META-INF")
  (let ((manifest-file (expand-file-name "META-INF/manifest.xml")))
    (with-current-buffer
	(find-file-noselect manifest-file t)
      (insert
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\" manifest:version=\"1.2\">\n")
      (mapc
       (lambda (file-entry)
	 (let* ((version (nth 2 file-entry))
		(extra (if version
			   (format  " manifest:version=\"%s\"" version)
			 "")))
	   (insert
	    (format org-e-odt-manifest-file-entry-tag
		    (nth 0 file-entry) (nth 1 file-entry) extra))))
       org-e-odt-manifest-file-entries)
      (insert "\n</manifest:manifest>"))))

(defun org-e-odt-update-meta-file (info) ; FIXME opt-plist
  (let ((title (org-export-data (plist-get info :title) info))
	(author (or (let ((auth (plist-get info :author)))
		      (and auth (org-export-data auth info))) ""))
	(date (org-e-odt-format-date (plist-get info :date)))
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
      (org-e-odt-format-author author) "\n"
      (format "<meta:initial-creator>%s</meta:initial-creator>\n" author)
      (format "<dc:date>%s</dc:date>\n" date)
      (format "<meta:creation-date>%s</meta:creation-date>\n" date)
      (format "<meta:generator>%s</meta:generator>\n"
	      (concat (and org-export-creator-info org-export-creator-string)))
      (format "<meta:keyword>%s</meta:keyword>\n" keywords)
      (format "<dc:subject>%s</dc:subject>\n" description)
      (format "<dc:title>%s</dc:title>\n" title)
      "\n"
      "  </office:meta>\n" "</office:document-meta>")
     nil (expand-file-name "meta.xml")))

  ;; create a manifest entry for meta.xml
  (org-e-odt-create-manifest-file-entry "text/xml" "meta.xml"))

(defun org-e-odt-update-styles-file (info)
  ;; write styles file
  (let ((styles-file (plist-get info :odt-styles-file)))
    (org-e-odt-copy-styles-file (and styles-file
				     (read (org-trim styles-file))))

    ;; FIXME: Who is opening an empty styles.xml before this point?
    (with-current-buffer
	(find-file-noselect (expand-file-name "styles.xml") t)
      (revert-buffer t t)))

  ;; Write custom styles for source blocks
  (org-e-odt-insert-custom-styles-for-srcblocks
   (mapconcat
    (lambda (style)
      (format " %s\n" (cddr style)))
    hfy-user-sheet-assoc "")))

(defun org-e-odt-write-mimetype-file (format)
  ;; create mimetype file
  (let ((mimetype
	 (case format
	   (odt "application/vnd.oasis.opendocument.text")
	   (odf "application/vnd.oasis.opendocument.formula")
	   (t (error "Unknown OpenDocument backend %S" org-lparse-backend)))))
    (write-region mimetype nil (expand-file-name "mimetype"))
    mimetype))

(declare-function org-create-math-formula "org"
		  (latex-frag &optional mathml-file))

(defun org-e-odt-get (what &optional opt-plist)
  (case what
    (EXPORT-DIR (org-export-directory :html opt-plist))
    (TABLE-FIRST-COLUMN-AS-LABELS nil)
    (CODING-SYSTEM-FOR-WRITE 'utf-8)
    (CODING-SYSTEM-FOR-SAVE 'utf-8)
    (t (error "Unknown property: %s"  what))))

(defun org-e-odt-do-preprocess-latex-fragments ()
  "Convert LaTeX fragments to images."
  (let* ((latex-frag-opt (plist-get org-lparse-opt-plist :LaTeX-fragments))
	 (latex-frag-opt		;  massage the options
	  (or (and (member latex-frag-opt '(mathjax t))
		   (not (and (fboundp 'org-format-latex-mathml-available-p)
			     (org-format-latex-mathml-available-p)))
		   (prog1 org-lparse-latex-fragment-fallback
		     (org-lparse-warn
		      (concat
		       "LaTeX to MathML converter not available. "
		       (format "Using %S instead."
			       org-lparse-latex-fragment-fallback)))))
	      latex-frag-opt))
	 cache-dir display-msg)
    (cond
     ((eq latex-frag-opt 'dvipng)
      (setq cache-dir "ltxpng/")
      (setq display-msg "Creating LaTeX image %s"))
     ((member latex-frag-opt '(mathjax t))
      (setq latex-frag-opt 'mathml)
      (setq cache-dir "ltxmathml/")
      (setq display-msg "Creating MathML formula %s")))
    (when (and org-current-export-file)
      (org-format-latex
       (concat cache-dir (file-name-sans-extension
			  (file-name-nondirectory org-current-export-file)))
       org-current-export-dir nil display-msg
       nil nil latex-frag-opt))))

(eval-after-load 'org-odt
  '(ad-deactivate 'org-format-latex-as-mathml))

 ; FIXME

;; (defadvice org-format-latex-as-mathml	; FIXME
;;   (after org-e-odt-protect-latex-fragment activate)
;;   "Encode LaTeX fragment as XML.
;; Do this when translation to MathML fails."
;;   (when (or (not (> (length ad-return-value) 0))
;; 	    (get-text-property 0 'org-protected ad-return-value))
;;     (setq ad-return-value
;; 	  (org-propertize (org-e-odt-encode-plain-text (ad-get-arg 0))
;; 			  'org-protected t))))

(defun org-e-odt-zip-extract-one (archive member &optional target)
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

(defun org-e-odt-zip-extract (archive members &optional target)
  (when (atom members) (setq members (list members)))
  (mapc (lambda (member)
	  (org-e-odt-zip-extract-one archive member target))
	members))

(defun org-e-odt-copy-styles-file (&optional styles-file)
  ;; Non-availability of styles.xml is not a critical error. For now
  ;; throw an error purely for aesthetic reasons.
  (setq styles-file (or styles-file
			org-e-odt-styles-file
			(expand-file-name "OrgOdtStyles.xml"
					  org-e-odt-styles-dir)
			(error "org-e-odt: Missing styles file?")))
  (cond
   ((listp styles-file)
    (let ((archive (nth 0 styles-file))
	  (members (nth 1 styles-file)))
      (org-e-odt-zip-extract archive members)
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
	(copy-file styles-file (expand-file-name "styles.xml") t))
       ((member styles-file-type '("odt" "ott"))
	(org-e-odt-zip-extract styles-file "styles.xml")))))
   (t
    (error (format "Invalid specification of styles.xml file: %S"
		   org-e-odt-styles-file))))

  ;; create a manifest entry for styles.xml
  (org-e-odt-create-manifest-file-entry "text/xml" "styles.xml"))

(defun org-e-odt-configure-outline-numbering ()
  "Outline numbering is retained only upto LEVEL.
To disable outline numbering pass a LEVEL of 0."
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
  (save-buffer 0))

;;;###autoload
(defun org-export-as-odf (latex-frag &optional odf-file)
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
  (let* ((org-lparse-backend 'odf)
	 org-lparse-opt-plist
	 (filename (or odf-file
		       (expand-file-name
			(concat
			 (file-name-sans-extension
			  (or (file-name-nondirectory buffer-file-name)))
			 "." "odf")
			(file-name-directory buffer-file-name))))
	 (buffer (find-file-noselect (org-e-odt-init-outfile filename)))
	 (coding-system-for-write 'utf-8)
	 (save-buffer-coding-system 'utf-8))
    (set-buffer buffer)
    (set-buffer-file-coding-system coding-system-for-write)
    (let ((mathml (org-create-math-formula latex-frag)))
      (unless mathml (error "No Math formula created"))
      (insert mathml)
      (or (org-export-push-to-kill-ring
	   (upcase (symbol-name org-lparse-backend)))
	  (message "Exporting... done")))
    (org-e-odt-save-as-outfile filename nil ; FIXME
			       )))

;;;###autoload
(defun org-export-as-odf-and-open ()
  "Export LaTeX fragment as OpenDocument formula and immediately open it.
Use `org-export-as-odf' to read LaTeX fragment and OpenDocument
formula file."
  (interactive)
  (org-lparse-and-open
   nil nil nil (call-interactively 'org-export-as-odf)))




;;; Driver Starts here
;;; Dependencies

(require 'format-spec)
(eval-when-compile (require 'cl) (require 'table))



;;; Hooks

;; FIXME: it already exists in org-e-odt.el
;;; Function Declarations

(declare-function org-element-property "org-element" (property element))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-parse-secondary-string
		  "org-element" (string restriction &optional buffer))
(defvar org-element-string-restrictions)
(defvar org-element-object-restrictions)

(declare-function org-export-data "org-export" (data info))
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
(declare-function org-export-solidify-link-text "org-export" (s))
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





(declare-function hfy-face-to-style "htmlfontify" (fn))
(declare-function hfy-face-or-def-to-name "htmlfontify" (fn))
(declare-function archive-zip-extract "arc-mode.el" (archive name))

;;; Internal Variables

;;;; ODT Internal Variables

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
      (error "Error (org-e-odt): Cannot find factory styles files. Aborting."))
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

(defconst org-e-odt-tmpdir-prefix "%s-")
(defconst org-e-odt-bookmark-prefix "OrgXref.")

(defconst org-e-odt-manifest-file-entry-tag
  "
<manifest:file-entry manifest:media-type=\"%s\" manifest:full-path=\"%s\"%s/>")



(defvar org-lparse-dyn-first-heading-pos) ; let bound during org-do-lparse

(defvar org-e-odt-suppress-xref nil)
(defvar org-e-odt-file-extensions
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

(defvar org-e-odt-default-org-styles-alist
  '((paragraph . ((default . "Text_20_body")
		  (fixedwidth . "OrgFixedWidthBlock")
		  (verse . "OrgVerse")
		  (quote . "Quotations")
		  (blockquote . "Quotations")
		  (center . "OrgCenter")
		  (left . "OrgLeft")
		  (right . "OrgRight")
		  (title . "OrgTitle")
		  (subtitle . "OrgSubtitle")
		  (footnote . "Footnote")
		  (src . "OrgSrcBlock")
		  (illustration . "Illustration")
		  (table . "Table")
		  (listing . "Listing")
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
	     (descriptive . "OrgDescriptionList"))))
  "Default styles for various entities.")

(defvar org-e-odt-org-styles-alist org-e-odt-default-org-styles-alist)

;;;_. callbacks
;;;_. control callbacks
;;;_ , document body

(defvar org-lparse-body-only)		; let bound during org-do-lparse
(defvar org-lparse-opt-plist)		    ; bound during org-do-lparse
(defvar org-lparse-list-stack) ; dynamically bound in org-do-lparse
(defvar org-e-odt-list-stack-stashed)
(defvar org-lparse-table-ncols)
(defvar org-e-odt-table-rowgrp-open)
(defvar org-e-odt-table-rownum)
(defvar org-e-odt-table-cur-rowgrp-is-hdr)
(defvar org-lparse-table-is-styled)
(defvar org-lparse-table-rowgrp-info)
(defvar org-lparse-table-colalign-vector)

(defvar org-e-odt-table-style nil
  "Table style specified by \"#+ATTR_ODT: <style-name>\" line.
This is set during `org-e-odt-begin-table'.")

(defvar org-e-odt-table-style-spec nil
  "Entry for `org-e-odt-table-style' in `org-e-odt-table-styles'.")


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

(defvar org-e-odt-table-indentedp nil)
(defvar org-lparse-table-colalign-info)
(defvar org-lparse-link-description-is-image nil)


(defvar org-src-block-paragraph-format
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

(defvar org-e-odt-entity-labels-alist nil
  "Associate Labels with the Labeled entities.
Each element of the alist is of the form (LABEL-NAME
CATEGORY-NAME SEQNO LABEL-STYLE-NAME).  LABEL-NAME is same as
that specified by \"#+LABEL: ...\" line.  CATEGORY-NAME is the
type of the entity that LABEL-NAME is attached to.  CATEGORY-NAME
can be one of \"Table\", \"Figure\" or \"Equation\".  SEQNO is
the unique number assigned to the referenced entity on a
per-CATEGORY basis.  It is generated sequentially and is 1-based.
LABEL-STYLE-NAME is a key `org-e-odt-label-styles'.

See `org-e-odt-add-label-definition' and
`org-e-odt-fixup-label-references'.")

(defvar org-e-odt-entity-counts-plist nil
  "Plist of running counters of SEQNOs for each of the CATEGORY-NAMEs.
See `org-e-odt-entity-labels-alist' for known CATEGORY-NAMEs.")

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

(defcustom org-e-odt-category-strings
  '(("en" "Table" "Figure" "Equation" "Equation" "Listing"))
  "Specify category strings for various captionable entities.
Captionable entity can be one of a Table, an Embedded Image, a
LaTeX fragment (generated with dvipng) or a Math Formula.

For example, when `org-export-default-language' is \"en\", an
embedded image will be captioned as \"Figure 1: Orgmode Logo\".
If you want the images to be captioned instead as \"Illustration
1: Orgmode Logo\", then modify the entry for \"en\" as shown
below.

  \(setq org-e-odt-category-strings
	'\(\(\"en\" \"Table\" \"Illustration\"
	   \"Equation\" \"Equation\"\)\)\)"
  :group 'org-export-e-odt
  :version "24.1"
  :type '(repeat (list (string :tag "Language tag")
		       (choice :tag "Table"
			       (const :tag "Use Default" nil)
			       (string :tag "Category string"))
		       (choice :tag "Figure"
			       (const :tag "Use Default" nil)
			       (string :tag "Category string"))
		       (choice :tag "Math Formula"
			       (const :tag "Use Default" nil)
			       (string :tag "Category string"))
		       (choice :tag "Dvipng Image"
			       (const :tag "Use Default" nil)
			       (string :tag "Category string"))
		       (choice :tag "Listing"
			       (const :tag "Use Default" nil)
			       (string :tag "Category string")))))

(defvar org-e-odt-category-map-alist
  '(("__Table__" "Table" "value")
    ("__Figure__" "Illustration" "value")
    ("__MathFormula__" "Text" "math-formula")
    ("__DvipngImage__" "Equation" "value")
    ("__Listing__" "Listing" "value")
    ;; ("__Table__" "Table" "category-and-value")
    ;; ("__Figure__" "Figure" "category-and-value")
    ;; ("__DvipngImage__" "Equation" "category-and-value")
    )
  "Map a CATEGORY-HANDLE to OD-VARIABLE and LABEL-STYLE.
This is a list where each entry is of the form \\(CATEGORY-HANDLE
OD-VARIABLE LABEL-STYLE\\).  CATEGORY_HANDLE identifies the
captionable entity in question.  OD-VARIABLE is the OpenDocument
sequence counter associated with the entity.  These counters are
declared within
\"<text:sequence-decls>...</text:sequence-decls>\" block of
`org-e-odt-content-template-file'.  LABEL-STYLE is a key
into `org-e-odt-label-styles' and specifies how a given entity
should be captioned and referenced.

The position of a CATEGORY-HANDLE in this list is used as an
index in to per-language entry for
`org-e-odt-category-strings' to retrieve a CATEGORY-NAME.
This CATEGORY-NAME is then used for qualifying the user-specified
captions on export.")

(defvar org-e-odt-manifest-file-entries nil)
(defvar hfy-user-sheet-assoc)		; bound during org-do-lparse
(defvar org-lparse-latex-fragment-fallback) ; set by org-do-lparse


;;;; HTML Internal Variables

(defvar org-e-odt-option-alist
  '(
    ;; (:agenda-style nil nil org-agenda-export-html-style)
    ;; (:convert-org-links nil nil org-e-odt-link-org-files-as-html)
    ;; ;; FIXME Use (org-xml-encode-org-text-skip-links s) ??
    ;; ;; (:expand-quoted-html nil "@" org-e-odt-expand)
    ;; (:inline-images nil nil org-e-odt-inline-images)
    ;; ;; (:link-home nil nil org-e-odt-link-home) FIXME
    ;; ;; (:link-up nil nil org-e-odt-link-up) FIXME
    ;; (:style nil nil org-e-odt-style)
    ;; (:style-extra nil nil org-e-odt-style-extra)
    ;; (:style-include-default nil nil org-e-odt-style-include-default)
    ;; (:style-include-scripts nil nil org-e-odt-style-include-scripts)
    ;; ;; (:timestamp nil nil org-e-odt-with-timestamp)
    ;; (:html-extension nil nil org-e-odt-extension)
    ;; (:html-postamble nil nil org-e-odt-postamble)
    ;; (:html-preamble nil nil org-e-odt-preamble)
    ;; (:html-table-tag nil nil org-e-odt-table-tag)
    ;; (:xml-declaration nil nil org-e-odt-xml-declaration)
    (:odt-styles-file "ODT_STYLES_FILE" nil nil t)
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

;; FIXME: it already exists in org-e-odt.el
(defconst org-e-odt-cvt-link-fn
   nil
   "Function to convert link URLs to exportable URLs.
Takes two arguments, TYPE and PATH.
Returns exportable url as (TYPE PATH), or nil to signal that it
didn't handle this case.
Intended to be locally bound around a call to `org-export-as-html'." )




(defvar org-e-odt-format-table-no-css)
(defvar htmlize-buffer-places)  ; from htmlize.el
(defvar body-only) ; dynamically scoped into this.

(defvar org-e-odt-table-rowgrp-open)
(defvar org-e-odt-table-rownum)
(defvar org-e-odt-table-cur-rowgrp-is-hdr)
(defvar org-lparse-table-is-styled)


(defvar org-e-odt-headline-formatter
  (lambda (level snumber todo todo-type priority
		 title tags target extra-targets extra-class)
    (concat snumber " " title)))



;;; User Configuration Variables

(defgroup org-export-e-odt nil
  "Options for exporting Org mode files to ODT."
  :tag "Org Export ODT"
  :group 'org-export)

(defcustom org-e-odt-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-e-html-protect'."
  :group 'org-export-e-html
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "ODT equivalent"))))
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


(defcustom org-e-odt-inline-image-extensions
  '("png" "jpeg" "jpg" "gif")
  "Extensions of image files that can be inlined into HTML."
  :type '(repeat (string :tag "Extension"))
  :group 'org-export-e-odt
  :version "24.1")

(defcustom org-e-odt-pixels-per-inch display-pixels-per-inch
  "Scaling factor for converting images pixels to inches.
Use this for sizing of embedded images.  See Info node `(org)
Images in ODT export' for more information."
  :type 'float
  :group 'org-export-e-odt
  :version "24.1")

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

(defcustom org-e-odt-table-styles
  '(("OrgEquation" "OrgEquation"
     ((use-first-column-styles . t)
      (use-last-column-styles . t))))
  "Specify how Table Styles should be derived from a Table Template.
This is a list where each element is of the
form (TABLE-STYLE-NAME TABLE-TEMPLATE-NAME TABLE-CELL-OPTIONS).

TABLE-STYLE-NAME is the style associated with the table through
`org-e-odt-table-style'.

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
(defcustom org-e-odt-fontify-srcblocks t
  "Specify whether or not source blocks need to be fontified.
Turn this option on if you want to colorize the source code
blocks in the exported file.  For colorization to work, you need
to make available an enhanced version of `htmlfontify' library."
  :type 'boolean
  :group 'org-export-e-odt
  :version "24.1")

(defcustom org-e-odt-prettify-xml t ; FIXME
  "Specify whether or not the xml output should be prettified.
When this option is turned on, `indent-region' is run on all
component xml buffers before they are saved.  Turn this off for
regular use.  Turn this on if you need to examine the xml
visually."
  :group 'org-export-e-odt
  :version "24.1"
  :type 'boolean)

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

;;;; Debugging


;;;; Document

;;;; Document Header (Styles)

;;;; Document Header (Scripts)

;;;; Document Header (Mathjax)

;;;; Preamble

;;;; Postamble

;;;; Emphasis

;;;; Todos

;;;; Tags

;;;; Timestamps
;;;; Statistics Cookie
;;;; Subscript
;;;; Superscript

;;;; Inline images

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

;;;; Table

;;;; Target
;;;; Timestamp

;;;; Verbatim
;;;; Verse Block
;;;; Headline

;;;; Links
;;;; Drawers
;;;; Inlinetasks
;;;; Publishing

;;;; Compilation



;;; User Configurable Variables (MAYBE)

;;;; Preamble

;;;; Headline

;;;; Emphasis

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

;;;; Footnotes

;;;; Timestamps

(defcustom org-e-odt-active-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-e-odt
  :type 'string)

(defcustom org-e-odt-inactive-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-e-odt
  :type 'string)

(defcustom org-e-odt-diary-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-e-odt
  :type 'string)


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

;;;; Tables

(defcustom org-e-odt-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-e-odt
  :type 'boolean)

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


;; Src blocks

;;;; Plain text

(defcustom org-e-odt-quotes
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


;;;; Compilation



;;; Internal Functions (HTML)

;; (defun org-e-odt-format-inline-image (path &optional caption label attr)
;;   ;; FIXME: alt text missing here?
;;   (let ((inline-image (format "<img src=\"%s\" alt=\"%s\"/>"
;; 			      path (file-name-nondirectory path))))
;;     (if (not label) inline-image
;;       (org-e-odt-format-section inline-image "figure" label))))

;;;; Bibliography

(defun org-e-odt-bibliography ()
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

(defun org-e-odt-format-table (lines olines)
  (let ((org-e-odt-format-table-no-css nil))
    (org-lparse-format-table lines olines)))

(defun org-e-odt-splice-attributes (tag attributes)
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

(defun org-e-odt-toc-entry-formatter
  (level snumber todo todo-type priority
	 headline tags target extra-targets extra-class)
  (org-e-odt-format-toc-entry snumber todo headline tags target))

(defun org-e-odt-make-string (n string)
  (let (out) (dotimes (i n out) (setq out (concat string out)))))

(defun org-e-odt-toc-text (toc-entries)
  (let* ((prev-level (1- (nth 1 (car toc-entries))))
	 (start-level prev-level))
    (mapconcat
     (lambda (entry)
       (let ((headline (nth 0 entry))
	     (level (nth 1 entry)))
	 (prog1 (org-e-odt-format-toc-item headline level prev-level)
	   (setq prev-level level))))
     toc-entries "")))

(defun* org-e-odt-format-toc-headline
    (todo todo-type priority text tags
	  &key level section-number headline-label &allow-other-keys)
  ;; FIXME
  (setq text (concat
	      (and org-export-with-section-numbers
		   (concat section-number ". "))
	      text
	      (and tags
		   (concat
		    (org-e-odt-format-spaces 3)
		    (org-e-odt-format-fontify tags "tag")))))
  (when todo
    (setq text (org-e-odt-format-fontify text "todo")))

  (let ((org-e-odt-suppress-xref t))
    (org-e-odt-format-link text (concat "#" headline-label))))

(defun org-e-odt-toc (depth info)
  (assert (wholenump depth))
  (let* ((headlines (org-export-collect-headlines info depth))
	 (toc-entries
	  (loop for headline in headlines collect
		(list (org-e-odt-format-headline--wrap
		       headline info 'org-e-odt-format-toc-headline)
		      (org-export-get-relative-level headline info)))))
    (when toc-entries
      (let* ((lang-specific-heading "Table of Contents")) ; FIXME
	(concat
	 (org-e-odt-begin-toc  lang-specific-heading depth)
	 (org-e-odt-toc-text toc-entries)
	 (org-e-odt-end-toc))))))

(defun org-e-odt-begin-outline (level1 snumber title tags
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

(defun org-e-odt-end-outline ()
  (org-lparse-insert-tag  "</div>"))

(defun org-e-odt-suffix-from-snumber (snumber)
  (let* ((snu (replace-regexp-in-string "\\." "-" snumber))
	 (href (cdr (assoc (concat "sec-" snu)
			   org-export-preferred-target-alist))))
    (org-solidify-link-text (or href snu))))

(defun org-e-odt-format-outline (contents level1 snumber title
					  tags target extra-targets extra-class)
)

;; (defun org-e-odt-format-line (line)
;;   (case org-lparse-dyn-current-environment
;;     ((quote fixedwidth) (concat (org-e-odt-encode-plain-text line) "\n"))
;;     (t (concat line "\n"))))

(defun org-e-odt-fix-class-name (kwd) 	; audit callers of this function
  "Turn todo keyword into a valid class name.
Replaces invalid characters with \"_\"."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  kwd)

(defun org-e-odt-format-internal-link (text href &optional extra)
  (org-e-odt-format-link text (concat "#" href) extra))

(defun org-e-odt-format-extra-targets (extra-targets)
  (if (not extra-targets) ""
    (mapconcat (lambda (x)
		 (when x
		   (setq x (org-solidify-link-text
			    (if (org-uuidgen-p x) (concat "ID-" x) x)))
		   (org-e-odt-format-anchor "" x))) extra-targets "")))

(defun org-e-odt-format-org-tags (tags)
  (if (not tags) ""
    (org-e-odt-format-fontify
     (mapconcat
      (lambda (x)
	(org-e-odt-format-fontify
	 x (concat "" ;; org-e-odt-tag-class-prefix
		   (org-e-odt-fix-class-name x))))
      tags
      (org-e-odt-format-spaces 1)) "tag")))

(defun org-e-odt-format-section-number (&optional snumber level)
  ;; FIXME
  (and nil org-export-with-section-numbers
       ;; (not org-lparse-body-only)
       snumber level
       (org-e-odt-format-fontify snumber (format "section-number-%d" level))))

;; (defun org-e-odt-format-headline (title extra-targets tags
;; 				       &optional snumber level)
;;   (concat
;;    (org-e-odt-format-extra-targets extra-targets)
;;    (concat (org-e-odt-format-section-number snumber level) " ")
;;    title
;;    (and tags (concat (org-e-odt-format-spaces 3)
;; 		     (org-e-odt-format-org-tags tags)))))

;; (defun org-e-odt-format-date (info)
;;   (let ((date (plist-get info :date)))
;;     (cond
;;      ((and date (string-match "%" date))
;;       (format-time-string date))
;;      (date date)
;;      (t (format-time-string "%Y-%m-%d %T %Z")))))



;;; Internal Functions (Ngz)

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

(defun org-e-odt--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

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

(defun org-e-odt--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-odt--caption/label-string'."
  ;; (let ((label (org-element-property :name element)))
  ;;   (if (or (not output) (not label) (string= output "") (string= label ""))
  ;; 	output
  ;;     (concat (format "\\label{%s}\n" label) output)))
  output)



;;; Transcode Helpers

(defun* org-e-odt-format-headline
    (todo todo-type priority text tags
	  &key level section-number headline-label &allow-other-keys)
  (concat  (org-e-odt-todo todo) (and todo " ") text
	   (and tags (org-e-odt-format-spaces 3))
	   (and tags (org-e-odt-format-org-tags tags))))

;;;; Src Code

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
    (htmlfontify-string line)))

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
	     (setq loc (org-e-odt-format-stylized-paragraph par-style loc))
	     (if (not line-num) loc
	       (org-e-odt-format-tags
		'("<text:list-item>" . "</text:list-item>") loc)))
	   num-start refs))
    (cond
     ((not num-start) code)
     ((equal num-start 0)
      (org-e-odt-format-tags
       '("<text:list text:style-name=\"OrgSrcBlockNumberedLine\"%s>"
	 . "</text:list>") code " text:continue-numbering=\"false\""))
     (t (org-e-odt-format-tags
	 '("<text:list text:style-name=\"OrgSrcBlockNumberedLine\"%s>"
	   . "</text:list>") code " text:continue-numbering=\"true\"")))))

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



;;; Template

(defun org-e-odt-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."
  ;; write meta file
  (org-e-odt-update-meta-file info)
  (with-temp-buffer
    (insert-file-contents
     (or org-e-odt-content-template-file
	 (expand-file-name "OrgOdtContentTemplate.xml"
			   org-e-odt-styles-dir)))
    (goto-char (point-min))
    (re-search-forward "</office:text>" nil nil)
    (goto-char (match-beginning 0))

    ;; Title
    (insert (org-e-odt-format-preamble info))
    ;; Table of Contents
    (let ((depth (plist-get info :with-toc)))
      (when (wholenump depth) (insert (org-e-odt-toc depth info))))

    ;; Copy styles.xml.  Also dump htmlfontify styles, if there is any.
    (org-e-odt-update-styles-file info)

    ;; Update styles.xml - take care of outline numbering
    (with-current-buffer
	(find-file-noselect (expand-file-name "styles.xml") t)
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
      (org-e-odt-configure-outline-numbering))

    ;; Contents
    (insert contents)
    (buffer-substring-no-properties (point-min) (point-max))))



;;; Transcode Functions

;;;; Bold

(defun org-e-odt-bold (bold contents info)
  "Transcode BOLD from Org to HTML.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-e-odt-format-fontify contents 'bold))


;;;; Center Block

(defun org-e-odt-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-e-odt--wrap-label center-block contents))


;;;; Clock

(defun org-e-odt-clock (clock contents info)
  "Transcode a CLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-odt-format-fontify
   (concat (org-e-odt-format-fontify org-clock-string "timestamp-kwd")
	   (org-e-odt-format-fontify
	    (concat (org-translate-time (org-element-property :value clock))
		    (let ((time (org-element-property :time clock)))
		      (and time (format " (%s)" time))))
	    "timestamp"))
   "timestamp-wrapper"))


;;;; Code

(defun org-e-odt-code (code contents info)
  "Transcode a CODE object from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-odt-format-fontify (org-element-property :value code) 'code))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-odt-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.
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
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-e-odt--wrap-label dynamic-block contents))


;;;; Entity

(defun org-e-odt-entity (entity contents info)
  "Transcode an ENTITY object from Org to HTML.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  ;; (let ((ent (org-element-property :latex entity)))
  ;;   (if (org-element-property :latex-math-p entity)
  ;; 	(format "$%s$" ent)
  ;;     ent))
  (org-element-property :utf-8 entity))


;;;; Example Block

(defun org-e-odt-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((options (or (org-element-property :options example-block) ""))
	 (value (org-export-handle-code example-block info nil nil t)))
    (org-e-odt--wrap-label
     example-block (org-e-odt-format-source-code-or-example value nil))))


;;;; Export Snippet

(defun org-e-odt-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-odt)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-e-odt-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "ODT")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-e-odt-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-odt--wrap-label
   fixed-width
   (org-e-odt-format-source-code-or-example
    (org-element-property :value fixed-width) nil)))


;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-e-odt-footnote-def (raw info) ; FIXME
  (if (equal (org-element-type raw) 'org-data)
      (org-trim (org-export-data raw info)) ; fix paragraph style
    (org-e-odt-format-stylized-paragraph
     'footnote (org-trim (org-export-data raw info)))))

(defvar org-e-odt-footnote-separator
  (org-e-odt-format-fontify "," 'superscript))

(defun org-e-odt-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       org-e-odt-footnote-separator))
   (cond
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     (let* ((n (org-export-get-footnote-number footnote-reference info)))
       (org-e-odt-format-footnote-reference n "IGNORED" 100)))
    ;; Inline definitions are secondary strings.
    ((eq (org-element-property :type footnote-reference) 'inline)
     (let* ((raw (org-export-get-footnote-definition footnote-reference info))
	    (n (org-export-get-footnote-number footnote-reference info))
	    (def (org-e-odt-footnote-def raw info)))
       (org-e-odt-format-footnote-reference n def 1)))
    ;; Non-inline footnotes definitions are full Org data.
    (t
     (let* ((raw (org-export-get-footnote-definition footnote-reference info))
	    (n (org-export-get-footnote-number footnote-reference info))
	    (def (org-e-odt-footnote-def raw info)))
       (org-e-odt-format-footnote-reference n def 1))))))


;;;; Headline

(defun org-e-odt-todo (todo)
  (when todo
    (org-e-odt-format-fontify
     (concat
      ""				; org-e-odt-todo-kwd-class-prefix
      (org-e-odt-fix-class-name todo))
     (list (if (member todo org-done-keywords) "done" "todo")
	   todo))))

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
		    (org-element-property :tags headline)))
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

(defun org-e-odt-headline (headline contents info)
  "Transcode an HEADLINE element from Org to HTML.
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
     ((org-export-low-level-p headline info) ; FIXME (or (not section-fmt))
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'unordered 'unordered)) ; FIXME
	     (itemized-body (org-e-odt-format-list-item
			     contents type nil nil full-text)))
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (org-e-odt-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-e-odt-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((extra-ids (list (org-element-property :custom-id headline)
			      (org-element-property :id headline)))
	     (extra-ids nil)		; FIXME
	     (id (concat "sec-" (mapconcat 'number-to-string
					   (org-export-get-headline-number
					    headline info) "-"))))
	(concat
	 (org-e-odt-format-tags
	  '("<text:h text:style-name=\"Heading_20_%s\" text:outline-level=\"%s\">" .
	    "</text:h>")
	  (concat (org-e-odt-format-extra-targets extra-ids)
		  (if (not id) full-text (org-e-odt-format-target full-text id) ))
	  level level)
	 contents))))))


;;;; Horizontal Rule

(defun org-e-odt-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((attr (mapconcat #'identity
			 (org-element-property :attr_odt horizontal-rule)
			 " ")))
    (org-e-odt--wrap-label horizontal-rule
			    (org-e-odt-format-horizontal-line))))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-odt-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block))
	 (separator (org-e-odt--find-verb-separator code)))
    (error "FIXME")))


;;;; Inlinetask

(defun org-e-odt-format-section (text class &optional id)
  (let ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<div class=\"%s\"%s>\n" class extra) text "</div>\n")))

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
       (org-e-odt-format-stylized-paragraph
	nil (org-e-odt-format-textbox
	     (concat (org-e-odt-format-stylized-paragraph
		      "OrgInlineTaskHeading" (org-e-odt-format-headline--wrap
					      inlinetask info))
		     contents)
	     nil nil "OrgInlineTaskFrame" " style:rel-width=\"100%\""))))))

;;;; Italic

(defun org-e-odt-italic (italic contents info)
  "Transcode ITALIC from Org to HTML.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-e-odt-format-fontify contents 'italic))


;;;; Item

(defun org-e-odt-format-list-item (contents type checkbox
					    &optional term-counter-id
					    headline)
  (when checkbox
    (setq checkbox
	  (org-e-odt-format-fontify (case checkbox
				      (on "[X]")
				      (off "[&nbsp;]")
				      (trans "[-]")) 'code)))
  (concat
   (org-e-odt-begin-list-item type term-counter-id headline)
   ;; FIXME checkbox (and checkbox " ")
   contents
   (org-e-odt-end-list-item type)))

(defun org-e-odt-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item info))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info)))))
    (org-e-odt-format-list-item
     contents type checkbox (or tag counter))))


;;;; Keyword

(defun org-e-odt-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
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

(defun org-e-odt-format-latex (latex-frag processing-type)
  (let* ((prefix (case processing-type
		   (dvipng "ltxpng/")
		   (mathml "ltxmathml/")))
	 (cache-relpath
	  (concat prefix (file-name-sans-extension
			  (file-name-nondirectory (buffer-file-name)))))
	 (cache-dir (file-name-directory (buffer-file-name )))
	 (display-msg (case processing-type
			(dvipng "Creating LaTeX Image...")
			(mathml "Creating MathML snippet..."))))
    (with-temp-buffer
      (insert latex-frag)
      (org-format-latex cache-relpath cache-dir nil display-msg
			nil nil processing-type)
      (buffer-string))))

(defun org-e-odt-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
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
     (cond
      ((member processing-type '(t mathjax))
       (org-e-odt-format-formula latex-environment info))
      ((equal processing-type 'dvipng)
       (org-e-odt-format-stylized-paragraph
	nil (org-e-odt-link--inline-image latex-environment info)))
      (t latex-frag)))))


;;;; Latex Fragment


;; (when latex-frag			; FIXME
;; 	(setq href (org-propertize href :title "LaTeX Fragment"
;; 				   :description latex-frag)))
;; handle verbatim
;; provide descriptions

(defun org-e-odt-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((latex-frag (org-element-property :value latex-fragment))
	 (processing-type (plist-get info :LaTeX-fragments)))
    (cond
     ((member processing-type '(t mathjax))
      (org-e-odt-format-formula latex-fragment info))
     ((equal processing-type 'dvipng)
      (org-e-odt-link--inline-image latex-fragment info))
     (t latex-frag))))


;;;; Line Break

(defun org-e-odt-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "<text:line-break/>\n")


;;;; Link

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
				    (org-element-property
				     :value element)))
		       (formula-link (org-e-odt-format-latex
				      latex-frag 'dvipng)))
		  (and formula-link
		       (string-match "file:\\([^]]*\\)" formula-link)
		       (match-string 1 formula-link))))
	       (t (error "what is this?"))))
	 (href (org-e-odt-format-tags
		"<draw:image xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>" ""
		(org-e-odt-copy-image-file src)))
	 ;; extract attributes from #+ATTR_ODT line.
	 (attr-from (case (org-element-type element)
		      (link (org-export-get-parent-paragraph element info))
		      (t element)))
	 ;; convert attributes to a plist.
	 (attr-plist (org-e-odt-element-attributes attr-from info))
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
		src (plist-get attr-plist :width)
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
	 default-frame-params frame-params)
    (cond
     ((not caption)
      (setq default-frame-params (nth 2 entity-style))
      (setq frame-params (org-e-odt-merge-frame-params
			  default-frame-params user-frame-params))
      (apply 'org-e-odt-format-frame href width height frame-params))
     (t
      (setq default-frame-params (nth 3 entity-style))
      (setq frame-params (org-e-odt-merge-frame-params
			  default-frame-params user-frame-params))
      (apply 'org-e-odt-format-textbox
	     (org-e-odt-format-stylized-paragraph
	      'illustration
	      (concat
	       (apply 'org-e-odt-format-frame href width height
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

(defvar org-e-odt-standalone-image-predicate
  (function (lambda (paragraph)
	      (or (org-element-property :caption paragraph)
		  (org-element-property :name paragraph)))))

(defun org-e-odt-standalone-image-p (element info &optional predicate)
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
  (let ((paragraph (case (org-element-type element)
		     (paragraph element)
		     (link (and (org-export-inline-image-p
				 element org-e-odt-inline-image-rules)
				(org-export-get-parent element info)))
		     (t nil))))
    (when paragraph
      (assert (eq (org-element-type paragraph) 'paragraph))
      (when (or (not (and (boundp 'org-e-odt-standalone-image-predicate)
			  (functionp org-e-odt-standalone-image-predicate)))
		(funcall org-e-odt-standalone-image-predicate paragraph))
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
  "Transcode a LINK object from Org to HTML.

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
		   ;; TODO: Not implemented yet.  Concat also:
		   ;; (org-export-directory :HTML info)
		   (concat "file://" raw-path)))
		(t raw-path)))
	 protocol)
    (cond
     ;; Image file.
     ((and (not desc) (org-export-inline-image-p
		       link org-e-odt-inline-image-rules))
      (org-e-odt-link--inline-image link info))
     ;; Radioed target: Target's name is obtained from original raw
     ;; link.  Path is parsed and transcoded in order to have a proper
     ;; display of the contents.
     ((string= type "radio")
      (org-e-odt-format-internal-link
       (org-export-data
	(org-element-parse-secondary-string
	 path (org-element-restriction 'radio-target))
	info)
       (org-export-solidify-link-text path)))
     ;; Links pointing to an headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; Fuzzy link points nowhere.
	  ('nil
	   (org-e-odt-format-fontify
	    (or desc (org-export-data
		      (org-element-property :raw-link link) info))
	    'emphasis))
	  ;; Fuzzy link points to an invisible target.
	  (keyword nil)
	  ;; LINK points to an headline.  If headlines are numbered
	  ;; and the link has no description, display headline's
	  ;; number.  Otherwise, display description or headline's
	  ;; title.
	  (headline
	   (let* ((headline-no (org-export-get-headline-number destination info))
		  (label (format "sec-%s" (mapconcat 'number-to-string
						     headline-no "-")))
		  (section-no (mapconcat 'number-to-string headline-no ".")))
	     (setq desc
		   (cond
		    (desc desc)
		    ((plist-get info :section-numbers) section-no)
		    (t (org-export-data
			(org-element-property :title destination) info))))
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
	     (org-e-odt-suppress-xref nil)
	     (href (org-xml-format-href (concat "#coderef-" path))))
	(format fmt (org-e-odt-format-link res href))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'html))
     ;; External link with a description part.
     ((and path desc) (org-e-odt-format-link desc path))
     ;; External link without a description part.
     (path (org-e-odt-format-link path path))
     ;; No path, only description.  Try to do something useful.
     (t (org-e-odt-format-fontify desc 'emphasis)))))


;;;; Babel Call

;; Babel Calls are ignored.


;;;; Macro

(defun org-e-odt-macro (macro contents info)
  "Transcode a MACRO element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-odt-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((style nil)			; FIXME
	 (class (cdr (assoc style '((footnote . "footnote")
				    (verse . nil)))))
	 (extra (if class (format " class=\"%s\"" class) ""))
	 (parent (org-export-get-parent paragraph info))
	 (parent-type (org-element-type parent))
	 (style (case parent-type
		  (quote-block 'quote)
		  (center-block 'center)
		  (footnote-definition 'footnote)
		  (t nil))))
    (org-e-odt-format-stylized-paragraph style contents)))


;;;; Plain List

(defun org-e-odt-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* (arg1 ;; FIXME
	 (type (org-element-property :type plain-list))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_odt plain-list)
			  " ")))
    (org-e-odt--wrap-label
     plain-list (format "%s\n%s%s"
			(org-e-odt-begin-plain-list type)
			contents (org-e-odt-end-plain-list type)))))

;;;; Plain Text

(defun org-e-odt-convert-special-strings (string)
  "Convert special characters in STRING to ODT."
  (let ((all org-e-odt-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(setq string (replace-match rpl t nil string))))
    string))

;; (defun org-e-odt-encode-plain-text (s)
;;   "Convert plain text characters to HTML equivalent.
;; Possible conversions are set in `org-export-html-protect-char-alist'."
;;   (let ((cl org-e-odt-protect-char-alist) c)
;;     (while (setq c (pop cl))
;;       (let ((start 0))
;; 	(while (string-match (car c) s start)
;; 	  (setq s (replace-match (cdr c) t t s)
;; 		start (1+ (match-beginning 0))))))
;;     s))

(defun org-e-odt-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (setq text (org-e-odt-encode-plain-text text t))
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
  ;; (setq text (org-e-odt--quotation-marks text info))
  ;; Convert special strings.
  ;; (when (plist-get info :with-special-strings)
  ;;   (while (string-match (regexp-quote "...") text)
  ;;     (setq text (replace-match "\\ldots{}" nil t text))))
  (when (plist-get info :with-special-strings)
    (setq text (org-e-odt-convert-special-strings text)))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
					 text)))
  ;; Return value.
  text)


;;;; Planning

(defun org-e-odt-planning (planning contents info)
  "Transcode a PLANNING element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-odt-format-fontify
   (concat
    (let ((closed (org-element-property :closed planning)))
      (when closed
	(concat (org-e-odt-format-fontify org-closed-string "timestamp-kwd")
		(org-e-odt-format-fontify (org-translate-time closed)
					  "timestamp"))))
    (let ((deadline (org-element-property :deadline planning)))
      (when deadline
	(concat (org-e-odt-format-fontify org-deadline-string "timestamp-kwd")
		(org-e-odt-format-fontify (org-translate-time deadline)
					  "timestamp"))))
    (let ((scheduled (org-element-property :scheduled planning)))
      (when scheduled
	(concat (org-e-odt-format-fontify org-scheduled-string "timestamp-kwd")
		(org-e-odt-format-fontify (org-translate-time scheduled)
					  "timestamp")))))
   "timestamp-wrapper"))


;;;; Property Drawer

(defun org-e-odt-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")


;;;; Quote Block

(defun org-e-odt-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-odt--wrap-label quote-block contents))


;;;; Quote Section

(defun org-e-odt-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (org-e-odt-format-source-code-or-example value nil))))


;;;; Section

(defun org-e-odt-section (section contents info) ; FIXME
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

;;;; Radio Target

(defun org-e-odt-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (org-e-odt-format-anchor
   text (org-export-solidify-link-text
	 (org-element-property :value radio-target))))


;;;; Special Block

(defun org-e-odt-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-e-odt--wrap-label
     special-block
     (format "\\begin{%s}\n%s\\end{%s}" type contents type))))


;;;; Src Block

(defun org-e-odt-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
	 (caption (org-element-property :caption src-block))
	 (short-caption (and (cdr caption)
			     (org-export-data (cdr caption) info)))
	 (caption (and (car caption) (org-export-data (car caption) info)))
	 (label (org-element-property :name src-block)))
    ;; FIXME: Handle caption
    ;; caption-str (when caption)
    ;; (main (org-export-data (car caption) info))
    ;; (secondary (org-export-data (cdr caption) info))
    ;; (caption-str (org-e-odt--caption/label-string caption label info))
    (let* ((captions (org-e-odt-format-label src-block info 'definition))
	   (caption (car captions)) (short-caption (cdr captions)))
      (concat
       (and caption (org-e-odt-format-stylized-paragraph 'listing caption))
       (org-e-odt-format-code src-block info)))))


;;;; Statistics Cookie

(defun org-e-odt-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (org-e-odt-format-fontify cookie-value 'code)))


;;;; Strike-Through

(defun org-e-odt-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-e-odt-format-fontify contents 'strike))


;;;; Subscript

(defun org-e-odt-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  ;; (format (if (= (length contents) 1) "$_%s$" "$_{\\mathrm{%s}}$") contents)
  (org-e-odt-format-fontify contents 'subscript))


;;;; Superscript

(defun org-e-odt-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  ;; (format (if (= (length contents) 1) "$^%s$" "$^{\\mathrm{%s}}$") contents)
  (org-e-odt-format-fontify contents 'superscript))


;;;; Table Cell

(defun org-e-odt-table-style-spec (element info)
  (let* ((table (org-export-get-parent-table element info))
	 (table-attributes (org-e-odt-element-attributes table info))
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
			    (org-export-get-parent-table table-cell info)
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
	 (table-row (org-export-get-parent table-cell info))
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
		    (org-export-get-parent-table table-row info) info))
	      "OrgTableHeading")
	     ((and (zerop c) t ;; (org-lparse-get 'TABLE-FIRST-COLUMN-AS-LABELS)
		   )
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
     (org-e-odt-format-tags
      '("<table:table-cell%s>" . "</table:table-cell>")
      (org-e-odt-format-stylized-paragraph paragraph-style contents)
      cell-attributes)
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
		      (org-export-get-parent-table table-row info) info))
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
       (org-e-odt-format-tags
	'("<table:table-row>" . "</table:table-row>") contents)
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

(defun org-e-odt-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (case (org-element-property :type table)
    (table.el nil)
    (t
     (let* ((captions (org-e-odt-format-label table info 'definition))
	    (caption (car captions)) (short-caption (cdr captions))
	    (attributes (org-e-odt-element-attributes table info))
	    (custom-table-style (nth 1 (org-e-odt-table-style-spec table info)))
	    (table-column-specs
	     (function
	      (lambda (table info)
		(let* ((table-style (or custom-table-style "OrgTable"))
		       (column-style (format "%sColumn" table-style)))
		  (mapconcat
		   (lambda (table-cell)
		     (let ((width (1+ (or (org-export-table-cell-width
					   table-cell info) 0))))
		       (org-e-odt-make-string
			width
			(org-e-odt-format-tags
			 "<table:table-column table:style-name=\"%s\"/>"
			 "" column-style))))
		   (org-e-odt-table-first-row-data-cells table info) "\n"))))))
       (concat
	;; caption.
	(when caption (org-e-odt-format-stylized-paragraph 'table caption))
	;; begin table.
	(let* ((automatic-name
		(org-e-odt-add-automatic-style "Table" attributes)))
	  (format
	   "\n<table:table table:name=\"%s\" table:style-name=\"%s\">\n"
	   (or short-caption (car automatic-name))
	   (or custom-table-style (cdr automatic-name) "OrgTable")))
	;; column specification.
      	(funcall table-column-specs table info)
	;; actual contents.
	"\n" contents
	;; end table.
	"</table:table>")))))


;;;; Target

(defun org-e-odt-target (target contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-e-odt-format-anchor
   "" (org-export-solidify-link-text (org-element-property :value target))))


;;;; Timestamp

(defun org-e-odt-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-odt-format-fontify
   (org-e-odt-format-fontify
    (org-translate-time (org-element-property :value timestamp))
    "timestamp")
   "timestamp-wrapper"))


;;;; Underline

(defun org-e-odt-underline (underline contents info)
  "Transcode UNDERLINE from Org to HTML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-e-odt-format-fontify contents 'underline))


;;;; Verbatim

(defun org-e-odt-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-odt-format-fontify (org-element-property :value verbatim) 'verbatim))


;;;; Verse Block

(defun org-e-odt-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Replace each newline character with line break.  Also replace
  ;; each blank line with a line break.
  (setq contents (replace-regexp-in-string
		  "^ *\\\\\\\\$" "<br/>\n"
		  (replace-regexp-in-string
		   "\\(\\\\\\\\\\)?[ \t]*\n" " <br/>\n" contents)))

  ;; Replace each white space at beginning of a line with a
  ;; non-breaking space.
  (while (string-match "^[ \t]+" contents)
    (let ((new-str (org-e-odt-format-spaces
		    (length (match-string 0 contents)))))
      (setq contents (replace-match new-str nil t contents))))

  (org-e-odt--wrap-label
   verse-block (format "<p class=\"verse\">\n%s</p>" contents)))




;;; Filter Functions

;;;; Filter Settings
;;;; Filters

;;; Interactive functions

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
  (setq debug-on-error t)

  ;; (let* ((outfile (org-export-output-file-name ".html" subtreep pub-dir))
  ;; 	 (outfile "content.xml"))
  ;;   (org-export-to-file
  ;;    'e-odt outfile subtreep visible-only body-only ext-plist))

  (let* ((outbuf (org-e-odt-init-outfile))
	 (target (org-export-output-file-name ".odt" subtreep pub-dir))
	 (outdir (file-name-directory (buffer-file-name outbuf)))
	 (default-directory outdir))

    ;; FIXME: for copying embedded images
    (setq org-current-export-file
	  (file-name-directory
	   (org-export-output-file-name ".odt" subtreep nil)))

    (org-export-to-buffer
     'e-odt outbuf
     (memq 'subtree optns) (memq 'visible optns) (memq 'body optns))

    (setq org-lparse-opt-plist nil) 	; FIXME
    (org-e-odt-save-as-outfile target	;; info
			       nil
			       )

    ;; return outfile
    (if (not org-e-odt-preferred-output-format) target
      (or (org-e-odt-convert target org-e-odt-preferred-output-format)
	  target))))





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

;;; FIXMES, TODOS, FOR REVIEW etc

;; (defun org-e-odt-discontinue-list ()
;;   (let ((stashed-stack org-lparse-list-stack))
;;     (loop for list-type in stashed-stack
;; 	  do (org-lparse-end-list-item-1 list-type)
;; 	  (org-lparse-end-list list-type))
;;     (setq org-e-odt-list-stack-stashed stashed-stack)))

;; (defun org-e-odt-continue-list ()
;;   (setq org-e-odt-list-stack-stashed (nreverse org-e-odt-list-stack-stashed))
;;   (loop for list-type in org-e-odt-list-stack-stashed
;; 	do (org-lparse-begin-list list-type)
;; 	(org-lparse-begin-list-item list-type)))

;; FIXME: Begin indented table
;; (setq org-e-odt-table-indentedp (not (null org-lparse-list-stack)))
;; (setq org-e-odt-table-indentedp nil) ; FIXME
;; (when org-e-odt-table-indentedp
;;   ;; Within the Org file, the table is appearing within a list item.
;;   ;; OpenDocument doesn't allow table to appear within list items.
;;   ;; Temporarily terminate the list, emit the table and then
;;   ;; re-continue the list.
;;   (org-e-odt-discontinue-list)
;;   ;; Put the Table in an indented section.
;;   (let ((level (length org-e-odt-list-stack-stashed)))
;; 	(org-e-odt-begin-section (format "OrgIndentedSection-Level-%d" level))))

;; FIXME: End indented table
;; (when org-e-odt-table-indentedp
;;   (org-e-odt-end-section)
;;   (org-e-odt-continue-list))


;;;; org-format-table-html
;;;; org-format-org-table-html
;;;; org-format-table-table-html
;;;; org-table-number-fraction
;;;; org-table-number-regexp
;;;; org-e-odt-table-caption-above

;;;; org-whitespace
;;;; "<span style=\"visibility:hidden;\">%s</span>"
;;;; Remove display properties

;;;; org-e-odt-with-timestamp
;;;; org-e-odt-html-helper-timestamp

;;;; org-export-as-html-and-open
;;;; org-export-as-html-batch
;;;; org-export-as-html-to-buffer
;;;; org-replace-region-by-html
;;;; org-export-region-as-html
;;;; org-export-as-html

;;;; (org-export-directory :html opt-plist)
;;;; (plist-get opt-plist :html-extension)
;;;; org-e-odt-toplevel-hlevel
;;;; org-e-odt-inline-image-extensions
;;;; org-e-odt-protect-char-alist
;;;; org-e-odt-table-use-header-tags-for-first-column
;;;; org-e-odt-todo-kwd-class-prefix
;;;; org-e-odt-tag-class-prefix
;;;; org-e-odt-footnote-separator


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

(defvar org-e-odt-display-outline-level 2)
(defun org-e-odt-enumerate-element (element info &optional predicate n)
  (let* ((numbered-parent-headline-at-<=-n
	  (function
	   (lambda (element n info)
	     (loop for x in (org-export-get-genealogy element info)
		   thereis (and (eq (org-element-type x) 'headline)
		   		(<= (org-export-get-relative-level x info) n)
		   		(org-export-numbered-headline-p x info)
		   		x)))))
	 (enumerate
	  (function
	   (lambda (element scope info &optional predicate)
	     (let ((counter 0))
	       (org-element-map
		(or scope (plist-get info :parse-tree))
		(org-element-type element)
		(lambda (el)
		  (and (or (not predicate) (funcall predicate el info))
		       (incf counter)
		       (equal element el)
		       counter))
		info 'first-match)))))
	 (scope (funcall numbered-parent-headline-at-<=-n
			 element (or n org-e-odt-display-outline-level) info))
	 (ordinal (funcall enumerate element scope info predicate))
	 (tag
	  (concat
	   ;; section number
	   (and scope
		(mapconcat 'number-to-string
			   (org-export-get-headline-number scope info) "."))
	   ;; separator
	   (and scope ".")
	   ;; ordinal
	   (number-to-string ordinal))))
    ;; (message "%s:\t%s" (org-element-property :name element) tag)
    tag))

(provide 'org-e-odt)

;;; org-e-odt.el ends here
