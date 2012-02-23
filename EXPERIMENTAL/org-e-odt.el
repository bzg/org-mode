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
(require 'org-lparse)

(defgroup org-export-e-odt nil
  "Options specific for ODT export of Org-mode files."
  :tag "Org Export ODT"
  :group 'org-export
  :version "24.1")

;; (defun org-e-odt-unload-function ()
;;   (org-lparse-unregister-backend 'odt)
;;   (remove-hook 'org-export-preprocess-after-blockquote-hook
;; 	       'org-export-e-odt-preprocess-latex-fragments)
;;   nil)

(defun org-e-odt-get-style-name-for-entity (category &optional entity)
  (let ((entity (or entity 'default)))
    (or
     (cdr (assoc entity (cdr (assoc category
				    org-export-e-odt-org-styles-alist))))
     (cdr (assoc entity (cdr (assoc category
				    org-export-e-odt-default-org-styles-alist))))
     (error "Cannot determine style name for entity %s of type %s"
	    entity category))))


;;;###autoload
(defun org-export-as-e-odt-and-open (arg)
  "Export the outline as ODT and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-lparse-and-open
   (or org-export-e-odt-preferred-output-format "odt") "odt" arg))

;;;###autoload
(defun org-export-as-e-odt-batch ()
  "Call the function `org-lparse-batch'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-e-odt-batch"
  (org-lparse-batch "odt"))

;;; org-export-as-e-odt
;;;###autoload
(defun org-export-as-e-odt (arg &optional hidden ext-plist
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
  (org-lparse (or org-export-e-odt-preferred-output-format "odt")
	      "odt" arg hidden ext-plist to-buffer body-only pub-dir))



;; Following variable is let bound when `org-do-lparse' is in
;; progress. See org-html.el.

(defun org-e-odt-format-preamble (info)
  (let* ((title (plist-get info :title))
	 (author (plist-get info :author))
	 (date (plist-get info :date))
	 (iso-date (org-e-odt-format-date date))
	 (date (org-e-odt-format-date date "%d %b %Y"))
	 (email (plist-get info :email))
	 ;; switch on or off above vars based on user settings
	 (author (and (plist-get info :with-author) (or author email)))
	 (email (and (plist-get info :with-email) email))
	 ;; (date (and (plist-get info :time-stamp-file) date))
	 )
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
	 (org-odt-format-tags
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
  (org-lparse-insert-tag "<office:annotation>")
  (when (setq author (org-e-odt-format-author author))
    (insert author))
  (insert (org-e-odt-format-tags
	   '("<dc:date>" . "</dc:date>")
	   (org-e-odt-format-date
	    (or date (plist-get org-lparse-opt-plist :date)))))
  (org-lparse-begin-paragraph))

(defun org-e-odt-end-annotation ()
  (org-lparse-insert-tag  "</office:annotation>"))

(defun org-e-odt-begin-environment (style env-options-plist)
  (case style
    (annotation
     (org-lparse-stash-save-paragraph-state)
     (org-e-odt-begin-annotation (plist-get env-options-plist 'author)
				 (plist-get env-options-plist 'date)))
    ((blockquote verse center quote)
     (org-lparse-begin-paragraph style)
     (list))
    ((fixedwidth native)
     (org-lparse-end-paragraph)
     (list))
    (t (error "Unknown environment %s" style))))

(defun org-e-odt-end-environment (style env-options-plist)
  (case style
    (annotation
     (org-lparse-end-paragraph)
     (org-e-odt-end-annotation)
     (org-lparse-stash-pop-paragraph-state))
    ((blockquote verse center quote)
     (org-lparse-end-paragraph)
     (list))
    ((fixedwidth native)
     (org-lparse-begin-paragraph)
     (list))
    (t (error "Unknown environment %s" style))))

(defun org-e-odt-begin-plain-list (ltype)
  ;; (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
  ;; 		  ltype))
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
  ;; (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
  ;; 		  ltype))
  (if ltype "</text:list>"
    (error "Unknown list type: %s" ltype)))

(defun org-e-odt-begin-list-item (ltype &optional arg headline)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
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
  ;; (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
  ;; 		  ltype))
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

(defun org-e-odt-discontinue-list ()
  (let ((stashed-stack org-lparse-list-stack))
    (loop for list-type in stashed-stack
	  do (org-lparse-end-list-item-1 list-type)
	  (org-lparse-end-list list-type))
    (setq org-e-odt-list-stack-stashed stashed-stack)))

(defun org-e-odt-continue-list ()
  (setq org-e-odt-list-stack-stashed (nreverse org-e-odt-list-stack-stashed))
  (loop for list-type in org-e-odt-list-stack-stashed
	do (org-lparse-begin-list list-type)
	(org-lparse-begin-list-item list-type)))

;; Following variables are let bound when table emission is in
;; progress. See org-lparse.el.
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

(defun org-e-odt-add-automatic-style (object-type &optional object-props)
  "Create an automatic style of type OBJECT-TYPE with param OBJECT-PROPS.
OBJECT-PROPS is (typically) a plist created by passing
\"#+ATTR_ODT: \" option of the object in question to
`org-lparse-get-block-params'.

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

(defun org-e-odt-format-table-columns ()
  (let* ((num-cols (length (plist-get table-info :alignment)))
	 (col-nos (loop for i from 1 below num-cols collect i))
	 (levels )
	 (col-widths (plist-get table-info :width))
	 (style (or (nth 1 org-e-odt-table-style-spec) "OrgTable")))
    (mapconcat
     (lambda (c)
       (let* ((width (or (and org-lparse-table-is-styled (aref col-widths c))
			 0)))
	 (org-e-odt-make-string
	  (1+ width)
	  (org-e-odt-format-tags
	   "<table:table-column table:style-name=\"%sColumn\"/>" "" style))))
     col-nos "\n")))


(defun org-e-odt-begin-table (caption label attributes)
  ;; (setq org-e-odt-table-indentedp (not (null org-lparse-list-stack)))
  (setq org-e-odt-table-indentedp nil)	; FIXME
  (when org-e-odt-table-indentedp
    ;; Within the Org file, the table is appearing within a list item.
    ;; OpenDocument doesn't allow table to appear within list items.
    ;; Temporarily terminate the list, emit the table and then
    ;; re-continue the list.
    (org-e-odt-discontinue-list)
    ;; Put the Table in an indented section.
    (let ((level (length org-e-odt-list-stack-stashed)))
      (org-e-odt-begin-section (format "OrgIndentedSection-Level-%d" level))))
  (setq attributes (org-lparse-get-block-params attributes))
  (setq org-e-odt-table-style (plist-get attributes :style))
  (setq org-e-odt-table-style-spec
	(assoc org-e-odt-table-style org-export-e-odt-table-styles))
  (concat
   (org-e-odt-format-stylized-paragraph
    'table (org-e-odt-format-entity-caption label caption "__Table__"))
   (let ((name-and-style (org-e-odt-add-automatic-style "Table" attributes)))
     (format
      "\n<table:table table:name=\"%s\" table:style-name=\"%s\">\n"
      (car name-and-style) (or (nth 1 org-e-odt-table-style-spec)
			       (cdr name-and-style) "OrgTable")))
   (org-e-odt-format-table-columns) "\n")

  ;; (org-e-html-pp  table-info)

  ;; (setq org-lparse-table-begin-marker (point))
  )

(defun org-e-odt-end-table ()
  ;; fill style attributes for table cells
  (when org-lparse-table-is-styled
    (while (re-search-forward "@@\\(table-cell:p\\|table-cell:style-name\\)@@\\([0-9]+\\)@@\\([0-9]+\\)@@" nil t)
      (let* (;; (spec (match-string 1))
  	     ;; (r (string-to-number (match-string 2)))
  	     ;; (c (string-to-number (match-string 3)))
  	     (cell-styles (org-e-odt-get-table-cell-styles
  			   r c org-e-odt-table-style-spec))
  	     (table-cell-style (car cell-styles))
  	     (table-cell-paragraph-style (cdr cell-styles)))
  	(cond
  	 ((equal spec "table-cell:p")
  	  (replace-match table-cell-paragraph-style t t))
  	 ((equal spec "table-cell:style-name")
  	  (replace-match table-cell-style t t))))))
  (goto-char (point-max))

  (concat
   "</table:table>"
   ;; (when org-e-odt-table-indentedp
   ;;   (org-e-odt-end-section)
   ;;   (org-e-odt-continue-list))
   ))

(defun org-e-odt-begin-table-rowgroup (&optional is-header-row)
  (prog1
      (concat (when org-e-odt-table-rowgrp-open
		(org-e-odt-end-table-rowgroup))
	      (if is-header-row "<table:table-header-rows>"
		"<table:table-rows>"))
    (setq org-e-odt-table-rowgrp-open t)
    (setq org-e-odt-table-cur-rowgrp-is-hdr is-header-row)))

(defun org-e-odt-end-table-rowgroup ()
  (when org-e-odt-table-rowgrp-open
    (setq org-e-odt-table-rowgrp-open nil)
    (if org-e-odt-table-cur-rowgrp-is-hdr
	"</table:table-header-rows>" "</table:table-rows>")))

(defun org-e-odt-format-table-row (row)
  (org-e-odt-format-tags
   '("<table:table-row>" . "</table:table-row>") row))

(defun org-e-odt-get-column-alignment (c)
  (let ((colalign-vector (plist-get table-info :alignment)))
    ;; FIXME
    (assoc-default (aref colalign-vector c)
		   '(("l" . "left")
		     ("r" . "right")
		     ("c" . "center")))))

(defun org-e-odt-get-table-cell-styles (r c &optional style-spec)
  "Retrieve styles applicable to a table cell.
R and C are (zero-based) row and column numbers of the table
cell.  STYLE-SPEC is an entry in `org-export-e-odt-table-styles'
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
  (cond
   (style-spec

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
		   (= c (1- org-lparse-table-ncols))) "LastColumn")
	     ((and (cdr (assoc 'use-first-row-styles cell-style-selectors))
		   (= r 0)) "FirstRow")
	     ((and (cdr (assoc 'use-last-row-styles cell-style-selectors))
		   (= r org-e-odt-table-rownum))
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
      (cons
       (concat template-name cell-type "TableCell")
       (concat template-name cell-type "TableParagraph"))))
   (t
    (cons
     (concat
      "OrgTblCell"
      (cond
       ((= r 0) "T")
       ((eq (cdr (assoc r nil ;; org-lparse-table-rowgrp-info FIXME
			))  :start) "T")
       (t ""))
      (when (= r org-e-odt-table-rownum) "B")
      (cond
       ((= c 0) "")
       ((or (memq (nth c org-table-colgroup-info) '(:start :startend))
	    (memq (nth (1- c) org-table-colgroup-info) '(:end :startend))) "L")
       (t "")))
     (capitalize (org-e-odt-get-column-alignment c))))))

(defun org-e-odt-get-paragraph-style-cookie-for-table-cell (r c)
  (concat
   (and (not org-e-odt-table-style-spec)
	(cond
	 (org-e-odt-table-cur-rowgrp-is-hdr "OrgTableHeading")
	 ((and (= c 0) nil
	       ;; (org-lparse-get 'TABLE-FIRST-COLUMN-AS-LABELS)
	       )
	  "OrgTableHeading")
	 (t "OrgTableContents")))
   (and org-lparse-table-is-styled
	(cdr (org-e-odt-get-table-cell-styles
	      r c org-e-odt-table-style-spec)))))

(defun org-e-odt-get-style-name-cookie-for-table-cell (r c)
  (when org-lparse-table-is-styled
    (let* ((cell-styles (org-e-odt-get-table-cell-styles
			 r c org-e-odt-table-style-spec))
	   (table-cell-style (car cell-styles)))
      table-cell-style)))

(defun org-e-odt-format-table-cell (data r c horiz-span)
  (concat
   (let* ((paragraph-style-cookie
	   (org-e-odt-get-paragraph-style-cookie-for-table-cell r c))
	  (style-name-cookie
	   (org-e-odt-get-style-name-cookie-for-table-cell r c))
	  (extra (and style-name-cookie
		      (format " table:style-name=\"%s\""  style-name-cookie)))
	  (extra (concat extra
			 (and (> horiz-span 0)
			      (format " table:number-columns-spanned=\"%d\""
				      (1+ horiz-span))))))
     (org-e-odt-format-tags
      '("<table:table-cell%s>" . "</table:table-cell>")
      (if org-lparse-list-table-p data
	(org-e-odt-format-stylized-paragraph paragraph-style-cookie data)) extra))
   (let (s)
     (dotimes (i horiz-span)
       (setq s (concat s "\n<table:covered-table-cell/>"))) s)
   "\n"))

(defun org-e-odt-begin-footnote-definition (n)
  (org-lparse-begin-paragraph 'footnote))

(defun org-e-odt-end-footnote-definition (n)
  (org-lparse-end-paragraph))

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
  (let ((style (format "Contents_20_%d"
		       (+ level (or ;; (org-lparse-get 'TOPLEVEL-HLEVEL)
				 1
				    1) -1))))
    (concat "\n" (org-e-odt-format-stylized-paragraph style toc-entry) "\n")))

;; Following variable is let bound during 'ORG-LINK callback. See
;; org-html.el

(defun org-e-odt-format-link (desc href &optional attr)
  (cond
   ((and (= (string-to-char href) ?#) (not org-e-odt-suppress-xref))
    (setq href (concat org-export-e-odt-bookmark-prefix (substring href 1)))
    (let ((xref-format "text"))
      (when (numberp desc)
	(setq desc (format "%d" desc) xref-format "number"))
      (org-e-odt-format-tags
       '("<text:bookmark-ref text:reference-format=\"%s\" text:ref-name=\"%s\">" .
	 "</text:bookmark-ref>")
       desc xref-format href)))
   (org-lparse-link-description-is-image
    (org-e-odt-format-tags
     '("<draw:a xlink:type=\"simple\" xlink:href=\"%s\" %s>" . "</draw:a>")
     desc href (or attr "")))
   (t
    (org-e-odt-format-tags
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


(defun org-e-odt-format-source-line-with-line-number-and-label
  (line rpllbl num fontifier par-style)

  (let ((keep-label (not (numberp rpllbl)))
	(ref (org-find-text-property-in-string 'org-coderef line)))
    (setq line (concat line (and keep-label ref (format "(%s)" ref))))
    (setq line (funcall fontifier line))
    (when ref
      (setq line (org-e-odt-format-target line (concat "coderef-" ref))))
    (setq line (org-e-odt-format-stylized-paragraph par-style line))
    (if (not num) line
      (org-e-odt-format-tags '("<text:list-item>" . "</text:list-item>") line))))

(defun org-e-odt-format-source-code-or-example-plain
  (lines lang caption textareap cols rows num cont rpllbl fmt)
  "Format source or example blocks much like fixedwidth blocks.
Use this when `org-export-e-odt-fontify-srcblocks' option is turned
off."
  (let* ((lines (org-split-string lines "[\r\n]"))
	 (line-count (length lines))
	 (i 0))
    (mapconcat
     (lambda (line)
       (incf i)
       (org-e-odt-format-source-line-with-line-number-and-label
	line rpllbl num 'org-e-odt-encode-plain-text
	(if (= i line-count) "OrgFixedWidthBlockLastLine"
	  "OrgFixedWidthBlock")))
     lines "\n")))

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
	 (style (and org-export-e-odt-create-custom-styles-for-srcblocks
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

(defun org-e-odt-format-source-code-or-example-colored
  (lines lang caption textareap cols rows num cont rpllbl fmt)
  "Format source or example blocks using `htmlfontify-string'.
Use this routine when `org-export-e-odt-fontify-srcblocks' option
is turned on."
  (let* ((lang-m (and lang (or (cdr (assoc lang org-src-lang-modes)) lang)))
	 (mode (and lang-m (intern (concat (if (symbolp lang-m)
					       (symbol-name lang-m)
					     lang-m) "-mode"))))
	 (org-inhibit-startup t)
	 (org-startup-folded nil)
	 (lines (with-temp-buffer
		  (insert lines)
		  (if (functionp mode) (funcall mode) (fundamental-mode))
		  (font-lock-fontify-buffer)
		  (buffer-string)))
	 (hfy-html-quote-regex "\\([<\"&> 	]\\)")
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
    (when (fboundp 'htmlfontify-string)
      (let* ((lines (org-split-string lines "[\r\n]"))
	     (line-count (length lines))
	     (i 0))
	(mapconcat
	 (lambda (line)
	   (incf i)
	   (org-e-odt-format-source-line-with-line-number-and-label
	    line rpllbl num 'htmlfontify-string
	    (if (= i line-count) "OrgSrcBlockLastLine" "OrgSrcBlock")))
	 lines "\n")))))

(defun org-e-odt-format-source-code-or-example (lines lang
						      &optional
						      caption textareap
						      cols rows num cont
						      rpllbl fmt)
  "Format source or example blocks for export.
Use `org-e-odt-format-source-code-or-example-plain' or
`org-e-odt-format-source-code-or-example-colored' depending on the
value of `org-export-e-odt-fontify-srcblocks."
  (setq ;; lines (org-export-number-lines
	;;        lines 0 0 num cont rpllbl fmt 'preprocess) FIXME
	lines (funcall
	       (or (and org-export-e-odt-fontify-srcblocks
			(or (featurep 'htmlfontify)
			    ;; htmlfontify.el was introduced in Emacs 23.2
			    ;; So load it with some caution
			    (require 'htmlfontify nil t))
			(fboundp 'htmlfontify-string)
			'org-e-odt-format-source-code-or-example-colored)
		   'org-e-odt-format-source-code-or-example-plain)
	       lines lang caption textareap cols rows num cont rpllbl fmt))
  (if (not num) lines
    (let ((extra (format " text:continue-numbering=\"%s\""
			 (if cont "true" "false"))))
      (org-e-odt-format-tags
       '("<text:list text:style-name=\"OrgSrcBlockNumberedLine\"%s>"
	 . "</text:list>") lines extra))))

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
    (org-e-odt-format-tags
     '("<text:span text:style-name=\"%s\">" . "</text:span>")
     text style-name)))

(defun org-e-odt-relocate-relative-path (path dir)
  (if (file-name-absolute-p path) path
    (file-relative-name (expand-file-name path dir)
			(expand-file-name "eyecandy" dir))))

(defun org-e-odt-format-inline-image (thefile
				      &optional caption label attrs ; FIXME - CLA
				      )
  (let* ((thelink (if (file-name-absolute-p thefile) thefile
		    (org-xml-format-href
		     (org-e-odt-relocate-relative-path
		      thefile org-current-export-file))))
	 (href
	  (org-e-odt-format-tags
	   "<draw:image xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>" ""
	   (if org-export-e-odt-embed-images
	       (org-e-odt-copy-image-file thefile) thelink))))
    (org-export-e-odt-format-image thefile href)))

(defun org-export-e-odt-format-formula (src href)
  (save-match-data
    (let* ((caption (org-find-text-property-in-string 'org-caption src))
	   (caption (and caption (org-xml-format-desc caption)))
	   (label (org-find-text-property-in-string 'org-label src))
	   (latex-frag (org-find-text-property-in-string 'org-latex-src src))
	   (embed-as (or (and latex-frag
			      (org-find-text-property-in-string
			       'org-latex-src-embed-type src))
			 (if (or caption label) 'paragraph 'character)))
	   width height)
      (when latex-frag
	(setq href (org-propertize href :title "LaTeX Fragment"
				   :description latex-frag)))
      (cond
       ((eq embed-as 'character)
	(org-e-odt-format-entity "InlineFormula" href width height))
       (t
	(org-lparse-end-paragraph)
	(org-lparse-insert-list-table
	 `((,(org-e-odt-format-entity
	      (if caption "CaptionedDisplayFormula" "DisplayFormula")
	      href width height :caption caption :label nil)
	    ,(if (not label) ""
	       (org-e-odt-format-entity-caption label nil "__MathFormula__"))))
	 nil nil nil ":style \"OrgEquation\"" nil '((1 "c" 8) (2 "c" 1)))
	(throw 'nextline nil))))))

(defun org-e-odt-copy-formula-file (path)
  "Returns the internal name of the file"
  (let* ((src-file (expand-file-name
		    path (file-name-directory org-current-export-file)))
	 (target-dir (format "Formula-%04d/"
			     (incf org-e-odt-embedded-formulas-count)))
	 (target-file (concat target-dir "content.xml")))
    (when (not org-lparse-to-buffer)
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

      (org-e-odt-create-manifest-file-entry "text/xml" target-file))
    target-file))

(defun org-e-odt-format-inline-formula (thefile)
  (let* ((thelink (if (file-name-absolute-p thefile) thefile
		    (org-xml-format-href
		     (org-e-odt-relocate-relative-path
		      thefile org-current-export-file))))
	 (href
	  (org-e-odt-format-tags
	   "<draw:object xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>" ""
	   (file-name-directory (org-e-odt-copy-formula-file thefile)))))
    (org-export-e-odt-format-formula thefile href)))

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
	      filename org-export-e-odt-inline-image-extensions)
	     (or (eq t org-export-e-odt-inline-images)
		 (and org-export-e-odt-inline-images (not descp))))
	(org-e-odt-format-inline-image thefile))
       ;; check for embedded formulas
       ((and (member type '("file"))
	     (not fragment)
	     (org-e-odt-is-formula-link-p filename)
	     (or (not descp)))
	(org-e-odt-format-inline-formula thefile))
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

(defun org-e-odt-format-heading (text level &optional id)
  (let* ((text (if id (org-e-odt-format-target text id) text)))
    (org-e-odt-format-tags
     '("<text:h text:style-name=\"Heading_20_%s\" text:outline-level=\"%s\">" .
       "</text:h>") text level level)))

(defun org-e-odt-format-headline (title extra-targets tags
					&optional snumber level)
  (concat
   (org-e-odt-format-extra-targets extra-targets)

   ;; No need to generate section numbers. They are auto-generated by
   ;; the application

   ;; (concat (org-lparse-format 'SECTION-NUMBER snumber level) " ")
   title
   (and tags (concat (org-e-odt-format-spaces 3)
		     (org-e-odt-format-org-tags tags)))))

(defun org-e-odt-format-anchor (text name &optional class)
  (org-e-odt-format-target text name))

(defun org-e-odt-format-bookmark (text id)
  (if id
      (org-e-odt-format-tags "<text:bookmark text:name=\"%s\"/>" text id)
    text))

(defun org-e-odt-format-target (text id)
  (let ((name (concat org-export-e-odt-bookmark-prefix id)))
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
    (org-e-odt-format-tags
     '("<text:note text:id=\"%s\" text:note-class=\"%s\">" .
       "</text:note>")
     (concat
      (org-e-odt-format-tags
       '("<text:note-citation>" . "</text:note-citation>")
       n)
      (org-e-odt-format-tags
       '("<text:note-body>" . "</text:note-body>")
       def))
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
    (org-e-odt-format-tags
     '("<text:span text:style-name=\"%s\">" . "</text:span>")
     (org-e-odt-format-tags
      '("<text:note-ref text:note-class=\"%s\" text:reference-format=\"%s\" text:ref-name=\"%s\">" . "</text:note-ref>")
      n note-class ref-format ref-name)
     "OrgSuperscript")))

(defun org-e-odt-get-image-name (file-name)
  (require 'sha1)
  (file-relative-name
   (expand-file-name
    (concat (sha1 file-name) "." (file-name-extension file-name)) "Pictures")))

(defun org-export-e-odt-format-image (src href)
  "Create image tag with source and attributes."
  (save-match-data
    (let* ((caption (org-find-text-property-in-string 'org-caption src))
	   (caption (and caption (org-xml-format-desc caption)))
	   (attr (org-find-text-property-in-string 'org-attributes src))
	   (label (org-find-text-property-in-string 'org-label src))
	   (latex-frag (org-find-text-property-in-string
			'org-latex-src src))
	   (category (and latex-frag "__DvipngImage__"))
	   (attr-plist (org-lparse-get-block-params attr))
	   (user-frame-anchor
	    (car (assoc-string (plist-get attr-plist :anchor)
			       '(("as-char") ("paragraph") ("page")) t)))
	   (user-frame-style
	    (and user-frame-anchor (plist-get attr-plist :style)))
	   (user-frame-attrs
	    (and user-frame-anchor (plist-get attr-plist :attributes)))
	   (user-frame-params
	    (list user-frame-style user-frame-attrs user-frame-anchor))
	   (embed-as (cond
		      (latex-frag
		       (symbol-name
			(case (org-find-text-property-in-string
			       'org-latex-src-embed-type src)
			  (paragraph 'paragraph)
			  (t 'as-char))))
		      (user-frame-anchor)
		      (t "paragraph")))
	   (size (org-e-odt-image-size-from-file
		  src (plist-get attr-plist :width)
		  (plist-get attr-plist :height)
		  (plist-get attr-plist :scale) nil embed-as))
	   (width (car size)) (height (cdr size)))
      (when latex-frag
	(setq href (org-propertize href :title "LaTeX Fragment"
				   :description latex-frag)))
      (let ((frame-style-handle (concat (and (or caption label) "Captioned")
					embed-as "Image")))
	(org-e-odt-format-entity
	 frame-style-handle href width height
	 :caption caption :label label :category category
	 :user-frame-params user-frame-params)))))

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

(defun org-e-odt-format-inlinetask (heading content
					    &optional todo priority tags)
  (org-e-odt-format-stylized-paragraph
   nil (org-e-odt-format-textbox
	(concat (org-e-odt-format-stylized-paragraph
		 "OrgInlineTaskHeading"
		 (org-lparse-format
		  'HEADLINE (concat (org-lparse-format-todo todo) " " heading)
		  nil tags))
		content) nil nil "OrgInlineTaskFrame" " style:rel-width=\"100%\"")))


(defun org-e-odt-merge-frame-params(default-frame-params user-frame-params)
  (if (not user-frame-params) default-frame-params
    (assert (= (length default-frame-params) 3))
    (assert (= (length user-frame-params) 3))
    (loop for user-frame-param in user-frame-params
	  for default-frame-param in default-frame-params
	  collect (or user-frame-param default-frame-param))))

(defun* org-e-odt-format-entity (entity href width height
					&key caption label category
					user-frame-params)
  (let* ((entity-style (assoc-string entity org-e-odt-entity-frame-styles t))
	 default-frame-params frame-params)
    (cond
     ((not (or caption label))
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
		      (nth 2 entity-style))
	       (org-e-odt-format-entity-caption
		label caption (or category (nth 1 entity-style)))))
	     width height frame-params)))))


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
    (when (not org-lparse-to-buffer)
      (message "Embedding %s as %s ..."
	       (substring-no-properties path) target-file)

      (when (= 1 org-e-odt-embedded-images-count)
	(make-directory target-dir)
	(org-e-odt-create-manifest-file-entry "" target-dir))

      (copy-file src-file target-file 'overwrite)
      (org-e-odt-create-manifest-file-entry media-type target-file))
    target-file))

(defun org-e-odt-do-image-size (probe-method file &optional dpi anchor-type)
  (setq dpi (or dpi org-export-e-odt-pixels-per-inch))
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
			  org-export-e-odt-default-image-sizes-alist))))))

(defun org-e-odt-image-size-from-file (file &optional user-width
					    user-height scale dpi embed-as)
  (unless (file-name-absolute-p file)
    (setq file (expand-file-name
		file (file-name-directory org-current-export-file))))
  (let* (size width height)
    (unless (and user-height user-width)
      (loop for probe-method in org-export-e-odt-image-size-probe-method
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
    (let ((max-width (car org-export-e-odt-max-image-size))
	  (max-height (cdr org-export-e-odt-max-image-size)))
      (when (or (> width max-width) (> height max-height))
	(let* ((scale1 (/ max-width width))
	       (scale2 (/ max-height height))
	       (scale (min scale1 scale2)))
	  (setq width (* scale width) height (* scale height)))))
    (cons width height)))

(defun org-e-odt-get-label-category-and-style (label default-category)
  "See `org-export-e-odt-get-category-from-label'."
  (let ((default-category-map
	  (assoc default-category org-e-odt-category-map-alist))
	user-category user-category-map category)
    (cond
     ((not org-export-e-odt-get-category-from-label)
      default-category-map)
     ((not (setq user-category
		 (save-match-data
		   (and (string-match "\\`\\(.*\\):.+" label)
			(match-string 1 label)))))
      default-category-map)
     (t
      (setq user-category-map
	    (or (assoc user-category org-e-odt-category-map-alist)
		(list nil user-category "category-and-value"))
	    category (nth 1 user-category-map))
      (if (member category org-export-e-odt-user-categories)
	  user-category-map
	default-category-map)))))

(defun org-e-odt-add-label-definition (label default-category)
  "Create an entry in `org-e-odt-entity-labels-alist' and return it."
  (setq label (substring-no-properties label))
  (let* ((label-props (org-e-odt-get-label-category-and-style
		       label default-category))
	 (category (nth 1 label-props))
	 (counter category)
	 (label-style (nth 2 label-props))
	 (sequence-var (intern (mapconcat
				'downcase
				(org-split-string counter) "-")))
	 (seqno (1+ (or (plist-get org-e-odt-entity-counts-plist sequence-var)
			0)))
	 (label-props (list label category seqno label-style)))
    (setq org-e-odt-entity-counts-plist
	  (plist-put org-e-odt-entity-counts-plist sequence-var seqno))
    (push label-props org-e-odt-entity-labels-alist)
    label-props))

(defun org-e-odt-format-label-definition (caption label category seqno label-style)
  (assert label)
  (format-spec
   (cadr (assoc-string label-style org-e-odt-label-styles t))
   `((?e . ,category)
     (?n . ,(org-e-odt-format-tags
	     '("<text:sequence text:ref-name=\"%s\" text:name=\"%s\" text:formula=\"ooow:%s+1\" style:num-format=\"1\">" . "</text:sequence>")
	     (format "%d" seqno) label category category))
     (?c . ,(or (and caption (concat ": " caption)) "")))))

(defun org-e-odt-format-label-reference (label category seqno label-style)
  (assert label)
  (save-match-data
    (let* ((fmt (cddr (assoc-string label-style org-e-odt-label-styles t)))
	   (fmt1 (car fmt))
	   (fmt2 (cadr fmt)))
      (org-e-odt-format-tags
       '("<text:sequence-ref text:reference-format=\"%s\" text:ref-name=\"%s\">"
	 . "</text:sequence-ref>")
       (format-spec fmt2 `((?e . ,category)
			   (?n . ,(format "%d" seqno)))) fmt1 label))))

(defun org-e-odt-fixup-label-references ()
  (goto-char (point-min))
  (while (re-search-forward
	  "<text:sequence-ref text:ref-name=\"\\([^\"]+\\)\">[ \t\n]*</text:sequence-ref>"
	  nil t)
    (let* ((label (match-string 1))
	   (label-def (assoc label org-e-odt-entity-labels-alist))
	   (rpl (and label-def
		     (apply 'org-e-odt-format-label-reference label-def))))
      (if rpl (replace-match rpl t t)
	(org-lparse-warn
	 (format "Unable to resolve reference to label \"%s\"" label))))))

(defun org-e-odt-format-entity-caption (label caption category)
  (or (and label
	   (apply 'org-e-odt-format-label-definition
		  caption (org-e-odt-add-label-definition label category)))
      caption ""))

(defun org-e-odt-format-tags-1 (tag text prefix suffix &rest args)
  (cond
   ((consp tag)
    (concat prefix (apply 'format (car tag) args) text suffix
	    (format (cdr tag))))
   ((stringp tag)			; singleton tag
    (concat prefix (apply 'format tag args) text))))

(defun org-e-odt-format-tags (tag text &rest args)
  (apply 'org-e-odt-format-tags-1 tag text "\n" "\n" args))

(defun org-e-odt-init-outfile ()
  (unless (executable-find "zip")
    ;; Not at all OSes ship with zip by default
    (error "Executable \"zip\" needed for creating OpenDocument files"))

  (let* ((outdir (make-temp-file
		  (format org-export-e-odt-tmpdir-prefix org-lparse-backend) t))
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

    ;; init conten.xml
    (with-current-buffer
	(find-file-noselect content-file t)
      (current-buffer))))



(defun org-e-odt-save-as-outfile (target opt-plist)
  ;; write automatic styles
  (org-e-odt-write-automatic-styles)

  ;; write styles file
  ;; (when (equal org-lparse-backend 'odt) FIXME
  ;;   )

  (org-e-odt-update-styles-file opt-plist)

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
    (when (equal org-lparse-backend 'odt)
      (push "styles.xml" xml-files))
    (message "Switching to directory %s" (expand-file-name zipdir))

    ;; save all xml files
    (mapc (lambda (file)
	    (with-current-buffer
		(find-file-noselect (expand-file-name file) t)
	      ;; prettify output if needed
	      (when org-export-e-odt-prettify-xml
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
  (let ((date (org-e-odt-format-date (plist-get info :date)))
	(author (or (plist-get info :author) ""))
	(email (plist-get info :email))
	(keywords (plist-get info :keywords))
	(description (plist-get info :description))
	(title (plist-get info :title)))
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
	      (when org-export-creator-info
		(format "Org-%s/Emacs-%s"
			org-version emacs-version)))
      (format "<meta:keyword>%s</meta:keyword>\n" keywords)
      (format "<dc:subject>%s</dc:subject>\n" description)
      (format "<dc:title>%s</dc:title>\n" title)
      "\n"
      "  </office:meta>\n" "</office:document-meta>")
  nil (expand-file-name "meta.xml")))

  ;; create a manifest entry for meta.xml
  (org-e-odt-create-manifest-file-entry "text/xml" "meta.xml"))

(defun org-e-odt-update-styles-file (opt-plist)
  ;; write styles file
  (let ((styles-file (plist-get opt-plist :odt-styles-file)))
    (org-e-odt-copy-styles-file (and styles-file
				     (read (org-trim styles-file)))))

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

    ;; Import local setting of `org-export-with-section-numbers'
    (org-lparse-bind-local-variables opt-plist)
    (org-e-odt-configure-outline-numbering
     (if org-export-with-section-numbers org-export-headline-levels 0)))

  ;; Write custom styles for source blocks
  ;; (org-e-odt-insert-custom-styles-for-srcblocks FIXME
  ;;  (mapconcat
  ;;   (lambda (style)
  ;;     (format " %s\n" (cddr style)))
  ;;   hfy-user-sheet-assoc ""))
  )

(defun org-e-odt-write-mimetype-file (format)
  ;; create mimetype file
  (let ((mimetype
	 (case format
	   (odt "application/vnd.oasis.opendocument.text")
	   (odf "application/vnd.oasis.opendocument.formula")
	   (t (error "Unknown OpenDocument backend %S" org-lparse-backend)))))
    (write-region mimetype nil (expand-file-name "mimetype"))
    mimetype))

(defun org-e-odt-finalize-outfile ()
  (org-e-odt-delete-empty-paragraphs))

(defun org-e-odt-delete-empty-paragraphs ()
  (goto-char (point-min))
  (let ((open "<text:p[^>]*>")
	(close "</text:p>"))
    (while (re-search-forward (format "%s[ \r\n\t]*%s" open close) nil t)
      (replace-match ""))))

(declare-function org-create-math-formula "org"
		  (latex-frag &optional mathml-file))

;;;###autoload
(defun org-export-e-odt-convert (&optional in-file out-fmt prefix-arg)
  "Convert IN-FILE to format OUT-FMT using a command line converter.
IN-FILE is the file to be converted.  If unspecified, it defaults
to variable `buffer-file-name'.  OUT-FMT is the desired output
format.  Use `org-export-e-odt-convert-process' as the converter.
If PREFIX-ARG is non-nil then the newly converted file is opened
using `org-open-file'."
  (interactive
   (append (org-lparse-convert-read-params) current-prefix-arg))
  (org-lparse-do-convert in-file out-fmt prefix-arg))

(defun org-e-odt-get (what &optional opt-plist)
  (case what
    (BACKEND 'odt)
    (EXPORT-DIR (org-export-directory :html opt-plist))
    (FILE-NAME-EXTENSION "odt")
    (EXPORT-BUFFER-NAME "*Org ODT Export*")
    (ENTITY-CONTROL org-e-odt-entity-control-callbacks-alist)
    (ENTITY-FORMAT org-e-odt-entity-format-callbacks-alist)
    (INIT-METHOD 'org-e-odt-init-outfile)
    (FINAL-METHOD 'org-e-odt-finalize-outfile)
    (SAVE-METHOD 'org-e-odt-save-as-outfile)
    (CONVERT-METHOD
     (and org-export-e-odt-convert-process
	  (cadr (assoc-string org-export-e-odt-convert-process
			      org-export-e-odt-convert-processes t))))
    (CONVERT-CAPABILITIES
     (and org-export-e-odt-convert-process
	  (cadr (assoc-string org-export-e-odt-convert-process
			      org-export-e-odt-convert-processes t))
	  org-export-e-odt-convert-capabilities))
    (TOPLEVEL-HLEVEL 1)
    (SPECIAL-STRING-REGEXPS org-export-e-odt-special-string-regexps)
    (INLINE-IMAGES 'maybe)
    (INLINE-IMAGE-EXTENSIONS '("png" "jpeg" "jpg" "gif" "svg"))
    (PLAIN-TEXT-MAP '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")))
    (TABLE-FIRST-COLUMN-AS-LABELS nil)
    (FOOTNOTE-SEPARATOR )
    (CODING-SYSTEM-FOR-WRITE 'utf-8)
    (CODING-SYSTEM-FOR-SAVE 'utf-8)
    (t (error "Unknown property: %s"  what))))

(defun org-export-e-odt-do-preprocess-latex-fragments ()
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

(defadvice org-format-latex-as-mathml
  (after org-e-odt-protect-latex-fragment activate)
  "Encode LaTeX fragment as XML.
Do this when translation to MathML fails."
  (when (or (not (> (length ad-return-value) 0))
	    (get-text-property 0 'org-protected ad-return-value))
    (setq ad-return-value
	  (org-propertize (org-e-odt-encode-plain-text (ad-get-arg 0))
			  'org-protected t))))

(defun org-export-e-odt-preprocess-latex-fragments ()
  (when (equal org-export-current-backend 'odt)
    (org-export-e-odt-do-preprocess-latex-fragments)))

(defun org-export-e-odt-preprocess-label-references ()
  (goto-char (point-min))
  (let (label label-components category value pretty-label)
    (while (re-search-forward "\\\\ref{\\([^{}\n]+\\)}" nil t)
      (org-if-unprotected-at (match-beginning 1)
	(replace-match
	 (let ((org-lparse-encode-pending t)
	       (label (match-string 1)))
	   ;; markup generated below is mostly an eye-candy.  At
	   ;; pre-processing stage, there is no information on which
	   ;; entity a label reference points to.  The actual markup
	   ;; is generated as part of `org-e-odt-fixup-label-references'
	   ;; which gets called at the fag end of export.  By this
	   ;; time we would have seen and collected all the label
	   ;; definitions in `org-e-odt-entity-labels-alist'.
	   (org-e-odt-format-tags
	    '("<text:sequence-ref text:ref-name=\"%s\">" .
	      "</text:sequence-ref>")
	    "" (org-add-props label '(org-protected t)))) t t)))))

;; process latex fragments as part of
;; `org-export-preprocess-after-blockquote-hook'. Note that this hook
;; is the one that is closest and well before the call to
;; `org-export-attach-captions-and-attributes' in
;; `org-export-preprocess-string'.  The above arrangement permits
;; captions, labels and attributes to be attached to png images
;; generated out of latex equations.
(add-hook 'org-export-preprocess-after-blockquote-hook
	  'org-export-e-odt-preprocess-latex-fragments)

(defun org-export-e-odt-preprocess (parameters)
  (org-export-e-odt-preprocess-label-references))


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
			org-export-e-odt-styles-file
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
	(copy-file styles-file "styles.xml" t))
       ((member styles-file-type '("odt" "ott"))
	(org-e-odt-zip-extract styles-file "styles.xml")))))
   (t
    (error (format "Invalid specification of styles.xml file: %S"
		   org-export-e-odt-styles-file))))

  ;; create a manifest entry for styles.xml
  (org-e-odt-create-manifest-file-entry "text/xml" "styles.xml"))

(defun org-e-odt-configure-outline-numbering (level)
  "Outline numbering is retained only upto LEVEL.
To disable outline numbering pass a LEVEL of 0."
  (goto-char (point-min))
  (let ((regex
	 "<text:outline-level-style\\([^>]*\\)text:level=\"\\([^\"]*\\)\"\\([^>]*\\)>")
	(replacement
	 "<text:outline-level-style\\1text:level=\"\\2\" style:num-format=\"\">"))
    (while (re-search-forward regex nil t)
      (when (> (string-to-number (match-string 2)) level)
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

(defvar org-e-odt-after-blockquotes-hook nil
  "Hook run during HTML export, after blockquote, verse, center are done.")

(defvar org-e-odt-final-hook nil
  "Hook run at the end of HTML export, in the new buffer.")

;; FIXME: it already exists in org-e-odt.el
;;; Function Declarations

(declare-function org-element-property "org-element" (property element))
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





(declare-function hfy-face-to-style "htmlfontify" (fn))
(declare-function hfy-face-or-def-to-name "htmlfontify" (fn))
(declare-function archive-zip-extract "arc-mode.el" (archive name))

;;; Internal Variables

;;;; ODT Internal Variables

(defconst org-e-odt-lib-dir
  (file-name-directory load-file-name)
  "Location of ODT exporter.
Use this to infer values of `org-e-odt-styles-dir' and
`org-export-e-odt-schema-dir'.")

(defvar org-e-odt-data-dir
  (expand-file-name "../etc/" org-e-odt-lib-dir)
  "Data directory for ODT exporter.
Use this to infer values of `org-e-odt-styles-dir' and
`org-export-e-odt-schema-dir'.")




(defconst org-export-e-odt-special-string-regexps
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
	  (expand-file-name "./schema/" org-e-odt-data-dir)))
   (expand-file-name "../contrib/odt/etc/schema/" org-e-odt-lib-dir) ; git
   )
  "List of directories to search for OpenDocument schema files.
Use this list to set the default value of
`org-export-e-odt-schema-dir'.  The entries in this list are
populated heuristically based on the values of `org-e-odt-lib-dir'
and `org-e-odt-data-dir'.")


(defconst org-e-odt-styles-dir-list
  (list
   (and org-e-odt-data-dir
	(expand-file-name "./styles/" org-e-odt-data-dir)) ; bail out
   (eval-when-compile
     (and (boundp 'org-e-odt-data-dir) org-e-odt-data-dir ; see make install
	  (expand-file-name "./styles/" org-e-odt-data-dir)))
   (expand-file-name "../etc/styles/" org-e-odt-lib-dir) ; git
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
 `org-export-e-odt-styles-file' and
 `org-export-e-odt-content-template-file'.

The default value of this variable varies depending on the
version of org in use and is initialized from
`org-e-odt-styles-dir-list'.  Note that the user could be using org
from one of: org's own private git repository, GNU ELPA tar or
standard Emacs.")

(defconst org-export-e-odt-tmpdir-prefix "%s-")
(defconst org-export-e-odt-bookmark-prefix "OrgXref.")

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

(defvar org-export-e-odt-embed-images t
  "Should the images be copied in to the odt file or just linked?")

(defvar org-export-e-odt-inline-images 'maybe)
(defvar org-export-e-odt-default-org-styles-alist
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

(defvar org-export-e-odt-org-styles-alist org-export-e-odt-default-org-styles-alist)

(defvar org-e-odt-entity-control-callbacks-alist
  `((EXPORT
     . (org-e-odt-begin-export org-e-odt-end-export))
    (DOCUMENT-CONTENT
     . (org-e-odt-begin-document-content org-e-odt-end-document-content))
    (DOCUMENT-BODY
     . (org-e-odt-begin-document-body org-e-odt-end-document-body))
    (TOC
     . (org-e-odt-begin-toc org-e-odt-end-toc))
    (ENVIRONMENT
     . (org-e-odt-begin-environment org-e-odt-end-environment))
    (FOOTNOTE-DEFINITION
     . (org-e-odt-begin-footnote-definition org-e-odt-end-footnote-definition))
    (TABLE
     . (org-e-odt-begin-table org-e-odt-end-table))
    (TABLE-ROWGROUP
     . (org-e-odt-begin-table-rowgroup org-e-odt-end-table-rowgroup))
    (LIST
     . (org-e-odt-begin-list org-e-odt-end-list))
    (LIST-ITEM
     . (org-e-odt-begin-list-item org-e-odt-end-list-item))
    (OUTLINE
     . (org-e-odt-begin-outline org-e-odt-end-outline))
    (OUTLINE-TEXT
     . (org-e-odt-begin-outline-text org-e-odt-end-outline-text))
    (PARAGRAPH
     . (org-e-odt-begin-paragraph org-e-odt-end-paragraph)))
  "")

(defvar org-e-odt-entity-format-callbacks-alist
  `((EXTRA-TARGETS . org-lparse-format-extra-targets)
    (ORG-TAGS . org-lparse-format-org-tags)
    (SECTION-NUMBER . org-lparse-format-section-number)
    (HEADLINE . org-e-odt-format-headline)
    (TOC-ENTRY . org-e-odt-format-toc-entry)
    (TOC-ITEM . org-e-odt-format-toc-item)
    (TAGS . org-e-odt-format-tags)
    (SPACES . org-e-odt-format-spaces)
    (TABS . org-e-odt-format-tabs)
    (LINE-BREAK . org-e-odt-format-line-break)
    (FONTIFY . org-e-odt-format-fontify)
    (TODO . org-lparse-format-todo)
    (LINK . org-e-odt-format-link)
    (INLINE-IMAGE . org-e-odt-format-inline-image)
    (ORG-LINK . org-e-odt-format-org-link)
    (HEADING . org-e-odt-format-heading)
    (ANCHOR . org-e-odt-format-anchor)
    (TABLE . org-lparse-format-table)
    (TABLE-ROW . org-e-odt-format-table-row)
    (TABLE-CELL . org-e-odt-format-table-cell)
    (FOOTNOTES-SECTION . ignore)
    (FOOTNOTE-REFERENCE . org-e-odt-format-footnote-reference)
    (HORIZONTAL-LINE . org-e-odt-format-horizontal-line)
    (COMMENT . org-e-odt-format-comment)
    (LINE . org-e-odt-format-line)
    (ORG-ENTITY . org-e-odt-format-org-entity))
  "")

;;;_. callbacks
;;;_. control callbacks
;;;_ , document body

(defvar org-lparse-toc)
(defvar org-lparse-body-only)		; let bound during org-do-lparse
(defvar org-lparse-to-buffer)		; let bound during org-do-lparse
(defvar org-lparse-opt-plist)		    ; bound during org-do-lparse
(defvar org-lparse-list-stack) ; dynamically bound in org-do-lparse
(defvar org-e-odt-list-stack-stashed)
(defvar org-lparse-table-begin-marker)
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
  "Entry for `org-e-odt-table-style' in `org-export-e-odt-table-styles'.")


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
\"#+ATTR_ODT: \" option to `org-lparse-get-block-params'.

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

(defvar org-export-e-odt-image-size-probe-method
  (append (and (executable-find "identify") '(imagemagick)) ; See Bug#10675
	  '(emacs fixed))
  "Ordered list of methods for determining image sizes.")

(defvar org-export-e-odt-default-image-sizes-alist
  '(("as-char" . (5 . 0.4))
    ("paragraph" . (5 . 5)))
  "Hardcoded image dimensions one for each of the anchor
  methods.")

;; A4 page size is 21.0 by 29.7 cms
;; The default page settings has 2cm margin on each of the sides. So
;; the effective text area is 17.0 by 25.7 cm
(defvar org-export-e-odt-max-image-size '(17.0 . 20.0)
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
  '(("text" "(%n)" "text" "(%n)")
    ("category-and-value" "%e %n%c" "category-and-value" "%e %n")
    ("value" "%e %n%c" "value" "%n"))
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
  '(("__Table__" "Table" "value")
    ("__Figure__" "Figure" "value")
    ("__MathFormula__" "Equation" "text")
    ("__DvipngImage__" "Equation" "value")
    ;; ("__Table__" "Table" "category-and-value")
    ;; ("__Figure__" "Figure" "category-and-value")
    ;; ("__DvipngImage__" "Equation" "category-and-value")
    )
  "Map a CATEGORY-HANDLE to CATEGORY-NAME and LABEL-STYLE.
This is an alist where each element is of the form
\\(CATEGORY-HANDLE CATEGORY-NAME LABEL-STYLE\\).  CATEGORY_HANDLE
could either be one of the internal handles (as seen above) or be
derived from the \"#+LABEL:<label-name>\" specification.  See
`org-export-e-odt-get-category-from-label'.  CATEGORY-NAME and
LABEL-STYLE are used for generating ODT labels.  See
`org-e-odt-label-styles'.")

(defvar org-export-e-odt-user-categories
  '("Illustration" "Table" "Text" "Drawing" "Equation" "Figure"))

(defvar org-export-e-odt-get-category-from-label nil
  "Should category of label be inferred from label itself.
When this option is non-nil, a label is parsed in to two
component parts delimited by a \":\" (colon) as shown here -
#+LABEL:[CATEGORY-HANDLE:]EXTRA.  The CATEGORY-HANDLE is mapped
to a CATEGORY-NAME and LABEL-STYLE using
`org-e-odt-category-map-alist'.  (If no such map is provided and
CATEGORY-NAME is set to CATEGORY-HANDLE and LABEL-STYLE is set to
\"category-and-value\").  If CATEGORY-NAME so obtained is listed
under `org-export-e-odt-user-categories' then the user specified
styles are used.  Otherwise styles as determined by the internal
CATEGORY-HANDLE is used.  See
`org-e-odt-get-label-category-and-style' for details.")

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
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)

(defcustom org-e-odt-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-e-html-protect'."
  :group 'org-export-e-html
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "ODT equivalent"))))
(defcustom org-export-e-odt-schema-dir
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
    "Set `org-export-e-odt-schema-dir'.
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
    (when org-export-e-odt-schema-dir
      (eval-after-load 'rng-loc
	'(add-to-list 'rng-schema-locating-files
		      (expand-file-name "schemas.xml"
					org-export-e-odt-schema-dir))))))

(defcustom org-export-e-odt-content-template-file nil
  "Template file for \"content.xml\".
The exporter embeds the exported content just before
\"</office:text>\" element.

If unspecified, the file named \"OrgOdtContentTemplate.xml\"
under `org-e-odt-styles-dir' is used."
  :type 'file
  :group 'org-export-e-odt
  :version "24.1")

(defcustom org-export-e-odt-styles-file nil
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


(defcustom org-export-e-odt-inline-image-extensions
  '("png" "jpeg" "jpg" "gif")
  "Extensions of image files that can be inlined into HTML."
  :type '(repeat (string :tag "Extension"))
  :group 'org-export-e-odt
  :version "24.1")

(defcustom org-export-e-odt-pixels-per-inch display-pixels-per-inch
  "Scaling factor for converting images pixels to inches.
Use this for sizing of embedded images.  See Info node `(org)
Images in ODT export' for more information."
  :type 'float
  :group 'org-export-e-odt
  :version "24.1")

(defcustom org-export-e-odt-create-custom-styles-for-srcblocks t
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
`org-export-e-odt-fontify-srcblocks' is turned on."
  :group 'org-export-e-odt
  :version "24.1"
  :type 'boolean)

(defcustom org-export-e-odt-preferred-output-format nil
  "Automatically post-process to this format after exporting to \"odt\".
Interactive commands `org-export-as-e-odt' and
`org-export-as-e-odt-and-open' export first to \"odt\" format and
then use `org-export-e-odt-convert-process' to convert the
resulting document to this format.  During customization of this
variable, the list of valid values are populated based on
`org-export-e-odt-convert-capabilities'."
  :group 'org-export-e-odt
  :version "24.1"
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,c ,c))
			     (org-lparse-reachable-formats "odt")))))

(defcustom org-export-e-odt-table-styles
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
`org-export-e-odt-content-template-file'.

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

\(setq org-export-e-odt-table-styles
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
(defcustom org-export-e-odt-fontify-srcblocks t
  "Specify whether or not source blocks need to be fontified.
Turn this option on if you want to colorize the source code
blocks in the exported file.  For colorization to work, you need
to make available an enhanced version of `htmlfontify' library."
  :type 'boolean
  :group 'org-export-e-odt
  :version "24.1")

(defcustom org-export-e-odt-prettify-xml t ; FIXME
  "Specify whether or not the xml output should be prettified.
When this option is turned on, `indent-region' is run on all
component xml buffers before they are saved.  Turn this off for
regular use.  Turn this on if you need to examine the xml
visually."
  :group 'org-export-e-odt
  :version "24.1"
  :type 'boolean)

(defcustom org-export-e-odt-convert-processes
  '(("LibreOffice"
     "soffice --headless --convert-to %f%x --outdir %d %i")
    ("unoconv"
     "unoconv -f %f -o %d %i"))
  "Specify a list of document converters and their usage.
The converters in this list are offered as choices while
customizing `org-export-e-odt-convert-process'.

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
%x extra options as set in `org-export-e-odt-convert-capabilities'."
  :group 'org-export-e-odt
  :version "24.1"
  :type
  '(choice
    (const :tag "None" nil)
    (alist :tag "Converters"
	   :key-type (string :tag "Converter Name")
	   :value-type (group (string :tag "Command line")))))

(defcustom org-export-e-odt-convert-process "LibreOffice"
  "Use this converter to convert from \"odt\" format to other formats.
During customization, the list of converter names are populated
from `org-export-e-odt-convert-processes'."
  :group 'org-export-e-odt
  :version "24.1"
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,(car c) ,(car c)))
			     org-export-e-odt-convert-processes))))

(defcustom org-export-e-odt-convert-capabilities
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
  "Specify input and output formats of `org-export-e-odt-convert-process'.
More correctly, specify the set of input and output formats that
the user is actually interested in.

This variable is an alist where each element is of the
form (DOCUMENT-CLASS INPUT-FMT-LIST OUTPUT-FMT-ALIST).
INPUT-FMT-LIST is a list of INPUT-FMTs.  OUTPUT-FMT-ALIST is an
alist where each element is of the form (OUTPUT-FMT
OUTPUT-FILE-EXTENSION EXTRA-OPTIONS).

The variable is interpreted as follows:
`org-export-e-odt-convert-process' can take any document that is in
INPUT-FMT-LIST and produce any document that is in the
OUTPUT-FMT-LIST.  A document converted to OUTPUT-FMT will have
OUTPUT-FILE-EXTENSION as the file name extension.  OUTPUT-FMT
serves dual purposes:
- It is used for populating completion candidates during
  `org-export-e-odt-convert' commands.
- It is used as the value of \"%f\" specifier in
  `org-export-e-odt-convert-process'.

EXTRA-OPTIONS is used as the value of \"%x\" specifier in
`org-export-e-odt-convert-process'.

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

;;;; Time-stamps
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
;;;; Time-stamp

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

;;;; Time-stamps

(defcustom org-e-odt-active-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to active time-stamps."
  :group 'org-export-e-odt
  :type 'string)

(defcustom org-e-odt-inactive-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to inactive time-stamps."
  :group 'org-export-e-odt
  :type 'string)

(defcustom org-e-odt-diary-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to diary time-stamps."
  :group 'org-export-e-odt
  :type 'string)


;;;; Links

(defcustom org-e-odt-image-default-option "width=.9\\linewidth"
  "Default option for images."
  :group 'org-export-e-odt
  :type 'string)

(defcustom org-e-odt-default-figure-position "htb"
  "Default position for latex figures."
  :group 'org-export-e-odt
  :type 'string)

(defcustom org-e-odt-inline-image-rules
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

(defun org-e-odt-format-image (src)
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

(defun org-export-e-odtize-region-for-paste (beg end)
  "Convert the region to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-export-e-odtize-output-type)
	 (htmlize-css-name-prefix org-export-e-odtize-css-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-export-e-odtize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-e-odtize-output-type' to `css', calls to
the function `org-export-e-odtize-region-for-paste' will produce code
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

;; (defun org-e-odt-format-toc-entry (snumber todo headline tags href)
;;   (setq headline (concat
;; 		  ;; section number
;; 		  (and org-export-with-section-numbers (concat snumber " "))
;; 		  ;; headline
;; 		  headline
;; 		  ;; tags
;; 		  (and tags (concat
;; 			     (org-e-odt-format-spaces 3)
;; 			     (org-e-odt-format-fontify tags "tag")))))
;;   ;; fontify headline based on TODO keyword
;;   (when todo (setq headline (org-e-odt-format-fontify headline "todo")))
;;   (org-e-odt-format-link headline (concat  "#" href)))

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

(defun org-e-odt-toc (depth info)
  (assert (wholenump depth))
  (let* ((headlines (org-export-collect-headlines info depth))
	 (toc-entries
	  (loop for headline in headlines collect
		(list (org-e-odt-headline-text
		       headline info 'org-e-odt-toc-entry-formatter)
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


;; (defun org-e-odt-format-heading (text level &optional id)
;;   (let* ((extra (concat (when id (format " id=\"%s\"" id)))))
;;     (concat (format "<h%d%s>" level extra) text (format "</h%d>" level))))

(defun org-e-odt-suffix-from-snumber (snumber)
  (let* ((snu (replace-regexp-in-string "\\." "-" snumber))
	 (href (cdr (assoc (concat "sec-" snu)
			   org-export-preferred-target-alist))))
    (org-solidify-link-text (or href snu))))

(defun org-e-odt-format-outline (contents level1 snumber title
					  tags target extra-targets extra-class)
  (concat
   (org-e-odt-format-heading
   (org-e-odt-format-headline title extra-targets tags snumber level1)
   level1 target)
   contents))

;; (defun org-e-odt-begin-outline-text (level1 snumber extra-class)
;;   (let* ((class (format "outline-text-%d" level1))
;; 	 (class (if extra-class (concat  class " " extra-class) class))
;; 	 (id (format "text-%s" (org-lparse-suffix-from-snumber snumber)))
;; 	 (extra (concat (when id (format " id=\"%s\"" id))
;; 			(when class (format " class=\"%s\"" class)))))
;;     (org-lparse-insert-tag "<div%s>" extra)))

;; (defun org-e-odt-end-outline-text ()
;;   (org-lparse-insert-tag "</div>"))

;; (defun org-e-odt-format-spaces (n)
;;   (let (out) (dotimes (i n out) (setq out (concat out "&nbsp;")))))

(defun org-e-odt-format-tabs (&optional n)
  (ignore))

;; (defun org-e-odt-format-line (line)
;;   (case org-lparse-dyn-current-environment
;;     ((quote fixedwidth) (concat (org-e-odt-encode-plain-text line) "\n"))
;;     (t (concat line "\n"))))

(defun org-e-odt-format-comment (fmt &rest args)
  (let ((comment (apply 'format fmt args)))
    (format "\n<!-- %s  -->\n" comment)))

(defun org-e-odt-fix-class-name (kwd) 	; audit callers of this function
  "Turn todo keyword into a valid class name.
Replaces invalid characters with \"_\"."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  kwd)

;; (defun org-e-odt-format-fontify (text style &optional id)
;;   (let (class extra how)
;;     (cond
;;      ((eq style 'underline)
;;       (setq extra " style=\"text-decoration:underline;\"" ))
;;      ((setq how (cdr (assoc style
;; 			    '((bold . ("<b>" . "</b>"))
;; 			      (emphasis . ("<i>" . "</i>"))
;; 			      (code . ("<code>" . "</code>"))
;; 			      (verbatim . ("<code>" . "</code>"))
;; 			      (strike . ("<del>" . "</del>"))
;; 			      (subscript . ("<sub>" . "</sub>"))
;; 			      (superscript . ("<sup>" . "</sup>")))))))
;;      ((listp style)
;;       (setq class (mapconcat 'identity style " ")))
;;      ((stringp style)
;;       (setq class style))
;;      (t (error "Unknown style %S" style)))

;;     (setq extra (concat (when class (format " class=\"%s\"" class))
;; 			(when id (format " id=\"%s\""  id))
;; 			extra))

;;     (let ((tags (or how '("<span%s>" . "</span>"))))
;;       (concat (format (car tags) extra) text  (cdr tags)))))

;; (defun org-e-odt-format-link (text href &optional extra)
;;   (let ((extra (concat (format " href=\"%s\"" href)
;; 		       (and extra (concat  " " extra)))))
;;     (format "<a%s>%s</a>" extra text)))

(defun org-e-odt-format-internal-link (text href &optional extra)
  (org-e-odt-format-link text (concat "#" href) extra))

;; (defun org-e-odt-format-heading (text level &optional id)
;;   (let* ((extra (concat (when id (format " id=\"%s\"" id)))))
;;     (concat (format "<h%d%s>" level extra) text (format "</h%d>\n" level))))

;; (defun org-e-odt-format-anchor (text name &optional class)
;;   (let* ((id name)
;; 	 (extra (concat
;; 		 (when name (format " name=\"%s\""  name))
;; 		 (when id (format " id=\"%s\""  id))
;; 		 (when class (format " class=\"%s\""  class)))))
;;     (format "<a%s>%s</a>" extra text)))

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
      (org-split-string tags ":")
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

;; (defun org-e-odt-format-footnote-reference (n def refcnt)
;;   (let ((extra (if (= refcnt 1) "" (format ".%d"  refcnt))))
;;     (format org-e-odt-footnote-format
;; 	    (format
;; 	     "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>"
;; 	     n extra n n))))

(defun org-e-odt-format-footnotes-section (section-name definitions)
  (if (not definitions) ""
    (format org-e-odt-footnotes-section section-name definitions)))

;; (defun org-e-odt-format-footnote-definition (fn)
;;   (let ((n (car fn)) (def (cdr fn)))
;;     (format
;;      "<tr>\n<td>%s</td>\n<td>%s</td>\n</tr>\n"
;;      (format
;;       (format org-e-odt-footnote-format
;; 	      "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>")
;;       n n n) def)))

(defun org-e-odt-get-coding-system-for-write ()
  (or org-e-odt-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

(defun org-e-odt-get-coding-system-for-save ()
  (or org-e-odt-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

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
	      (org-export-secondary-string (cdr caption) 'e-odt info)
	      label-str
	      (org-export-secondary-string (car caption) 'e-odt info)))
     ;; Standard caption format.
     ;; (t (format "\\caption{%s%s}\n"
     ;; 		label-str
     ;; 		(org-export-secondary-string (car caption) 'e-odt info)))

     (t (org-export-secondary-string (car caption) 'e-odt info)))))

(defun org-e-odt--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

(defun org-e-odt--make-option-string (options)
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



;;; Template

(defun org-e-odt-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."


  ;; write meta file
  (org-e-odt-update-meta-file info)


  (with-temp-buffer
    (insert-file-contents
     (or org-export-e-odt-content-template-file
	 (expand-file-name "OrgOdtContentTemplate.xml"
			   org-e-odt-styles-dir)))
    (goto-char (point-min))
    (re-search-forward "</office:text>" nil nil)
    (goto-char (match-beginning 0))

    ;; Title
    (insert (org-e-odt-format-preamble info))
    ;; Table of Contents
    (let ((depth (plist-get info :with-toc)))
      (when (wholenump depth) (org-e-odt-toc depth info)))

    ;; Contents
    (insert contents)
    (buffer-substring-no-properties (point-min) (point-max))))



;;; Transcode Functions

;;;; Block

(defun org-e-odt-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-odt--wrap-label
   center-block
   (format "<div style=\"text-align: center\">\n%s</div>" contents)))


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
holding contextual information.  See
`org-export-data'."
  (org-e-odt--wrap-label dynamic-block contents))


;;;; Emphasis

(defun org-e-odt-emphasis (emphasis contents info)
  "Transcode EMPHASIS from Org to HTML.
CONTENTS is the contents of the emphasized text.  INFO is a plist
holding contextual information.."
  ;; (format (cdr (assoc (org-element-property :marker emphasis)
  ;; 		      org-e-odt-emphasis-alist))
  ;; 	  contents)
  (org-e-odt-format-fontify
   contents (cadr (assoc
		   (org-element-property :marker emphasis)
		   '(("*" bold)
		     ("/" emphasis)
		     ("_" underline)
		     ("=" code)
		     ("~" verbatim)
		     ("+" strike))))))


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


;; (defun org-odt-format-source-code-or-example-colored
;;   (lines lang caption textareap cols rows num cont rpllbl fmt))

;; (defun org-e-odt-format-source-code-or-example-plain
;;   (lines lang caption textareap cols rows num cont rpllbl fmt)
;;   (setq lines
;; 	(concat
;; 	 "<pre class=\"example\">\n"
;; 	 (cond
;; 	  (textareap
;; 	   (concat
;; 	    (format "<p>\n<textarea cols=\"%d\" rows=\"%d\">"
;; 		    cols rows)
;; 	    lines "</textarea>\n</p>\n"))
;; 	  (t
;; 	   (with-temp-buffer
;; 	     (insert lines)
;; 	     (goto-char (point-min))
;; 	     (while (re-search-forward "[<>&]" nil t)
;; 	       (replace-match (cdr (assq (char-before)
;; 					 '((?&."&amp;")(?<."&lt;")(?>."&gt;"))))
;; 			      t t))
;; 	     (buffer-string))))
;; 	 "</pre>\n"))

;;   (unless textareap
;;     (setq lines (org-export-number-lines lines 1 1 num cont rpllbl fmt)))

;;   ;; (when (string-match "\\(\\`<[^>]*>\\)\n" lines)
;;   ;;   (setq lines (replace-match "\\1" t nil lines)))

;;   lines)

;; (defun org-e-odt-format-source-code-or-example-colored
;;   (lines lang caption textareap cols rows num cont rpllbl fmt)
;;   (let* ((lang-m (when lang
;; 		   (or (cdr (assoc lang org-src-lang-modes))
;; 		       lang)))
;; 	 (mode (and lang-m (intern
;; 			    (concat
;; 			     (if (symbolp lang-m)
;; 				 (symbol-name lang-m)
;; 			       lang-m)
;; 			     "-mode"))))
;; 	 (org-inhibit-startup t)
;; 	 (org-startup-folded nil))
;;     (setq lines
;; 	  (with-temp-buffer
;; 	    (insert lines)
;; 	    (if (functionp mode)
;; 		(funcall mode)
;; 	      (fundamental-mode))
;; 	    (font-lock-fontify-buffer)
;; 	    ;; markup each line separately
;; 	    (org-remove-formatting-on-newlines-in-region
;; 	     (point-min) (point-max))
;; 	    (org-src-mode)
;; 	    (set-buffer-modified-p nil)
;; 	    (org-export-e-odtize-region-for-paste
;; 	     (point-min) (point-max))))

;;     (when (string-match "<pre\\([^>]*\\)>\n*" lines)
;;       (setq lines (replace-match
;; 		   (format "<pre class=\"src src-%s\">\n" lang) t t lines)))

;;     (when caption
;;       (setq lines
;; 	    (concat
;; 	     "<div class=\"org-src-container\">"
;; 	     (format "<label class=\"org-src-name\">%s</label>" caption)
;; 	     lines "</div>")))

;;     (unless textareap
;;       (setq lines (org-export-number-lines lines 1 1 num cont rpllbl fmt)))

;;     ;; (when (string-match "\\(\\`<[^>]*>\\)\n" lines)
;;     ;;   (setq lines (replace-match "\\1" t nil lines)))
;;     lines))

;; (defun org-e-odt-format-source-code-or-example
;;   (lang code &optional opts indent caption)
;;   "Format CODE from language LANG and return it formatted for export.
;; The CODE is marked up in `org-export-current-backend' format.

;; Check if a function by name
;; \"org-<backend>-format-source-code-or-example\" is bound. If yes,
;; use it as the custom formatter. Otherwise, use the default
;; formatter. Default formatters are provided for docbook, html,
;; latex and ascii backends. For example, use
;; `org-e-odt-format-source-code-or-example' to provide a custom
;; formatter for export to \"html\".

;; If LANG is nil, do not add any fontification.
;; OPTS contains formatting options, like `-n' for triggering numbering lines,
;; and `+n' for continuing previous numbering.
;; Code formatting according to language currently only works for HTML.
;; Numbering lines works for all three major backends (html, latex, and ascii).
;; INDENT was the original indentation of the block."
;;   (save-match-data
;;     (let* ((backend-formatter 'org-e-odt-format-source-code-or-example-plain)
;; 	   num cont rtn rpllbl keepp textareap preserve-indentp cols rows fmt)
;;       (setq opts (or opts "")
;; 	    num (string-match "[-+]n\\>" opts)
;; 	    cont (string-match "\\+n\\>" opts)
;; 	    rpllbl (string-match "-r\\>" opts)
;; 	    keepp (string-match "-k\\>" opts)
;; 	    textareap (string-match "-t\\>" opts)
;; 	    preserve-indentp (or org-src-preserve-indentation
;; 				 (string-match "-i\\>" opts))
;; 	    cols (if (string-match "-w[ \t]+\\([0-9]+\\)" opts)
;; 		     (string-to-number (match-string 1 opts))
;; 		   80)
;; 	    rows (if (string-match "-h[ \t]+\\([0-9]+\\)" opts)
;; 		     (string-to-number (match-string 1 opts))
;; 		   (org-count-lines code))
;; 	    fmt (if (string-match "-l[ \t]+\"\\([^\"\n]+\\)\"" opts)
;; 		    (match-string 1 opts)))
;;       (when (and textareap
;; 		 ;; (eq org-export-current-backend 'html)
;; 		 )
;; 	;; we cannot use numbering or highlighting.
;; 	(setq num nil cont nil lang nil))
;;       (if keepp (setq rpllbl 'keep))
;;       (setq rtn (if preserve-indentp code (org-remove-indentation code)))
;;       (when (string-match "^," rtn)
;; 	(setq rtn (with-temp-buffer
;; 		    (insert rtn)
;; 		    ;; Free up the protected lines
;; 		    (goto-char (point-min))
;; 		    (while (re-search-forward "^," nil t)
;; 		      (if (or (equal lang "org")
;; 			      (save-match-data
;; 				(looking-at "\\([*#]\\|[ \t]*#\\+\\)")))
;; 			  (replace-match ""))
;; 		      (end-of-line 1))
;; 		    (buffer-string))))
;;       (when lang
;; 	(if (featurep 'xemacs)
;; 	    (require 'htmlize)
;; 	  (require 'htmlize nil t)))

;;       (setq backend-formatter
;; 	    (cond
;; 	     ((fboundp 'htmlize-region-for-paste)
;; 	      'org-e-odt-format-source-code-or-example-colored)
;; 	     (t
;; 	      (message
;; 	       "htmlize.el 1.34 or later is needed for source code formatting")
;; 	      'org-e-odt-format-source-code-or-example-plain)))
;;       (funcall backend-formatter rtn lang caption textareap cols rows
;; 	       num cont rpllbl fmt))))

(defun org-e-odt-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((options (or (org-element-property :options example-block) ""))
	 (value (org-export-handle-code example-block info)))
    ;; (org-e-odt--wrap-label
    ;;  example-block (format "\\begin{verbatim}\n%s\\end{verbatim}" value))
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
  (when (string= (org-element-property :type export-block) "latex")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-e-odt-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((value (org-element-normalize-string
		 (replace-regexp-in-string
		  "^[ \t]*: ?" ""
		  (org-element-property :value fixed-width)))))
    (org-e-odt--wrap-label
     fixed-width (org-e-odt-format-source-code-or-example value nil))))


;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-e-odt-footnote-def (raw info) ; FIXME
  (if (equal (org-element-type raw) 'org-data)
      (org-trim (org-export-data raw 'e-odt info))
    (org-odt-format-stylized-paragraph
     'footnote (org-trim (org-export-secondary-string raw 'e-odt info)))))

(defvar org-e-odt-footnote-separator
  (org-e-odt-format-fontify "," 'superscript))

(defun org-e-odt-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (and (listp prev) (eq (car prev) 'footnote-reference))
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

(defun org-e-odt-headline-text (headline info &optional formatter)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((numberedp (plist-get info :section-numbers))
	 (level (org-export-get-relative-level headline info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property
				 :todo-keyword headline)))
		      (and todo
			   (org-export-secondary-string todo 'e-odt info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-secondary-string
		(org-element-property :title headline) 'e-odt info))
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
			org-e-odt-headline-formatter)))
    (funcall formatter level section-no todo todo-type priority
	     text tags primary-target secondary-targets extra-class)))

(defun org-e-odt-headline (headline contents info)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :latex-class))
	 (numberedp (plist-get info :section-numbers))
	 ;; Get level relative to current parsed data.
	 (level (org-export-get-relative-level headline info))
	 ;; (class-sectionning (assoc class org-e-odt-classes))
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
		(org-element-property :title headline) 'e-odt info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property
				 :todo-keyword headline)))
		      (and todo
			   (org-export-secondary-string todo 'e-odt info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-element-property :tags headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 ;; Create the headline text.
	 (full-text (if (functionp org-e-odt-format-headline-function)
			;; User-defined formatting function.
			(funcall org-e-odt-format-headline-function
				 todo todo-type priority text tags)
		      ;; Default formatting.
		      (concat
		       ;; (when todo
		       ;; 	 (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
		       (org-e-odt-todo todo) " "
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
      ;; (format section-fmt full-text
      ;; 	(concat headline-label pre-blanks contents))

      (org-e-odt-format-outline contents level section-no full-text tags
				 (car (last headline-labels))
				 (butlast headline-labels) nil)))))


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
  "Transcode an INLINETASK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-secondary-string
	       (org-element-property :title inlinetask) 'e-odt info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-property
				:todo-keyword inlinetask)))
		     (and todo
			  (org-export-secondary-string todo 'e-odt info)))))
	(todo-type (org-element-property :todo-type inlinetask))
	(tags (and (plist-get info :with-tags)
		   (org-element-property :tags inlinetask)))
	(priority (and (plist-get info :with-priority)
		       (org-element-property :priority inlinetask))))
    ;; If `org-e-odt-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-e-odt-format-inlinetask-function)
	(funcall org-e-odt-format-inlinetask-function
		 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-e-odt--wrap-label
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
  ;; Grab `:level' from plain-list properties, which is always the
  ;; first element above current item.
  (let* ((plain-list (org-export-get-parent item info))
	 (type (org-element-property :type plain-list))
	 (level (org-element-property :level plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-secondary-string tag 'e-odt info)))))
    (org-e-odt-format-list-item
     contents type checkbox (or tag counter))))


;;;; Keyword

(defun org-e-odt-keyword (keyword contents info)
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
	    (when (wholenump depth) (org-e-odt-toc depth info))))
	 ((string= "tables" value) "\\listoftables")
	 ((string= "figures" value) "\\listoffigures")
	 ((string= "listings" value)
	  (cond
	   ;; At the moment, src blocks with a caption are wrapped
	   ;; into a figure environment.
	   (t "\\listoffigures")))))))))


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
   (let ((latex-frag
	  (org-remove-indentation
	   (org-element-property :value latex-environment)))
	 (processing-type (plist-get info :LaTeX-fragments)))
     (cond
      ((member processing-type '(t mathjax))
       (org-e-odt-format-latex latex-frag 'mathml))
      ((equal processing-type 'dvipng)
       (let* ((formula-link (org-e-odt-format-latex
			     latex-frag processing-type)))
	 (when (and formula-link
		    (string-match "file:\\([^]]*\\)" formula-link))
	   (org-e-odt-format-inline-image (match-string 1 formula-link)))))
      (t
       latex-frag)))))


;;;; Latex Fragment

(defun org-e-odt-latex-fragment (latex-fragment contents info)
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
	(org-e-odt-format-internal-link text href)))
     (t (let ((processing-type (plist-get info :LaTeX-fragments)))
	  (cond
	   ((member processing-type '(t mathjax))
	    (org-e-odt-format-latex latex-frag 'mathjax))
	   ((equal processing-type 'dvipng)
	    (let* ((formula-link (org-e-odt-format-latex
				  latex-frag processing-type)))
	      (when (and formula-link
			 (string-match "file:\\([^]]*\\)" formula-link))
		(org-e-odt-format-inline-image
		 (match-string 1 formula-link)))))
	   (t latex-frag)))))))


;;;; Line Break

(defun org-e-odt-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "<br/>")


;;;; Link

(defun org-e-odt-link--inline-image (link info)
  "Return HTML code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-paragraph link info))
	 (path (let ((raw-path (org-element-property :path link)))
		 (if (not (file-name-absolute-p raw-path)) raw-path
		   (expand-file-name raw-path))))
	 (caption (org-e-odt--caption/label-string
		   (org-element-property :caption parent)
		   (org-element-property :name parent)
		   info))
	 (label (org-element-property :name parent))
	 ;; Retrieve latex attributes from the element around.
	 (attr (let ((raw-attr
		      (mapconcat #'identity
				 (org-element-property :attr_odt parent)
				 " ")))
		 (unless (string= raw-attr "") raw-attr))))
    ;; Now clear ATTR from any special keyword and set a default
    ;; value if nothing is left.
    (setq attr (if (not attr) "" (org-trim attr)))
    ;; Return proper string, depending on DISPOSITION.
    (let ((href (and label (org-export-solidify-link-text label))))
      (org-e-odt-format-inline-image path caption href attr))))

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
     (imagep (org-e-odt-link--inline-image link info))
     ;; Target or radioed target: replace link with the normalized
     ;; custom-id/target name.
     ((member type '("target" "radio"))
      (org-e-odt-format-internal-link
       (or desc (org-export-secondary-string path 'e-odt info))
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
	   (org-e-odt-format-internal-link
	    (or desc
		(org-export-secondary-string
		 (org-element-property :raw-link link)
		 'e-odt info))
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
	       (org-e-odt-format-internal-link
		(or desc
		    (org-export-secondary-string
		     (org-element-property :title destination)
		     'e-odt info)) label))))
	  ;; Fuzzy link points nowhere.
	  (otherwise
	   (org-e-odt-format-fontify
	    (or desc
		(org-export-secondary-string
		 (org-element-property :raw-link link)
		 'e-odt info)) 'emphasis)))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path (or desc ""))
	      (org-export-resolve-coderef path info)))
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
	 (parent (car (org-export-get-genealogy paragraph info))))
    (cond
     ;; ((and (equal (car parent) 'item)
     ;; 	   (= (org-element-property :begin paragraph)
     ;; 	      (org-element-property :contents-begin parent)))
     ;;  ;; leading paragraph in a list item have no tags
     ;;  contents)
     (t (org-e-odt-format-stylized-paragraph nil contents)))))


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
  (let ((all org-export-e-odt-special-string-regexps)
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
  (org-e-odt--wrap-label
   quote-block (format "<blockquote>\n%s</blockquote>" contents)))


;;;; Quote Section

(defun org-e-odt-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format "<pre>\n%s</pre>" value))))


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
	 (org-element-property :raw-value radio-target))))


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
	 (code (org-export-handle-code src-block info))
	 (caption (org-element-property :caption src-block))
	 (label (org-element-property :name src-block)))
    ;; FIXME: Handle caption

    ;; caption-str (when caption)
    ;; (main (org-export-secondary-string (car caption) 'e-odt info))
    ;; (secondary (org-export-secondary-string (cdr caption) 'e-odt info))
    ;; (caption-str (org-e-odt--caption/label-string caption label info))
    (org-e-odt-format-source-code-or-example code lang)))


;;;; Statistics Cookie

(defun org-e-odt-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (org-e-odt-format-fontify cookie-value 'code)))


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


;;;; Table

;; (defun org-e-odt-begin-table (caption label attributes)
;;   (let* ((html-table-tag (or (plist-get info :html-table-tag) ; FIXME
;; 			     org-e-odt-table-tag))
;; 	 (html-table-tag
;; 	  (org-e-odt-splice-attributes html-table-tag attributes)))
;;     (when label
;;       (setq html-table-tag
;; 	    (org-e-odt-splice-attributes
;; 	     html-table-tag
;; 	     (format "id=\"%s\"" (org-solidify-link-text label)))))
;;     (concat "\n" html-table-tag
;; 	    (format "\n<caption>%s</caption>" (or caption "")))))

;; (defun org-e-odt-end-table ()
;;   "</table>\n")

;; (defun org-e-odt-format-table-cell (text r c horiz-span)
;;   (let ((cell-style-cookie
;; 	 (if org-e-odt-table-align-individual-fields
;; 	     (format (if (and (boundp 'org-e-odt-format-table-no-css)
;; 			      org-e-odt-format-table-no-css)
;; 			 " align=\"%s\"" " class=\"%s\"")
;; 		     (or (aref (plist-get table-info :alignment) c) "left")) ""))) ;; FIXME
;;     (cond
;;      (org-e-odt-table-cur-rowgrp-is-hdr
;;       (concat
;;        (format (car org-export-table-header-tags) "col" cell-style-cookie)
;;        text (cdr org-export-table-header-tags)))
;;      ((and (= c 0) org-e-odt-table-use-header-tags-for-first-column)
;;       (concat
;;        (format (car org-export-table-header-tags) "row" cell-style-cookie)
;;        text (cdr org-export-table-header-tags)))
;;      (t
;;       (concat
;;        (format (car org-export-table-data-tags) cell-style-cookie)
;;        text (cdr org-export-table-data-tags))))))

(defun org-e-odt-table-row (fields &optional text-for-empty-fields)
  (incf org-e-odt-table-rownum)
  (let ((i -1))
    (org-e-odt-format-table-row
     (mapconcat
      (lambda (x)
	(when (and (string= x "") text-for-empty-fields)
	  (setq x text-for-empty-fields))
	(incf i)
	(let (horiz-span)
	  (org-e-odt-format-table-cell
	   x org-e-odt-table-rownum i (or horiz-span 0))))
      fields "\n"))))

;; (defun org-e-odt-end-table-rowgroup ()
;;   (when org-e-odt-table-rowgrp-open
;;     (setq org-e-odt-table-rowgrp-open nil)
;;     (if org-e-odt-table-cur-rowgrp-is-hdr "</thead>" "</tbody>")))

;; (defun org-e-odt-begin-table-rowgroup (&optional is-header-row)
;;   (concat
;;    (when org-e-odt-table-rowgrp-open
;;      (org-e-odt-end-table-rowgroup))
;;    (progn
;;      (setq org-e-odt-table-rowgrp-open t)
;;      (setq org-e-odt-table-cur-rowgrp-is-hdr is-header-row)
;;      (if is-header-row "<thead>" "<tbody>"))))

(defun org-e-odt-table-preamble ()
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
		    (alignspec (if (and (boundp 'org-e-odt-format-table-no-css)
					org-e-odt-format-table-no-css)
				   " align=\"%s\"" " class=\"%s\""))
		    (extra (format alignspec  align)))
	       (format "<col%s />" extra))
	     (when (memq gr '(end start-end))
	       (setq colgropen nil)
	       "</colgroup>"))))
    (concat preamble (if colgropen "</colgroup>"))))

(defun org-e-odt-list-table (lines caption label attributes)
  (setq lines (org-e-odt-org-table-to-list-table lines))
  (let* ((splice nil) head
	 (org-e-odt-table-rownum -1)
	 i (cnt 0)
	 fields line
	 org-e-odt-table-cur-rowgrp-is-hdr
	 org-e-odt-table-rowgrp-open
	 n
	 (org-lparse-table-style 'org-table)
	 org-lparse-table-is-styled)
    (cond
     (splice
      (setq org-lparse-table-is-styled nil)
      (mapconcat 'org-e-odt-table-row lines "\n"))
     (t
      (setq org-lparse-table-is-styled t)

      (concat
       (org-e-odt-begin-table caption label attributes)
       ;; FIXME (org-e-odt-table-preamble)
       (org-e-odt-begin-table-rowgroup head)

       (mapconcat
	(lambda (line)
	  (cond
	   ((equal line 'hline) (org-e-odt-begin-table-rowgroup))
	   (t (org-e-odt-table-row line))))
	lines "\n")

       (org-e-odt-end-table-rowgroup)
       (org-e-odt-end-table))))))

(defun org-e-odt-transcode-table-row (row)
  (if (string-match org-table-hline-regexp row) 'hline
    (mapcar
     (lambda (cell)
       (org-export-secondary-string
	(let ((cell (org-element-parse-secondary-string
		     cell
		     (cdr (assq 'table org-element-string-restrictions)))))
	  cell)
	'e-odt info))
     (org-split-string row "[ \t]*|[ \t]*"))))

(defun org-e-odt-org-table-to-list-table (lines &optional splice)
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
	  (push (org-e-odt-transcode-table-row line) lines-1))))
     (t (while (setq line (pop lines))
	  (cond
	   ((string-match "^[ \t]*|-" line)
	    (when lines (push 'hline lines-1)))
	   (t (push (org-e-odt-transcode-table-row line) lines-1))))))
    (nreverse lines-1)))

(defun org-e-odt-table-table (raw-table)
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

(defun org-e-odt-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((label (org-element-property :name table))
	 (caption (org-e-odt--caption/label-string
		   (org-element-property :caption table) label info))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_odt table)
			  " "))
	 (raw-table (org-element-property :raw-table table))
	 (table-type (org-element-property :type table)))
    (case table-type
      (table.el
       ;; (org-e-odt-table-table raw-table)
       )
      (t
       (let* ((table-info (org-export-table-format-info raw-table))
	      (columns-number (length (plist-get table-info :alignment)))
	      (lines (org-split-string
		      (org-export-clean-table
		       raw-table (plist-get table-info :special-column-p)) "\n")))
	 (org-e-odt-list-table lines caption label attr))))))


;;;; Target

(defun org-e-odt-target (target text info)
  "Transcode a TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (org-e-odt-format-anchor
   text (org-export-solidify-link-text
	 (org-element-property :raw-value target))))


;;;; Time-stamp

(defun org-e-odt-time-stamp (time-stamp contents info)
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
  ;; 		   (format org-e-odt-active-timestamp-format value))
  ;; 		  ((memq type '(inactive inactive-range))
  ;; 		   (format org-e-odt-inactive-timestamp-format value))
  ;; 		  (t
  ;; 		   (format org-e-odt-diary-timestamp-format value)))))
  (let ((value (org-element-property :value time-stamp))
        (type (org-element-property :type time-stamp))
        (appt-type (org-element-property :appt-type time-stamp)))
    (setq value (org-export-secondary-string value 'e-odt info))
    (org-e-odt-format-fontify
     (concat
      (org-e-odt-format-fontify
       (cond ((eq appt-type 'scheduled) org-scheduled-string)
	     ((eq appt-type 'deadline) org-deadline-string)
	     ((eq appt-type 'closed) org-closed-string)) "timestamp-kwd")
      ;; FIXME: (org-translate-time value)
      (org-e-odt-format-fontify value "timestamp"))
     "timestamp-wrapper")))


;;;; Verbatim

(defun org-e-odt-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-odt-emphasis
   verbatim (org-element-property :value verbatim) info))


;;;; Verse Block

(defun org-e-odt-verse-block (verse-block contents info)
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
		     'e-odt info)))))

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

(defconst org-e-odt-filters-alist
  '((:filter-final-output . org-e-odt-final-function))
  "Alist between filters keywords and back-end specific filters.
See `org-export-filters-alist' for more information.")


;;;; Filters

(defun org-e-odt-final-function (contents backend info)
  (if (not org-export-e-odt-prettify-xml) contents
    (with-temp-buffer
      (nxml-mode)
      (insert contents)
      (indent-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))


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

  ;; FIXME
  (with-current-buffer (get-buffer-create "*debug*")
    (erase-buffer))

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
    (org-e-odt-save-as-outfile target ;; info
			       nil
			       )

    ;; return outfile
    target))




;;; FIXMES, TODOS, FOR REVIEW etc

;;;; org-format-table-html
;;;; org-format-org-table-html
;;;; org-format-table-table-html
;;;; org-table-number-fraction
;;;; org-table-number-regexp
;;;; org-e-odt-table-caption-above

;;;; org-whitespace
;;;; "<span style=\"visibility:hidden;\">%s</span>"
;;;; Remove display properties
;;;; org-e-odt-final-hook

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
;;;; org-e-odt-special-string-regexps
;;;; org-e-odt-coding-system
;;;; org-e-odt-coding-system
;;;; org-e-odt-inline-images
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

;; register the odt exporter with the pre-processor
(add-to-list 'org-export-backends 'odt)

;; register the odt exporter with org-lparse library
(org-lparse-register-backend 'odt)

(eval-after-load 'org-exp
  '(add-to-list 'org-export-inbuffer-options-extra
		'("ODT_STYLES_FILE" :odt-styles-file)))

(provide 'org-e-odt)

;;; org-e-odt.el ends here
