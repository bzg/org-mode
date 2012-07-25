;;; org-md.el --- Markdown Back-End for Org Export Engine

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Keywords: org, wp, tex

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

;; This library implements a Markdown back-end (vanilla flavour) for
;; Org exporter, based on `e-html'.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-md-export-as-markdown' (temporary buffer) and
;; `org-md-export-to-markdown' ("md" file).

;;; Code:

(require 'org-e-html)



;;; Define Back-End

(org-export-define-derived-backend md e-html
  :export-block ("MD" "MARKDOWN")
  :filters-alist ((:filter-parse-tree . org-md-separate-elements))
  :translate-alist ((bold . org-md-bold)
		    (code . org-md-verbatim)
		    (example-block . org-md-example-block)
		    (footnote-definition . ignore)
		    (footnote-reference . ignore)
		    (headline . org-md-headline)
		    (horizontal-rule . org-md-horizontal-rule)
		    (inline-src-block . org-md-verbatim)
		    (italic . org-md-italic)
		    (item . org-md-item)
		    (line-break . org-md-line-break)
		    (link . org-md-link)
		    (paragraph . org-md-paragraph)
		    (plain-list . org-md-plain-list)
		    (plain-text . org-md-plain-text)
		    (quote-block . org-md-quote-block)
		    (quote-section . org-md-example-block)
		    (section . org-md-section)
		    (src-block . org-md-example-block)
		    (template . org-md-template)
		    (verbatim . org-md-verbatim)))



;;; Filters

(defun org-md-separate-elements (tree backend info)
  "Make sure elements are separated by at least one blank line.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Assume BACKEND is `md'."
  (org-element-map
   tree org-element-all-elements
   (lambda (elem)
     (unless (eq (org-element-type elem) 'org-data)
       (org-element-put-property
	elem :post-blank
	(let ((post-blank (org-element-property :post-blank elem)))
	  (if (not post-blank) 1 (max 1 post-blank)))))))
  ;; Return updated tree.
  tree)



;;; Transcode Functions

;;;; Bold

(defun org-md-bold (bold contents info)
  "Transcode BOLD object into Markdown format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "**%s**" contents))


;;;; Code and Verbatim

(defun org-md-verbatim (verbatim contents info)
  "Transcode VERBATIM object into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (format (cond ((not (string-match "`" value)) "`%s`")
		  ((or (string-match "\\``" value)
		       (string-match "`\\'" value))
		   "`` %s ``")
		  (t "``%s``"))
	    value)))


;;;; Example Block and Src Block

(defun org-md-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "    "
   (org-remove-indentation
    (org-element-property :value example-block))))


;;;; Headline

(defun org-md-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((level (org-export-get-relative-level headline info))
	  (title (org-export-data (org-element-property :title headline) info))
	  (todo (and (plist-get info :with-todo-keywords)
		     (let ((todo (org-element-property :todo-keyword headline)))
		       (and todo (org-export-data todo info)))))
	  (tags (and (plist-get info :with-tags)
		     (org-export-get-tags headline info)))
	  (priority (and (plist-get info :with-priority)
			 (org-element-property :priority headline))))
      (concat (make-string level ?#)
	      (and todo (concat " " todo))
	      (and priority (concat " " priority))
	      " " title
	      (and tags (format " :%s:"
				(mapconcat 'identity tags ":")))
	      "\n\n" contents))))


;;;; Horizontal Rule

(defun org-md-horizontal-rule (horizontal-rule contents info)
  "Transcode HORIZONTAL-RULE element into Markdown format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "---")


;;;; Italic

(defun org-md-italic (italic contents info)
  "Transcode ITALIC object into Markdown format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "*%s*" contents))


;;;; Item

(defun org-md-item (item contents info)
  "Transcode ITEM element into Markdown format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "-"
		   (concat (number-to-string
			    (car (last (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct)))))
			   "."))))
    (concat bullet
	    (make-string (- 4 (length bullet)) ? )
	    (case (org-element-property :checkbox item)
	      (on "[X] ")
	      (trans "[-] ")
	      (off "[ ] "))
	    (let ((tag (org-element-property :tag item)))
	      (and tag (format "**%s:** "(org-export-data tag info))))
	    (org-trim (replace-regexp-in-string "^" "    " contents)))))


;;;; Line Break

(defun org-md-line-break (line-break contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  ")


;;;; Link

(defun org-md-link (link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((--link-org-files-as-html-maybe
	 (function
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
            ;; needed.  See `org-e-html-link-org-files-as-html'.
	    (cond
	     ((and org-e-html-link-org-files-as-html
		   (string= ".org"
			    (downcase (file-name-extension raw-path "."))))
	      (concat (file-name-sans-extension raw-path) "."
		      (plist-get info :html-extension)))
	     (t raw-path)))))
	(type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
	   (let ((destination (org-export-resolve-id-link link info)))
	     (if (stringp destination)	; External file.
		 (let ((path (funcall --link-org-files-as-html-maybe
				      destination info)))
		   (if (not contents) (format "<%s>" path)
		     (format "[%s](%s)" contents path)))
	       (concat
		(and contents (concat contents " "))
		(format "(%s)"
			(format (org-export-translate "See section %s" :html info)
				(mapconcat 'number-to-string
					   (org-export-get-headline-number
					    destination info)
					   ".")))))))
	  ((org-export-inline-image-p link org-e-html-inline-image-rules)
	   (format "![%s](%s)"
		   (let ((caption
			  (org-element-property
			   :caption (org-export-get-parent-element link))))
		     (when caption (org-export-data (car caption) info)))
		   path))
	  ((string= type "coderef")
	   (let ((ref (org-element-property :path link)))
	     (format (org-export-get-coderef-format ref contents)
		     (org-export-resolve-coderef ref info))))
	  ((equal type "radio")
	   (let ((destination (org-export-resolve-radio-link link info)))
	     (org-export-data (org-element-contents destination) info)))
	  ((equal type "fuzzy")
	   (let ((destination (org-export-resolve-fuzzy-link link info)))
	     ;; Ignore invisible "#+TARGET: path".
	     (unless (eq (org-element-type destination) 'keyword)
	       (if (org-string-nw-p contents) contents
		 (when destination
		   (let ((number (org-export-get-ordinal destination info)))
		     (when number
		       (if (atom number) (number-to-string number)
			 (mapconcat 'number-to-string number ".")))))))))
	  (t (let* ((raw-path (org-element-property :path link))
		    (path (cond
			   ((member type '("http" "https" "ftp"))
			    (concat type ":" raw-path))
			   ((equal type "file")
			    ;; Extract just the file path and strip
			    ;; all other components.
			    (when (string-match "\\(.+\\)::.+" raw-path)
			      (setq raw-path (match-string 1 raw-path)))
			    ;; Treat links to ".org" files as ".html",
			    ;; if needed.
			    (setq raw-path
				  (funcall --link-org-files-as-html-maybe
					   raw-path info))
			    ;; If file path is absolute, prepend it
			    ;; with protocol component - "file://".
			    (if (not (file-name-absolute-p raw-path)) raw-path
			      (concat "file://" (expand-file-name raw-path))))
			   (t raw-path))))
	       (if (not contents) (format "<%s>" path)
		 (format "[%s](%s)" contents path)))))))


;;;; Paragraph

(defun org-md-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain List

(defun org-md-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into Markdown format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain Text

(defun org-md-plain-text (text info)
  "Transcode a TEXT string into Markdown format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Protect `, *, _ and \
  (setq text
	(replace-regexp-in-string
	 "[`*_\\]" (lambda (rep) (concat "\\\\" (match-string 1 rep))) text))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-e-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)


;;;; Quote Block

(defun org-md-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into Markdown format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))


;;;; Section

(defun org-md-section (section contents info)
  "Transcode SECTION element into Markdown format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  ;; Protect ambiguous #.  It isn't handled at the plain-text level
  ;; since it requires a better view of the problem.
  (replace-regexp-in-string
   "^\\(?:[ \t]*>[ >]*\\)?\\(#\\)" "\\\\#" contents nil nil 1))


;;;; Template

(defun org-md-template (contents info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)



;;; Interactive function

;;;###autoload
(defun org-md-export-as-markdown (&optional subtreep visible-only)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outbuf (org-export-to-buffer
		 'md "*Org MD Export*" subtreep visible-only)))
    (with-current-buffer outbuf (text-mode))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf))))


;;;###autoload
(defun org-md-export-to-markdown (&optional subtreep visible-only pub-dir)
  "Export current buffer to a Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep pub-dir)))
    (org-export-to-file 'md outfile subtreep visible-only)))


(provide 'org-md)
;;; org-md.el ends here
