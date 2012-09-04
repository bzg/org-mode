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

;;; Define Back-End

(require 'org-e-html)

(setq org-e-freemind-toplevel-hlevel 1)

(defcustom org-e-freemind-node-styles
  '((0 . (:node-attrs
	  "COLOR=\"#000000\""
	  :node-elements
	  "<font NAME=\"SansSerif\" SIZE=\"20\"/>"))
    (1 . (:node-attrs
	  "COLOR=\"#0033ff\""
	  :node-elements
	  "<edge STYLE=\"sharp_bezier\" WIDTH=\"8\"/>
           <font NAME=\"SansSerif\" SIZE=\"18\"/>"))

    (2 . (:node-attrs
	  "COLOR=\"#00b439\""
	  :node-elements
	  "<edge STYLE=\"bezier\" WIDTH=\"thin\"/>
           <font NAME=\"SansSerif\" SIZE=\"16\"/>"))
    (3 . (:node-attrs
	  "COLOR=\"#990000\""
	  :node-elements
	  "<font NAME=\"SansSerif\" SIZE=\"14\"/>"))
    (default . (:node-attrs
    		"COLOR=\"#111111\""
    		:node-elements
    		nil)))
  "Styles to apply to node.
NOT READY YET."
  :type '(alist :options (0 1 2 3 default)
                :key-type (choice :tag "Node selector"
				  (integer :tag "Outline level")
				  (const :tag "Default value" default)
				  (string :tag "Node style"))
                :value-type (plist :options (:node-attrs :node-elements)
				   :value-type
				   (choice
				    (string :tag "Value")
				    (const :tag "None" nil))))
  :group 'org-freemind)


(org-export-define-derived-backend e-freemind e-html
  :export-block "FREEMIND"
  :options-alist nil
  :translate-alist ((headline . org-e-freemind-headline)
		    (template . org-e-freemind-template)
		    (section . org-e-freemind-section)))



;;; Transcoders


;;;; Template

(defun org-e-freemind-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."
  (format
   "<map version=\"0.9.0\">\n%s\n</map>"
   (format
    "<node COLOR=\"#407000\" STYLE=\"fork\" POSITION=\"%s\" TEXT=\"%s\">%s</node>"
    (if t "left" "right")
    (org-export-data (plist-get info :title) info)
    (concat
     contents
     "\n<edge COLOR=\"#808080\" STYLE=\"bezier\" WIDTH=\"thin\"/>\n<font NAME=\"SansSerif\" SIZE=\"12\"/>
"))))

;;;; Headline

(defun org-e-freemind-format-headline (todo todo-type priority text tags)
  (let ((todo (org-e-html--todo todo))
	(tags (org-e-html--tags tags)))
    (concat  todo (and todo " ") text
	     (and tags "&nbsp;&nbsp;&nbsp;") tags)))

(defun org-e-freemind-headline (headline contents info)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Empty contents?
  (setq contents (or contents ""))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 (level (org-export-get-relative-level headline info))
	 (text (org-export-data (org-element-property :title headline) info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 (org-export-get-headline-number
					  headline info) ".")))
	 ;; Create the headline text.
	 (full-text (org-e-html-format-headline--wrap headline info))
	 ;; Headline order.
	 (headline-order
	  ;; (car (last (org-export-get-headline-number headline info)))
	  (car (org-export-get-headline-number headline info)))
	 (left-p (zerop (% headline-order 2))))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info) ; FIXME (or (not section-fmt))
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'unordered 'unordered)) ; FIXME
	     (itemized-body (org-e-freemind-format-list-item
			     contents type nil nil full-text)))
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (org-e-freemind-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-e-freemind-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((section-number (mapconcat 'number-to-string
					(org-export-get-headline-number
					 headline info) "-"))
	     (ids (remove 'nil
			  (list (org-element-property :custom-id headline)
				(concat "sec-" section-number)
				(org-element-property :id headline))))
	     (preferred-id (car ids))
	     (extra-ids (cdr ids))
	     (extra-class (org-element-property :html-container-class headline))
	     (level1 level))
	(concat
	 (format "<node %s STYLE=\"bubble\" POSITION=\"%s\" FOLDED=\"%s\" TEXT=\"%s\">%s</node>"
		 (or (plist-get (assoc-default level org-e-freemind-node-styles)
				:node-attrs)
		     (plist-get (assoc-default 'default org-e-freemind-node-styles)
				:node-attrs)
		     "")
		 (if left-p "left" "right")
		 (if (= level 1) "true" "false")
		 full-text
		 (concat
		  contents
		  (or
		   (plist-get (assoc-default level org-e-freemind-node-styles)
			      :node-elements)
		   (plist-get (assoc-default 'default org-e-freemind-node-styles)
			      :node-elements)
		   ""))))

	;; (format "<div id=\"%s\" class=\"%s\">%s%s</div>\n"
	;; 	(format "outline-container-%s"
	;; 		(or (org-element-property :custom-id headline)
	;; 		    section-number))
	;; 	(concat (format "outline-%d" level1) (and extra-class " ")
	;; 		extra-class)
	;; 	(format "\n<h%d id=\"%s\">%s%s</h%d>\n"
	;; 		level1
	;; 		preferred-id
	;; 		(mapconcat
	;; 		 (lambda (x)
	;; 		   (let ((id (org-export-solidify-link-text
	;; 			      (if (org-uuidgen-p x) (concat "ID-" x)
	;; 				x))))
	;; 		     (org-e-freemind--anchor id)))
	;; 		 extra-ids "")
	;; 		full-text
	;; 		level1)
	;; 	contents)
	)))))


(defun org-e-freemind-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ( ;; (type "NOTE")
	(type "NODE"))
    (if (string= (org-trim contents) "") ""

      (format "<node style=\"bubble\" background_color=\"#eeee00\">%s</node>"
	      (format "\n<richcontent TYPE=\"%s\">\n%s\n</richcontent>"
		      type
		      (format "\n<html>\n<head>\n</head>\n%s\n</html>"
			      (format "<body>\n%s\n</body>"
				      contents)))))))

;;; Interactive functions

;;;###autoload
(defun org-e-freemind-export-as-freemind
  (&optional subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org E-FREEMIND Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (let ((outbuf
	 (org-export-to-buffer
	  'e-freemind "*Org E-FREEMIND Export*"
	  subtreep visible-only body-only ext-plist)))
    ;; Set major mode.
    (with-current-buffer outbuf (nxml-mode))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf))))

;;;###autoload
(defun org-e-freemind-export-to-freemind
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
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let* ((extension ".mm")
	 (file (org-export-output-file-name extension subtreep pub-dir))
	 (org-export-coding-system 'utf-8))
    (org-export-to-file
     'e-freemind file subtreep visible-only body-only ext-plist)))

(provide 'org-e-freemind.el)
