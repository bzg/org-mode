;;; org-e-confluence --- Confluence Wiki Back-End for Org Export Engine

;; Copyright (C) 2012 Sébastien Delafond

;; Author: Sébastien Delafond <sdelafond at gmx dot net>
;; Keywords: outlines, confluence, wiki

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
;;
;; org-confluence.el lets you convert Org files to confluence files using
;; the org-export.el experimental engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'org-confluence)
;;
;; Export Org files to confluence:
;; M-x org-e-confluence-export-as-confluence RET
;;
;;; Code:

(require 'org-export)
(require 'org-e-ascii)

;; Define the backend itself
(org-export-define-derived-backend e-confluence e-ascii
  :translate-alist ((bold . org-e-confluence-bold)
                    (example-block . org-e-confluence-example-block)
                    (fixed-width . org-e-confluence-fixed-width)
                    (footnote-definition . org-e-confluence-empty)
                    (footnote-reference . org-e-confluence-empty)
                    (headline . org-e-confluence-headline)
                    (italic . org-e-confluence-italic)
                    (link . org-e-confluence-link)
                    (section . org-e-confluence-section)
                    (src-block . org-e-confluence-src-block)
                    (strike-through . org-e-confluence-strike-through)
                    (table . org-e-confluence-table)
                    (table-cell . org-e-confluence-table-cell)
                    (table-row . org-e-confluence-table-row)
                    (template . org-e-confluence-template)
                    (underline . org-e-confluence-underline)))

;; All the functions we use
(defun org-e-confluence-bold (bold contents info)
  (format "*%s*" contents))

(defun org-e-confluence-empty (empy contents info)
  "")

(defun org-e-confluence-example-block (example-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let ((content (org-export-format-code-default example-block info)))
    (org-e-confluence--block "none" "Confluence" content)))

(defun org-e-confluence-italic (italic contents info)
  (format "_%s_" contents))

(defun org-e-confluence-fixed-width (fixed-width contents info)
  (format "\{\{%s\}\}" contents))

(defun org-e-confluence-headline (headline contents info)
  (let ((low-level-rank (org-export-low-level-p headline info))
        (text (org-export-data (org-element-property :title headline)
                               info))
        (level (org-export-get-relative-level headline info)))
    ;; Else: Standard headline.
    (format "h%s. %s\n%s" level text
            (if (org-string-nw-p contents) contents
              ""))))

(defun org-e-confluence-link (link desc info)
  (let ((raw-link (org-element-property :raw-link link)))
    (concat "["
            (when (org-string-nw-p desc) (format "%s|" desc))
            (cond
             ((string-match "^confluence:" raw-link)
              (replace-regexp-in-string "^confluence:" "" raw-link))
             (t
              raw-link))
            "]")))
(defun org-e-confluence-section (section contents info)
  contents)

(defun org-e-confluence-src-block (src-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let* ((lang (org-element-property :language src-block))
         (language (if (string= lang "sh") "bash" ;; FIXME: provide a mapping of some sort
                     lang))
         (content (org-export-format-code-default src-block info)))
    (org-e-confluence--block language "Emacs" content)))

(defun org-e-confluence-strike-through (strike-through contents info)
  (format "-%s-" contents))

(defun org-e-confluence-table (table contents info)
  contents)

(defun org-e-confluence-table-row  (table-row contents info)
  (concat
   (if (org-string-nw-p contents) (format "|%s" contents)
     "")
   (when (org-export-table-row-ends-header-p table-row info)
     "|")))

(defun org-e-confluence-table-cell  (table-cell contents info)
  (let ((table-row (org-export-get-parent table-cell)))
    (concat
     (when (org-export-table-row-starts-header-p table-row info)
       "|")
     contents "|")))

(defun org-e-confluence-template (contents info)
  (let ((depth (plist-get info :with-toc)))
    (concat (when depth "\{toc\}\n\n") contents)))

(defun org-e-confluence-underline (underline contents info)
  (format "+%s+" contents))

(defun org-e-confluence--block (language theme contents)
  (concat "\{code:theme=" theme
          (when language (format "|language=%s" language))
          "}\n"
          contents
          "\{code\}\n"))

;; main interactive entrypoint
(defun org-e-confluence-export-as-confluence
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org E-Confluence Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org E-Confluence Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (text-mode)
	      (org-export-add-to-stack (current-buffer) 'e-confluence)))
	`(org-export-as 'e-confluence ,subtreep ,visible-only ,body-only
			',ext-plist))
    (let ((outbuf (org-export-to-buffer
		   'e-confluence "*Org E-Confluence Export*"
		   subtreep visible-only body-only ext-plist)))
      (with-current-buffer outbuf (text-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

(provide 'org-e-confluence)
