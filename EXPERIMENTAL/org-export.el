;;; org-export.el --- Export engine for Org
;;
;; Copyright 2008 2010 Bastien Guerry
;;
;; Emacs Lisp Archive Entry
;; Filename: org-export.el
;; Version: 0.3
;; Author: Bastien <bzg AT altern DOT org>
;; Maintainer: Bastien <bzg AT altern DOT org>
;; Keywords:
;; Description:
;; URL: [Not distributed yet]
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; org-export.el implements a new experimental export engine for Org.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-export)
;;
;;; Todo:
;; 
;; Rewrite `org-export-export-preprocess-string'.
;;
;;; Code:

(eval-when-compile
  (require 'cl))

;;; Preparation functions:

(defvar org-export-structure nil)
(defvar org-export-content nil)
(defvar org-export-properties nil)

(defun org-export-set-backend (suffix)
  "Set the backend functions names from SUFFIX."
  (setq org-export-structure
	`((header ,(intern (concat "org-" suffix "-export-header")))
	  (first-lines ,(intern (concat "org-" suffix "-export-first-lines")))
	  (section-beginning ,(intern (concat "org-" suffix "-export-section-beginning")))
	  (heading ,(intern (concat "org-" suffix "-export-heading")))
	  (section-end ,(intern (concat "org-" suffix "-export-section-end")))
	  (footer ,(intern (concat "org-" suffix "-export-footer")))))
  (setq org-export-content
	`((fonts ,(intern (concat "org-" suffix "-export-fonts")))
	  (links ,(intern (concat "org-" suffix "-export-links")))
	  (lists ,(intern (concat "org-" suffix "-export-lists")))
	  (envs ,(intern (concat "org-" suffix "-export-quote-verse-center")))
	  (tables ,(intern (concat "org-" suffix "-export-tables"))))))

;;; Parsing functions:

(defun org-export-parse (&optional level)
  "Recursively parse the current buffer.
If LEVEL is set, do the parsing at that level of sectioning.
Return a nested list containing the structure of the parsed
buffer and information about each section, including its
content."
  (let (output eos)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-complex-heading-regexp nil t)
	(let ((heading (match-string 4))
	      (properties (org-entry-properties)))
	  (save-restriction
	    (narrow-to-region (if (looking-at "\n") (1+ (point)) (point))
			      (save-excursion
				(setq eos (org-end-of-subtree t t))))
	    (setq output
		  (append output
			  (list
			   (list :level (or level 1)
				 :heading heading
				 :properties properties
				 :content (org-export-get-entry-content)
				 :subtree (org-export-parse
					   (if level (1+ level) 2)))))))
	  (goto-char (1- eos)))))
    output))

(defun org-export-get-entry-content ()
  "Extract the content of an entry.
The content of a entry is the part before its first subtree or
the end of the entry."
  (save-excursion
    (goto-char (point-min))
    ;; FIXME The following shouldn't be necessary since we are cleaning
    ;; up the buffer ith org-export-preprocess-string
    (while (or (looking-at org-property-drawer-re)
	       (looking-at org-clock-drawer-re)
	       (looking-at org-keyword-time-regexp))
      (move-beginning-of-line 1))
    (buffer-substring
     (point)
     (if (re-search-forward org-complex-heading-regexp nil t)
	 (match-beginning 0) (point-max)))))

;;; Rendering functions:

(defun org-export-render (&optional filter)
  "Render the current Org buffer and export it.
First parse the buffer and return it as a nested list.  If FILTER
is set, use it to filter this list (see `org-export-filter') then
export the (filtered) list with `org-export-render-structure'."
  (setq org-export-properties 
	(org-combine-plists (org-default-export-plist)
			    (org-infile-export-plist)))
  (let* (first-lines
	 (bstring (buffer-string))
	 (parsed-buffer
	  (with-temp-buffer
	    (org-mode)
	    (insert (apply 'org-export-export-preprocess-string 
			   bstring org-export-properties))
	    (goto-char (point-min))
	    (setq first-lines (org-export-get-entry-content))
	    (org-export-parse))))
    (switch-to-buffer (get-buffer-create "*Org export*"))
    (erase-buffer)
    (funcall (cadr (assoc 'header org-export-structure)))
    (funcall (cadr (assoc 'first-lines org-export-structure)) first-lines)
    (org-export-render-structure parsed-buffer filter)
    (funcall (cadr (assoc 'footer org-export-structure)))))

(defun org-export-render-structure (parsed-buffer &optional filter)
  "Render PARSED-BUFFER.
An optional argument FILTER specifies a filter to pass to the
rendering engine."
  (mapc (lambda(s)
	  (funcall (cadr (assoc 'section-beginning org-export-structure)) s)
	  (funcall (cadr (assoc 'heading org-export-structure)) s)
	  (insert (org-export-render-content s) "\n\n")
	  (org-export-render-structure (plist-get s :subtree) filter)
	  (funcall (cadr (assoc 'section-end org-export-structure)) s))
	(org-export-filter parsed-buffer filter)))

(defun org-export-render-content (section)
  "Render SECTION.  
SECTION is either a string or a property list containing
informations (including content) for a section."
  (with-temp-buffer
    (insert (if (listp section) (plist-get section :content) section))
    (mapc (lambda(e)
	    (goto-char (point-min))
	    (funcall (cadr (assoc e org-export-content))))
	  '(fonts tables lists envs links))
    (buffer-string)))

(defun org-export-filter (parsed-buffer filter)
  "Filter out PARSED-BUFFER with FILTER.
PARSED-BUFFER is a nested list of sections and subsections, as
produced by `org-export-parse'.  FILTER is an alist of rules to
apply to PARSED-BUFFER.  For the syntax of a filter, please check
the docstring of `org-export-latex-filter'."
  ;; FIXME where is org-export-latex-filter
  (delete
   nil
   (mapcar
    (lambda(s)
      (if (delete
	   nil
	   (mapcar
	    (lambda(f)
	      (let ((cnd (car f)) (re (cadr f)) prop-cnd)
		(or (and (eq cnd 'heading)
			 (string-match re (plist-get s :heading)))
		    (and (eq cnd 'content)
			 (string-match re (plist-get s :content)))
		    (and (setq prop-cnd
			       (assoc cnd (plist-get s :properties)))
			 (string-match re (cadr prop-cnd))))))
	    filter))
	  nil  ;; return nil if the section is filtered out
	(progn (plist-put s :subtree
			  (org-export-filter (plist-get s :subtree) filter))
	       s))) ;; return the section if it isn't filtered out
    parsed-buffer)))

;; FIXME This function is a copy of `org-export-preprocess-string' which
;; should be rewritten for this export engine to work okay.
(defun org-export-export-preprocess-string (string &rest parameters)
  "Cleanup STRING so that that the true exported has a more consistent source.
This function takes STRING, which should be a buffer-string of an org-file
to export.  It then creates a temporary buffer where it does its job.
The result is then again returned as a string, and the exporter works
on this string to produce the exported version."
  (interactive)
  (let* ((htmlp (plist-get parameters :for-html))
	 (asciip (plist-get parameters :for-ascii))
	 (latexp (plist-get parameters :for-LaTeX))
	 (docbookp (plist-get parameters :for-docbook))
	 (backend (cond (htmlp 'html)
			(latexp 'latex)
			(asciip 'ascii)
			(docbookp 'docbook)))
	 (archived-trees (plist-get parameters :archived-trees))
	 (inhibit-read-only t)
	 (drawers org-drawers)
	 (outline-regexp "\\*+ ")
	 target-alist rtn)

    (setq org-export-target-aliases nil
	  org-export-preferred-target-alist nil
	  org-export-id-target-alist nil
	  org-export-code-refs nil)

    (with-current-buffer (get-buffer-create " org-mode-tmp")
      (erase-buffer)
      (insert string)
      (setq case-fold-search t)

      (let ((inhibit-read-only t))
	(remove-text-properties (point-min) (point-max)
				'(read-only t)))

      ;; Remove license-to-kill stuff
      ;; The caller marks some stuff for killing, stuff that has been
      ;; used to create the page title, for example.
      (org-export-kill-licensed-text)

      (let ((org-inhibit-startup t)) (org-mode))
      (setq case-fold-search t)
      (org-install-letbind)

      ;; Call the hook
      (run-hooks 'org-export-preprocess-hook)

      ;; Process the macros
      (org-export-preprocess-apply-macros)
      (run-hooks 'org-export-preprocess-after-macros-hook)

      (untabify (point-min) (point-max))

      ;; Handle include files, and call a hook
      (org-export-handle-include-files-recurse)
      (run-hooks 'org-export-preprocess-after-include-files-hook)

      ;; Get rid of archived trees
      (org-export-remove-archived-trees archived-trees)

      ;; Remove comment environment and comment subtrees
      (org-export-remove-comment-blocks-and-subtrees)

      ;; Get rid of excluded trees, and call a hook
      (org-export-handle-export-tags (plist-get parameters :select-tags)
				     (plist-get parameters :exclude-tags))
      (run-hooks 'org-export-preprocess-after-tree-selection-hook)

      ;; Mark end of lists
      (org-export-mark-list-ending backend)

      ;; Handle source code snippets
      ;; (org-export-export-replace-src-segments-and-examples backend)

      ;; Protect short examples marked by a leading colon
      (org-export-protect-colon-examples)

      ;; Normalize footnotes
      (when (plist-get parameters :footnotes)
	(org-footnote-normalize nil t))

      ;; Find all headings and compute the targets for them
      (setq target-alist (org-export-define-heading-targets target-alist))

      (run-hooks 'org-export-preprocess-after-headline-targets-hook)

      ;; Find HTML special classes for headlines
      (org-export-remember-html-container-classes)

      ;; Get rid of drawers
      (org-export-remove-or-extract-drawers
       drawers (plist-get parameters :drawers) backend)

      ;; Get the correct stuff before the first headline
      (when (plist-get parameters :skip-before-1st-heading)
	(goto-char (point-min))
	(when (re-search-forward "^\\(#.*\n\\)?\\*+[ \t]" nil t)
	  (delete-region (point-min) (match-beginning 0))
	  (goto-char (point-min))
	  (insert "\n")))
      (when (plist-get parameters :text)
	(goto-char (point-min))
	(insert (plist-get parameters :text) "\n"))

      ;; Remove todo-keywords before exporting, if the user has requested so
      (org-export-remove-headline-metadata parameters)

      ;; Find targets in comments and move them out of comments,
      ;; but mark them as targets that should be invisible
      (setq target-alist (org-export-handle-invisible-targets target-alist))

      ;; Select and protect backend specific stuff, throw away stuff
      ;; that is specific for other backends
      (run-hooks 'org-export-preprocess-before-selecting-backend-code-hook)
      (org-export-select-backend-specific-text backend)

      ;; Protect quoted subtrees
      (org-export-protect-quoted-subtrees)

      ;; Remove clock lines
      (org-export-remove-clock-lines)

      ;; Protect verbatim elements
      (org-export-protect-verbatim)

      ;; Blockquotes, verse, and center
      (org-export-mark-blockquote-verse-center)
      (run-hooks 'org-export-preprocess-after-blockquote-hook)

      ;; Remove timestamps, if the user has requested so
      (unless (plist-get parameters :timestamps)
	(org-export-remove-timestamps))

      ;; Attach captions to the correct object
      ;; (setq target-alist (org-export-attach-captions-and-attributes
      ;; 			  backend target-alist))

      ;; Find matches for radio targets and turn them into internal links
      (org-export-mark-radio-links)
      (run-hooks 'org-export-preprocess-after-radio-targets-hook)

      ;; Find all links that contain a newline and put them into a single line
      (org-export-concatenate-multiline-links)

      ;; Normalize links: Convert angle and plain links into bracket links
      ;; and expand link abbreviations
      (run-hooks 'org-export-preprocess-before-normalizing-links-hook)
      (org-export-normalize-links)

      ;; Find all internal links.  If they have a fuzzy match (i.e. not
      ;; a *dedicated* target match, let the link  point to the
      ;; corresponding section.
      (org-export-target-internal-links target-alist)

      ;; Find multiline emphasis and put them into single line
      (when (plist-get parameters :emph-multiline)
	(org-export-concatenate-multiline-emphasis))

      ;; Remove special table lines
      (when org-export-table-remove-special-lines
	(org-export-remove-special-table-lines))

      ;; Another hook
      (run-hooks 'org-export-preprocess-before-backend-specifics-hook)

      ;; LaTeX-specific preprocessing
      (when latexp
	(require 'org-latex nil)
	(org-export-latex-preprocess parameters))

      ;; ASCII-specific preprocessing
      (when asciip
	(org-export-ascii-preprocess parameters))

      ;; HTML-specific preprocessing
      (when htmlp
	(org-export-html-preprocess parameters))

      ;; DocBook-specific preprocessing
      (when docbookp
	(require 'org-docbook nil)
	(org-export-docbook-preprocess parameters))

      ;; Remove or replace comments
      (org-export-handle-comments (plist-get parameters :comments))

      ;; Remove #+TBLFM and #+TBLNAME lines
      (org-export-handle-table-metalines)
      
      ;; Run the final hook
      (run-hooks 'org-export-preprocess-final-hook)

      (setq rtn (buffer-string))
      (kill-buffer " org-mode-tmp"))
    rtn))

(provide 'org-export)

;;;  User Options, Variables

;;; org-export.el ends here
