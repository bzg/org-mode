;;; org-export.el --- Export engine for Org
;;
;; Copyright 2008-2011 Free Software Foundation, Inc.
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
;; This file is not part of GNU Emacs.
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
;;; Code:

(eval-when-compile
  (require 'cl))

;;; Preparation functions:

;; Currently needed for `org-export-preprocess-string'
(require 'org-exp)

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
	    (insert (apply 'org-export-preprocess-string 
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

(provide 'org-export)

;;;  User Options, Variables

;;; org-export.el ends here
