;;; org-export.el --- Export engine for Org
;;
;; Copyright 2008 Bastien Guerry
;;
;; Emacs Lisp Archive Entry
;; Filename: org-export.el
;; Version: 0.1a
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
;; This is the export engine for Org.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-export)
;;
;;; Code:

(eval-when-compile
  (require 'cl))

;;; Parsing functions:

(defun org-export-parse (&optional level)
  "Parse the current buffer.
Return a nested list reflecting the sectioning structure of the
file and containing all information about each section, including
its content."
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
				 :content (org-export-parse-clean-content-string
					   (org-export-parse-content))
				 :subtree (org-export-parse 
					   (if level (1+ level) 2)))))))
	  (goto-char (1- eos)))))
    output))

(defun org-export-parse-content ()
  "Extract the content of a section.
The content of a section is the part before a subtree."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring
     (point)
     (if (re-search-forward org-complex-heading-regexp nil t)
	 (match-beginning 0) (point-max)))))

(defun org-export-parse-clean-content-string (s)
  "From the content string S, remove stuff also captured by get-properties.
So this will remove the clock drawer, the property drawer, and the lines
with planning info (DEADLINE, SCHEDULED, CLOSED)."
  (if (string-match org-property-drawer-re s)
      (setq s (replace-match "" t t s)))
  (if (string-match org-clock-drawer-re s)
      (setq s (replace-match "" t t s)))
  (while (string-match (concat org-keyword-time-regexp ".*\n?") s)
    (setq s (replace-match "" t t s)))
  s)

;;; Rendering functions:

(defun org-export-buffer (filter struct-backend content-backend)
  "Render the current buffer.
It first parses the current buffer into a list.  Then it filters
this list with FILTER.  Finally it uses STRUCT-BACKEND and
CONTENT-BACKEND to render the structure of the buffer and the
content of each section."
  (save-excursion
    (let* ((props (org-combine-plists 
		   (org-default-export-plist)
		   (org-infile-export-plist)))
	   (first-lines (org-export-parse-content))
	   (parsed-buffer (org-export-parse)))
      (switch-to-buffer (get-buffer-create "*Org export*"))
      (erase-buffer)
      (funcall (cdr (assoc 'header struct-backend)) props)
      (funcall (cdr (assoc 'first-lines struct-backend)) 
	       first-lines props)
      (org-export-render-structure parsed-buffer props filter
				   struct-backend content-backend)
      (funcall (cdr (assoc 'footer struct-backend)) props))))

(defun org-export-render-structure
  (parsed-buffer props filter struct-backend content-backend)
  "Render PARSED-BUFFER.
The optional argument FILTER specifies a filter to pass to the
rendering engine."
  (mapc (lambda(s)
	  (funcall (cdr (assoc 'section-beginning struct-backend)) s)
	  (funcall (cdr (assoc 'heading struct-backend)) s)
	  (insert (org-export-render-content s props content-backend) "\n\n")
	  (org-export-render-structure (plist-get s :subtree) props
				       filter struct-backend content-backend)
	  (funcall (cdr (assoc 'section-end struct-backend)) s))
	(org-export-filter parsed-buffer filter)))

(defun org-export-render-content (section props content-backend)
  "Render SECTION with PROPS.  SECTION is the property list
defining the information for the section.  PROPS is the property
list defining information for the current export.
CONTENT-BACKEND is an association list defining possible
exporting directive the content of this section."
  (with-temp-buffer
    (insert (plist-get section :content))
    (if (not (plist-get props :with-comment))
	(funcall (cdr (assoc 'comment content-backend))))
    (buffer-string)))

(defun org-export-strip-drawer ()
  "Strip DRAWERS in the current buffer.
Stripped drawers are those matched by `org-drawer-regexp'."
  (save-excursion 
    (while (re-search-forward org-drawer-regexp nil t)
      (let ((beg (match-beginning 0))
	    (end (and (search-forward ":END:" nil t)
		      (match-end 0))))
	(delete-region beg end)))))

;;; Filtering functions:

(defun org-export-filter (parsed-buffer filter)
  "Filter out PARSED-BUFFER with FILTER.
PARSED-BUFFER is a nested list a sections and subsections, as
produced by `org-export-parse'.  FILTER is an alist of rules to
apply to PARSED-BUFFER.  For the syntax of a filter, please check
the docstring of `org-export-latex-filter'."
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
