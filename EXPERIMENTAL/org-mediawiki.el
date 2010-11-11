;;; org-mediawiki.el --- mediawiki backend for org-export.el
;;
;; Copyright 2010 Bastien Guerry
;;
;; Emacs Lisp Archive Entry
;; Filename: org-mediawiki.el
;; Version: 0.3a
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
;; org-mediawiki.el lets you convert Org files to mediawiki files using
;; the org-export.el experimental engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-mediawiki)
;;
;; You also need to fetch Org's git repository and add the EXPERIMENTAL/
;; directory in your load path.
;; 
;; Fetch Org's git repository:
;; 
;; ~$ cd ~/install/git/
;; ~$ git clone git://repo.or.cz/org-mode.git
;;
;; Put this in your .emacs.el:
;; 
;; (add-to-list 'load-path "~/install/git/org-mode/EXPERIMENTAL/")
;;
;; Export Org files to mediawiki: M-x org-mw-export RET
;;
;;; Todo:
;; 
;; - handle radio links
;; - support caption and attributes in tables
;; - better handline of source code and examples
;; - handle inline HTML
;;
;;; Code:

(require 'org-export)

(defvar org-mw-emphasis-alist
  '(("*" "'''%s'''" nil)
    ("/" "''%s''" nil)
    ("_" "<u>%s</u>" nil)
    ("+" "<s>%s</s>" nil)
    ("=" "<tt>%s</tt>" nil))
  "The list of fontification expressions for mediawiki.")

(defvar org-mw-export-table-table-style "")
(defvar org-mw-export-table-header-style "")
(defvar org-mw-export-table-cell-style "")

(defun org-mw-export ()
  "Export the current buffer to Mediawiki."
  (interactive)
  (org-export-set-backend "mw")
  ;; FIXME see the problem `org-mw-export-footnotes'
  ;; (add-hook 'org-export-preprocess-final-hook 'org-mw-export-footnotes)
  (add-hook 'org-export-preprocess-before-backend-specifics-hook
	    'org-mw-export-src-example)
  (org-export-render)
  (remove-hook 'org-export-preprocess-final-hook 'org-mw-export-footnotes)
  (remove-hook 'org-export-preprocess-before-backend-specifics-hook 
	       'org-mw-export-src-example))

(defun org-mw-export-header ()
  "Export the header part."
  (let* ((p org-export-properties)
	 (title (plist-get p :title))
	 (author (plist-get p :author))
	 (date (plist-get p :date)))
    (insert (format "= %s by %s =\n\n" title author))
    (unless (plist-get p :table-of-contents)
      (insert "__NOTOC__\n\n"))))

(defun org-mw-export-first-lines (first-lines)
  "Export first lines."
  (insert (org-export-render-content first-lines) "\n")
  (goto-char (point-max)))

(defun org-mw-export-heading (section-properties)
  "Export mediawiki heading"
  (let* ((p section-properties)
	 (h (plist-get p :heading))
	 (s (make-string (1+ (plist-get p :level)) ?=)))
    (insert (format "%s %s %s\n" s h s))))

(defun org-mw-export-quote-verse-center ()
  "Export #+BEGIN_QUOTE/VERSE/CENTER environments."
  (let (rpl e)
    (while (re-search-forward "^[ \t]*ORG-\\([A-Z]+\\)-\\(START\\|END\\).*$" nil t)
      (setq e (if (equal (match-string 2) "END") "/" "")) 
      (setq rpl 
	    (cond ((equal (match-string 1) "BLOCKQUOTE") "blockquote>")
		  ((equal (match-string 1) "VERSE") "pre>")
		  ((equal (match-string 1) "CENTER") "center>")))
      (replace-match (concat "<" e rpl) t))))

(defun org-mw-export-fonts ()
  "Export fontification."
  (while (re-search-forward org-emph-re nil t)
    (let* ((emph (assoc (match-string 3) org-mw-emphasis-alist))
	   (beg (match-beginning 0))
	   (begs (match-string 1))
	   (end (match-end 0))
	   (ends (match-string 5))
	   (rpl (format (cadr emph) (match-string 4))))
      (delete-region beg end)
      (insert begs rpl ends))))

(defun org-mw-export-links ()
  "Replace Org links with DokiWiki links."
  ;; FIXME: This function could be more clever, of course.
  (while (re-search-forward org-bracket-link-analytic-regexp nil t)
    (cond ((and (equal (match-string 1) "file:")
		(save-match-data
		  (string-match (org-image-file-name-regexp) (match-string 3))))
	   (replace-match 
	    (concat "[[Image:" (file-name-nondirectory (match-string 3)) "]]")))
	  (t 
	   (replace-match 
	    (concat "[\\1\\3" (if (match-string 5) " \\5]" "]")))))))

;; FIXME this function should test whether [1] is really a footnote.
;; `org-footnote-normalize' should add properties to the normalized
;; footnotes so that we can recognize them.
(defun org-mw-export-footnotes ()
  "Export footnotes."
  (goto-char (point-min))
  (let (refpos rpl begnote begfullnote endnote)
    (while (re-search-forward "\[[0-9]+\]" nil t)
	(save-excursion
	  (save-match-data
	    (goto-char (point-max))
	    (search-backward (concat (match-string 0) " ") nil t)
	      (setq begfullnote (match-beginning 0))
	      (setq begnote (match-end 0))
	      (goto-char (match-end 0))
	      (re-search-forward "^\[[0-9]+\]\\|\\'" nil t)
	      (setq endnote (match-beginning 0))
	      (setq rpl (replace-regexp-in-string
			 "\n" " " (buffer-substring endnote begnote)))
	      (setq rpl (replace-regexp-in-string "[ \t]+$" "" rpl))
	      (delete-region begfullnote endnote)))
	(replace-match (concat "<ref>" rpl "</ref>")))))

(defun org-mw-export-src-example ()
  "Export #+BEGIN_EXAMPLE and #+BEGIN_SRC."
  (goto-char (point-min))
  (let (start env)
    (while (re-search-forward "^[ \t]*#\\+BEGIN_\\(EXAMPLE\\|SRC\\).*\n" nil t)
      (setq env (match-string 1))
      (replace-match "")
      (setq start (point))
      (re-search-forward (concat "^[ \t]*#\\+END_" env ".*\n") nil t)
      (replace-match "")
      (string-rectangle start (point) ": ")
      (delete-char 1))))

(defun org-mw-export-lists ()
  "Export lists"
  (while (re-search-forward org-item-beginning-re nil t)
    (move-beginning-of-line 1)
    (org-list-to-mw (org-list-parse-list t))))

(defun org-list-to-mw (list &optional level leveluptype)
  "Convert LIST into a mediawiki list.
LIST is a list returned by `org-list-parse-list'.  A second
optional LEVEL argument defines the level at which the parsing
starts.  An optional third argument LEVELUPTYPE tells what type
of list we are in at LEVEL."
  (let ((lvl (or level 1))
	(ltype (cond ((eq (car list) 'unordered) ?*)
		     ((eq (car list) 'ordered) ?#)
		     ((eq (car list) 'descriptive) ?\;)))
	(luptype leveluptype))
    (dolist (item (cdr list))
      (if (stringp item)
	  (progn
	    (setq item (replace-regexp-in-string "\n[ \t]*" " " item))
	    (when (eq ltype ?\;) 
	      (setq item (replace-regexp-in-string "::" ":" item)))
	    (insert (if luptype (make-string (1- lvl) luptype) "")
		    (char-to-string ltype) " " item "\n"))
	(org-list-to-mw item (1+ lvl) ltype)))))

(defun org-mw-export-tables ()
  "Convert tables in the current buffer to mediawiki tables."
  (while (re-search-forward "^\\([ \t]*\\)|" nil t)
    (org-if-unprotected-at (1- (point))
      (org-table-align)
      (let* ((beg (org-table-begin))
             (end (org-table-end))
             (raw-table (buffer-substring beg end)) lines)
	(setq lines (org-split-string raw-table "\n"))
	(apply 'delete-region (list beg end))
	(when org-export-table-remove-special-lines
	  (setq lines (org-table-clean-before-export lines 'maybe-quoted)))
	(setq lines
	      (mapcar
	       (lambda(elem)
		 (or (and (string-match "[ \t]*|-+" elem) 'hline)
		     (org-split-string (org-trim elem) "|")))
	       lines))
	(insert (orgtbl-to-mw lines nil))))))

(defun orgtbl-to-mw (table params)
  "Convert TABLE into a mediawiki table."
  (let ((params2 (list
		  :tstart (concat "{| class=\"wikitable\" " 
				  org-mw-export-table-table-style)
		  :tend "|}\n"
		  :lstart "|-\n"
		  :lend ""
		  :sep "\n"
		  :fmt (concat "\| " org-mw-export-table-cell-style " | %s")
		  :hfmt (concat "! scope=row " org-mw-export-table-header-style " | %s")
		  :hlsep "\n"
		  )))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

;; Various empty function for org-export.el to work:
(defun org-mw-export-footer () "")
(defun org-mw-export-section-beginning (section-properties) "")
(defun org-mw-export-section-end (section-properties) "")
