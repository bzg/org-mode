;;; org-ascii.el --- ASCII export for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.26d
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

(require 'org-exp)

(defgroup org-export-ascii nil
  "Options specific for ASCII export of Org-mode files."
  :tag "Org Export ASCII"
  :group 'org-export)

(defcustom org-export-ascii-underline '(?\$ ?\# ?^ ?\~ ?\= ?\-)
  "Characters for underlining headings in ASCII export.
In the given sequence, these characters will be used for level 1, 2, ..."
  :group 'org-export-ascii
  :type '(repeat character))

(defcustom org-export-ascii-bullets '(?* ?+ ?-)
  "Bullet characters for headlines converted to lists in ASCII export.
The first character is used for the first lest level generated in this
way, and so on.  If there are more levels than characters given here,
the list will be repeated.
Note that plain lists will keep the same bullets as the have in the
Org-mode file."
  :group 'org-export-ascii
  :type '(repeat character))

(defcustom org-export-ascii-links-to-notes t
  "Non-nil means, convert links to notes before the next headline.
When nil, the link will be exported in place.  If the line becomes long
in this way, it will be wrapped."
  :group 'org-export-ascii
  :type 'boolean)


;;; ASCII export

(defvar org-ascii-current-indentation nil) ; For communication

;;;###autoload
(defun org-export-as-ascii (arg)
  "Export the outline as a pretty ASCII file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines.  The default is 3."
  (interactive "P")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					(org-infile-export-plist)))
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (when region-p
	    (save-excursion
	      (goto-char rbeg)
	      (and (org-at-heading-p)
		   (>= (org-end-of-subtree t t) rend)))))
	 (level-offset (if subtree-p
			   (save-excursion
			     (goto-char rbeg)
			     (+ (funcall outline-level)
				(if org-odd-levels-only 1 0)))
			 0))
	 (opt-plist (setq org-export-opt-plist
			  (if subtree-p
			      (org-export-add-subtree-options opt-plist rbeg)
			    opt-plist)))
	 (custom-times org-display-custom-times)
	 (org-ascii-current-indentation '(0 . 0))
	 (level 0) line txt
	 (umax nil)
	 (umax-toc nil)
	 (case-fold-search nil)
	 (bfname (buffer-file-name (or (buffer-base-buffer) (current-buffer))))
	 (filename (concat (file-name-as-directory
			    (org-export-directory :ascii opt-plist))
			   (file-name-sans-extension
			    (or (and subtree-p
				     (org-entry-get (region-beginning)
						    "EXPORT_FILE_NAME" t))
				(file-name-nondirectory bfname)))
			   ".txt"))
	 (filename (if (equal (file-truename filename)
			      (file-truename bfname))
		       (concat filename ".txt")
		     filename))
	 (buffer (find-file-noselect filename))
	 (org-levels-open (make-vector org-level-max nil))
	 (odd org-odd-levels-only)
	 (date  (plist-get opt-plist :date))
	 (author      (plist-get opt-plist :author))
	 (title       (or (and subtree-p (org-export-get-title-from-subtree))
			  (plist-get opt-plist :title)
			  (and (not
				(plist-get opt-plist :skip-before-1st-heading))
			       (org-export-grab-title-from-buffer))
			  (file-name-sans-extension
			   (file-name-nondirectory bfname))))
	 (email       (plist-get opt-plist :email))
	 (language    (plist-get opt-plist :language))
	 (quote-re0   (concat "^[ \t]*" org-quote-string "\\>"))
;	 (quote-re    (concat "^\\(\\*+\\)\\([ \t]*" org-quote-string "\\>\\)"))
	 (todo nil)
	 (lang-words nil)
	 (region
	  (buffer-substring
	   (if (org-region-active-p) (region-beginning) (point-min))
	   (if (org-region-active-p) (region-end) (point-max))))
	 (lines (org-split-string
		 (org-export-preprocess-string
		  region
		  :for-ascii t
		  :skip-before-1st-heading
		  (plist-get opt-plist :skip-before-1st-heading)
		  :drawers (plist-get opt-plist :drawers)
		  :tags (plist-get opt-plist :tags)
		  :priority (plist-get opt-plist :priority)
		  :footnotes (plist-get opt-plist :footnotes)
		  :timestamps (plist-get opt-plist :timestamps)
		  :todo-keywords (plist-get opt-plist :todo-keywords)
		  :verbatim-multiline t
		  :select-tags (plist-get opt-plist :select-tags)
		  :exclude-tags (plist-get opt-plist :exclude-tags)
		  :archived-trees
		  (plist-get opt-plist :archived-trees)
		  :add-text (plist-get opt-plist :text))
		 "\n"))
	 thetoc have-headings first-heading-pos
	 table-open table-buffer link-buffer link desc desc0 rpl wrap)
    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (setq org-min-level (org-get-min-level lines level-offset))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    (find-file-noselect filename)

    (setq lang-words (or (assoc language org-export-language-setup)
			 (assoc "en" org-export-language-setup)))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (fundamental-mode)
    ;; create local variables for all options, to make sure all called
    ;; functions get the correct information
    (mapc (lambda (x)
	    (set (make-local-variable (nth 2 x))
		 (plist-get opt-plist (car x))))
	  org-export-plist-vars)
    (org-set-local 'org-odd-levels-only odd)
    (setq umax (if arg (prefix-numeric-value arg)
		 org-export-headline-levels))
    (setq umax-toc (if (integerp org-export-with-toc)
		       (min org-export-with-toc umax)
		     umax))

    ;; File header
    (if title (org-insert-centered title ?=))
    (insert "\n")
    (if (and (or author email)
	     org-export-author-info)
	(insert (concat (nth 1 lang-words) ": " (or author "")
			(if email (concat " <" email ">") "")
			"\n")))

    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date)))
     (date)
     (t (setq date (format-time-string "%Y-%m-%d %T %Z"))))

    (if (and date org-export-time-stamp-file)
	(insert (concat (nth 2 lang-words) ": " date"\n")))

    (insert "\n\n")

    (if org-export-with-toc
	(progn
	  (push (concat (nth 3 lang-words) "\n") thetoc)
	  (push (concat (make-string (string-width (nth 3 lang-words)) ?=)
			"\n") thetoc)
	  (mapc '(lambda (line)
		   (if (string-match org-todo-line-regexp
				     line)
		       ;; This is a headline
		       (progn
			 (setq have-headings t)
			 (setq level (- (match-end 1) (match-beginning 1)
					level-offset)
			       level (org-tr-level level)
			       txt (match-string 3 line)
			       todo
			       (or (and org-export-mark-todo-in-toc
					(match-beginning 2)
					(not (member (match-string 2 line)
						     org-done-keywords)))
					; TODO, not DONE
				   (and org-export-mark-todo-in-toc
					(= level umax-toc)
					(org-search-todo-below
					 line lines level))))
			 (setq txt (org-html-expand-for-ascii txt))

			 (while (string-match org-bracket-link-regexp txt)
			   (setq txt
				 (replace-match
				  (match-string (if (match-end 2) 3 1) txt)
				  t t txt)))

			 (if (and (memq org-export-with-tags '(not-in-toc nil))
				  (string-match
				   (org-re "[ \t]+:[[:alnum:]_@:]+:[ \t]*$")
				   txt))
			     (setq txt (replace-match "" t t txt)))
			 (if (string-match quote-re0 txt)
			     (setq txt (replace-match "" t t txt)))

			 (if org-export-with-section-numbers
			     (setq txt (concat (org-section-number level)
					       " " txt)))
			 (if (<= level umax-toc)
			     (progn
			       (push
				(concat
				 (make-string
				  (* (max 0 (- level org-min-level)) 4) ?\ )
				 (format (if todo "%s (*)\n" "%s\n") txt))
				thetoc)
			       (setq org-last-level level))
			   ))))
		lines)
	  (setq thetoc (if have-headings (nreverse thetoc) nil))))

    (org-init-section-numbers)
    (while (setq line (pop lines))
      (when (and link-buffer (string-match "^\\*+ " line))
	(org-export-ascii-push-links (nreverse link-buffer))
	(setq link-buffer nil))
      (setq wrap nil)
      ;; Remove the quoted HTML tags.
      (setq line (org-html-expand-for-ascii line))
      ;; Replace links with the description when possible
      (while (string-match org-bracket-link-regexp line)
	(setq link (match-string 1 line)
	      desc0 (match-string 3 line)
	      desc (or desc0 (match-string 1 line)))
	(if (and (> (length link) 8)
		 (equal (substring link 0 8) "coderef:"))
	    (setq line (replace-match
			(format (org-export-get-coderef-format (substring link 8) desc)
				(cdr (assoc
				      (substring link 8)
				      org-export-code-refs)))
			t t line))
	  (setq rpl (concat "[" 
			    (or (match-string 3 line) (match-string 1 line))
			    "]"))
	  (when (and desc0 (not (equal desc0 link)))
	    (if org-export-ascii-links-to-notes
		(push (cons desc0 link) link-buffer)
	      (setq rpl (concat rpl " (" link ")")
		    wrap (+ (length line) (- (length (match-string 0)))
			    (length desc)))))
	  (setq line (replace-match rpl t t line))))
      (when custom-times
	(setq line (org-translate-time line)))
      (cond
       ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	;; a Headline
	(setq first-heading-pos (or first-heading-pos (point)))
	(setq level (org-tr-level (- (match-end 1) (match-beginning 1)
				     level-offset))
	      txt (match-string 2 line))
	(org-ascii-level-start level txt umax lines))

       ((and org-export-with-tables
	     (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	(if (not table-open)
	    ;; New table starts
	    (setq table-open t table-buffer nil))
	;; Accumulate lines
	(setq table-buffer (cons line table-buffer))
	(when (or (not lines)
		  (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
				     (car lines))))
	  (setq table-open nil
		table-buffer (nreverse table-buffer))
	  (insert (mapconcat
		   (lambda (x)
		     (org-fix-indentation x org-ascii-current-indentation))
		   (org-format-table-ascii table-buffer)
		   "\n") "\n")))
       (t
	(if (string-match "^\\([ \t]*\\)\\([-+*][ \t]+\\)\\(.*?\\)\\( ::\\)" line)
	    (setq line (replace-match "\\1\\3:" t nil line)))
	(setq line (org-fix-indentation line org-ascii-current-indentation))
	;; Remove forced line breaks
	(if (string-match "\\\\\\\\[ \t]*$" line)
	    (setq line (replace-match "" t t line)))
	(if (and org-export-with-fixed-width
		 (string-match "^\\([ \t]*\\)\\(:\\( \\|$\\)\\)" line))
	    (setq line (replace-match "\\1" nil nil line))
	  (if wrap (setq line (org-export-ascii-wrap line wrap))))
	(insert line "\n"))))

    (org-export-ascii-push-links (nreverse link-buffer))

    (normal-mode)

    ;; insert the table of contents
    (when thetoc
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\[TABLE-OF-CONTENTS\\][ \t]*$" nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (replace-match ""))
	(goto-char first-heading-pos))
      (mapc 'insert thetoc)
      (or (looking-at "[ \t]*\n[ \t]*\n")
	  (insert "\n\n")))

    ;; Convert whitespace place holders
    (goto-char (point-min))
    (let (beg end)
      (while (setq beg (next-single-property-change (point) 'org-whitespace))
	(setq end (next-single-property-change beg 'org-whitespace))
	(goto-char beg)
	(delete-region beg end)
	(insert (make-string (- end beg) ?\ ))))

    (save-buffer)
    ;; remove display and invisible chars
    (let (beg end)
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'display))
	(setq end (next-single-property-change beg 'display))
	(delete-region beg end)
	(goto-char beg)
	(insert "=>"))
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'org-cwidth))
	(setq end (next-single-property-change beg 'org-cwidth))
	(delete-region beg end)
	(goto-char beg)))
    (goto-char (point-min))))

(defun org-export-ascii-preprocess (parameters)
  "Do extra work for ASCII export"
  ;; Put quotes around verbatim text
  (goto-char (point-min))
  (while (re-search-forward org-verbatim-re nil t)
    (goto-char (match-end 2))
    (backward-delete-char 1) (insert "'")
    (goto-char (match-beginning 2))
    (delete-char 1) (insert "`")
    (goto-char (match-end 2)))
  ;; Remove target markers
  (goto-char (point-min))
  (while (re-search-forward  "<<<?\\([^<>]*\\)>>>?\\([ \t]*\\)" nil t)
    (replace-match "\\1\\2")))

(defun org-html-expand-for-ascii (line)
  "Handle quoted HTML for ASCII export."
  (if org-export-html-expand
      (while (string-match "@<[^<>\n]*>" line)
	;; We just remove the tags for now.
	(setq line (replace-match "" nil nil line))))
  line)

(defun org-export-ascii-wrap (line where)
  "Wrap LINE at or before WHERE."
  (let ((ind (org-get-indentation line))
	pos)
    (catch 'found
      (loop for i from where downto (/ where 2) do
	    (and (equal (aref line i) ?\ )
		 (setq pos i)
		 (throw 'found t))))
    (if pos
	(concat (substring line 0 pos) "\n"
		(make-string ind ?\ )
		(substring line (1+ pos)))
      line)))
			   
(defun org-export-ascii-push-links (link-buffer)
  "Push out links in the buffer."
  (when link-buffer
    ;; We still have links to push out.
    (insert "\n")
    (let ((ind ""))
      (save-match-data
	(if (save-excursion
	      (re-search-backward
	       "^\\(\\([ \t]*\\)\\|\\(\\*+ \\)\\)[^ \t\n]" nil t))
	    (setq ind (or (match-string 2)
			  (make-string (length (match-string 3)) ?\ )))))
      (mapc (lambda (x) (insert ind "[" (car x) "]: " (cdr x) "\n"))
	    link-buffer))
    (insert "\n")))

(defun org-ascii-level-start (level title umax &optional lines)
  "Insert a new level in ASCII export."
  (let (char (n (- level umax 1)) (ind 0))
    (if (> level umax)
	(progn
	  (insert (make-string (* 2 n) ?\ )
		  (char-to-string (nth (% n (length org-export-ascii-bullets))
				       org-export-ascii-bullets))
		  " " title "\n")
	  ;; find the indentation of the next non-empty line
	  (catch 'stop
	    (while lines
	      (if (string-match "^\\* " (car lines)) (throw 'stop nil))
	      (if (string-match "^\\([ \t]*\\)\\S-" (car lines))
		  (throw 'stop (setq ind (org-get-indentation (car lines)))))
	      (pop lines)))
	  (setq org-ascii-current-indentation (cons (* 2 (1+ n)) ind)))
      (if (or (not (equal (char-before) ?\n))
	      (not (equal (char-before (1- (point))) ?\n)))
	  (insert "\n"))
      (setq char (nth (- umax level) (reverse org-export-ascii-underline)))
      (unless org-export-with-tags
	(if (string-match (org-re "[ \t]+\\(:[[:alnum:]_@:]+:\\)[ \t]*$") title)
	    (setq title (replace-match "" t t title))))
      (if org-export-with-section-numbers
	  (setq title (concat (org-section-number level) " " title)))
      (insert title "\n" (make-string (string-width title) char) "\n")
      (setq org-ascii-current-indentation '(0 . 0)))))

(defun org-insert-centered (s &optional underline)
  "Insert the string S centered and underline it with character UNDERLINE."
  (let ((ind (max (/ (- fill-column (string-width s)) 2) 0)))
    (insert (make-string ind ?\ ) s "\n")
    (if underline
	(insert (make-string ind ?\ )
		(make-string (string-width s) underline)
		"\n"))))

(defvar org-table-colgroup-info nil)
(defun org-format-table-ascii (lines)
  "Format a table for ascii export."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (not (string-match "^[ \t]*|" (car lines)))
      ;; Table made by table.el - test for spanning
      lines

    ;; A normal org table
    ;; Get rid of hlines at beginning and end
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (when org-export-table-remove-special-lines
      ;; Check if the table has a marking column.  If yes remove the
      ;; column and the special lines
      (setq lines (org-table-clean-before-export lines)))
    ;; Get rid of the vertical lines except for grouping
    (let ((vl (org-colgroup-info-to-vline-list org-table-colgroup-info))
	  rtn line vl1 start)
      (while (setq line (pop lines))
	(if (string-match org-table-hline-regexp line)
	    (and (string-match "|\\(.*\\)|" line)
		 (setq line (replace-match " \\1" t nil line)))
	  (setq start 0 vl1 vl)
	  (while (string-match "|" line start)
	    (setq start (match-end 0))
	    (or (pop vl1) (setq line (replace-match " " t t line)))))
	(push line rtn))
      (nreverse rtn))))

(defun org-colgroup-info-to-vline-list (info)
  (let (vl new last)
    (while info
      (setq last new new (pop info))
      (if (or (memq last '(:end :startend))
	      (memq new  '(:start :startend)))
	  (push t vl)
	(push nil vl)))
    (setq vl (nreverse vl))
    (and vl (setcar vl nil))
    vl))

(provide 'org-ascii)

;; arch-tag: aa96f882-f477-4e13-86f5-70d43e7adf3c

;;; org-ascii.el ends here
