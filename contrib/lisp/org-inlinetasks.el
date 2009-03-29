;;; org-inlinetask.el --- Tasks outside the outline hierarchy
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.01
;;
;; This file is not yet part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;

This module implements inline tasks in Org-mode.  Inline tasks are
tasks that have all the properties of normal outline nodes, including
the ability to store meta data like scheduling dates, TODO state, tags
and properties.  However, these nodes are treated specially by the
visibility cycling commands and by the export commands.

Visibility cycling exempts these nodes from cycling, so whenever their
parent is opened, so are these tasks.

Export commands do not treat the tasks as part of the sectioning
structure, but as a spe

;;; Code

(defcustom org-inlinetask-export 'arrow+content
  "What should be done with inlinetasks upon export?
Possible values:

nil            Remove entirely
arrow          Insert arrow and headline
arrow+content  Insert arrow and headline, add content like example
example        Turn headline and content into example"
  :group 'org-export-general
  :type 'boolean)

(defun org-inlinetask-export-handler ()
  "Handle headlines with level larger than `org-cycle-max-level'.
Either remove headline and meta data, or do special formatting."
  (goto-char (point-min))
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-cycle-max-level 1000)))
		   (or org-cycle-max-level 1000)))
	 (re1 (format "^\\(\\*\\{%d,\\}\\) .*\n" nstars))
	 (re2 (concat "^[ \t]*" org-keyword-time-regexp))
	 headline beg end stars content)
    (while (re-search-forward re1 nil t)
      (setq headline (match-string 0)
	    stars (match-string 1)
	    content nil)
      (replace-match "")
      (while (looking-at re2)
	(delete-region (point) (1+ (point-at-eol))))
      (while (looking-at org-drawer-regexp)
	(setq beg (point))
	(if (re-search-forward org-property-end-re nil t)
	    (delete-region beg (1+ (match-end 0)))))
      (setq beg (point))
      (when (and (re-search-forward "^\\(\\*+\\) " nil t)
		 (= (length (match-string 1)) (length stars))
		 (progn (goto-char (match-end 0))
			(looking-at "END[ \t]*$")))
	(setq content (buffer-substring beg (1- (point-at-bol))))
	(delete-region beg (1+ (match-end 0))))
      (when (and org-inlinetask-export
		 (string-match org-complex-heading-regexp headline))
	(when (memq org-inlinetask-export '(arrow+content arrow))
	  (insert "\n\n\\Rightarrow *"
		  (if (match-end 2) (concat (match-string 2 headline) " ") "")
		  (match-string 4 headline) "*\n"))
	(when (eq org-inlinetask-export 'arrow+content)
	  (insert "#+BEGIN_EXAMPLE\n" content "\n#+END_EXAMPLE\n"))
	(insert "\n")))))

(defun org-inlinetask-fontify (limit)
  "Fontify the inline tasks."
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-cycle-max-level 1000)))
		   (or org-cycle-max-level 1000)))
	 (re (concat "^\\(\\*\\)\\(\\*\\{"
		    (format "%d" (- nstars 2))
		    ",\\}\\)\\(\\*\\* .*\\)")))
    (while (re-search-forward re limit t)
      (add-text-properties (match-beginning 1) (match-end 1)
			   '(face org-warning font-lock-fontified t))
      (add-text-properties (match-beginning 2) (match-end 2)
			   '(face org-hide font-lock-fontified t))
      (add-text-properties (match-beginning 3) (match-end 3)
			   '(face shadow font-lock-fontified t)))))

(eval-after-load "org-exp"
  '(add-hook 'org-export-preprocess-after-tree-selection-hook
	     'org-inlinetask-export-handler))
(eval-after-load "org"
  '(add-hook 'org-font-lock-hook 'org-inlinetask-fontify))

(provide 'org-inlinetask)

;;; org-inlinetask.el ends here

