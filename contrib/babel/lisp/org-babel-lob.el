;;; org-babel-lob.el --- The Library of Babel: off-the-shelf functions for data analysis and plotting using org-babel

;; Copyright (C) 2009 Eric Schulte, Dan Davison

;; Author: Eric Schulte, Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See org-babel.org in the parent directory for more information

;;; Code:
(require 'org-babel)
(require 'org-babel-table)
(require 'org-babel-exp)

(defvar org-babel-library-of-babel nil
  "Library of source-code blocks.  This is an association list.
Populate the library by adding files to `org-babel-lob-files'.")

(defcustom org-babel-lob-files '()
  "Files used to populate the `org-babel-library-of-babel'.  To
add files to this list use the `org-babel-lob-ingest' command."
  :group 'org-babel
  :type 'list)

(defun org-babel-lob-ingest (&optional file)
  "Add all source-blocks defined in FILE to `org-babel-library-of-babel'."
  (interactive "f")
  (org-babel-map-source-blocks file
    (let* ((info (org-babel-get-src-block-info))
	   (source-name (intern (fifth info))))
      (when source-name
        (setq org-babel-library-of-babel
              (cons (cons source-name info)
                    (assq-delete-all source-name org-babel-library-of-babel)))))))

(defconst org-babel-lob-call-aliases '("lob" "call")
  "These can be used interchangeably to call a source block
  function. If you change the value of this variable then your
  files may become unusable by other org-babel users, and vice
  versa.")
  
(defconst org-babel-lob-one-liner-regexp
  (concat "^[ \t]*#\\+\\(?:"
	  (mapconcat #'regexp-quote org-babel-lob-call-aliases "\\|")
	  "\\):[ \t]+\\([^\(\)\n]+\\)\(\\([^\n]*\\)\)[ \t]*\\([^\n]*\\)")
  "Regexp to match calls to predefined source block functions")

;; functions for executing lob one-liners

(defun org-babel-lob-execute-maybe ()
  "Detect if this is context for a org-babel Library Of Babel
src-block and if so then run the appropriate source block from
the Library."
  (interactive)
  (let ((info (org-babel-lob-get-info)))
    (if (first info) (progn (org-babel-lob-execute info) t) nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-lob-execute-maybe)

(defun org-babel-lob-get-info ()
  "Return the function call supplied on the current Library of
Babel line as a string.

This function is analogous to org-babel-get-src-block-name. For
both functions, after they are called, (match-string 1) matches
the function name, and (match-string 2) matches the function
arguments inside the parentheses. I think perhaps these functions
should be renamed to bring out this similarity, perhaps involving
the word 'call'."
  (let ((case-fold-search t))
    (save-excursion
      (move-beginning-of-line 1)
      (if (looking-at org-babel-lob-one-liner-regexp)
          (mapcar #'org-babel-clean-text-properties 
		  (list (format "%s(%s)" (match-string 1) (match-string 2))
			(match-string 3)))))))
  
(defun org-babel-lob-execute (info)
  (let ((params (org-babel-merge-params
		 org-babel-default-header-args
                 (org-babel-params-from-properties)
		 (org-babel-parse-header-arguments
		  (org-babel-clean-text-properties
		   (concat ":var results=" (mapconcat #'identity info " ")))))))
    ;; (message "lob-params=%S" params) ;; debugging
    (org-babel-execute-src-block nil (list "emacs-lisp" "results" params))))

(provide 'org-babel-lob)
;;; org-babel-lob.el ends here
