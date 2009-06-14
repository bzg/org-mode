;;; org-babel-lob.el --- The Library of Babel: off-the-shelf functions for data analysis and plotting using org-babel

;; Copyright (C) 2009 Dan Davison, Eric Schulte

;; Author: Dan Davison, Eric Schulte
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

(org-babel-add-interpreter "babel")

(setq org-babel-library-of-babel
      (progn (set-buffer
	      (find-file-noselect "../library-of-babel.org"))
	     (org-babel-get-all-src-block-infos)))

(defun org-babel-execute:babel (body params)
  "Execute a library-of-babel block.

  These blocks do not have their own body. Instead they use
  a :srcname header argument to reference a different source
  block, whose body they use. Source blocks in the library of
  babel should use a standard naming scheme for the variable
  containing the input data for analysis / plotting. E.g. if that
  variable is always called __data__ then one of these bodyless
  babel blocks will call a library of babel block using :var
  __data__=<some reference>. The header args from a babel block
  are appended to the header args from the target block.

  This function is called by `org-babel-execute-src-block'."
  (message "executing babel source code block...")
  (save-window-excursion
    (let* ((srcname (cdr (assoc :srcname params)))
	   (info (or (save-excursion
		       (goto-char (org-babel-find-named-block srcname))
		       (org-babel-get-src-block-info))
		     (gethash srcname org-babel-library-of-babel))))
      (org-babel-execute-src-block nil info params))))

;; alternate 1-liner syntax, this uses `seb' from org-babel-table.el
(require 'org-babel-table)

(defvar org-babel-lob-one-liner-regexp
  "#\\+lob:\\([^ \t\n\r]+\\)\\([ \t]+\\([^\n]+\\)\\)?\n")

(defun org-babel-lob-execute-maybe ()
  "Detect if this is context for a org-babel Library Of Babel
src-block and if so then run the appropriate source block from
the Library."
  (interactive)
  (let ((info (org-babel-lob-get-info)))
    (if info (progn (org-babel-lob-execute info) t) nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-lob-execute-maybe)

(defun org-babel-lob-get-info ()
  "Return the information of the current Library of Babel line as
a list of the following form.

  (source-block-name header-arguments-alist)"
  (let ((case-fold-search t))
    (save-excursion
      (move-beginning-of-line 1)
      (if (looking-at org-babel-lob-one-liner-regexp)
          (cons (org-babel-clean-text-properties (match-string 1))
                (delq nil (mapcar (lambda (assignment)
                                    (save-match-data
                                      (if (string-match "\\(.+\\)=\\(.+\\)" assignment)
                                          (list (org-babel-clean-text-properties (match-string 1 assignment))
                                                (org-babel-clean-text-properties (match-string 2 assignment)))
                                        nil)))
                                  (split-string (match-string 3)))))))))

(defun org-babel-lob-execute (info)
  (let ((params (org-babel-parse-header-arguments
                       (concat ":var results="
                               (car info)
                               "("
                               (mapconcat (lambda (var-spec)
                                            (format "%s=%s" (first var-spec) (second var-spec)))
                                          (cdr info) ", ")
                               ")"))))
    (message "params=%S" params)
    (org-babel-execute-src-block t (list "emacs-lisp" "results" params))))

(provide 'org-babel-lob)
