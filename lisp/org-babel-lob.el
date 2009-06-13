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
(require 'org)

(defvar org-babel-lob-regexp
  (concat "#\\+babel[ \t]*"
	  "\\([ \t]+\\([^\n]+\\)\\)?\n") ;; match header arguments
  "Regexp used to test when on a babel library call line")

(defvar org-babel-lob-inline-regexp nil
  "Regexp used to test whether at an inline babel library call")

(defun org-babel-lob-execute-maybe ()
  "Detect if this is a babel library line and if so
then run `org-babel-lob-execute'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-execute-src-block current-prefix-arg info) t) nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-lob-call)

(defun org-babel-lob-execute ()
  "Execute an org-babel library function."
  (interactive))
  
(defun org-babel-lob-get-src-block-info ()
  "This is taken from `org-babel-get-src-block-info'. Maybe we could abstract and unify.

Return the information of the current source block as a list
of the following form.  (language body header-arguments-alist)"
  (let ((case-fold-search t) head)
    (if (setq head (org-babel-lob-where-is-block-head))
        (save-excursion (goto-char head) (org-babel-lob-parse-lob-line-match))
      (if (save-excursion ;; inline source block
            (re-search-backward "[ \f\t\n\r\v]" nil t)
            (forward-char 1)
            (looking-at org-babel-lob-inline-regexp))
          (org-babel-parse-inline-src-block-match)
        nil)))) ;; indicate that no source block was found

(defun org-babel-lob-parse-lob-line-match ()
  (list nil ;; no language
        nil ;; no head
        (org-combine-plists
	 org-babel-default-header-args
	 (org-babel-parse-header-arguments
	  (org-babel-clean-text-properties
	   (or (match-string 3) ""))))))

(defun org-babel-lob-where-is-block-head ()
  "Return point at beginning of #+babel line."
  (save-excursion
    (beginning-of-line 1)
    (and (looking-at org-babel-lob-regexp)
	 (point))))



(provide 'org-babel-lob)
