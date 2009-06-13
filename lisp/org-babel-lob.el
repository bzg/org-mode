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
  "Regexp used to test when on a babel library line")

(defun org-babel-lob-execute-maybe ()
  "Detect if this is a babel library line and if so
then run `org-babel-lob-execute'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-execute-src-block current-prefix-arg info) t) nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-lob-call)

(defun org-babel-lob-execute ()
  "Execute an org-babel library function."
  (interactive)
  


(provide 'org-babel-lob)
