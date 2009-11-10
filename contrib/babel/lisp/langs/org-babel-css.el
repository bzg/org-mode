;;; org-babel-css.el --- org-babel functions for css evaluation

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
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

;; Since CSS can't be executed, this file exists solely for tangling
;; CSS from org-mode files.

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "css")

(add-to-list 'org-babel-tangle-langs '("css" "css" nil t))

(defun org-babel-execute:css (body params)
  "Execute a block of CSS code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing CSS source code block")
  body)

(defun org-babel-prep-session:css (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "CSS sessions are nonsensical"))

(provide 'org-babel-css)
;;; org-babel-css.el ends here
