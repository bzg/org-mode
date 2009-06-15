;;; org-babel-tangle.el --- Extract source code from org-mode files

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

;; Extract the code from source blocks out into raw source-code files.

;;; Code:
(require 'org-babel)

(defvar org-babel-tangle-langs nil
  "Association list matching source-block languages.  The car of
each element should be a string indicating the source block
language, and the cdr should be a list containing the extension
and shebang(#!) line to use when writing out the language to
file.")

(defun org-bable-tangle ()
  "Extract the bodies of all source code blocks form the current
file into their own source-specific files."
  (interactive)
  (let (by-lang)
    (org-babel-map-source-blocks (buffer-file-name)
      (let ((link (progn (org-store-link nil) (pop org-stored-links)))
            (source-name (intern (org-babel-get-src-block-name)))
            (info (org-babel-get-src-block-info)))
        ;; once collected these can be added to by-lang and then
        ;; dropped out to buffers
        ))))

(provide 'org-babel-tangle)
;;; org-babel-tangle.el ends here
