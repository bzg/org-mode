;;; org-babel-init.el --- loads org-babel

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

;; for more information see the comments in org-babel.el

;;; Code:
(require 'cl)
(require 'org)
(require 'org-exp-blocks)
(require 'org-babel)
(require 'org-babel-ref)
(require 'org-babel-exp)
(require 'org-babel-table)
(require 'org-babel-comint)
(require 'org-babel-lob)
(require 'org-babel-tangle)

;; language specific files
(add-to-list 'load-path (expand-file-name "langs" (file-name-directory (or load-file-name buffer-file-name))))
(require 'org-babel-lisp)
(require 'org-babel-sh)

;; Library of babel
(defvar org-babel-lob-dir
  (expand-file-name ".."
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "The directory holding the library-of-bael")
(defun org-babel-load-library-of-babel ()
  (org-babel-lob-ingest (expand-file-name "library-of-babel.org" org-babel-lob-dir)))

(provide 'org-babel-init)
;;; org-babel-init.el ends here
