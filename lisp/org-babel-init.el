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

;; load utility files
(let ((util-dir (expand-file-name
                 "util"
                 (expand-file-name
                  ".."
                  (file-name-directory (or load-file-name buffer-file-name))))))
  ;; other versions of htmlize can cause export problems
  (load (expand-file-name "htmlize.el" util-dir))
  ;; the newest version of inf-ruby.el is required by org-babel-ruby.el
  (load (expand-file-name "inf-ruby.el" util-dir)))

;; language specific files
(add-to-list 'load-path (expand-file-name "langs" (file-name-directory (or load-file-name buffer-file-name))))
(require 'org-babel-ruby)
(require 'org-babel-python)
(require 'org-babel-sh)
(require 'org-babel-lisp)
(require 'org-babel-R)
(require 'org-babel-ditaa)

(provide 'org-babel-init)
;;; org-babel-init.el ends here
