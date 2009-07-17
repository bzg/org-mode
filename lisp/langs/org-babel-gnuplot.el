;;; org-babel-gnuplot.el --- org-babel functions for gnuplot evaluation

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

;; Org-Babel support for evaluating gnuplot source code.
;;
;; This differs from most standard languages in that
;;
;; 1) we are generally only going to return results of type "file"
;;
;; 2) we are adding the "file" and "cmdline" header arguments

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "gnuplot")

(add-to-list 'org-babel-tangle-langs '("gnuplot" "gnuplot"))

(defun org-babel-execute:gnuplot (body params)
  "Execute a block of Gnuplot code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Gnuplot source code block"))

(defun org-babel-prep-session:gnuplot (session params))

(provide 'org-babel-gnuplot)
;;; org-babel-gnuplot.el ends here
