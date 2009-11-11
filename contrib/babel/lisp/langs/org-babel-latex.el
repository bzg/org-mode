;;; org-babel-latex.el --- org-babel functions for latex "evaluation"

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

;; Org-Babel support for evaluating LaTeX source code.
;;
;; Currently on evaluation this returns raw LaTeX code, however at
;; some point it *may* (it may not) make sense to have LaTeX blocks
;; compile small pdfs on evaluation.

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "latex")

(add-to-list 'org-babel-tangle-langs '("latex" "tex"))

(defvar org-babel-default-header-args:latex
  '((:results . "latex") (:exports . "results"))
  "Default arguments to use when evaluating a latex source block.")

(defun org-babel-execute:latex (body params)
  "Execute a block of Latex code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Latex source code block")
  (mapc (lambda (pair) ;; replace variables
          (setq body
                (replace-regexp-in-string
                 (regexp-quote (format "%S" (car pair)))
                 (if (stringp (cdr pair))
                     (cdr pair) (format "%S" (cdr pair)))
                 body))) (second (org-babel-process-params params)))
  body)

(defun org-babel-prep-session:latex (session params)
  (error "Latex does not support sessions"))

(provide 'org-babel-latex)
;;; org-babel-latex.el ends here
