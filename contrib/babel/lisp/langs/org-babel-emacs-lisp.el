;;; org-babel-emacs-lisp.el --- org-babel functions for emacs-lisp code evaluation

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

;; Org-Babel support for evaluating emacs-lisp code

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "emacs-lisp")

(add-to-list 'org-babel-tangle-langs '("emacs-lisp" "el"))

(defun org-babel-execute:emacs-lisp (body params)
  "Execute a block of emacs-lisp code with org-babel.  This
function is called by `org-babel-execute-src-block' via multiple-value-bind."
  (message "executing emacs-lisp code block...")
  (save-window-excursion
    (let ((print-level nil) (print-length nil))
      (eval `(let ,(mapcar (lambda (var) `(,(car var) ',(cdr var))) vars)
	       ,(read (concat "(progn "
			      (if (or (member "code" result-params)
				      (member "pp" result-params))
				  (concat "(pp " body ")") body)
			      ")")))))))
(provide 'org-babel-emacs-lisp)
;;; org-babel-emacs-lisp.el ends here
