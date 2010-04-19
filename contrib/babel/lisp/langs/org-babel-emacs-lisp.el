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

(defvar org-babel-default-header-args:emacs-lisp
  '((:hlines . "no") (:colnames . "no"))
  "Default arguments to use when evaluating an emacs-lisp source block.")

(defun org-babel-execute:emacs-lisp (body params)
  "Execute a block of emacs-lisp code with org-babel."
  (message "executing emacs-lisp code block...")
  (save-window-excursion
    (let* ((processed-params (org-babel-process-params params))
           (result-params (third processed-params))
           (vars (second processed-params))
           (print-level nil) (print-length nil))
      (org-babel-reassemble-table
       (eval `(let ,(mapcar (lambda (var) `(,(car var) ',(cdr var)))
                            vars)
                ,(read (concat "(progn "
                               (if (or (member "code" result-params)
                                       (member "pp" result-params))
                                   (concat "(pp " body ")") body)
                               ")"))))
       (nth 4 processed-params) (nth 5 processed-params)))))

(provide 'org-babel-emacs-lisp)
;;; org-babel-emacs-lisp.el ends here
