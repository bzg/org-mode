;;; org-babel-haskell.el --- org-babel functions for haskell evaluation

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

;; Org-Babel support for evaluating haskell source code.  This one will
;; be sort of tricky because haskell programs must be compiled before
;; they can be run, but haskell code can also be run through an
;; interactive interpreter.
;;
;; For now lets only allow evaluation using the haskell interpreter.

;;; Requirements:

;; - haskell-mode :: http://www.iro.umontreal.ca/~monnier/elisp/#haskell-mode
;;
;; - inf-haskell :: http://www.iro.umontreal.ca/~monnier/elisp/#haskell-mode

;;; Code:
(require 'org-babel)
(require 'haskell-mode)
(require 'inf-haskell)

(org-babel-add-interpreter "haskell")

(add-to-list 'org-babel-tangle-langs '("haskell" "hs"))

(defvar org-babel-haskell-eoe "\"org-babel-haskell-eoe\"")

(defun org-babel-execute:haskell (body params)
  "Execute a block of Haskell code with org-babel.  This function
is called by `org-babel-execute-src-block' with the following
variables pre-set using `multiple-value-bind'.

  (session vars result-params result-type)"
  (message "executing haskell source code block")
  (let* ((full-body (concat
                     (mapconcat
                      (lambda (pair) (format "let %s = %s;" (car pair) (cdr pair)))
                      vars "\n") "\n" body "\n"))
         (session (org-babel-prep-session:haskell session params))
         (raw (org-babel-comint-with-output session org-babel-haskell-eoe t
                (insert (org-babel-trim full-body))
                (comint-send-input nil t)
                (insert org-babel-haskell-eoe)
                (comint-send-input nil t)))
         (results (mapcar
                   #'org-babel-haskell-read-string
                   (cdr (member org-babel-haskell-eoe
                                (reverse (mapcar #'org-babel-trim raw)))))))
    (case result-type
      (output (mapconcat #'identity (reverse (cdr results)) "\n"))
      (value (org-babel-haskell-table-or-string (car results))))))

(defun org-babel-haskell-read-string (string)
  "Strip \\\"s from around haskell string"
  (if (string-match "\"\\([^\000]+\\)\"" string)
      (match-string 1 string)
    string))

(defun org-babel-prep-session:haskell (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (save-window-excursion (run-haskell) (current-buffer)))

(defun org-babel-haskell-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results) (string-match "^\\[.+\\]$" results))
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               "," " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

(provide 'org-babel-haskell)
;;; org-babel-haskell.el ends here
