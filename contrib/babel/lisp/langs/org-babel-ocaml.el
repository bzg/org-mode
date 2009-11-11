;;; org-babel-ocaml.el --- org-babel functions for ocaml evaluation

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

;; Org-Babel support for evaluating ocaml source code.  This one will
;; be sort of tricky because ocaml programs must be compiled before
;; they can be run, but ocaml code can also be run through an
;; interactive interpreter.
;;
;; For now lets only allow evaluation using the ocaml interpreter.

;;; Requirements:

;; - tuareg-mode :: http://www-rocq.inria.fr/~acohen/tuareg/

;;; Code:
(require 'org-babel)
(require 'tuareg)

(org-babel-add-interpreter "ocaml")

(add-to-list 'org-babel-tangle-langs '("ocaml" "ml"))

(defvar org-babel-ocaml-eoe-indicator "\"org-babel-ocaml-eoe\";;")
(defvar org-babel-ocaml-eoe-output "org-babel-ocaml-eoe")

(defun org-babel-execute:ocaml (body params)
  "Execute a block of Ocaml code with org-babel."
  (message "executing ocaml source code block")
  (let* ((processed-params (org-babel-process-params params))
         (vars (second processed-params))
         (full-body (concat
                     (mapconcat
                      (lambda (pair) (format "let %s = %s;" (car pair) (cdr pair)))
                      vars "\n") "\n" body "\n"))
         (session (org-babel-prep-session:ocaml session params))
         (raw (org-babel-comint-with-output session org-babel-ocaml-eoe-output t
                (insert (concat (org-babel-chomp full-body) " ;;"))
                (comint-send-input nil t)
                (insert org-babel-ocaml-eoe-indicator)
                (comint-send-input nil t))))
    (org-babel-ocaml-parse-output (org-babel-trim (car raw)))))

(defun org-babel-prep-session:ocaml (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let ((tuareg-interactive-buffer-name (if (and (not (string= session "none"))
                                                 (not (string= session "default"))
                                                 (stringp session))
                                            session
                                          tuareg-interactive-buffer-name)))
    (save-window-excursion (tuareg-run-caml)
                           (get-buffer tuareg-interactive-buffer-name))))

(defun org-babel-ocaml-parse-output (output)
  (let ((regexp "%s = \\(.+\\)$"))
    (cond
     ((string-match (format regexp "string") output)
      (org-babel-read (match-string 1 output)))
     ((or (string-match (format regexp "int") output)
          (string-match (format regexp "float") output))
      (string-to-number (match-string 1 output)))
     ((string-match (format regexp "list") output)
      (org-babel-ocaml-read-list (match-string 1 output)))
     ((string-match (format regexp "array") output)
      (org-babel-ocaml-read-array (match-string 1 output)))
     (t (message "don't recognize type of %s" output) output))))

(defun org-babel-ocaml-read-list (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results) (string-match "^\\[.+\\]$" results))
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               "; " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

(defun org-babel-ocaml-read-array (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results) (string-match "^\\[.+\\]$" results))
       (org-babel-read
        (replace-regexp-in-string
         "\\[|" "(" (replace-regexp-in-string
                    "|\\]" ")" (replace-regexp-in-string
                               "; " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

(provide 'org-babel-ocaml)
;;; org-babel-ocaml.el ends here
