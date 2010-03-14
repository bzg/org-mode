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
;;
;; - (optionally) lhs2tex :: http://people.cs.uu.nl/andres/lhs2tex/

;;; Code:
(require 'org-babel)
(require 'haskell-mode)
(require 'inf-haskell)

(org-babel-add-interpreter "haskell")

(add-to-list 'org-babel-tangle-langs '("haskell" "hs"))

(defvar org-babel-haskell-lhs2tex-command "lhs2tex")

(defvar org-babel-haskell-eoe "\"org-babel-haskell-eoe\"")

(defun org-babel-execute:haskell (body params)
  "Execute a block of Haskell code with org-babel."
  (message "executing haskell source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (first processed-params))
         (vars (second processed-params))
         (result-type (fourth processed-params))
         (full-body (concat
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
  (if (string-match "^\"\\([^\000]+\\)\"$" string)
      (match-string 1 string)
    string))

(defun org-babel-haskell-initiate-session (&optional session params)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  ;; TODO: make it possible to have multiple sessions
  (run-haskell) (current-buffer))

(defun org-babel-load-session:haskell (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let* ((buffer (org-babel-prep-session:haskell session params))
           (load-file (concat (make-temp-file "org-babel-haskell-load") ".hs")))
      (with-temp-buffer
        (insert body) (write-file load-file)
        (haskell-mode) (inferior-haskell-load-file))
      buffer)))

(defun org-babel-prep-session:haskell (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (save-window-excursion
    (org-babel-haskell-initiate-session session)
    (let* ((vars (org-babel-ref-variables params))
           (var-lines (mapconcat ;; define any variables
                       (lambda (pair)
                         (format "%s=%s"
                                 (car pair)
                                 (org-babel-ruby-var-to-ruby (cdr pair))))
                       vars "\n"))
           (vars-file (concat (make-temp-file "org-babel-haskell-vars") ".hs")))
      (when vars
        (with-temp-buffer
          (insert var-lines) (write-file vars-file)
          (haskell-mode) (inferior-haskell-load-file)))
      (current-buffer))))

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

(defun org-babel-haskell-export-to-lhs (&optional arg)
  "Export to a .lhs with all haskell code blocks escaped
appropriately.  When called with a prefix argument the resulting
.lhs file will be exported to a .tex file.  This function will
create two new files, base-name.lhs and base-name.tex where
base-name is the name of the current org-mode file.

Note that all standard org-babel literate programming
constructs (header arguments, no-web syntax etc...) are ignored."
  (interactive "P")
  (let* ((contents (buffer-string))
         (haskell-regexp
          (concat "^\\([ \t]*\\)#\\+begin_src[ \t]haskell*\\(.*\\)?[\r\n]"
                  "\\([^\000]*?\\)[\r\n][ \t]*#\\+end_src.*"))
         (base-name (file-name-sans-extension (buffer-file-name)))
         (tmp-file (make-temp-file "ob-haskell"))
         (tmp-org-file (concat tmp-file ".org"))
         (tmp-tex-file (concat tmp-file ".tex"))
         (lhs-file (concat base-name ".lhs"))
         (tex-file (concat base-name ".tex"))
         (command (concat org-babel-haskell-lhs2tex-command " " lhs-file " > " tex-file))
         (preserve-indentp org-src-preserve-indentation)
         indentation)
    ;; escape haskell source-code blocks
    (with-temp-file tmp-org-file
      (insert contents)
      (goto-char (point-min))
      (while (re-search-forward haskell-regexp nil t)
        (save-match-data (setq indentation (length (match-string 1))))
        (replace-match (save-match-data
                         (concat
                          "#+begin_latex\n\\begin{code}\n"
                          (if (or preserve-indentp
                                  (string-match "-i" (match-string 2)))
                              (match-string 3)
                            (org-remove-indentation (match-string 3)))
                          "\n\\end{code}\n#+end_latex\n"))
                       t t)
        (indent-code-rigidly (match-beginning 0) (match-end 0) indentation)))
    (save-excursion
      ;; export to latex w/org and save as .lhs
      (find-file tmp-org-file) (funcall 'org-export-as-latex nil)
      (kill-buffer)
      (delete-file tmp-org-file)
      (find-file tmp-tex-file)
      (goto-char (point-min)) (forward-line 2)
      (insert "%include polycode.fmt\n")
      ;; ensure all \begin/end{code} statements start at the first column
      (while (re-search-forward "^[ \t]+\\\\begin{code}[^\000]+\\\\end{code}" nil t)
        (replace-match (save-match-data (org-remove-indentation (match-string 0)))
                       t t))
      (setq contents (buffer-string))
      (save-buffer) (kill-buffer))
    (delete-file tmp-tex-file)
    ;; save org exported latex to a .lhs file
    (with-temp-file lhs-file (insert contents))
    (if (not arg)
        (find-file lhs-file)
      ;; process .lhs file with lhs2tex
      (message "running %s" command) (shell-command command) (find-file tex-file))))

(provide 'org-babel-haskell)
;;; org-babel-haskell.el ends here
