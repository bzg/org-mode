;;; org-babel-shell.el --- org-babel functions for shell evaluation

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

;; Org-Babel support for evaluating shell source code.

;;; Code:
(require 'org-babel)
(require 'shell)

(org-babel-add-interpreter "sh")

(defun org-babel-execute:sh (body params)
  "Execute a block of Shell commands with org-babel.  This
function is called by `org-babel-execute-src-block'."
  (message "executing Shell command block")
  (let* ((vars (org-babel-ref-variables params))
         (result-params (split-string (or (cdr (assoc :results params)) "")))
         (result-type (cond ((member "output" result-params) 'output)
                            ((member "value" result-params) 'value)
                            (t 'value)))
         (full-body (concat
                     (mapconcat ;; define any variables
                      (lambda (pair)
                        (format "%s=%s"
                                (car pair)
                                (org-babel-shell-var-to-shell (cdr pair))))
                      vars "\n") "\n" body "\n\n")) ;; then the source block body
         (session (org-babel-shell-initiate-session (cdr (assoc :session params))))
         (results (org-babel-shell-evaluate session full-body result-type)))
    (if (member "scalar" result-params)
        results
      (setq results (let ((tmp-file (make-temp-file "org-babel-ruby")))
                      (with-temp-file tmp-file (insert results))
                      (org-babel-import-elisp-from-file tmp-file)))
      (if (and (member "vector" results) (not (listp results)))
          (list (list results))
        results))))

(defun org-babel-shell-var-to-shell (var)
  "Convert an elisp var into a string of shell commands
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-shell-var-to-shell var ", ") "]")
    (format "%S" var)))

(defun org-babel-shell-table-or-results (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (string-match "^\\[.+\\]$" results)
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               ", " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

;; functions for comint evaluation

(defun org-babel-shell-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (save-window-excursion (shell session)
                         (org-babel-comint-wait-for-output (current-buffer))
                         (current-buffer)))

(defvar org-babel-shell-eoe-indicator "echo 'org_babel_shell_eoe'"
  "Used to indicate that evaluation is has completed.")
(defvar org-babel-shell-eoe-output "org_babel_shell_eoe"
  "Used to indicate that evaluation is has completed.")

(defun org-babel-shell-evaluate (buffer body &optional result-type)
  "Pass BODY to the Shell process in BUFFER.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY."
  (org-babel-comint-in-buffer buffer
    (let ((string-buffer "")
          (full-body (mapconcat #'org-babel-chomp
                                (list body org-babel-shell-eoe-indicator) "\n"))
          results)
      (flet ((my-filt (text) (setq string-buffer (concat string-buffer text))))
        ;; setup filter
        (add-hook 'comint-output-filter-functions 'my-filt)
        ;; pass FULL-BODY to process
        (goto-char (process-mark (get-buffer-process buffer)))
        (insert full-body)
        (comint-send-input)
        ;; wait for end-of-evaluation indicator
        (while (progn
                 (goto-char comint-last-input-end)
                 (not (save-excursion (and (re-search-forward comint-prompt-regexp nil t)
                                           (re-search-forward (regexp-quote org-babel-shell-eoe-output) nil t)))))
          (accept-process-output (get-buffer-process buffer)))
        ;; remove filter
        (remove-hook 'comint-output-filter-functions 'my-filt))
      ;; (message (format "raw-results=%S" string-buffer)) ;; debugging
      ;; (message (format "split-results=%S" (mapcar #'org-babel-trim (split-string string-buffer comint-prompt-regexp)))) ;; debugging
      ;; split results with `comint-prompt-regexp'
      (setq results (cdr (member org-babel-shell-eoe-output
                                 (reverse (mapcar #'org-babel-shell-strip-weird-long-prompt
                                                  (mapcar #'org-babel-trim (split-string string-buffer comint-prompt-regexp)))))))
      (message (replace-regexp-in-string "%" "%%" (format "processed-results=%S" results))) ;; debugging
      (or (case result-type
            (output (org-babel-trim (mapconcat #'org-babel-trim (reverse results) "\n")))
            (value (car results))
            (t (reverse results))) ""))))

(defun org-babel-shell-strip-weird-long-prompt (string)
  (while (string-match "^% +[\r\n$]+ *" string)
    (setq string (substring string (match-end 0))))
  string)

(provide 'org-babel-shell)
;;; org-babel-shell.el ends here
