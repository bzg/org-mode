;;; org-babel-python.el --- org-babel functions for python evaluation

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

;; Org-Babel support for evaluating python source code.

;;; Code:
(require 'org-babel)
(require 'python)

(org-babel-add-interpreter "python")

(defun org-babel-execute:python (body params)
  "Execute a block of Python code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Python source code block")
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
                                (org-babel-python-var-to-python (cdr pair))))
                      vars "\n") "\n" body "\n")) ;; then the source block body
         (session (org-babel-python-initiate-session (cdr (assoc :session params))))
         (results (org-babel-python-evaluate session full-body result-type)))
    (if (member "scalar" result-params)
        results
      (setq results (case result-type ;; process results based on the result-type
                      ('output (let ((tmp-file (make-temp-file "org-babel-python")))
                                 (with-temp-file tmp-file (insert results))
                                 (org-babel-import-elisp-from-file tmp-file)))
                      ('value (org-babel-python-table-or-results results))))
      (if (and (member "vector" results) (not (listp results)))
          (list (list results))
        results))))

(defun org-babel-python-var-to-python (var)
  "Convert an elisp var into a string of python source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-python-var-to-python var ", ") "]")
    (format "%S" var)))

(defun org-babel-python-table-or-results (results)
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

(defun org-babel-python-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (let ((python-buffer (or session python-buffer)))
    (save-window-excursion (run-python nil session) (current-buffer))))

(defvar org-babel-python-last-value-eval "_"
  "When evaluated by Python this returns the return value of the last statement.")
(defvar org-babel-python-eoe-indicator "'org_babel_python_eoe'"
  "Used to indicate that evaluation is has completed.")

(defun org-babel-python-evaluate (buffer body &optional result-type)
  "Pass BODY to the Python process in BUFFER.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY."
  (org-babel-comint-in-buffer buffer
    (let ((string-buffer "")
          (full-body (mapconcat #'org-babel-chomp
                                (list body org-babel-python-last-value-eval org-babel-python-eoe-indicator) "\n"))
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
                                           (re-search-forward (regexp-quote org-babel-python-eoe-indicator) nil t)))))
          (accept-process-output (get-buffer-process buffer)))
        ;; remove filter
        (remove-hook 'comint-output-filter-functions 'my-filt))
      ;; remove echo'd FULL-BODY from input
      (if (string-match (replace-regexp-in-string "\n" "\r\n" (regexp-quote full-body)) string-buffer)
          (setq string-buffer (substring string-buffer (match-end 0))))
      ;; split results with `comint-prompt-regexp'
      (setq results (cdr (member org-babel-python-eoe-indicator
                                 (reverse (mapcar #'org-babel-trim (split-string string-buffer comint-prompt-regexp))))))
      (org-babel-trim (case result-type
                        (output (mapconcat #'identity (reverse (cdr results)) "\n"))
                        (value (car results))
                        (t (reverse results)))))))

(provide 'org-babel-python)
;;; org-babel-python.el ends here
