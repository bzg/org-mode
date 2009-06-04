;;; org-babel-script.el --- org-babel functions for scripting languages

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

;; Org-Babel support for evaluating ruby, and python source code.

;;; Code:
(require 'org-babel)
(require 'inf-ruby)
(require 'python)

;; org-babel introduction and formalities
(defun org-babel-script-add-interpreter (var cmds)
  (set-default var cmds)
  (mapc (lambda (cmd)
          (org-babel-add-interpreter cmd)
          (eval
           `(defun ,(intern (concat "org-babel-execute:" cmd)) (body params)
              ,(concat "Evaluate a block of " cmd " script with org-babel. This function is
called by `org-babel-execute-src-block'.  This function is an
automatically generated wrapper for `org-babel-script-execute'.")
              (org-babel-script-execute ,cmd body params))))
        cmds))

(defcustom org-babel-script-interpreters '("ruby" "python")
  "List of interpreters of scripting languages which can be
executed through org-babel."
  :group 'org-babel
  :set 'org-babel-script-add-interpreter)

(mapc #'org-babel-add-interpreter org-babel-script-interpreters)

;; main execute function used by org-babel
(defun org-babel-script-execute (interpreter body params)
  "Pass BODY to INTERPRETER obeying any options set with PARAMS."
  (message (format "executing %s code block..." cmd))
  (let* ((vars (org-babel-ref-variables params))
         (full-body (concat
                     (mapconcat ;; define any variables
                      (lambda (pair)
                        (format "\t%s=%s"
                                (car pair)
                                (org-babel-script-var-to-ruby/python (cdr pair))))
                      vars "\n") body "\n"))) ;; then the source block body
    (org-babel-script-input-command interpreter full-body)
    (org-babel-script-table-or-results (org-babel-script-last-value interpreter))))

(defun org-babel-script-var-to-ruby/python (var)
  "Convert an elisp var into a string of ruby or python source
code specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-script-var-to-ruby/python var ", ") "]")
    (format "%S" var)))

(defun org-babel-script-table-or-results (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (setq results (org-babel-chomp results))
  (org-babel-read
   (if (string-match "^\\[.+\\]$" results)
       ;; somewhat hacky, but thanks to similarities between languages
       ;; it seems to work
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               ", " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     (org-babel-chomp results))))

;; functions for interacting with comint
(defvar org-babel-script-ruby-buffer nil
  "variable to hold the current ruby buffer")

(defvar org-babel-script-python-buffer nil
  "variable to hold the current python buffer")

(defun org-babel-script-interpreter-buffer (interpreter)
  (intern (format "org-babel-script-%s-buffer" interpreter)))

(defun org-babel-script-initiate-session (interpreter)
  "If there is not a current inferior-process-buffer for
INTERPRETER then create one.  Return the buffer in which the
session has been created."
  (save-window-excursion
    (let ((buffer (org-babel-script-interpreter-buffer interpreter)))
      (unless (and (buffer-live-p buffer) (get-buffer buffer))
        (case (intern interpreter)
          ('ruby (funcall #'run-ruby))
          ('python (funcall #'run-python)))))))

(defun org-babel-script-wait-for-output (interpreter)
  "Wait until output arrives"
  (save-window-excursion
    (save-match-data
      (set-buffer (org-babel-script-initiate-session interpreter))
      (while (progn
               (goto-char comint-last-input-end)
               (not (re-search-forward comint-prompt-regexp nil t)))
        (accept-process-output (get-buffer-process (current-buffer)))))))

(defun org-babel-script-input-command (interpreter cmd)
  "Pass CMD to INTERPRETER"
  (comint-send-string (get-buffer-process (org-babel-script-initiate-session interpreter)) (concat cmd "\n"))
  (org-babel-script-wait-for-output interpreter))

(defun org-babel-script-command-to-string (interpreter cmd)
  (let ((buffer (org-babel-script-interpreter-buffer interpreter)))
    (org-babel-script-input-command interpreter cmd)
    (org-babel-script-last-value interpreter)))

(defun org-babel-script-last-value (interpreter)
  "Return the last value passed to INTERPRETER"
  (save-excursion
    (save-match-data
      (set-buffer (org-babel-script-initiate-session interpreter))
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (org-babel-clean-text-properties (buffer-substring comint-last-input-end (- (point) 1))))))

(provide 'org-babel-script)
;;; org-babel-script.el ends here
