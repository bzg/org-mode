;;; org-babel-ruby.el --- org-babel functions for ruby evaluation

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

;; Org-Babel support for evaluating ruby source code.

;;; Code:
(require 'org-babel)
(require 'inf-ruby)

(org-babel-add-interpreter "ruby")

(defun org-babel-execute:ruby (body params)
  "Execute a block of Ruby code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Ruby source code block")
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
                                (org-babel-ruby-var-to-ruby (cdr pair))))
                      vars "\n") "\n" body "\n")) ;; then the source block body
         (session (org-babel-ruby-initiate-session (cdr (assoc :session params))))
         (results (org-babel-ruby-evaluate session full-body result-type)))
    (if (member "scalar" result-params)
        results
      (case result-type ;; process results based on the result-type
        ('output (let ((tmp-file (make-temp-file "org-babel-ruby")))
                   (with-temp-file tmp-file (insert results))
                   (org-babel-import-elisp-from-file tmp-file)))
        ('value (org-babel-ruby-table-or-results results))))))

(defun org-babel-ruby-var-to-ruby (var)
  "Convert an elisp var into a string of ruby source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-ruby-var-to-ruby var ", ") "]")
    (format "%S" var)))

(defun org-babel-ruby-table-or-results (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (message "results are %s" results)
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

(defun org-babel-ruby-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (save-window-excursion (run-ruby nil session) (current-buffer)))

(defvar org-babel-ruby-last-value-eval "_"
  "When evaluated by Ruby this returns the return value of the last statement.")
(defvar org-babel-ruby-eoe-indicator ":org_babel_ruby_eoe"
  "Used to indicate that evaluation is has completed.")

(defun org-babel-ruby-evaluate (buffer body &optional result-type)
  "Pass BODY to the Ruby process in BUFFER.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY."
  (let* ((full-body (mapconcat #'org-babel-chomp
                               (list body org-babel-ruby-last-value-eval org-babel-ruby-eoe-indicator) "\n"))
         (raw (org-babel-comint-with-output buffer org-babel-ruby-eoe-indicator t
                (insert full-body) (comint-send-input)))
         (results (cdr (member org-babel-ruby-eoe-indicator
                               (reverse (mapcar #'org-babel-ruby-read-string
                                                (mapcar #'org-babel-trim raw)))))))
    (case result-type
      (output (mapconcat #'identity (reverse (cdr results)) "\n"))
      (value (car results))
      (t (reverse results)))))

(defun org-babel-ruby-read-string (string)
  "Strip \\\"s from around ruby string"
  (if (string-match "\"\\([^\000]+\\)\"" string)
      (match-string 1 string)
    string))

(provide 'org-babel-ruby)
;;; org-babel-ruby.el ends here
