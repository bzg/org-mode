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
         (results-params (split-string (or (cdr (assoc :results params)) "")))
         (full-body (concat
                     (mapconcat ;; define any variables
                      (lambda (pair)
                        (format "\t%s=%s"
                                (car pair)
                                (org-babel-script-var-to-ruby/python (cdr pair))))
                      vars "\n") body "\n"))) ;; then the source block body
    (org-babel-script-initiate-session interpreter)
    (cond
     ((member "script" results-params) ;; collect all output
      (let ((tmp-file (make-temp-file "org-babel-R-script-output")))
        ;; this is totally not working well
        (org-babel-script-input-command interpreter (concat
                                                     "org_babel_io_holder = $stdout;"
                                                     (format "org_babel_tmp_file_holder = File.open('%s', 'w'); "  tmp-file)
                                                     "$stdout = org_babel_tmp_file_holder; "
                                                     body
                                                     "org_babel_tmp_file_holder.flush; "
                                                     "$stdout = org_babel_io_holder;"))
        (with-temp-buffer (insert-file-contents tmp-file) (buffer-string))))
     ((member "last" results-params) ;; the value of the last statement
      (org-babel-script-input-command interpreter full-body)
      (org-babel-script-table-or-results
       (org-babel-script-command-to-string interpreter "_"))))))

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

;; functions for interacting with comint sessions
(defvar org-babel-script-ruby-session nil)
(defvar org-babel-script-python-session nil)

(defun org-babel-script-session (interpreter)
  (case (if (symbolp interpreter) interpreter (intern interpreter))
          ('ruby 'org-babel-script-ruby-session)
          ('python 'org-babel-script-python-session)))

(defun org-babel-script-initiate-session (interpreter)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (case (intern (format "%s" interpreter))
    ('ruby
     (setq org-babel-script-ruby-session (save-window-excursion
                                            (funcall #'run-ruby nil)
                                            (current-buffer))))
    ('python
     (setq org-babel-script-python-session (save-window-excursion
                                             (funcall #'run-python)
                                             (current-buffer))))))

(defun org-babel-script-input-command (interpreter cmd)
  (org-babel-comint-input-command
   (eval (org-babel-script-session interpreter)) (org-babel-chomp cmd)))

(defun org-babel-script-command-to-string (interpreter cmd)
  (org-babel-comint-command-to-string
   (eval (org-babel-script-session interpreter)) (org-babel-chomp cmd)))

(provide 'org-babel-script)
;;; org-babel-script.el ends here
