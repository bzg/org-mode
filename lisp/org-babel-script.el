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

(defvar org-babel-script-ruby-wrapper-method
  "
def main
%s
end
results = main()
puts (results.class == String) ? results : results.inspect
")

(defvar org-babel-script-python-wrapper-method
  "
def main():
%s

print main()")

(defcustom org-babel-script-interpreters '("ruby" "python")
  "List of interpreters of scripting languages which can be
executed through org-babel."
  :group 'org-babel
  :set 'org-babel-script-add-interpreter)

(mapc #'org-babel-add-interpreter org-babel-script-interpreters)

(defun org-babel-script-execute (cmd body params)
  "Run CMD on BODY obeying any options set with PARAMS."
  (message (format "executing %s code block..." cmd))
  (let ((vars (org-babel-ref-variables params)))
    (save-window-excursion
      (with-temp-buffer
        (insert
         (format
          (case (intern cmd)
            ('ruby org-babel-script-ruby-wrapper-method)
            ('python org-babel-script-python-wrapper-method))
          (concat
           (mapconcat ;; define any variables
            (lambda (pair)
              (format "\t%s=%s"
                      (car pair)
                      (org-babel-script-var-to-ruby/python (cdr pair))))
            vars "\n")
           "\n"
           (let ((body-lines (split-string body "[\n\r]+" t)))
             (concat
              (mapconcat (lambda (line) (format "\t%s" line)) (butlast body-lines) "\n")
              (format "\n\treturn %s\n" (car (last body-lines))))))))
        ;; (message (buffer-substring (point-min) (point-max))) ;; debug script
        (shell-command-on-region (point-min) (point-max) cmd nil 'replace)
        ;; (message (format "shell output = %s" (buffer-string))) ;; debug results
        (org-babel-script-table-or-results (buffer-string))))))

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

(provide 'org-babel-script)
;;; org-babel-script.el ends here
