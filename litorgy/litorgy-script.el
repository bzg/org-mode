;;; litorgy-script.el --- litorgy functions for scripting languages

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

;; Litorgy support for evaluating ruby, and python source code.

;;; Code:
(require 'litorgy)

(defun litorgy-script-add-interpreter (var cmds)
  (set-default var cmds)
  (mapc (lambda (cmd)
          (setq litorgy-interpreters (cons cmd litorgy-interpreters))
          (eval
           `(defun ,(intern (concat "litorgy-execute:" cmd)) (body params)
              ,(concat "Evaluate a block of " cmd " script with litorgy. This function is
called by `litorgy-execute-src-block'.  This function is an
automatically generated wrapper for `litorgy-script-execute'.")
              (litorgy-script-execute ,cmd body params))))
        cmds))

(defvar litorgy-script-ruby-wrapper-method
  "
def main
%s
end
results = main()
puts (results.class == String) ? results : results.inspect
")

(defvar litorgy-script-python-wrapper-method
  "
def main():
%s

print main()")

(defcustom litorgy-script-interpreters '("ruby" "python")
  "List of interpreters of scripting languages which can be
executed through litorgy."
  :group 'litorgy
  :set 'litorgy-script-add-interpreter)

(defun litorgy-script-execute (cmd body params)
  "Run CMD on BODY obeying any options set with PARAMS."
  (message (format "executing %s code block..." cmd))
  (let ((vars (litorgy-ref-variables params)))
    (save-window-excursion
      (with-temp-buffer
        (insert
         (format
          (case (intern cmd)
            ('ruby litorgy-script-ruby-wrapper-method)
            ('python litorgy-script-python-wrapper-method))
          (concat
           (mapconcat ;; define any variables
            (lambda (pair)
              (format "\t%s=%s"
                      (car pair)
                      (litorgy-script-var-to-ruby/python (cdr pair))))
            vars "\n")
           "\n"
           (let ((body-lines (split-string body "[\n\r]+" t)))
             (mapconcat
              (lambda (line)
                (format "\t%s\n" line))
              (butlast body-lines) "\n")
             (format "\treturn %s\n" (car (last body-lines)))))))
        ;; (message (buffer-substring (point-min) (point-max))) ;; debug script
        (shell-command-on-region (point-min) (point-max) cmd nil 'replace)
        ;; (message (format "shell output = %s" (buffer-string))) ;; debug results
        (litorgy-script-table-or-results (buffer-string))))))

(defun litorgy-script-var-to-ruby/python (var)
  "Convert an elisp var into a string of ruby or python source
code specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'litorgy-script-var-to-ruby/python var ", ") "]")
    (format "%S" var)))

(defun litorgy-script-table-or-results (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (setq results (litorgy-chomp results))
  (litorgy-read
   (if (string-match "^\\[.+\\]$" results)
       ;; somewhat hacky, but thanks to similarities between languages
       ;; it seems to work
       (litorgy-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               ", " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     (litorgy-chomp results))))

(provide 'litorgy-script)
;;; litorgy-script.el ends here
