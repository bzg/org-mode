;;; org-babel-shell.el --- org-babel functions for shell execution

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

;; Org-Babel support for evaluating sh, bash, and zsh shells

;;; Code:
(require 'org-babel)

(defun org-babel-shell-add-interpreter (var cmds)
  (set-default var cmds)
  (mapc (lambda (cmd)
          (org-babel-add-interpreter cmd)
          (eval
           `(defun ,(intern (concat "org-babel-execute:" cmd)) (body params)
              ,(concat "Evaluate a block of " cmd " shell with org-babel. This function is
called by `org-babel-execute-src-block'.  This function is an
automatically generated wrapper for `org-babel-shell-execute'.")
              (org-babel-shell-execute ,cmd body params))))
        cmds))

(defcustom org-babel-shell-interpreters '("sh" "bash" "zsh")
  "List of interpreters of shelling languages which can be
executed through org-babel."
  :group 'org-babel
  :set 'org-babel-shell-add-interpreter)

(defun org-babel-shell-execute (cmd body params)
  "Run CMD on BODY obeying any options set with PARAMS."
  (message (format "executing %s code block..." cmd))
  (let ((vars (org-babel-ref-variables params)))
    (save-window-excursion
      (with-temp-buffer
        (if (> (length vars) 0)
            (error "currently no support for passing variables to shells"))
        (insert body)
        (shell-command-on-region (point-min) (point-max) cmd nil 'replace)
        (org-babel-shell-to-elisp (buffer-string))))))

(defun org-babel-shell-to-elisp (result)
  (let ((tmp-file (make-temp-file "org-babel-shell")))
    (with-temp-file tmp-file
      (insert result))
    (with-temp-buffer
      (org-table-import tmp-file nil)
      (delete-file tmp-file)
      (setq result (mapcar (lambda (row)
                             (mapcar #'org-babel-read row))
                           (org-table-to-lisp)))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
          (if (consp (car result))
              (if (null (cdr (car result)))
                  (caar result)
                result)
            (car result))
        result))))

(provide 'org-babel-shell)
;;; org-babel-shell.el ends here
