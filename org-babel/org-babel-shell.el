;;; litorgy-shell.el --- litorgy functions for shell execution

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

;; Litorgy support for evaluating sh, bash, and zsh shells

;;; Code:
(require 'litorgy)

(defun litorgy-shell-add-interpreter (var cmds)
  (set-default var cmds)
  (mapc (lambda (cmd)
          (setq litorgy-interpreters (cons cmd litorgy-interpreters))
          (eval
           `(defun ,(intern (concat "litorgy-execute:" cmd)) (body params)
              ,(concat "Evaluate a block of " cmd " shell with litorgy. This function is
called by `litorgy-execute-src-block'.  This function is an
automatically generated wrapper for `litorgy-shell-execute'.")
              (litorgy-shell-execute ,cmd body params))))
        cmds))

(defcustom litorgy-shell-interpreters '("sh" "bash" "zsh")
  "List of interpreters of shelling languages which can be
executed through litorgy."
  :group 'litorgy
  :set 'litorgy-shell-add-interpreter)

(defun litorgy-shell-execute (cmd body params)
  "Run CMD on BODY obeying any options set with PARAMS."
  (message (format "executing %s code block..." cmd))
  (let ((vars (litorgy-ref-variables params)))
    (save-window-excursion
      (with-temp-buffer
        (if (> (length vars) 0)
            (error "currently no support for passing variables to shells"))
        (insert body)
        (shell-command-on-region (point-min) (point-max) cmd nil 'replace)
        (litorgy-shell-to-elisp (buffer-string))))))

(defun litorgy-shell-to-elisp (result)
  (let ((tmp-file (make-temp-file "litorgy-shell")))
    (with-temp-file tmp-file
      (insert result))
    (with-temp-buffer
      (org-table-import tmp-file nil)
      (delete-file tmp-file)
      (setq result (mapcar (lambda (row)
                             (mapcar #'litorgy-read row))
                           (org-table-to-lisp)))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
          (if (consp (car result))
              (if (null (cdr (car result)))
                  (caar result)
                result)
            (car result))
        result))))

(provide 'litorgy-shell)
;;; litorgy-shell.el ends here
