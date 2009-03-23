;;; litorgy-lisp.el --- litorgy functions for lisp code evaluation

;; Copyright (C) 2009 Eric Schulte, Dan Davison, Austin F. Frank

;; Author: Eric Schulte, Dan Davison, Austin F. Frank
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

;; Litorgy support for evaluating lisp code

;;; Code:
(require 'litorgy)

(litorgy-add-interpreter "emacs-lisp")

(defun litorgy-execute:emacs-lisp (body params)
  "Execute a block of emacs-lisp code with litorgy.  This
function is called by `litorgy-execute-src-block'."
  (save-window-excursion
    (let ((print-level nil) (print-length nil) results)
      (message "executing emacs-lisp code block...")
      (format "%S" (eval (read body))))))

(provide 'litorgy-lisp)
;;; litorgy-lisp.el ends here
