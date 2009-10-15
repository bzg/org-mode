;;; org-babel-dot.el --- org-babel functions for dot evaluation

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

;; Org-Babel support for evaluating dot source code.
;;
;; For information on dot see http://www.graphviz.org/
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in dot
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "dot")

(add-to-list 'org-babel-tangle-langs '("dot" "dot"))

(defvar org-babel-default-header-args:dot '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a dot source block.")

(defun org-babel-execute:dot (body params)
  "Execute a block of Dot code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Dot source code block")
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
        (out-file (cdr (assoc :file params)))
        (cmdline (cdr (assoc :cmdline params)))
        (cmd (or (cdr (assoc :cmd params)) "dot"))
        (in-file (make-temp-file "org-babel-dot")))
    (with-temp-file in-file (insert body))
    (message (concat cmd " " in-file " " cmdline " -o " out-file))
    (shell-command (concat cmd " " in-file " " cmdline " -o " out-file))
    out-file))

(defun org-babel-prep-session:dot (session params)
  (error "Dot does not support sessions"))

(provide 'org-babel-dot)
;;; org-babel-dot.el ends here
