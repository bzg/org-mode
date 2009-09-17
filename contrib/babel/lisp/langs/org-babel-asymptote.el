;;; org-babel-asymptote.el --- org-babel functions for asymptote evaluation

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

;; Org-Babel support for evaluating asymptote source code.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in asymptote
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments, if file
;;    is omitted then the -V option is passed to the asy command for
;;    interactive viewing
;;
;; 4) there are no variables (at least for now)

;;; Requirements:

;; - The asymptote program :: http://asymptote.sourceforge.net/
;;
;; - asy-mode :: Major mode for editing asymptote files

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "asymptote")

(add-to-list 'org-babel-tangle-langs '("asymptote" "asymptote"))

(defvar org-babel-default-header-args:asymptote '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a asymptote source block.")

(defun org-babel-execute:asymptote (body params)
  "Execute a block of Asymptote code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Asymptote source code block")
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
         (out-file (cdr (assoc :file params)))
         (format (or (and out-file
                          (string-match ".+\\.\\(.+\\)" out-file)
                          (match-string 1 out-file))
                     "pdf"))
         (cmdline (cdr (assoc :cmdline params)))
         (in-file (make-temp-file "org-babel-asymptote"))
         (cmd (concat "asy "
                      (if out-file
                          (concat "-globalwrite -f " format " -o " out-file)
                        "-V")
                      " " cmdline " " in-file)))
    (with-temp-file in-file (insert body))
    (message cmd) (shell-command cmd)
    out-file))

(defun org-babel-prep-session:asymptote (session params)
  (error "Asymptote does not support sessions"))

(provide 'org-babel-asymptote)
;;; org-babel-asymptote.el ends here
