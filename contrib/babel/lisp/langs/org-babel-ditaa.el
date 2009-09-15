;;; org-babel-ditaa.el --- org-babel functions for ditaa evaluation

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

;; Org-Babel support for evaluating ditaa source code.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in ditaa
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "ditaa")

(add-to-list 'org-babel-tangle-langs '("ditaa" "ditaa"))

(defvar org-babel-default-header-args:ditaa
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a ditaa source block.")

(defun org-babel-execute:ditaa (body params)
  "Execute a block of Ditaa code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Ditaa source code block")
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
        (out-file (cdr (assoc :file params)))
        (cmdline (cdr (assoc :cmdline params)))
        (in-file (make-temp-file "org-babel-ditaa")))
    (unless (file-exists-p org-ditaa-jar-path)
      (error (format "Could not find ditaa.jar at %s" org-ditaa-jar-path)))
    (with-temp-file in-file (insert body))
    (message (concat "java -jar " org-ditaa-jar-path " " cmdline " " in-file " " out-file))
    (shell-command (concat "java -jar " org-ditaa-jar-path " " cmdline " " in-file " " out-file))
    out-file))

(defun org-babel-prep-session:ditaa (session params)
  (error "Ditaa does not support sessions"))

(provide 'org-babel-ditaa)
;;; org-babel-ditaa.el ends here
