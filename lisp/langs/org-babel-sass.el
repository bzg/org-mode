;;; org-babel-sass.el --- org-babel functions for the sass css generation language

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

;; For more information on sass see http://sass-lang.com/
;;
;; This accepts a 'file' header argument which is the target of the
;; compiled sass.  The default output type for sass evaluation is
;; either file (if a 'file' header argument was given) or scalar if no
;; such header argument was supplied.
;;
;; A 'cmdline' header argument can be supplied to pass arguments to
;; the sass command line.

;;; Requirements:

;; - sass-mode :: http://github.com/nex3/haml/blob/master/extra/sass-mode.el

;;; Code:
(require 'org-babel)
(require 'sass-mode)

(org-babel-add-interpreter "sass")

(add-to-list 'org-babel-tangle-langs '("sass" "sass"))

(defun org-babel-execute:sass (body params)
  "Execute a block of Sass code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Sass source code block")
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
         (file (cdr (assoc :file params)))
         (out-file (or file (make-temp-file "org-babel-sass-out")))
         (cmdline (cdr (assoc :cmdline params)))
         (in-file (make-temp-file "org-babel-sass-in"))
         (cmd (concat "sass " (or cmdline "") in-file " " out-file)))
    (with-temp-file in-file (insert body)) (shell-command cmd)
    (or file (with-temp-buffer (insert-file-contents out-file) (buffer-string)))))

(defun org-babel-prep-session:sass (session params)
  (error "Sass does not support sessions"))

(provide 'org-babel-sass)
;;; org-babel-sass.el ends here
