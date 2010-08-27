;;; ob-js.el --- org-babel functions for Javascript

;; Copyright (C) 2010 Free Software Foundation

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, js
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

;; Now working with SBCL for both session and external evaluation.
;;
;; This certainly isn't optimally robust, but it seems to be working
;; for the basic use cases.

;;; Requirements:

;; node.js | http://nodejs.org/

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:js '()
  "Default header arguments for js code blocks.")

(defcustom org-babel-js-cmd "node"
  "Name of command used to evaluate js blocks."
  :group 'org-babel
  :type 'string)

(defvar org-babel-js-function-wrapper
  "require('sys').print(require('sys').inspect(function(){%s}()));"
  "Javascript code to print value of body.")

(defun org-babel-expand-body:js (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair) (format "var %s=%s;"
			(car pair) (org-babel-js-var-to-js (cdr pair))))
      vars "\n") "\n" body "\n")))

(defun org-babel-execute:js (body params)
  "Execute a block of Javascript code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
	 (session (not (string= (nth 0 processed-params) "none")))
         (result-type (nth 3 processed-params))
         (full-body (org-babel-expand-body:js body params processed-params)))
    (org-babel-js-read
     (if session
         (error "javascript sessions are not yet supported.")
       (let ((script-file (org-babel-temp-file "js-script-")))
         (with-temp-file script-file
           (insert
            ;; return the value or the output
            (if (string= result-type "value")
                (format org-babel-js-function-wrapper full-body)
              full-body)))
         (org-babel-eval (format "%s %s" org-babel-js-cmd script-file) ""))))))

(defun org-babel-js-read (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results) (string-match "^\\[.+\\]$" results))
       (org-babel-read
        (concat "'"
                (replace-regexp-in-string
                 "\\[" "(" (replace-regexp-in-string
                            "\\]" ")" (replace-regexp-in-string
                                       ", " " " (replace-regexp-in-string
						 "'" "\"" results))))))
     results)))

(defun org-babel-js-var-to-js (var)
  "Convert VAR into a js variable.
Convert an elisp value into a string of js source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-js-var-to-js var ", ") "]")
    (format "%S" var)))

(defun org-babel-prep-session:js (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "not yet implemented"))

(defun org-babel-js-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (error "Javascript sessions are not yet supported."))

(provide 'ob-js)

;; arch-tag: 84401fb3-b8d9-4bb6-9a90-cbe2d103d494

;;; ob-js.el ends here
