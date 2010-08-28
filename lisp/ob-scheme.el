;;; ob-scheme.el --- org-babel functions for Scheme

;; Copyright (C) 2010 Free Software Foundation

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, scheme
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

;; - a working scheme implementation
;;   (e.g. guile http://www.gnu.org/software/guile/guile.html)

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:scheme '()
  "Default header arguments for scheme code blocks.")

(defcustom org-babel-scheme-cmd "guile"
  "Name of command used to evaluate scheme blocks."
  :group 'org-babel
  :type 'string)

(defun org-babel-expand-body:scheme (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (if (> (length vars) 0)
        (concat "(let ("
                (mapconcat
                 (lambda (var) (format "%S" (print `(,(car var) ',(cdr var)))))
                 vars "\n      ")
                ")\n" body ")")
      body)))

(defun org-babel-execute:scheme (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
	 (session (not (string= (nth 0 processed-params) "none")))
         (result-type (nth 3 processed-params))
         (full-body (org-babel-expand-body:scheme body params processed-params)))
    (read
     (if session
         ;; session evaluation
         (error "Scheme sessions are not yet supported.")
       ;; external evaluation
       (let ((script-file (org-babel-temp-file "lisp-script-")))
         (with-temp-file script-file
           (insert
            ;; return the value or the output
            (if (string= result-type "value")
                (format "(display %s)" full-body)
              full-body)))
         (org-babel-eval
	  (format "%s %s" org-babel-scheme-cmd script-file) ""))))))

(defun org-babel-prep-session:scheme (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "not yet implemented"))

(defun org-babel-scheme-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (error "Scheme sessions are not yet supported."))

(provide 'ob-scheme)

;; arch-tag: 6b2fe76f-4b25-4e87-ad1c-225b2f282a71

;;; ob-scheme.el ends here
