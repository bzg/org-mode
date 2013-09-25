;;; ob-abc.el --- org-babel functions for template evaluation

;; Copyright (C) Free Software Foundation

;; Author: William Waites
;; Keywords: literate programming, music
;; Homepage: http://www.tardis.ed.ac.uk/wwaites
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

;;; This file adds support to Org Babel for music in ABC notation.
;;; It requires that the abcm2ps program is installed.
;;; See http://moinejf.free.fr/

(require 'ob)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("abc" . "abc"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:abc
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating an ABC source block.")

(defun org-babel-expand-body:abc (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "\$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body))))
     vars)
    body))

(defun org-babel-execute:abc (body params)
  "Execute a block of ABC code with org-babel.  This function is
   called by `org-babel-execute-src-block'"
  (message "executing Abc source code block")
  (let* ((result-params (split-string (or (cdr (assoc :results params)))))
	 (cmdline (cdr (assoc :cmdline params)))
	 (out-file ((lambda (el)
		      (or el
			  (error "abc code block requires :file header argument")))
		    ;;; For SVG or EPS output, abcm2ps will add a number for a particular page
		    ;;; automatically. This needs to be specified in the :file argument and stripped
		    ;;; stripped out here. There is likely a better way to do this.
		    (replace-regexp-in-string "001" "" (cdr (assoc :file params)))))
	 (in-file (org-babel-temp-file "abc-"))
	 (cmd (concat "abcm2ps" " " cmdline
		      " -O " (org-babel-process-file-name out-file)
		      " " (org-babel-process-file-name in-file))))
    (with-temp-file in-file (insert (org-babel-expand-body:abc body params)))
    (org-babel-eval cmd "")
    ;;; indicate that the file has been written
    nil))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:abc (session params)
  "Return an error because abc does not support sessions."
  (error "ABC does not support sessions"))

(provide 'ob-abc)
;;; ob-abc.el ends here
