;;; org-babel-perl.el --- org-babel functions for perl evaluation

;; Copyright (C) 2009 Dan Davison, Eric Schulte

;; Author: Dan Davison, Eric Schulte
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

;; Org-Babel support for evaluating perl source code.

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "perl")

(add-to-list 'org-babel-tangle-langs '("perl" "pl" "#!/usr/bin/env perl"))

(defun org-babel-execute:perl (body params)
  "Execute a block of Perl code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Perl source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (first processed-params))
         (vars (second processed-params))
         (result-params (third processed-params))
         (result-type (fourth processed-params))
         (full-body (concat
		    (mapconcat ;; define any variables
		     (lambda (pair)
		       (format "$%s=%s;"
			       (car pair)
			       (org-babel-perl-var-to-perl (cdr pair))))
		     vars "\n") "\n" (org-babel-trim body) "\n")) ;; then the source block body
	(session (org-babel-perl-initiate-session session)))
    (org-babel-perl-evaluate session full-body result-type)))

(defun org-babel-prep-session:perl (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Sessions are not supported for Perl."))

;; helper functions

(defun org-babel-perl-var-to-perl (var)
  "Convert an elisp var into a string of perl source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-perl-var-to-perl var ", ") "]")
    (format "%S" var)))

(defvar org-babel-perl-buffers '(:default . nil))

(defun org-babel-perl-initiate-session (&optional session params)
  "Simply return nil, as sessions are not supported by perl"
nil)

(defvar org-babel-perl-wrapper-method
  "
sub main {
%s
}
@r = main;
open(o, \">%s\");
print o join(\"\\n\", @r), \"\\n\"")

(defvar org-babel-perl-pp-wrapper-method
  nil)

(defun org-babel-perl-evaluate (session body &optional result-type)
  "Pass BODY to the Perl process in SESSION.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (if (not session)
      ;; external process evaluation
      (save-excursion
        (case result-type
          (output
           (with-temp-buffer
             (insert body)
             ;; (message "buffer=%s" (buffer-string)) ;; debugging
             (org-babel-shell-command-on-region (point-min) (point-max) "perl" 'current-buffer 'replace)
             (buffer-string)))
          (value
           (let* ((tmp-file (make-temp-file "perl-functional-results")) exit-code
		 (stderr
		  (with-temp-buffer
		    (insert
		     (format
		      (if (member "pp" result-params)
			  (error "Pretty-printing not implemented for perl")
			org-babel-perl-wrapper-method)
		      (mapconcat
		       (lambda (line) (format "\t%s" line))
		       (split-string
			(org-remove-indentation (org-babel-trim body)) "[\r\n]") "\n")
		      tmp-file))
		    ;; (message "buffer=%s" (buffer-string)) ;; debugging
		    (setq exit-code
			  (org-babel-shell-command-on-region
			   (point-min) (point-max) "perl" nil 'replace (current-buffer)))
		    (buffer-string))))
	     (if (> exit-code 0) (org-babel-error-notify exit-code stderr))
	     (org-babel-import-elisp-from-file (org-babel-maybe-remote-file tmp-file))))))
    ;; comint session evaluation
    (error "Sessions are not supported for Perl.")))

(provide 'org-babel-perl)
;;; org-babel-perl.el ends here
