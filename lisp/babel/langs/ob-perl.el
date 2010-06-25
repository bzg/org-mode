;;; ob-perl.el --- org-babel functions for perl evaluation

;; Copyright (C) 2009, 2010  Free Software Foundation

;; Author: Dan Davison, Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating perl source code.

;;; Code:
(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("perl" . "pl"))

(defvar org-babel-default-header-args:perl '())

(defun org-babel-expand-body:perl (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "$%s=%s;"
                (car pair)
                (org-babel-perl-var-to-perl (cdr pair))))
      vars "\n") "\n" (org-babel-trim body) "\n")))

(defun org-babel-execute:perl (body params)
  "Execute a block of Perl code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Perl source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (first processed-params))
         (vars (nth 1 processed-params))
         (result-params (nth 2 processed-params))
         (result-type (nth 3 processed-params))
         (full-body (org-babel-expand-body:perl
                     body params processed-params)) ;; then the source block body
	(session (org-babel-perl-initiate-session session)))
    (org-babel-reassemble-table
     (org-babel-perl-evaluate session full-body result-type)
     (org-babel-pick-name (nth 4 processed-params) (cdr (assoc :colnames params)))
     (org-babel-pick-name (nth 5 processed-params) (cdr (assoc :rownames params))))))

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
			org-babel-perl-wrapper-method) body tmp-file))
		    (setq exit-code
			  (org-babel-shell-command-on-region
			   (point-min) (point-max) "perl" nil 'replace (current-buffer)))
		    (buffer-string))))
	     (if (> exit-code 0) (org-babel-error-notify exit-code stderr))
	     (org-babel-import-elisp-from-file (org-babel-maybe-remote-file tmp-file))))))
    ;; comint session evaluation
    (error "Sessions are not supported for Perl.")))

(provide 'ob-perl)

;; arch-tag: 88ef9396-d857-4dc3-8946-5a72bdfa2337

;;; ob-perl.el ends here
