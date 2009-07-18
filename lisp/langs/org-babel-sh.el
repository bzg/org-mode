;;; org-babel-sh.el --- org-babel functions for shell evaluation

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

;; Org-Babel support for evaluating shell source code.

;;; Code:
(require 'org-babel)
(require 'shell)

(org-babel-add-interpreter "sh")

(add-to-list 'org-babel-tangle-langs '("sh" "sh" "#!/usr/bin/env sh"))

(defun org-babel-execute:sh (body params)
  "Execute a block of Shell commands with org-babel.  This
function is called by `org-babel-execute-src-block' via multiple-value-bind."
  (message "executing Shell source code block")
  (let* ((full-body (concat
                     (mapconcat ;; define any variables
                      (lambda (pair)
                        (format "%s=%s"
                                (car pair)
                                (org-babel-sh-var-to-sh (cdr pair))))
                      vars "\n") "\n" body "\n\n")) ;; then the source block body
         (session (org-babel-sh-initiate-session session)))
    (org-babel-sh-evaluate session full-body result-type)))

(defun org-babel-prep-session:sh (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-sh-initiate-session session))
         (vars (org-babel-ref-variables params))
         (var-lines (mapcar ;; define any variables
                     (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (org-babel-sh-var-to-sh (cdr pair))))
                     vars)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))))

;; helper functions

(defun org-babel-sh-var-to-sh (var)
  "Convert an elisp var into a string of shell commands
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-sh-var-to-sh var ", ") "]")
    (format "%S" var)))

(defun org-babel-sh-table-or-results (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (string-match "^\\[.+\\]$" results)
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               ", " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

(defvar org-babel-sh-buffers '(:default . nil))

(defun org-babel-sh-session-buffer (session)
  (cdr (assoc session org-babel-sh-buffers)))

(defun org-babel-sh-initiate-session-by-key (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (sh-buffer (org-babel-sh-session-buffer session))
           (newp (not (org-babel-comint-buffer-livep sh-buffer))))
      (if (and sh-buffer (get-buffer sh-buffer) (not (buffer-live-p sh-buffer)))
          (setq sh-buffer nil))
      (shell sh-buffer)
      (when newp
        (setq sh-buffer (current-buffer))
        (org-babel-comint-wait-for-output sh-buffer))
      (setq org-babel-sh-buffers (cons (cons session sh-buffer)
				       (assq-delete-all session org-babel-sh-buffers)))
      session)))

(defun org-babel-sh-initiate-session (&optional session)
  (unless (string= session "none")
    (org-babel-sh-session-buffer (org-babel-sh-initiate-session-by-key session))))

(defvar org-babel-sh-eoe-indicator "echo 'org_babel_sh_eoe'"
  "Used to indicate that evaluation is has completed.")
(defvar org-babel-sh-eoe-output "org_babel_sh_eoe"
  "Used to indicate that evaluation is has completed.")

(defun org-babel-sh-evaluate (buffer body &optional result-type)
  "Pass BODY to the Shell process in BUFFER.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY."
  (if (not session)
      ;; external process evaluation
      (save-window-excursion
        (with-temp-buffer
          (insert body)
          ;; (message "buffer=%s" (buffer-string)) ;; debugging
          (shell-command-on-region (point-min) (point-max) "sh" 'replace)
	  (case result-type
	    (output (buffer-string))
	    (value ;; TODO: figure out how to return non-output values from shell scripts
	     (let ((tmp-file (make-temp-file "org-babel-sh"))
		   (results (buffer-string)))
	       (with-temp-file tmp-file (insert results))
	       (org-babel-import-elisp-from-file tmp-file))))))
    ;; comint session evaluation
    (let* ((tmp-file (make-temp-file "org-babel-sh"))
	   (full-body (mapconcat #'org-babel-chomp
                                 (list body org-babel-sh-eoe-indicator) "\n"))
           (raw (org-babel-comint-with-output buffer org-babel-sh-eoe-output nil
                  (insert full-body) (comint-send-input nil t)))
           (results (cdr (member org-babel-sh-eoe-output
                                 (reverse (mapcar #'org-babel-sh-strip-weird-long-prompt
                                                  (mapcar #'org-babel-trim raw)))))))
      ;; (message (replace-regexp-in-string "%" "%%" (format "processed-results=%S" results))) ;; debugging
      (or (case result-type
            (output (org-babel-trim (mapconcat #'org-babel-trim (reverse results) "\n")))
            (value (with-temp-file tmp-file (insert (car results)))
		   (org-babel-import-elisp-from-file tmp-file)))) "")))

(defun org-babel-sh-strip-weird-long-prompt (string)
  (while (string-match "^% +[\r\n$]+ *" string)
    (setq string (substring string (match-end 0))))
  string)

(provide 'org-babel-sh)
;;; org-babel-sh.el ends here
