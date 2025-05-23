;;; ob-eval.el --- Babel Functions for External Code Evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, comint
;; URL: https://orgmode.org

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These functions build existing Emacs support for executing external
;; shell commands.

;;; Code:

(require 'org-macs)
(org-assert-version)

(eval-when-compile (require 'subr-x))  ; For `string-empty-p', Emacs < 29

(defvar org-babel-error-buffer-name "*Org-Babel Error Output*"
  "The buffer name Org Babel evaluate error output.")
(declare-function org-babel-temp-file "ob-core" (prefix &optional suffix))

(defun org-babel-eval-error-notify (exit-code stderr)
  "Open a buffer to display STDERR and a message with the value of EXIT-CODE.
If EXIT-CODE is nil, display the message without a code."
  (let ((buf (get-buffer-create org-babel-error-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (save-excursion
        (unless (bolp) (insert "\n"))
        (insert stderr)
        (if exit-code
            (insert (format "[ Babel evaluation exited with code %S ]" exit-code))
          (insert "[ Babel evaluation exited abnormally ]"))))
    (display-buffer buf))
  (if exit-code
      (message "Babel evaluation exited with code %S" exit-code)
    (message "Babel evaluation exited abnormally")))

(defun org-babel-eval (command query)
  "Run COMMAND on QUERY.
Return standard output produced by COMMAND.  If COMMAND exits
with a non-zero code or produces error output, show it with
`org-babel-eval-error-notify'.

Writes QUERY into a temp-buffer that is processed with
`org-babel--shell-command-on-region'."
  (let ((error-buffer (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer error-buffer (erase-buffer))
    (with-temp-buffer
      ;; Ensure trailing newline.  It is required for cmdproxy.exe.
      (insert query "\n")
      (setq exit-code
            (org-babel--shell-command-on-region
             command error-buffer))
      (let ((stderr (with-current-buffer error-buffer (buffer-string))))
        (if (or (not (numberp exit-code))
                (> exit-code 0)
                (not (string-empty-p stderr)))
            (progn
              (org-babel-eval-error-notify exit-code stderr)
              (save-excursion
                (when (get-buffer org-babel-error-buffer-name)
                  (with-current-buffer org-babel-error-buffer-name
                    (unless (derived-mode-p 'compilation-mode)
                      (compilation-mode))
                    ;; Compilation-mode enforces read-only, but
                    ;; Babel expects the buffer modifiable.
                    (setq buffer-read-only nil))))
              ;; Return output, if any.
              (buffer-string))
          (buffer-string))))))

(defun org-babel-eval-read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer (insert-file-contents file)
		    (buffer-string)))

(defun org-babel--shell-command-on-region (command error-buffer)
  "Execute COMMAND in an inferior shell with region as input.
Stripped down version of `shell-command-on-region' for internal use in
Babel only.  This lets us work around errors in the original function
in various versions of Emacs.  This expects the query to be run to be
in the current temp buffer.  This is written into
input-file.  ERROR-BUFFER is the name of the file which
`org-babel-eval' has created to use for any error messages that are
returned."

  (let ((input-file (org-babel-temp-file "ob-input-"))
	(error-file (if error-buffer (org-babel-temp-file "ob-error-") nil))
	(shell-file-name (org-babel--get-shell-file-name))
	exit-status)
    ;; we always call this with 'replace, remove conditional
    ;; Replace specified region with output from command.
    (org-babel--write-temp-buffer-input-file input-file)
    (setq exit-status
	  (process-file shell-file-name input-file
			(if error-file
			    (list t error-file)
			  t)
			nil shell-command-switch command))

    (when (and input-file (file-exists-p input-file)
	       ;; bind org-babel--debug-input around the call to keep
	       ;; the temporary input files available for inspection
	       (not (when (boundp 'org-babel--debug-input)
		      org-babel--debug-input)))
      (delete-file input-file))

    (when (and error-file (file-exists-p error-file))
      (when (< 0 (file-attribute-size (file-attributes error-file)))
	(with-current-buffer (get-buffer-create error-buffer)
	  (let ((pos-from-end (- (point-max) (point))))
	    (or (bobp)
		(insert "\f\n"))
	    ;; Do no formatting while reading error file,
	    ;; because that can run a shell command, and we
	    ;; don't want that to cause an infinite recursion.
	    (format-insert-file error-file nil)
	    ;; Put point after the inserted errors.
	    (goto-char (- (point-max) pos-from-end)))
	  (current-buffer)))
      (delete-file error-file))
    exit-status))

(defun org-babel--write-temp-buffer-input-file (input-file)
  "Write the contents of the current temp buffer into INPUT-FILE."
  (let ((start (point-min))
        (end (point-max)))
    (goto-char start)
    (push-mark (point) 'nomsg)
    (write-region start end input-file)
    (delete-region start end)
    (exchange-point-and-mark)))

(defun org-babel-eval-wipe-error-buffer ()
  "Delete the contents of the Org code block error buffer.
This buffer is named by `org-babel-error-buffer-name'."
  (when (get-buffer org-babel-error-buffer-name)
    (with-current-buffer org-babel-error-buffer-name
      (delete-region (point-min) (point-max)))))

(defun org-babel--get-shell-file-name ()
  "Return system `shell-file-name', defaulting to /bin/sh.
Unfortunately, `executable-find' does not support file name
handlers.  Therefore, we could use it in the local case only."
  ;; FIXME: Since Emacs 27, `executable-find' accepts optional second
  ;; argument supporting remote hosts.
  (cond ((and (not (file-remote-p default-directory))
	      (executable-find shell-file-name))
	 shell-file-name)
	((file-executable-p
	  (concat (file-remote-p default-directory) shell-file-name))
	 shell-file-name)
	("/bin/sh")))

(provide 'ob-eval)

;;; ob-eval.el ends here
