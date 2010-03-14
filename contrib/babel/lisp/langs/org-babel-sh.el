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

(defvar org-babel-sh-command "sh"
  "Command used to invoke a shell.  This will be passed to
  `shell-command-on-region'")

(defun org-babel-execute:sh (body params)
  "Execute a block of Shell commands with org-babel.  This
function is called by `org-babel-execute-src-block'."
  (message "executing Shell source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-sh-initiate-session (first processed-params)))
         (vars (second processed-params))
         (result-type (fourth processed-params))
         (sep (cdr (assoc :separator params)))
         (full-body (concat
                     (mapconcat ;; define any variables
                      (lambda (pair)
                        (format "%s=%s"
                                (car pair)
                                (org-babel-sh-var-to-sh (cdr pair) sep)))
                      vars "\n") "\n" body "\n\n"))) ;; then the source block body
    (org-babel-sh-evaluate session full-body result-type)))

(defun org-babel-prep-session:sh (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-sh-initiate-session session))
         (vars (org-babel-ref-variables params))
         (sep (cdr (assoc :separator params)))
         (var-lines (mapcar ;; define any variables
                     (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (org-babel-sh-var-to-sh (cdr pair) sep)))
                     vars)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:sh (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:sh session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-sh-var-to-sh (var &optional sep)
  "Convert an elisp var into a string of shell commands
specifying a var of the same value."
  (if (listp var)
      (flet ((deep-string (el)
                          (if (listp el)
                              (mapcar #'deep-string el)
                           (org-babel-sh-var-to-sh el sep))))
       (format "$(cat <<BABEL_TABLE\n%s\nBABEL_TABLE\n)"
                (orgtbl-to-generic (deep-string var) (list :sep (or sep "\t")))))
    (if (stringp var) (format "%s" var) (format "%S" var))))

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

(defun org-babel-sh-initiate-session (&optional session params)
  (unless (string= session "none")
    (save-window-excursion
      (or (org-babel-comint-buffer-livep session)
          (progn (shell session) (get-buffer (current-buffer)))))))

(defvar org-babel-sh-eoe-indicator "echo 'org_babel_sh_eoe'"
  "Used to indicate that evaluation is has completed.")
(defvar org-babel-sh-eoe-output "org_babel_sh_eoe"
  "Used to indicate that evaluation is has completed.")

(defun org-babel-sh-evaluate (session body &optional result-type)
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
          (org-babel-shell-command-on-region (point-min) (point-max) org-babel-sh-command 'current-buffer 'replace)
	  (case result-type
	    (output (buffer-string))
	    (value ;; TODO: figure out how to return non-output values from shell scripts
	     (let ((tmp-file (make-temp-file "org-babel-sh"))
		   (results (buffer-string)))
	       (with-temp-file tmp-file (insert results))
	       (org-babel-import-elisp-from-file tmp-file))))))
    ;; comint session evaluation
    (flet ((strip-empty (lst)
                        (delq nil (mapcar (lambda (el) (unless (= (length el) 0) el)) lst))))
      (let ((tmp-file (make-temp-file "org-babel-sh"))
            (results
             (cdr (member
                   org-babel-sh-eoe-output
                   (strip-empty
                    (reverse
                     (mapcar #'org-babel-sh-strip-weird-long-prompt
                             (mapcar #'org-babel-trim
                                     (org-babel-comint-with-output
                                         session org-babel-sh-eoe-output t
                                       (mapc (lambda (line) (insert line) (comint-send-input))
                                             (strip-empty (split-string body "\n")))
                                       (insert org-babel-sh-eoe-indicator)
                                       (comint-send-input))))))))))
        ;; (message (replace-regexp-in-string
        ;;           "%" "%%" (format "processed-results=%S" results))) ;; debugging
        (or (and results
                 (case result-type
                   (output (org-babel-trim (mapconcat #'org-babel-trim
                                                      (reverse results) "\n")))
                   (value (with-temp-file tmp-file
                            (insert (car results)) (insert "\n"))
                          (org-babel-import-elisp-from-file tmp-file))))
            "")))))

(defun org-babel-sh-strip-weird-long-prompt (string)
  (while (string-match "^% +[\r\n$]+ *" string)
    (setq string (substring string (match-end 0))))
  string)

(provide 'org-babel-sh)
;;; org-babel-sh.el ends here
