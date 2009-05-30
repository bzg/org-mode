;;; org-babel-comint.el --- org-babel functions for interaction with comint buffers

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, comint
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

;; This file should provide support for passing commands and results
;; to and from `comint-mode' buffers.

;;; Code:
(require 'org-babel)

(defun org-babel-comint-initiate-buffer (buffer ignite)
  "If BUFFER does not exist and currently have a process call
IGNITE from within BUFFER."
  (unless (and (buffer-live-p buffer) (get-buffer buffer) (get-buffer-process buffer))
    (save-excursion
      (get-buffer-create buffer)
      (funcall ignite)
      (setf buffer (current-buffer))
      (org-babel-comint-wait-for-output)
      (org-babel-comint-input-command ""))))

(defun org-babel-comint-command-to-string (buffer command)
  "Send COMMAND to BUFFER's process, and return the results as a string."
  (org-babel-comint-input-command buffer command)
  (org-babel-comint-last-output buffer))

(defun org-babel-comint-input-command (buffer command)
  "Pass COMMAND to the process running in BUFFER."
  (save-excursion
    (save-match-data
      (set-buffer buffer)
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (insert command)
      (comint-send-input)
      (org-babel-comint-wait-for-output))))

(defun org-babel-comint-wait-for-output (buffer)
  "Wait until output arrives"
  (save-excursion
    (save-match-data
      (set-buffer buffer)
      (while (progn
	       (goto-char comint-last-input-end)
	       (not (re-search-forward comint-prompt-regexp nil t)))
	(accept-process-output (get-buffer-process (current-buffer)))))))

(defun org-babel-comint-last-output (buffer)
  "Return BUFFER's the last output as a string"
  (save-excursion
    (save-match-data
      (set-buffer buffer)
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let ((raw (buffer-substring comint-last-input-end (- (point) 1)))
            output output-flag)
        (mapconcat
         (lambda (el)
           (if (stringp el)
               (format "%s" el)
             (format "%S" el)))
         (delq nil
               (mapcar
                (lambda (line)
                  (unless (string-match "^>" line)
                    (and (string-match "\\[[[:digit:]]+\\] *\\(.*\\)$" line)
                         (match-string 1 line))))
                ;; drop first, because it's the last line of input
                (cdr (split-string raw "[\n\r]")))) "\n")))))

;;; org-babel-comint.el ends here
