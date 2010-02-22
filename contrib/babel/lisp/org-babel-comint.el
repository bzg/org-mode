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

;; These functions build on comint to ease the sending and receiving
;; of commands and results from comint buffers.
;;
;; Note that the buffers in this file are analogous to sessions in
;; org-babel at large.

;;; Code:
(require 'org-babel)
(require 'comint)

(defun org-babel-comint-buffer-livep (buffer)
  (let ((buffer (if buffer (get-buffer buffer))))
    (and buffer (buffer-live-p buffer) (get-buffer-process buffer) buffer)))

(defmacro org-babel-comint-in-buffer (buffer &rest body)
  "Check BUFFER with `org-babel-comint-buffer-livep' then execute
body inside the protection of `save-window-excursion' and
`save-match-data'."
  (declare (indent 1))
  `(save-excursion
     (save-match-data
       (unless (org-babel-comint-buffer-livep ,buffer)
         (error (format "buffer %s doesn't exist or has no process" ,buffer)))
       (set-buffer ,buffer)
       ,@body)))

(defmacro org-babel-comint-with-output (buffer eoe-indicator remove-echo &rest body)
  "Evaluate BODY in BUFFER, wait until EOE-INDICATOR appears in
output, then return all process output.  This ensures that the
filter is removed in case of an error or user `keyboard-quit'
during execution of body."
  (declare (indent 3))
  `(org-babel-comint-in-buffer ,buffer
     (let ((string-buffer ""))
       (flet ((my-filt (text) (setq string-buffer (concat string-buffer text))))
         ;; setup filter
         (add-hook 'comint-output-filter-functions 'my-filt)
         (unwind-protect
             (progn
               ;; pass FULL-BODY to process
               (goto-char (process-mark (get-buffer-process (current-buffer))))
               ,@body
               ;; wait for end-of-evaluation indicator
               (while (progn
                        (goto-char comint-last-input-end)
                        (not (save-excursion
			       (and (re-search-forward comint-prompt-regexp nil t)
				    (re-search-forward (regexp-quote ,eoe-indicator) nil t)))))
                 (accept-process-output (get-buffer-process (current-buffer)))
                 ;; ;; thought this would allow async background running, but I was wrong...
                 ;; (run-with-timer .5 .5 'accept-process-output (get-buffer-process (current-buffer)))
                 ))
           ;; remove filter
           (remove-hook 'comint-output-filter-functions 'my-filt)))
       ;; remove echo'd FULL-BODY from input
       (if (and ,remove-echo
		(string-match
		 (replace-regexp-in-string "\n" "\r\n" (regexp-quote ,full-body)) string-buffer))
           (setq raw (substring string-buffer (match-end 0))))
       (split-string string-buffer comint-prompt-regexp))))

(defun org-babel-comint-input-command (buffer cmd)
  "Pass CMD to BUFFER  The input will not be echoed."
  (org-babel-comint-in-buffer buffer
    (goto-char (process-mark (get-buffer-process buffer)))
    (insert cmd)
    (comint-send-input)
    (org-babel-comint-wait-for-output buffer)))

(defun org-babel-comint-wait-for-output (buffer)
  "Wait until output arrives.  Note: this is only safe when
waiting for the result of a single statement (not large blocks of
code)."
  (org-babel-comint-in-buffer buffer
    (while (progn
             (goto-char comint-last-input-end)
             (not (and (re-search-forward comint-prompt-regexp nil t)
                       (goto-char (match-beginning 0))
                       (string= (face-name (face-at-point))
                                "comint-highlight-prompt"))))
      (accept-process-output (get-buffer-process buffer)))))

(provide 'org-babel-comint)
;;; org-babel-comint.el ends here
