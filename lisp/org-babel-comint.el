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

(defvar org-babel-comint-output-buffer nil
  "this is a string to buffer output, it should be set buffer local")

(defvar org-babel-comint-output-ring nil
  "ring to hold comint output")

(defvar org-babel-comint-output-ring-size 10
  "number of output to be help")

(defun org-babel-comint-init (buffer)
  "Initialize a buffer to use org-babel-comint."
  (save-excursion
    (set-buffer buffer)
    (set (make-local-variable 'org-babel-comint-output-buffer) "")
    ))

(defun org-babel-comint-buffer-livep (buffer)
  (and (buffer-live-p buffer) (get-buffer buffer) (get-buffer-process buffer)))

(defmacro org-babel-comint-in-buffer (buffer &rest body)
  `(save-window-excursion
     (save-match-data
       (unless (org-babel-comint-buffer-livep buffer)
         (error (format "buffer %s doesn't exist or has no process" buffer)))
       (set-buffer buffer)
       ,@body)))

(defun org-babel-comint-wait-for-output (buffer)
  "Wait until output arrives"
  (org-babel-comint-in-buffer buffer
   (while (progn
            (goto-char comint-last-input-end)
            (not (re-search-forward comint-prompt-regexp nil t)))
     (accept-process-output (get-buffer-process buffer)))))

(defun org-babel-comint-input-command (buffer cmd)
  "Pass CMD to BUFFER  The input will not be echoed."
  (org-babel-comint-in-buffer buffer
   (goto-char (process-mark (get-buffer-process buffer)))
   (insert cmd)
   (comint-send-input)
   (org-babel-comint-wait-for-output buffer)))

(defun org-babel-comint-command-to-output (buffer cmd)
  "Pass CMD to BUFFER using `org-babel-comint-input-command', and
then return the result as a string using
`org-babel-comint-last-value'."
  (org-babel-comint-input-command buffer cmd)
  (org-babel-comint-last-value buffer))

(defun org-babel-comint-command-to-last (buffer cmd)
  "Pass CMD to BUFFER using `org-babel-comint-input-command', and
then return the result as a string using
`org-babel-comint-last-value'."
  (org-babel-comint-input-command buffer cmd)
  (org-babel-comint-last-value buffer))

(defun org-babel-comint-last-value (buffer)
  "Return the last comint output in BUFFER as a string."
  (org-babel-comint-in-buffer buffer
   (goto-char (process-mark (get-buffer-process buffer)))
   (forward-line 0)
   (org-babel-clean-text-properties
    (buffer-substring (+ comint-last-input-end
                         ;; because comint insists on echoing input
                         (- comint-last-input-end
                            comint-last-input-start))
                      (- (point) 1)))))

;; output filter
;;
;; This will collect output, stripping away echo'd inputs, splitting
;; it by `comint-prompt-regexp', then sticking it into the
;; `org-babel-comint-output-ring'.
(defun org-babel-comint-hook ()
  (set (make-local-variable 'org-babel-comint-output-buffer) "")
  (set (make-local-variable 'org-babel-comint-output-ring) (make-ring 10)))

(add-hook 'comint-mode-hook 'org-babel-comint-hook)

(defun org-babel-comint-output-filter (text)
  "Filter the text of org-babel-comint"
  (setq org-babel-comint-output-buffer (concat org-babel-comint-output-buffer text))
  (let ((holder (split-string org-babel-comint-output-buffer comint-prompt-regexp)))
    (when (> (length holder) 1)
      (mapc (lambda (output) (ring-insert org-babel-comint-output-ring (org-babel-chomp output)))
            (butlast holder))
      (setq org-babel-comint-output-buffer (or (cdr (last holder)) "")))))

(add-hook 'comint-output-filter-functions 'org-babel-comint-output-filter)

;; debugging functions

(defun org-babel-show-output-buffer ()
  (interactive)
  (message org-babel-comint-output-buffer))

(defun org-babel-show-output-ring-size ()
  (interactive)
  (message (format "ring is %d" (ring-size org-babel-comint-output-ring))))

(defun org-babel-show-ring ()
  (interactive)
  (message (format "%S" (ring-elements org-babel-comint-output-ring))))

(provide 'org-babel-comint)
;;; org-babel-comint.el ends here
