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

(defvar org-babel-comint-buffer nil
  "the buffer currently in use")

(defun org-babel-comint-set-buffer (buffer)
  (setq org-babel-comint-buffer buffer))

(defun org-babel-ensure-buffer-livep ()
  (unless (and (buffer-live-p org-babel-comint-buffer) (get-buffer org-babel-comint-buffer) (get-buffer-process org-babel-comint-buffer))
    (error "`org-babel-comint-buffer' doesn't exist or has no process")))

(defmacro org-babel-comint-in-buffer (&rest body)
  `(save-window-excursion
     (save-match-data
       (org-babel-ensure-buffer-livep)
       (set-buffer org-babel-comint-buffer)
       ,@body)))

(defun org-babel-comint-wait-for-output ()
  "Wait until output arrives"
  (org-babel-comint-in-buffer
   (while (progn
            (goto-char comint-last-input-end)
            (not (re-search-forward comint-prompt-regexp nil t)))
     (accept-process-output (get-buffer-process org-babel-comint-buffer)))))

(defun org-babel-comint-input-command (cmd)
  "Pass CMD to `org-babel-comint-buffer'"
  (org-babel-comint-in-buffer
   (goto-char (process-mark (get-buffer-process org-babel-comint-buffer)))
   (insert cmd)
   (comint-send-input)
   (org-babel-comint-wait-for-output)))

(defun org-babel-comint-command-to-string (buffer cmd)
  (let ((buffer (org-babel-comint-buffer-buffer buffer)))
    (org-babel-comint-input-command buffer cmd)
    (org-babel-comint-last-value buffer)))

(defun org-babel-comint-last-value ()
  "Return the last value passed to BUFFER"
  (org-babel-comint-in-buffer
   (goto-char (process-mark (get-buffer-process org-babel-comint-buffer)))
   (forward-line 0)
   (org-babel-clean-text-properties (buffer-substring comint-last-input-end (- (point) 1)))))

;;; debugging functions
(defun org-babel-comint-pmark ()
  (org-babel-comint-in-buffer (comint-goto-process-mark) (point)))

;;; org-babel-comint.el ends here
