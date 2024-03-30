;;; org-mark-ring.el --- Org mark ring                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library implements Org mark ring.

;;; Code:

(require 'org-macs)
(org-assert-version)

(declare-function org-fold-show-context "org-fold" (&optional key))

(defcustom org-mark-ring-length 4
  "Number of different positions to be recorded in the ring.
Changing this requires a restart of Emacs to work correctly."
  :group 'org-link-follow
  :type 'integer)

(defvar org-mark-ring nil
  "Mark ring for positions before jumps in Org mode.")

(defvar org-mark-ring-last-goto nil
  "Last position in the mark ring used to go back.")

;; Fill and close the ring
(setq org-mark-ring nil)
(setq org-mark-ring-last-goto nil) ;in case file is reloaded

(dotimes (_ org-mark-ring-length) (push (make-marker) org-mark-ring))
(setcdr (nthcdr (1- org-mark-ring-length) org-mark-ring)
	org-mark-ring)

(defun org-mark-ring-push (&optional pos buffer)
  "Put the current position into the mark ring and rotate it.
Also push position into the Emacs mark ring.  If optional
argument POS and BUFFER are not nil, mark this location instead."
  (interactive)
  (let ((pos (or pos (point)))
	(buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (org-with-point-at pos (push-mark nil t)))
    (setq org-mark-ring (nthcdr (1- org-mark-ring-length) org-mark-ring))
    (move-marker (car org-mark-ring) pos buffer))
  (message
   (substitute-command-keys
    "Position saved to mark ring, go back with `\\[org-mark-ring-goto]'.")))

(defun org-mark-ring-goto (&optional n)
  "Jump to the previous position in the mark ring.
With prefix arg N, jump back that many stored positions.  When
called several times in succession, walk through the entire ring.
Org mode commands jumping to a different position in the current file,
or to another Org file, automatically push the old position onto the ring."
  (interactive "p")
  (let (p m)
    (if (eq last-command this-command)
	(setq p (nthcdr n (or org-mark-ring-last-goto org-mark-ring)))
      (setq p org-mark-ring))
    (setq org-mark-ring-last-goto p)
    (setq m (car p))
    (pop-to-buffer-same-window (marker-buffer m))
    (goto-char m)
    (when (or (org-invisible-p) (org-invisible-p2))
      (require 'org-fold)
      (org-fold-show-context 'mark-goto))))

(provide 'org-mark-ring)

;;; org-mark-ring.el ends here
