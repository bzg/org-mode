;;; org-babel-ui.el --- UI elements for org-babel

;; Copyright (C) 2009 Eric Schulte, Dan Davison, Austin F. Frank

;; Author: Eric Schulte, Dan Davison, Austin F. Frank
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

;; UI elements of org-babel
;; - code folding
;; - marking working code blocks

;;; Code:
(require 'org-babel)

(defun org-babel-ui-src-block-cycle-maybe ()
  "Detect if this is context for a org-babel src-block and if so
then run `org-babel-execute-src-block'."
  (let ((case-fold-search t))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at org-babel-src-block-regexp))
        (progn (call-interactively 'org-babel-ui-src-block-cycle)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

(defun org-babel-ui-src-block-cycle ()
  "Cycle the visibility of the current source code block"
  (interactive)
  ;; should really do this once in an (org-mode hook)
  (add-to-invisibility-spec '(org-babel-ui . t))
  (message "trying out source block")
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward org-babel-src-block-regexp nil t)
        (let ((start (- (match-beginning 4) 1)) ;; beginning of body
              (end (match-end 0))) ;; end of entire body
          (if (memq t (mapcar (lambda (overlay)
                                (eq (overlay-get overlay 'invisible) 'org-babel-ui))
                              (overlays-at start)))
              (remove-overlays start end 'invisible 'org-babel-ui)
            (overlay-put (make-overlay start end) 'invisible 'org-babel-ui)))
      (error "not looking at a source block"))))

;; org-tab-after-check-for-cycling-hook
(add-hook 'org-tab-first-hook 'org-babel-ui-src-block-cycle-maybe)

(provide 'org-babel-ui)
;;; org-babel-ui ends here
