;;; litorgy-ui.el --- UI elements for litorgy

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

;; UI elements of litorgy
;; - code folding
;; - marking working code blocks

;;; Code:
(require 'litorgy)

(defadvice org-cycle (around litorgy-ui-org-cycle-src-block activate)
  "Intercept calls to org-cycle to toggle the visibility of a source code block."
  (if (save-excursion
          (beginning-of-line 1)
          (looking-at litorgy-src-block-regexp))
      (litorgy-ui-src-block-cycle)
    ad-do-it))

(defun litorgy-ui-src-block-cycle ()
  "Cycle the visibility of the current source code block"
  (interactive)
  ;; should really do this once in an (org-mode hook)
  (add-to-invisibility-spec '(litorgy-ui . t))
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward litorgy-src-block-regexp nil t)
        (let ((start (- (match-beginning 4) 1)) ;; beginning of body
              (end (match-end 0))) ;; end of entire body
          (if (memq t (mapcar (lambda (overlay)
                                (eq (overlay-get overlay 'invisible) 'litorgy-ui))
                              (overlays-at start)))
              (remove-overlays start end 'invisible 'litorgy-ui)
            (overlay-put (make-overlay start end) 'invisible 'litorgy-ui)))
      (error "not looking at source code block"))))

(provide 'litorgy-ui)
;;; litorgy-ui ends here
