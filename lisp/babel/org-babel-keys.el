;;; org-babel-keys.el --- key bindings for org-babel

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

;; Add some org-babel keybindings to the org-mode keymap for exposing
;; org-babel functions.  These will all share the common C-c M-b
;; prefix.  See the value of `org-babel-key-bindings' for a list of
;; interactive functions and their associated keys.

;;; Code:
(require 'org-babel)

(defvar org-babel-key-prefix "\C-c\C-v"
  "The `org-babel-key-prefix' variable holds the key prefix
behind which all org-babel interactive key-binding are placed.
See `org-babel-key-bindings' for the list of interactive babel
functions which are assigned key bindings, and see
`org-babel-map' for the actual babel keymap.")

(defvar org-babel-map (make-sparse-keymap)
  "The keymap holding key bindings for interactive org-babel
functions.")

(define-key org-mode-map org-babel-key-prefix org-babel-map)

(defun org-babel-describe-bindings ()
  "Describe all key binding placed behind the
`org-babel-key-prefix' prefix."
  (interactive)
  (describe-bindings org-babel-key-prefix))

(defvar org-babel-key-bindings
  '(("\C-p" . org-babel-expand-src-block)
    ("p" . org-babel-expand-src-block)
    ("g" . org-babel-goto-named-source-block)
    ("\C-b" . org-babel-execute-buffer)
    ("b" . org-babel-execute-buffer)
    ("\C-s" . org-babel-execute-subtree)
    ("s" . org-babel-execute-subtree)
    ("\C-t" . org-babel-tangle)
    ("t" . org-babel-tangle)
    ("\C-f" . org-babel-tangle-file)
    ("f" . org-babel-tangle-file)
    ("\C-l" . org-babel-lob-ingest)
    ("l" . org-babel-lob-ingest)
    ("\C-z" . org-babel-switch-to-session)
    ("z" . org-babel-switch-to-session)
    ("\C-a" . org-babel-sha1-hash)
    ("a" . org-babel-sha1-hash)
    ("h" . org-babel-describe-bindings))
  "Alist associating key bindings with interactive Org-babel
functions.  This list associates interactive org-babel functions
with keys.  Each element of this list will add an entry to the
`org-babel-map' using the letter key which is the `car' of the
a-list placed behind the generic `org-babel-key-prefix'.")

(mapc (lambda (pair)
        (define-key org-babel-map (car pair) (cdr pair)))
      org-babel-key-bindings)

(provide 'org-babel-keys)
;;; org-babel-keys.el ends here
