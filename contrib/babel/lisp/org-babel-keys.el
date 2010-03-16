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

(defvar org-babel-key-prefix "\C-c\M-b"
  "Prefix behind which all org-babel interactive key-binding will
be placed.  See `org-babel-key-bindings' for the list of
interactive babel functions which are assigned key bindings.")

(defvar org-babel-key-bindings
  '(("t" . org-babel-tangle)
    ("T" . org-babel-tangle-file)
    ("e" . org-babel-execute-src-block)
    ("s" . org-babel-execute-subtree)
    ("b" . org-babel-execute-buffer)
    ("h" . org-babel-sha1-hash)
    ("g" . org-babel-goto-named-source-block)
    ("l" . org-babel-lob-ingest)
    ("z" . org-babel-switch-to-session))
  "Org-babel keybindings.  This list associates interactive
org-babel functions with keys.  Each element of this list will
add an entry to the `org-mode-map' using the letter key which is
the `car' of the a-list placed behind the generic
`org-babel-key-prefix'.")

(mapc (lambda (pair)
        (message "%S" pair)
        (define-key org-mode-map
          (concat org-babel-key-prefix (car pair))
          (cdr pair)))
      org-babel-key-bindings)

(provide 'org-babel-keys)
;;; org-babel-keys.el ends here
