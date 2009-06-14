;;; org-babel-lob.el --- The Library of Babel: off-the-shelf functions for data analysis and plotting using org-babel

;; Copyright (C) 2009 Dan Davison, Eric Schulte

;; Author: Dan Davison, Eric Schulte
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

;; See org-babel.org in the parent directory for more information

;;; Code:
(require 'org-babel)
(require 'org-babel-table)

(defvar org-babel-library-of-babel nil
  "Library of source-code blocks.  This is an association list.
Populate the library by adding files to `org-babel-lob-files'.")

(defcustom org-babel-lob-files '()
  "Files used to populate the `org-babel-library-of-babel'.  To
add files to this list use the `org-babel-lob-ingest' command."
  :group 'org-babel
  :type 'list)

(defun org-babel-lob-ingest (&optional file)
  "Add all source-blocks defined in FILE to `org-babel-library-of-babel'."
  (interactive "f")
  (org-babel-map-source-blocks file
    (let ((source-name (intern (org-babel-get-src-block-name)))
          (info (org-babel-get-src-block-info)))
      (setq org-babel-library-of-babel
            (cons (cons source-name info)
                  (assq-delete-all source-name org-babel-library-of-babel))))))

(org-babel-lob-ingest ;; actually add the source-blocks defined in library-of-babel.org
 (expand-file-name "library-of-babel.org"
                   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))

;; functions for executing lob one-liners

(defvar org-babel-lob-one-liner-regexp
  "#\\+lob:\\([^ \t\n\r]+\\)\\([ \t]+\\([^\n]+\\)\\)?\n")

(defun org-babel-lob-execute-maybe ()
  "Detect if this is context for a org-babel Library Of Babel
src-block and if so then run the appropriate source block from
the Library."
  (interactive)
  (let ((info (org-babel-lob-get-info)))
    (if info (progn (org-babel-lob-execute info) t) nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-lob-execute-maybe)

(defun org-babel-lob-get-info ()
  "Return the information of the current Library of Babel line as
a list of the following form.

  (source-block-name header-arguments-alist)"
  (let ((case-fold-search t))
    (save-excursion
      (move-beginning-of-line 1)
      (if (looking-at org-babel-lob-one-liner-regexp)
          (cons (org-babel-clean-text-properties (match-string 1))
                (delq nil (mapcar (lambda (assignment)
                                    (save-match-data
                                      (if (string-match "\\(.+\\)=\\(.+\\)" assignment)
                                          (list (org-babel-clean-text-properties (match-string 1 assignment))
                                                (org-babel-clean-text-properties (match-string 2 assignment)))
                                        nil)))
                                  (split-string (match-string 3)))))))))

(defun org-babel-lob-execute (info)
  (let ((params (org-babel-parse-header-arguments
                       (concat ":var results="
                               (car info)
                               "("
                               (mapconcat (lambda (var-spec)
                                            (format "%s=%s" (first var-spec) (second var-spec)))
                                          (cdr info) ", ")
                               ")"))))
    (org-babel-execute-src-block t (list "emacs-lisp" "results" params))))

(provide 'org-babel-lob)
;;; org-babel-lob.el ends here
