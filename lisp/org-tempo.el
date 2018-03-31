;;; org-tempo.el --- Template expansion for Org structures -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.
;;
;; Author: Rasmus Pank Roulund <emacs at pank dot eu>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Org Tempo reimplements completions of structure template before
;; point like `org-try-structure-completion' in Org v9.1 and earlier.
;; For example, strings like "<e" at the beginning of the line will be
;; expanded to an example block.
;;
;; All blocks defined in `org-structure-template-alist' are added as
;; Org Tempo shortcuts, in addition to keywords defined in
;; `org-tempo-keywords-alist'.
;;
;; `tempo' can also be used to define more sophisticated keywords
;; completions.  See the section "Additional keywords" below for
;; additional details.
;;
;;; Code:

(require 'tempo)
(require 'cl-lib)
(require 'org)

(defvar org-structure-template-alist)


(defgroup org-tempo nil
  "Options for template expansion of Org structures"
  :tag "Org structure"
  :group 'org)

(defvar org-tempo-tags nil
  "Tempo tags for Org mode")

(defcustom org-tempo-keywords-alist
  '((?L . "latex")
    (?H . "html")
    (?A . "ascii")
    (?i . "index"))
  "Keyword completion elements.

Like `org-structure-template-alist' this alist of KEY characters
and KEYWORD.  The tempo snippet \"<KEY\" is expand to the KEYWORD
value.

For example \"<l\" at the beginning of a line is expanded to
#+latex:"
  :group 'org-tempo
  :type '(repeat (cons (character :tag "Key")
		       (string :tag "Keyword")))
  :package-version '(Org . "9.2"))



;;; Org Tempo functions and setup.

(defun org-tempo-setup ()
  (org-tempo-add-templates)
  (tempo-use-tag-list 'org-tempo-tags)
  (setq-local tempo-match-finder "^ *\\(<[[:word:]]\\)\\="))

(defun org-tempo-add-templates ()
  "Update all Org Tempo templates.

Goes through `org-structure-template-alist' and
`org-tempo-keywords-alist'."
  (let ((keys (mapcar (lambda (pair) (format "<%c" (car pair)))
		      (append org-structure-template-alist
			      org-tempo-keywords-alist))))
    ;; Check for duplicated snippet keys and warn if any are found.
    (when (> (length keys) (length (delete-dups keys)))
      (warn
       "Duplicated keys in `org-structure-template-alist' and `org-tempo-keywords-alist'"))

    ;; Remove any keys already defined in case they have been updated.
    (setq org-tempo-tags
	  (cl-remove-if (lambda (tag) (member (car tag) keys)) org-tempo-tags))
    (mapc #'org-tempo-add-block org-structure-template-alist)
    (mapc #'org-tempo-add-keyword org-tempo-keywords-alist)))

(defun org-tempo-add-block (entry)
  "Add block entry from `org-structure-template-alist'."
  (let* ((key (format "<%c" (car entry)))
	 (name (cdr entry)))
    (tempo-define-template (format "org-%s" (replace-regexp-in-string " " "-" name))
			   `(,(format "#+begin_%s " name) p '> n n
			     ,(format "#+end_%s" (car (split-string name " ")))
			     >)
			   key
			   (format "Insert a %s block" name)
			   'org-tempo-tags)))

(defun org-tempo-add-keyword (entry)
  "Add keyword entry from `org-tempo-keywords-alist'."
  (let* ((key (format "<%c" (car entry)))
	 (name (cdr entry)))
    (tempo-define-template (format "org-%s" (replace-regexp-in-string " " "-" name))
			   `(,(format "#+%s: " name) p '>)
			   key
			   (format "Insert a %s keyword" name)
			   'org-tempo-tags)))

(defun org-tempo-complete-tag (&rest _)
  "Look for a tag and expand it silently.
Unlike to `tempo-complete-tag', do not give a signal if a partial
completion or no match at all is found.  Return nil if expansion
didn't succeed."
  ;; `tempo-complete-tag' returns its SILENT argument when there is no
  ;; completion available at all.
  (not (eq 'fail (tempo-complete-tag 'fail))))

;;; Additional keywords

(defun org-tempo--include-file ()
  "Ask for file name and take care of quit"
  (let ((inhibit-quit t))
    (unless (with-local-quit
	      (prog1 t
		(insert
		 (format "#+include: %S "
			 (file-relative-name
			  (read-file-name "Include file: "))))))
      (insert "<I")
      (setq quit-flag nil))))

(tempo-define-template "org-include"
		       '((org-tempo--include-file)
			 p >)
		       "<I"
		       "Include keyword"
		       'org-tempo-tags)


;;; Setup of Org Tempo
;;
;; Org Tempo is set up with each new Org buffer and potentially in the
;; current Org buffer.

(add-hook 'org-mode-hook 'org-tempo-setup)
(add-hook 'org-tab-before-tab-emulation-hook 'org-tempo-complete-tag)

(org-tempo-add-templates)

;; Enable Org Tempo in all open Org buffers.
(dolist (b (org-buffer-list 'files))
  (with-current-buffer b (org-tempo-setup)))

(provide 'org-tempo)

;;; org-tempo.el ends here
