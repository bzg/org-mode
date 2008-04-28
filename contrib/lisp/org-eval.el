;;; org-eval.el --- Display result of evaluating code in various languanges
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.01
;;
;; This file is not yet part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

(require 'org)

;;; Customization

(defgroup org-eval nil
  "Options concerning global entry identifiers in Org-mode."
  :tag "Org ID"
  :group 'org)

(defface org-eval
  (org-compatible-face nil
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey20"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey80"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face for fixed-with text like code snippets."
  :group 'org-eval
  :group 'org-faces
  :version "22.1")

(defun org-eval-handle-snippets (limit &optional replace)
  (let (a)
    (while (setq a (text-property-any (point) (or limit (point-max))
				      'org-eval t))
      (remove-text-properties
       a (next-single-property-change a 'org-eval nil limit)
       '(display t intangible t org-eval t))))
  (while (re-search-forward "<\\(lisp\\)>\\([^\000]+?\\)</\\1>" limit t)
    (let* ((beg (match-beginning 0))
	   (end (match-end 0))
	   (kind (match-string 1))
	   (code (match-string 2))
	   (value (org-eval-code kind code)))
      (if replace
	  (replace-match value t t)
	(add-text-properties
	 beg end
	 (list 'display value 'intangible t 'font-lock-multiline t
	       'face 'org-eval
	       'org-eval t))))))

(defun org-eval-replace-snippts ()
  "Replace EVAL snippets in the entire buffer.
This should go into the `org-export-preprocess-hook'."
  (goto-char (point-min))
  (org-eval-handle-snippets nil 'replace))

(add-hook 'org-export-preprocess-hook 'org-eval-replace-snippts)
(add-hook 'org-font-lock-hook 'org-eval-handle-snippets)

(defun org-eval-code (interpreter code)
  (cond
   ((equal interpreter "lisp")
    (org-eval-lisp (concat "(progn\n" code "\n)")))
   (t (error "Cannot evaluate code type %s" interpreter))))

(defun org-eval-lisp (form)
  "Evaluate the given form and return the result as a string."
  (require 'pp)
  (save-match-data
    (condition-case err
        (let ((object (eval (read form))))
          (cond
           ((stringp object) object)
           ((and (listp object)
                 (not (eq object nil)))
            (let ((string (pp-to-string object)))
              (substring string 0 (1- (length string)))))
           ((numberp object)
            (number-to-string object))
           ((eq object nil) "")
           (t
            (pp-to-string object))))
      (error
       (org-display-warning (format "%s: Error evaluating %s: %s"
                                     "???" form err))
       "; INVALID LISP CODE"))))

(defun org-display-warning (message)
  "Display the given MESSAGE as a warning."
  (if (fboundp 'display-warning)
      (display-warning 'org message
                       (if (featurep 'xemacs)
                           'warning
                         :warning))
    (let ((buf (get-buffer-create "*Org warnings*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "Warning (Org): " message)
        (unless (bolp)
          (newline)))
      (display-buffer buf)
      (sit-for 0))))

(provide 'org-eval)

;;; org-eval.el ends here

