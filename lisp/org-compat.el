;;; org-compat.el --- Compatibility Code for Older Emacsen -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2016 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains code needed for compatibility with older
;; versions of GNU Emacs.

;;; Code:

(require 'cl-lib)
(require 'org-macs)

;; As of Emacs 25.1, `outline-mode' functions are under the 'outline-'
;; prefix, `find-tag' is replaced with `xref-find-definition' and
;; `x-get-selection' with `gui-get-selection'.
(when (< emacs-major-version 25)
  (defalias 'outline-hide-entry 'hide-entry)
  (defalias 'outline-hide-sublevels 'hide-sublevels)
  (defalias 'outline-hide-subtree 'hide-subtree)
  (defalias 'outline-show-all 'show-all)
  (defalias 'outline-show-branches 'show-branches)
  (defalias 'outline-show-children 'show-children)
  (defalias 'outline-show-entry 'show-entry)
  (defalias 'outline-show-subtree 'show-subtree)
  (defalias 'xref-find-definitions 'find-tag)
  (defalias 'format-message 'format))

(eval-when-compile
  (when (< emacs-major-version 25)
    (defalias 'gui-get-selection 'x-get-selection)))


;;; Obsolete aliases

(define-obsolete-function-alias 'org-activate-mark 'activate-mark)

;;;; XEmacs compatibility, now removed.
(define-obsolete-function-alias 'org-add-hook 'add-hook "Org 9.0")
(define-obsolete-function-alias 'org-decompose-region 'decompose-region "Org 9.0")
(define-obsolete-function-alias 'org-defvaralias 'defvaralias "Org 9.0")
(define-obsolete-function-alias 'org-detach-overlay 'delete-overlay "Org 9.0")
(define-obsolete-function-alias 'org-file-equal-p 'file-equal-p "Org 9.0")
(define-obsolete-function-alias 'org-float-time 'float-time "Org 9.0")
(define-obsolete-function-alias 'org-indent-line-to 'indent-line-to "Org 9.0")
(define-obsolete-function-alias 'org-indent-to-column 'indent-to-column "Org 9.0")
(define-obsolete-function-alias 'org-looking-at-p 'looking-at-p "Org 9.0")
(define-obsolete-function-alias 'org-looking-back 'looking-back "Org 9.0")
(define-obsolete-function-alias 'org-match-string-no-properties 'match-string-properties "Org 9.0")
(define-obsolete-function-alias 'org-propertize 'propertize "Org 9.0")
(define-obsolete-function-alias 'org-select-frame-set-input-focus 'select-frame-set-input-focus "Org 9.0")

(defmacro org-re (s)
  "Replace posix classes in regular expression."
  (declare (debug (form)))
  s)
(make-obsolete 'org-re "It is now a no-op.  Please remove it altogether." "Org 9.0")

;;;; Functions from cl-lib that Org used to have its own implementation of.
(define-obsolete-function-alias 'org-count 'cl-count "Org 9.0")
(define-obsolete-function-alias 'org-every 'cl-every "Org 9.0")
(define-obsolete-function-alias 'org-find-if 'cl-find-if "Org 9.0")
(define-obsolete-function-alias 'org-reduce 'cl-reduce "Org 9.0")
(define-obsolete-function-alias 'org-remove-if 'cl-remove-if "Org 9.0")
(define-obsolete-function-alias 'org-remove-if-not 'cl-remove-if-not "Org 9.0")
(define-obsolete-function-alias 'org-some 'cl-some "Org 9.0")
(define-obsolete-function-alias 'org-floor* 'cl-floor "Org 9.0")

;;;; Functions available since Emacs 24.3
(define-obsolete-function-alias 'org-char-to-string 'char-to-string "Org 9.0")
(define-obsolete-function-alias 'org-buffer-narrowed-p 'buffer-narrowed-p "Org 9.0")
(define-obsolete-function-alias 'org-delete-directory 'delete-directory "Org 9.0")
(define-obsolete-function-alias 'org-number-sequence 'number-sequence "Org 9.0")
(define-obsolete-function-alias 'org-pop-to-buffer-same-window 'pop-to-buffer-same-window "Org 9.0")
(define-obsolete-function-alias 'org-string-match-p 'string-match-p "Org 9.0")

;;;; Variables declared obsolete.
(define-obsolete-variable-alias 'org-hierarchical-checkbox-statistics
  'org-checkbox-hierarchical-statistics "Org 8.0")
(define-obsolete-variable-alias 'org-description-max-indent
  'org-list-description-max-indent "Org 8.0")

(defun org-compatible-face (inherits specs)
  "Make a compatible face specification.
If INHERITS is an existing face and if the Emacs version supports it,
just inherit the face.  If INHERITS is set and the Emacs version does
not support it, copy the face specification from the inheritance face.
If INHERITS is not given and SPECS is, use SPECS to define the face."
  (when (and inherits (facep inherits) (not specs))
    (setq specs (or specs
		    (get inherits 'saved-face)
		    (get inherits 'face-defface-spec))))
  (cond
   ((and inherits (facep inherits)
	 (>= emacs-major-version 22)
	 ;; do not inherit outline faces before Emacs 23
	 (or (>= emacs-major-version 23)
	     (not (string-match "\\`outline-[0-9]+"
				(symbol-name inherits)))))
    (list (list t :inherit inherits)))
   ((< emacs-major-version 22)
    ;; These do not understand the `min-colors' attribute.
    (let (r e a)
      (while (setq e (pop specs))
	(cond
	 ((memq (car e) '(t default)) (push e r))
	 ((setq a (member '(min-colors 8) (car e)))
	  (nconc r (list (cons (cons '(type tty) (delq (car a) (car e)))
			       (cdr e)))))
	 ((setq a (assq 'min-colors (car e)))
	  (setq e (cons (delq a (car e)) (cdr e)))
	  (or (assoc (car e) r) (push e r)))
	 (t (or (assoc (car e) r) (push e r)))))
      (nreverse r)))
   (t specs)))
(put 'org-compatible-face 'lisp-indent-function 1)

(defun org-version-check (version feature level)
  (let* ((v1 (mapcar 'string-to-number (split-string version "[.]")))
	 (v2 (mapcar 'string-to-number (split-string emacs-version "[.]")))
	 (rmaj (or (nth 0 v1) 99))
	 (rmin (or (nth 1 v1) 99))
	 (rbld (or (nth 2 v1) 99))
	 (maj (or (nth 0 v2) 0))
	 (min (or (nth 1 v2) 0))
	 (bld (or (nth 2 v2) 0)))
    (if (or (< maj rmaj)
	    (and (= maj rmaj)
		 (< min rmin))
	    (and (= maj rmaj)
		 (= min rmin)
		 (< bld rbld)))
	(if (eq level :predicate)
	    ;; just return if we have the version
	    nil
	  (let ((msg (format "Emacs %s or greater is recommended for %s"
			     version feature)))
	    (display-warning 'org msg level)
	    t))
      t)))


;;; Miscellaneous functions

(defun org-get-x-clipboard (value)
  "Get the value of the X or Windows clipboard."
  (cond ((eq window-system 'x)
	 (org-no-properties
	  (ignore-errors
	    (or (gui-get-selection value 'UTF8_STRING)
		(gui-get-selection value 'COMPOUND_TEXT)
		(gui-get-selection value 'STRING)
		(gui-get-selection value 'TEXT)))))
	((and (eq window-system 'w32) (fboundp 'w32-get-clipboard-data))
	 (w32-get-clipboard-data))))

(defun org-add-props (string plist &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties
   0 (length string) (if props (append plist props) plist) string)
  string)
(put 'org-add-props 'lisp-indent-function 2)

(defun org-fit-window-to-buffer (&optional window max-height min-height
					   shrink-only)
  "Fit WINDOW to the buffer, but only if it is not a side-by-side window.
WINDOW defaults to the selected window.  MAX-HEIGHT and MIN-HEIGHT are
passed through to `fit-window-to-buffer'.  If SHRINK-ONLY is set, call
`shrink-window-if-larger-than-buffer' instead, the height limit is
ignored in this case."
  (cond ((if (fboundp 'window-full-width-p)
	     (not (window-full-width-p window))
	   ;; do nothing if another window would suffer
	   (> (frame-width) (window-width window))))
	((and (fboundp 'fit-window-to-buffer) (not shrink-only))
	 (fit-window-to-buffer window max-height min-height))
	((fboundp 'shrink-window-if-larger-than-buffer)
	 (shrink-window-if-larger-than-buffer window)))
  (or window (selected-window)))

;; `set-transient-map' is only in Emacs >= 24.4
(defalias 'org-set-transient-map
  (if (fboundp 'set-transient-map)
      'set-transient-map
    'set-temporary-overlay-map))

;;; Region compatibility

(defvar org-ignore-region nil
  "Non-nil means temporarily disable the active region.")

(defun org-region-active-p ()
  "Is `transient-mark-mode' on and the region active?"
  (if org-ignore-region
      nil
    (if (fboundp 'use-region-p)
	(use-region-p)
      (and transient-mark-mode mark-active)))) ; Emacs 22 and before

(defun org-cursor-to-region-beginning ()
  (when (and (org-region-active-p)
	     (> (point) (region-beginning)))
    (exchange-point-and-mark)))

;;; Invisibility compatibility

(defun org-remove-from-invisibility-spec (arg)
  "Remove elements from `buffer-invisibility-spec'."
  (if (fboundp 'remove-from-invisibility-spec)
      (remove-from-invisibility-spec arg)
    (if (consp buffer-invisibility-spec)
	(setq buffer-invisibility-spec
	      (delete arg buffer-invisibility-spec)))))

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?"
  (if (consp buffer-invisibility-spec)
      (member arg buffer-invisibility-spec)))

(defun org-move-to-column (column &optional force _buffer)
  "Move to column COLUMN.
Pass COLUMN and FORCE to `move-to-column'."
  (let ((buffer-invisibility-spec
	 (if (listp buffer-invisibility-spec)
	     (remove '(org-filtered) buffer-invisibility-spec)
	   buffer-invisibility-spec)))
    (move-to-column column force)))

(defmacro org-find-library-dir (library)
  `(file-name-directory (or (locate-library ,library) "")))

(defun org-count-lines (s)
  "How many lines in string S?"
  (let ((start 0) (n 1))
    (while (string-match "\n" s start)
      (setq start (match-end 0) n (1+ n)))
    (if (and (> (length s) 0) (= (aref s (1- (length s))) ?\n))
	(setq n (1- n)))
    n))

(defun org-kill-new (string &rest args)
  (remove-text-properties 0 (length string) '(line-prefix t wrap-prefix t)
			  string)
  (apply 'kill-new string args))

;; `font-lock-ensure' is only available from 24.4.50 on
(defalias 'org-font-lock-ensure
  (if (fboundp 'font-lock-ensure)
      #'font-lock-ensure
    (lambda (&optional _beg _end)
      (with-no-warnings (font-lock-fontify-buffer)))))

(defmacro org-no-popups (&rest body)
  "Suppress popup windows.
Let-bind some variables to nil around BODY to achieve the desired
effect, which variables to use depends on the Emacs version."
  (if (org-version-check "24.2.50" "" :predicate)
      `(let (pop-up-frames display-buffer-alist)
	 ,@body)
    `(let (pop-up-frames special-display-buffer-names special-display-regexps special-display-function)
       ,@body)))

;;;###autoload
(defmacro org-check-version ()
  "Try very hard to provide sensible version strings."
  (let* ((org-dir        (org-find-library-dir "org"))
	 (org-version.el (concat org-dir "org-version.el"))
	 (org-fixup.el   (concat org-dir "../mk/org-fixup.el")))
    (if (require 'org-version org-version.el 'noerror)
	'(progn
	   (autoload 'org-release     "org-version.el")
	   (autoload 'org-git-version "org-version.el"))
      (if (require 'org-fixup org-fixup.el 'noerror)
	  '(org-fixup)
	;; provide fallback definitions and complain
	(warn "Could not define org version correctly.  Check installation!")
	'(progn
	   (defun org-release () "N/A")
	   (defun org-git-version () "N/A !!check installation!!"))))))

(defmacro org-with-silent-modifications (&rest body)
  (if (fboundp 'with-silent-modifications)
      `(with-silent-modifications ,@body)
    `(org-unmodified ,@body)))
(def-edebug-spec org-with-silent-modifications (body))

;; Remove this when support for Emacs < 24.4 is dropped.
(defun org-define-error (name message)
  "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such
an error is signaled without being caught by a `condition-case'.
Implements `define-error' for older emacsen."
  (if (fboundp 'define-error) (define-error name message)
    (put name 'error-conditions
	 (copy-sequence (cons name (get 'error 'error-conditions))))))

(provide 'org-compat)

;;; org-compat.el ends here
