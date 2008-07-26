;;; org-indent.el --- Dynamic indentation for  Org-mode
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.07
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
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

;; This is an experimental implementation of dynamic virtual indentation.
;; It works by adding overlays to a buffer to make sure lines are
;; indented according to outline structure.  While this works, there are
;; problems with the implementation:  It uses overlays, which use markers,
;; and for large files, this is using too much resources.  It might be
;; possible to com up with an implementation using text properties,
;; I believe this is less resource intensive.  However, it does not work
;; to put the text property onto the newline, because that interferes with
;; outline visibility.  Maybe this is a bug in outline?

;;; Indentation

(defcustom org-startup-indented nil
  "Non-nil means, turn on `org-indent-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: localindent
   #+STARTUP: indent
   #+STARTUP: noindent"
  :group 'org-structure
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Locally" local)
	  (const :tag "Globally (slow on startup in large files)" t)))

(defconst org-indent-max 80
  "Maximum indentation in characters")
(defconst org-indent-strings nil
  "Vector with all indentation strings.
It is a const because it will be set only once in `org-indent-initialize'.")
(defvar org-indent-inhibit-after-change nil
  "Inhibit the action of the indentation after-change-hook.
This variable should be scoped dynamically.")
(defcustom org-indent-boundary-char ?\   ; comment to protect space char
  "The end of the virtual indentation strings, a single-character string.
The default is just a space, but if you wish, you can use \"|\" or so."
  :group 'org-structure
  :set (lambda (var val)
	 (set var val)
	 (and org-indent-strings (org-indent-initialize)))
  :type 'character)

(defun org-indent-initialize ()
  "Initialize the indentation strings and set the idle times."
  (unless org-indent-strings
;    (run-with-idle-timer 10   t 'org-indent-indent-buffer)
    (run-with-idle-timer  0.5  t 'org-indent-refresh-section)
    )
  (setq org-indent-strings (make-vector (1+ org-indent-max) nil))
  ;; Initialize the indentation strings
  (aset org-indent-strings 0 "")
  (loop for i from 1 to org-indent-max do
	(aset org-indent-strings i
	      (org-add-props 
		  (concat (make-string (1- i) ?\ )
			  (char-to-string org-indent-boundary-char))
		  nil 'face 'org-indent))))

(define-minor-mode org-indent-mode
  "Toggle the minor more `org-indent-mode'."
  nil " Ind" nil
  (if (org-bound-and-true-p org-inhibit-startup)
      (setq org-indent-mode nil)
    (if org-indent-mode
	(progn
	  (or org-indent-strings (org-indent-initialize))
	  (org-set-local 'org-adapt-indentation nil)
	  (org-add-hook 'after-change-functions
			'org-indent-after-change-function nil t)
	  (org-restart-font-lock)
	  (org-indent-indent-buffer))
      (save-excursion
	(save-restriction
	  (org-indent-remove-overlays (point-min) (point-max))
	  (remove-hook 'after-change-functions
		       'org-indent-after-change-function 'local)
	  (kill-local-variable 'org-adapt-indentation))))))

(defface org-indent
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background dark)
	(:underline nil :strike-through nil :foreground "grey10")))
      (((class color) (min-colors 16) (background light))
       (:underline nil :strike-through nil :foreground "grey90"))
      (t (:underline nil :strike-through nil))))
  "Face for outline indentation.
The default is to make it look like whitespace.  But you may find it
useful to make it, for example, look like the fringes."
  :group 'org-faces)

(defun org-indent-indent-buffer ()
  "Add indentation overlays for the whole buffer."
  (interactive)
  (when org-indent-mode
    (save-excursion
      (save-restriction
	(org-indent-remove-overlays (point-min) (point-max))
	(org-indent-add-overlays (point-min) (point-max))))))

(defun org-indent-remove-overlays (beg end)
  "Remove indentations between BEG and END."
  (mapc (lambda (o)
	  (and (org-overlay-get o 'org-indent)
	       (org-delete-overlay o)))
	(org-overlays-in beg end)))

(defun org-indent-add-overlays (beg end &optional n)
  "Add indentation overlays between BEG and END.
Assumes that BEG is at the beginning of a line."
  (when org-indent-mode
    (let (o)
      (save-excursion
	(goto-char beg)
	(while (and (<= (point) end) (not (eobp)))
	  (cond
	   ((not (bolp)))
	   ((looking-at outline-regexp)
	    (setq n (- (match-end 0) (match-beginning 0)))
	    (org-indent-remove-overlays (max (point-min) (1- (point))) (point)))
	   (n
	    (org-indent-indent-line n)))
	  (beginning-of-line 2))))))

(defun org-indent-indent-line (n)
  "Add an indentation overlay with width N to the current line.
Point is assumed to be at the beginning of the line for this."
  (let (ov)
    (setq ov (org-make-overlay (1- (point)) (point)))
    (org-overlay-put ov 'after-string (aref org-indent-strings n))
    (org-overlay-put ov 'evaporate t)
    (org-overlay-put ov 'org-indent n)
    (org-unmodified
     (put-text-property (max (point-min) (1- (point)))
			(point-at-eol) 'org-indent-level n))))

(defun org-indent-after-change-function (beg end ndel)
  (if (or (not org-indent-mode) (= beg end)
	  org-indent-inhibit-after-change)
      () ; not in the mood to do anything here....
    (let ((inhibit-quit t) n)
      (save-match-data
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char beg)
	    (when (search-forward "\n" end t)
	      ;; a newline was inserted
	      (setq n (or (get-text-property beg 'org-indent-level)
			  (get-text-property
			   (or (save-excursion (previous-single-property-change
						beg 'org-indent-level))
			       (point-min))
			   'org-indent-level)
			  0))
	      (org-indent-local-refresh beg end n))))))))

(defun org-indent-local-refresh (beg end n)
  "Refresh indentation locally from BEG to END, starting with indentation N."
  (goto-char end)
  (setq end (min (point-max) (1+ (point-at-eol))))
  (goto-char beg)
  (beginning-of-line 0)
  (org-indent-remove-overlays (max (point-min) (1- (point))) end)
  (org-indent-add-overlays (point) end n))

(defun org-indent-refresh-section ()
  "Refresh indentation overlays in the current outline subtree."
  (when org-indent-mode
    (save-excursion
      (let ((org-indent-inhibit-after-change t)
	    beg end)
	(condition-case nil
	    (progn
	      (outline-back-to-heading t)
	      (setq beg (point)))
	  (error (progn
		   (goto-char (point-min))
		   (setq beg (point)))))
	(outline-next-heading)
	(setq end (point))
	(org-indent-remove-overlays beg end)
	(org-indent-add-overlays beg end)))))

(defun org-indent-refresh-subtree ()
  "Refresh indentation overlays in the current outline subtree."
  (when org-indent-mode
    (save-excursion
      (let ((org-indent-inhibit-after-change t)
	    beg end)
	(setq beg (point))
	(setq end (save-excursion (org-end-of-subtree t t)))
	(org-indent-remove-overlays beg end)
	(org-indent-add-overlays beg end)))))

(provide 'org-indent)

;;; org-indent.el ends here
