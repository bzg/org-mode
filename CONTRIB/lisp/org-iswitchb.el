;;; org-iswitchb.el --- use iswitchb to select Org buffer
;;
;; Copyright 2007 2008 Bastien Guerry
;;
;; Author: bzg AT altern DOT org
;; Version: 0.1
;; Keywords: Org buffer
;; URL: http://www.cognition.ens.fr/~guerry/u/org-iswitchb.el
;;
;; This file is NOT part of GNU Emacs.
;;
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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-iswitchb)
;;
;;; Code:

(defun org-iswitchb (&optional arg)
  "Use `iswitchb-read-buffer' to prompt for an Org buffer to switch to.
With a prefix argument, restrict available to files.
With two prefix arguments, restrict available buffers to agenda files.

Due to some yet unresolved reason, global function
`iswitchb-mode' needs to be active for this function to work."
  (interactive "P")
  (eval-when-compile
    (require 'iswitchb))
  (let ((enabled iswitchb-mode) blist)
    (or enabled (iswitchb-mode 1))
    (setq blist (cond ((equal arg '(4)) (org-buffer-list 'files))
		      ((equal arg '(16)) (org-buffer-list 'agenda))
		      (t (org-buffer-list))))
   (unwind-protect
       (let ((iswitchb-make-buflist-hook
	      (lambda ()
		(setq iswitchb-temp-buflist
		      (mapcar 'buffer-name blist)))))
	 (switch-to-buffer
	  (iswitchb-read-buffer
	   "Switch-to: " nil t))
	 (or enabled (iswitchb-mode -1))))))

(defun org-buffer-list (&optional predicate tmp)
  "Return a list of Org buffers.
PREDICATE can be either 'export, 'files or 'agenda.

'export restrict the list to Export buffers.
'files  restrict the list to buffers visiting Org files.
'agenda restrict the list to buffers visiting agenda files.

If TMP is non-nil, don't include temporary buffers."
  (let (filter blist)
    (setq filter 
	  (cond ((eq predicate 'files) "\.org$")
		((eq predicate 'export) "\*Org .*Export")
		(t "\*Org \\|\.org$")))
    (setq blist
	  (mapcar 
	   (lambda(b)
	     (let ((bname (buffer-name b))
		   (bfile (buffer-file-name b)))
	       (if (and (string-match filter bname)
			(if (eq predicate 'agenda)
			    (member bfile
				    (mapcar (lambda(f) (file-truename f))
					    org-agenda-files)) t)
			(if tmp (not (string-match "tmp" bname)) t)) b)))
	   (buffer-list)))
    (delete nil blist)))

(provide 'org-iswitchb)

;;;  User Options, Variables

;;; org-iswitchb.el ends here
