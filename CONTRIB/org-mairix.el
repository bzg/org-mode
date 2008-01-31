;;; org-mairix.el --- 

;; Copyright 2007 Bastien Guerry
;;
;; Author: Bastien.Guerry@ens.fr
;; Version: $Id: org-mairix.el,v 0.0 2007/08/11 17:23:40 guerry Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;;; Commentary:

;; Code and ideas from Carsten Dominik, Adam Spiers and Georg C. F. Greve.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-mairix)

;;; Code:

(require 'org)

(defgroup org-mairix nil
  "Mairix link support for Org."
  :tag "Org Mairix"
  :group 'org)

(defcustom mairix-results-group "nnmaildir+index:mfolder"
  "Gnus groupe where to list mairix search results."
  :group 'org-mairix
  :type '(string))

(defun org-add-link-type (type &optional follow publish)
  "Add TYPE to the list of `org-link-types'.
Re-compute all regular expressions depending on `org-link-types'."
  (add-to-list 'org-link-types type t)
  (setq org-link-re-with-space
	(concat
	 "<?\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^" org-non-link-chars "]*"
	 "[^" org-non-link-chars " ]\\)>?"))
  (setq org-link-re-with-space2
	(concat
	 "<?\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^]\t\n\r]*"
	 "[^" org-non-link-chars " ]\\)>?"))
  (setq org-angle-link-re
	(concat
	 "<\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^" org-non-link-chars "]*"
	 "\\)>"))
  (setq org-plain-link-re
	(concat
	 "\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^]\t\n\r<>,;() ]+\\)"))
  (setq org-bracket-link-analytic-regexp
	(concat
	 "\\[\\["
	 "\\(\\(" (mapconcat 'identity org-link-types "\\|") "\\):\\)?"
	 "\\([^]]+\\)"
	 "\\]"
	 "\\(\\[" "\\([^]]+\\)" "\\]\\)?"
	 "\\]"))
  (add-hook 'org-follow-link-functions follow)
  (add-hook 'org-publish-link-functions publish))

(defun org-mairix-follow-link (path)
  "Follow a Mairix link."
  (require 'gnus)
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if gnus-other-frame-object (select-frame gnus-other-frame-object))
  (mairix-search path))

(defun org-mairix-publish-link (path)
  "Convert mairix PATH into a (dummy) raw link."
  ;; FIXME: should we have a format argument for HTML/LaTeX publishing?
  (if (string-match org-bracket-link-analytic-regexp path)
    (match-string 5 path) path))

(defun org-mairix-store-link (path)
  "Store a mairix link."
  (when (memq major-mode '(gnus-summary-mode gnus-article-mode))
    (let* ((group gnus-newsgroup-name)
	   (article (gnus-summary-article-number))
	   (header (gnus-summary-article-header article))
	   (from (mail-header-from header))
	   (message-id (mail-header-id header))
	   (date (mail-header-date header))
	   (subject (gnus-summary-subject-string)))
      (org-store-link-props :type "mairix" 
			    :from from 
			    :subject subject
			    :message-id message-id 
			    :group group)
;; FIXME: what about cpltxt and link vars we used so far?
;;       (setq cpltxt (org-email-link-description))
;;       (setq link (org-make-link "mairix:m:" 
;; 				(substring message-id 1 -1))))))
      (org-make-link "mairix:m:" (substring message-id 1 -1)))))

;; mairix internals
(defun mairix-result-evaluate (string)
  "Display search results of previous mairix process."
  (let ((mmatches (string-to-number (substring string 7 -8))))
    (if (eq mmatches 0)
	(message "Mairix returned no matches, sorry.")
      (message "Mairix returned %d matches." mmatches)
      (gnus-group-quick-select-group 0 mairix-results-group)
      (gnus-summary-reselect-current-group t t))))


(org-add-link-type "mairix"
		   'org-mairix-follow-link 
		   'org-mairix-publish-link)

(add-hook 'org-store-link-functions 'org-mairix-store-link)

(defun mairix-search (string)
  "Uses mairix to search through my mail, replacing current search results."
  (interactive "MMairix search: ")
  (mairix-result-evaluate
	(shell-command-to-string (concat "mairix " string))))

(provide 'org-mairix)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





;;; org-mairix.el ends here
