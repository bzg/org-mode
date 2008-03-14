;;; org-wl.el - Support for links to Wanderlust messages in Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 1.0
;;
;; This file is part of GNU Emacs.
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

;; This file implements links to Wanderlust messages for Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

(require 'org)

;; Declare external functions and variables
(declare-function elmo-folder-exists-p "ext:elmo" (folder) t)
(declare-function elmo-message-entity-field "ext:elmo-msgdb"
		  (entity field &optional type))
(declare-function elmo-message-field "ext:elmo"
		  (folder number field &optional type) t)
(declare-function elmo-msgdb-overview-get-entity "ext:elmo" (&rest unknown) t)
;; Backward compatibility to old version of wl
(declare-function wl-summary-buffer-msgdb "ext:wl-folder" (&rest unknown) t)
(declare-function wl-folder-get-elmo-folder "ext:wl-folder"
		  (entity &optional no-cache))
(declare-function wl-summary-goto-folder-subr "ext:wl-summary"
		  (&optional name scan-type other-window sticky interactive
			     scoring force-exit))
(declare-function wl-summary-jump-to-msg-by-message-id "ext:wl-summary"
		  (&optional id))
(declare-function wl-summary-line-from "ext:wl-summary" ())
(declare-function wl-summary-line-subject "ext:wl-summary" ())
(declare-function wl-summary-message-number "ext:wl-summary" ())
(declare-function wl-summary-redisplay "ext:wl-summary" (&optional arg))
(defvar wl-summary-buffer-elmo-folder)
(defvar wl-summary-buffer-folder-name)

;; Install the link type
(org-add-link-type "wl" 'org-wl-open)
(add-hook 'org-store-link-functions 'org-wl-store-link)

;; Implementation
(defun org-wl-store-link ()
  "Store a link to an WL folder or message."
  (when (eq major-mode 'wl-summary-mode)
    (let* ((msgnum (wl-summary-message-number))
	   (message-id (elmo-message-field wl-summary-buffer-elmo-folder
					   msgnum 'message-id))
	   (wl-message-entity
	    (if (fboundp 'elmo-message-entity)
		(elmo-message-entity
		 wl-summary-buffer-elmo-folder msgnum)
	      (elmo-msgdb-overview-get-entity
	       msgnum (wl-summary-buffer-msgdb))))
	   (from (wl-summary-line-from))
	   (to (car (elmo-message-entity-field wl-message-entity 'to)))
	   (subject (let (wl-thr-indent-string wl-parent-message-entity)
		      (wl-summary-line-subject)))
	   desc link)
      (org-store-link-props :type "wl" :from from :to to
			    :subject subject :message-id message-id)
      (setq message-id (org-remove-angle-brackets message-id))
      (setq desc (org-email-link-description))
      (setq link (org-make-link "wl:" wl-summary-buffer-folder-name
				"#" message-id))
      (org-add-link-props :link link :description desc))))

(defun org-wl-open (path)
  "Follow an WL message link."
  (let (folder article)
    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	(error "Error in Wanderlust link"))
    (setq folder (match-string 1 path)
	  article (match-string 3 path))
    (org-wl-follow-link folder article)))

(defun org-wl-follow-link (folder article)
  "Follow a Wanderlust link to FOLDER and ARTICLE."
  (if (and (string= folder "%")
	   article
	   (string-match "^\\([^#]+\\)\\(#\\(.*\\)\\)?" article))
      ;; XXX: imap-uw supports folders starting with '#' such as "#mh/inbox".
      ;; Thus, we recompose folder and article ids.
      (setq folder (format "%s#%s" folder (match-string 1 article))
	    article (match-string 3 article)))
  (if (not (elmo-folder-exists-p (wl-folder-get-elmo-folder folder)))
      (error "No such folder: %s" folder))
  (wl-summary-goto-folder-subr folder 'no-sync t nil t nil nil)
  (and article
       (wl-summary-jump-to-msg-by-message-id (org-add-angle-brackets article))
       (wl-summary-redisplay)))

(provide 'org-wl)

;;; org-wl.el ends here
