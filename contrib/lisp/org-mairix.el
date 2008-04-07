;;; org-mairix.el - Support for hooking mairix search into Org for different MUAs
;;
;; Copyright (C) 2007 Georg C. F. Greve
;;
;; Author: Georg C. F. Greve <greve at fsfeurope dot org>
;; Keywords: outlines, hypermedia, calendar, wp, email, mairix
;; Purpose: Integrate mairix email searching into Org mode
;; See http://orgmode.org and http://www.rpcurnow.force9.co.uk/mairix/
;; Version: 0.4
;;
;; This file is Free Software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USAGE NOTE
;;
;; You will need to configure mairix first, which involves setting up your
;; .mairixrc in your home directory. Once it is working, you should set up
;; your way to display results in your favorite way -- usually a MUA, in my
;; case gnus.
;;
;; After both steps are done, all you should need to hook mairix, org
;; and your MUA together is to do (require 'org-mairix) in your
;; startup file. Everything can then be configured normally through
;; Emacs customisation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

;;; The custom variables

(defgroup org-mairix nil
  "Mairix support/integration in org."
  :tag "Org Mairix"
  :group 'org-links)

(defcustom org-mairix-threaded-links t
  "Should new links be created as threaded links?
If t, links will be stored as threaded searches.
If nil, links will be stored as non-threaded searches."
  :group 'org-mairix
  :type 'boolean)

(defcustom org-mairix-augmented-links nil
  "Should new links be created as augmenting searches?
If t, links will be stored as augmenting searches.
If nil, links will be stored as normal searches.

Attention: When activating this option, you will need
to remove old articles from your mairix results group
in some other way, mairix will not do it for you."
  :group 'org-mairix
  :type 'boolean)

(defcustom org-mairix-display-hook 'org-mairix-gnus-display-results
  "Hook to call to display the results of a successful mairix search.
Defaults to Gnus, feel free to add your own MUAs or methods."
  :group 'org-mairix
  :type 'hook)

(defcustom org-mairix-executable "mairix"
  "The mairix executable to call. If your paths are set up
correctly, you should not need to change this."
  :group 'org-mairix
  :type 'string)

(defgroup org-mairix-gnus nil
  "Use gnus for mairix support in org."
  :tag "Org Mairix Gnus"
  :group 'org-mairix)

(defcustom org-mairix-gnus-results-group "nnmaildir:mairix"
  "The group that is configured to hold the mairix search results,
which needs to be setup independently of the org-mairix integration,
along with general mairix configuration."
  :group 'org-mairix-gnus
  :type 'string)

(defcustom org-mairix-gnus-select-display-group-function 'org-mairix-gnus-select-display-group-function-gg
  "Hook to call to select the group that contains the matching articles.
We should not need this, it is owed to a problem of gnus that people were
not yet able to figure out, see
 http://article.gmane.org/gmane.emacs.gnus.general/65248
 http://article.gmane.org/gmane.emacs.gnus.general/65265
 http://article.gmane.org/gmane.emacs.gnus.user/9596
for reference.

It seems gnus needs a 'forget/ignore everything you think you
know about that group' function. Volunteers?"
  :group 'org-mairix-gnus
  :type 'hook)


;;; The hooks to integrate mairix into org

(org-add-link-type "mairix" 'org-mairix-open)
(add-hook 'org-store-link-functions 'org-mairix-store-link)


;;; Generic org-mairix functions

(defun org-mairix-store-link ()
  "Store a link to the current message as a Mairix search for its
Message ID."

  ;; gnus integration
  (when (memq major-mode '(gnus-summary-mode gnus-article-mode))
    (and (eq major-mode 'gnus-article-mode) (gnus-article-show-summary))
    (let* ((article (gnus-summary-article-number))
	   (header (gnus-summary-article-header article))
	   (from (mail-header-from header))
	   (message-id (mail-header-id header))
	   (subject (gnus-summary-subject-string)))
      (org-store-link-props :type "mairix" :from from :subject subject
			    :message-id message-id)
      (setq cpltxt (org-email-link-description))
      (org-store-link-props :link (concat "mairix:"
					  (if org-mairix-threaded-links "t:")
					  (if org-mairix-augmented-links "a:")
					  "@@" (org-remove-angle-brackets message-id))
			    :description cpltxt))))

(defun org-mairix-message-send-and-exit-with-link ()
  "Function that can be assigned as an alternative sending function,
it sends the message and then stores a mairix link to it before burying
the buffer just like 'message-send-and-exit' does."
  (interactive)
  (message-send)
  (let* ((message-id (message-fetch-field "Message-Id"))
	 (subject (message-fetch-field "Subject"))
	 (link (concat "mairix:"
		       (if org-mairix-threaded-links "t:")
		       (if org-mairix-augmented-links "a:")
		       "@@" (org-remove-angle-brackets message-id)))
	 (desc (concat "Email: '" subject "'")))
    (setq org-stored-links
	  (cons (list link desc) org-stored-links)))
  (message-bury (current-buffer)))

(defun org-mairix-open (path)
  "Function to open mairix link.

We first need to split it into its individual parts, and then
extract the message-id to be passed on to the display function
before call mairix, evaluate the number of matches returned, and
make sure to only call display of mairix succeeded in matching."
  (let* ((cmdline org-mairix-executable))
    (if (string< "t:" path)
	(progn (setq path (substring path 2 nil))
	       (setq cmdline (concat cmdline " --threads"))))
    (if (string< "a:" path)
	(progn (setq path (substring path 2 nil))
	       (setq cmdline (concat cmdline " --augment"))))
    (let* ((message-id (substring path 2 nil)))
      (setq cmdline (concat cmdline " m:" message-id))

      (print cmdline)

      (setq retval (shell-command-to-string
		    (concat cmdline " m:" message-id)))
      (string-match "\[0-9\]+" retval)
      (setq matches (string-to-number (match-string 0 retval)))
      (if (eq matches 0) (message "Link failed: no matches, sorry")
	(message "Link returned %d matches" matches)
	(run-hook-with-args 'org-mairix-display-hook message-id)))))


;;; Functions necessary for gnus integration

(defun org-mairix-gnus-display-results (message-id)
  "Display results of mairix search in Gnus.

Note: This does not work as cleanly as I would like it to. The
problem being that Gnus should simply reread the group cleanly,
without remembering anything. At the moment it seems to be unable
to do that -- so you're likely to see zombies floating around.

If you can improve this, please do!"
  (require 'gnus)
  (require 'gnus-sum)
  ;; FIXME: (bzg/gg) We might need to make sure gnus is running here,
  ;;        and to start it in case it isn't running already. Does
  ;;        anyone know a function to do that? It seems main org mode
  ;;        does not do this, either.
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if gnus-other-frame-object (select-frame gnus-other-frame-object))

  ;; FIXME: This is horribly broken. Please see
  ;;  http://article.gmane.org/gmane.emacs.gnus.general/65248
  ;;  http://article.gmane.org/gmane.emacs.gnus.general/65265
  ;;  http://article.gmane.org/gmane.emacs.gnus.user/9596
  ;; for reference.
  ;;
  ;; It seems gnus needs a "forget/ignore everything you think you
  ;; know about that group" function. Volunteers?
  ;;
  ;; For now different methods seem to work differently well for
  ;; different people. So we're playing hook-selection here to make
  ;; it easy to play around until we found a proper solution.
  (run-hook-with-args 'org-mairix-gnus-select-display-group-function)
  (gnus-summary-select-article
   nil t t (car (gnus-find-matching-articles "message-id" message-id))))

(provide 'org-mairix)

(defun org-mairix-gnus-select-display-group-function-gg ()
  "Georg's hack to select a group that gnus (falsely) believes to be
empty to then call rebuilding of the summary. It leaves zombies of
old searches around, though."
  (gnus-group-quick-select-group 0 org-mairix-gnus-results-group)
  (gnus-group-clear-data)
  (gnus-summary-reselect-current-group t t))

(defun org-mairix-gnus-select-display-group-function-bzg ()
  "This is the classic way the org mode is using, and it seems to be
using better for Bastien, so it may work for you."
  (gnus-group-clear-data org-mairix-gnus-results-group)
  (gnus-group-read-group t nil org-mairix-gnus-results-group))

;;; org-mairix.el ends here
