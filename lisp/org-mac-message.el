;;; org-mac-message.el --- Support for links to Apple Mail messages from within Org-mode

;; Copyright (C) 2008, 2009 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;;         Christopher Suckling <suckling at gmail dot com>

;; Version: 6.25
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

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

;;; Commentary:
;; This file implements links to Apple Mail messages from within Org-mode.
;; Org-mode does not load this module by default - if you would actually like
;; this to happen then configure the variable `org-modules'.

;; If you would like to create links to all flagged messages in an
;; Apple Mail account, please customize the variable
;; org-mac-mail-account and then call one of the following functions:

;; (org-mac-create-flagged-mail) copies a formatted list of links to
;; the kill ring.

;; (org-mac-insert-flagged-mail) searches within an org-mode buffer
;; for a specific heading, creating it if it doesn't exist. Any
;; message:// links within the first level of the heading are deleted
;; and replaced with links to flagged messages.

;; If you have Growl installed and would like more visual feedback
;; whilst AppleScript searches for messages, please uncomment lines
;; 125 to 130.

;;; Code:

(require 'org)

(defgroup org-mac-flagged-mail nil
  "Options concerning linking to flagged Mail.app messages"
  :tag "Org Mail.app"
  :group 'org-link)

(defcustom org-mac-mail-account "customize"
  "The Mail.app account in which to search for flagged messages"
  :group 'org-mac-flagged-mail
  :type 'string)

(org-add-link-type "message" 'org-mac-message-open)

;; In mac.c, removed in Emacs 23.
(declare-function do-applescript "org-mac-message" (script))
(unless (fboundp 'do-applescript)
  ;; Need to fake this using shell-command-to-string
  (defun do-applescript (script)
    (let (start cmd return)
      (while (string-match "\n" script)
	(setq script (replace-match "\r" t t script)))
      (while (string-match "'" script start)
	(setq start (+ 2 (match-beginning 0))
	      script (replace-match "\\'" t t script)))
      (setq cmd (concat "osascript -e '" script "'"))
      (setq return (shell-command-to-string cmd))
      (concat "\"" (org-trim return) "\""))))

(defun org-mac-message-open (message-id)
  "Visit the message with the given MESSAGE-ID.
This will use the command `open' with the message URL."
  (start-process (concat "open message:" message-id) nil
		 "open" (concat "message://<" (substring message-id 2) ">")))

(defun org-mac-message-insert-link ()
  "Insert a link to the messages currently selected in Apple Mail.
This will use applescript to get the message-id and the subject of the
active mail in AppleMail and make a link out of it."
  (interactive)
  (org-mac-message-get-link)
  (yank))

(defun org-mac-message-get-link ()
  "Insert a link to the messages currently selected in Apple Mail.
This will use applescript to get the message-id and the subject of the
active mail in AppleMail and make a link out of it."
  (let* ((as-link-list
	  (do-applescript
	   (concat
	    "tell application \"Mail\"\n"
	    "set theLinkList to {}\n"
	    "set theSelection to selection\n"
	    "repeat with theMessage in theSelection\n"
	    "set theID to message id of theMessage\n"
	    "set theSubject to subject of theMessage\n"
	    "set theLink to \"message://\" & theID & \"::split::\" & theSubject & \"\n\"\n"
	    "copy theLink to end of theLinkList\n"
	    "end repeat\n"
	    "return theLinkList as string\n"
	    "end tell")))
	 (link-list (split-string as-link-list "\n"))
	 split-link
	 URL
	 description
	 orglink
	 orglink-insert
	 (orglink-list nil))
    (while link-list
      (progn
	(setq split-link (split-string (pop link-list) "::split::"))
	(setq URL (car split-link))
	(setq description (cadr split-link))
	(if (not (string= URL ""))
	    (progn
	      (setq orglink (org-make-link-string URL description))
	      (push orglink orglink-list)))))
    (with-temp-buffer      
      (while orglink-list
	(insert (concat (pop orglink-list)) "\n"))
      (kill-region (point-min) (point-max)))))

(defun org-mac-create-flagged-mail ()
  "Create links to flagged messages in a Mail.app account and
copy them to the kill ring"
  (interactive)
  (message "AppleScript: searching mailboxes...")
  (let* ((as-link-list
	  (do-applescript
	   (concat
	    "tell application \"Mail\"\n"
	    "set theMailboxes to every mailbox of account \"" org-mac-mail-account "\"\n"
	    "set theLinkList to {}\n"
	    "repeat with aMailbox in theMailboxes\n"
	    "set theSelection to (every message in aMailbox whose flagged status = true)\n"
	    "repeat with theMessage in theSelection\n"
	    "set theID to message id of theMessage\n"
	    "set theSubject to subject of theMessage\n"
	    "set theLink to \"message://\" & theID & \"::split::\" & theSubject & \"\n\"\n"
	    "copy theLink to end of theLinkList\n"
	    ;; "tell application \"GrowlHelperApp\"\n"
	    ;; "set the allNotificationsList to {\"FlaggedMail\"}\n"
	    ;; "set the enabledNotificationsList to allNotificationsList\n"
	    ;; "register as application \"FlaggedMail\" all notifications allNotificationsList default notifications enabledNotificationsList icon of application \"Mail\"\n"
	    ;; "notify with name \"FlaggedMail\" title \"Importing flagged message\" description theSubject application name \"FlaggedMail\"\n"
	    ;; "end tell\n"
	    "end repeat\n"
	    "end repeat\n"
	    "return theLinkList as string\n"
	    "end tell")))
	 (link-list (split-string as-link-list "\n"))
	 split-link
	 URL
	 description
	 orglink
	 (orglink-list nil))
    (while link-list
      (progn
	(setq split-link (split-string (pop link-list) "::split::"))
	(setq URL (car split-link))
	(setq description (cadr split-link))
	(if (not (string= URL ""))
	    (progn
	      (setq orglink (org-make-link-string URL description))
	      (push orglink orglink-list)))))
    (with-temp-buffer      
      (while orglink-list
	(insert (concat (pop orglink-list)) "\n"))
      (kill-region (point-min) (point-max))
      (message "Flagged messages copied to kill ring"))))

(defun org-mac-insert-flagged-mail (org-buffer org-heading)
  "Asks for an org buffer and a heading within it. If heading
exists, delete all message:// links within heading's first
level. If heading doesn't exist, create it at point-max. Insert
list of message:// links to flagged mail after heading."
  (interactive "bBuffer in which to insert links: \nsHeading after which to insert links: ")
  (save-excursion
    (set-buffer org-buffer)
    (goto-char (point-min))
    (let ((isearch-forward t)
	  (message-re "\\[\\[\\(message:\\)?\\([^]]+\\)\\]\\(\\[\\([^]]+\\)\\]\\)?\\]"))
      (if (org-goto-local-search-headings org-heading nil t)
	  (if (not (eobp))
	      (progn
		(save-excursion
		  (while (re-search-forward message-re (save-excursion (outline-next-heading)) t)
		    
		    (delete-region (match-beginning 0) (match-end 0)))
		  (org-mac-create-flagged-mail)
		  (yank))
		(flush-lines "^$" (point) (outline-next-heading)))
	    (insert "\n")
	    (org-mac-create-flagged-mail)
	    (yank))
	(goto-char (point-max))
	(insert "\n")
	(org-insert-heading)
	(insert (concat org-heading "\n"))
	(org-mac-create-flagged-mail)
	(yank)))))

(provide 'org-mac-message)

;; arch-tag: 3806d0c1-abe1-4db6-9c31-f3ed7d4a9b32

;;; org-mac-message.el ends here
