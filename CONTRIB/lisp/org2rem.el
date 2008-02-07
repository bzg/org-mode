;;; org2rem.el --- Convert org appointments into reminders

;; Copyright 2006 Bastien Guerry
;;
;; Author: bzg AT altern DOT fr
;; Version: $Id: org2rem.el,v 0.1 2006/12/04 09:21:03 guerry Exp guerry $
;; Keywords: org-mode remind reminder appointment diary calendar
;; X-URL: http://www.cognition.ens.fr/~guerry/u/org2rem.el

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

;; Not so much to say here.  Just try org2rem in your org-mode buffer.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org2rem)

;;; Code:

(provide 'org2rem)
(eval-when-compile
  (require 'cl))

(defvar org2rem-scheduled-reminders nil)
(defvar org2rem-deadline-reminders nil)
(defvar org2rem-scheduled-remind-file 
  "~/.reminders.org.scheduled")
(defvar org2rem-deadline-remind-file 
  "~/.reminders.org.deadline")

(defun org2rem-list-reminders (regexp)
  "Make a list of appointments. 
REGEXP is either SCHEDULED: or DEADLINE:."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward 
	    (concat "^[ \t]*" regexp
		    "[ \t]*"  org-ts-regexp2) nil t)
      (let* ((system-time-locale "C") ;; make sure we use english dates
	     (year (string-to-number (match-string-no-properties 2)))
	     (month (string-to-number (match-string-no-properties 3)))
	     (day (string-to-number (match-string-no-properties 4)))
	     (encoded-time (encode-time 0 0 0 day month year))
	     (rem-time (format-time-string " %d %b %Y " encoded-time))
	     task rem-task)
	(save-excursion
	  (re-search-backward org-todo-line-regexp nil t)
	  (setq task
		(replace-regexp-in-string 
		 org-bracket-link-regexp 
		 "\\3" (match-string-no-properties 3)))
	  (setq rem-task (concat "REM" rem-time "MSG " task "%"))
	  (if (equal regexp org-scheduled-string)
	      (push rem-task org2rem-scheduled-reminders)
	    (push rem-task org2rem-deadline-reminders)))))))

(defun org2rem-write-file (file reminders)
  "Write reminders list to files."
  (with-temp-buffer
    (find-file file)
    (erase-buffer)
    (dolist (rem reminders)
      (insert rem "\n"))
    (write-file file)
    (kill-buffer (file-name-nondirectory file))))

(defun org2rem ()
  "Convert apptointment from local org-mode buffer to reminders.
Store scheduled appointments in `org2rem-scheduled-remind-file'
and `org2rem-deadline-remind-file'."
  (interactive)
  (setq org2rem-scheduled-reminders nil)
  (setq org2rem-deadline-reminders nil)
  (save-window-excursion
    (org2rem-list-reminders org-scheduled-string)
    (org2rem-list-reminders org-deadline-string)
    (org2rem-write-file "~/.reminders.org.scheduled" 
			org2rem-scheduled-reminders)
    (org2rem-write-file "~/.reminders.org.deadline"
			org2rem-deadline-reminders)))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





;;; org2rem.el ends here
