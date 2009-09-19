;;; org-mobile.el --- Code for asymmetric sync with a mobile device
;; Copyright (C) 2009 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.30trans
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains the code to interact with Richard Moreland's iPhone
;; application MobileOrg.  This code is documented in Appendix B of the
;; Org-mode manual.

(require 'org)
(require 'org-agenda)

(defgroup org-mobile nil
  "Options concerning support for a viewer on a mobile device."
  :tag "Org Mobile"
  :group 'org)

(defcustom org-mobile-directory ""
  "The WebDAV directory where the interaction with the mobile takes place."
  :group 'org-mobile
  :type 'directory)

(defcustom org-mobile-inbox-for-pull "~/org/from-mobile.org"
  "The file where captured notes and flags will be appended to.
During the execution of `org-mobile-pull', the file
`org-mobile-capture-file' will be emptied it's contents have
been appended to the file given here."
  :group 'org-mobile
  :type 'file)

(defconst org-mobile-capture-file "mobileorg.org"
  "The capture file where the mobile stores captured notes and flags.
This should not be changed, because MobileOrg assumes this name.")

(defcustom org-mobile-index-file "index.org"
  "The index file with inks to all Org files that should be loaded by MobileOrg.
Relative to `org-mobile-directory'.  The Address field in the MobileOrg setup
should point to this file."
  :group 'org-mobile
  :type 'file)

(defcustom org-mobile-force-id-on-agenda-items t
  "Non-nil means make all agenda items carry and ID."
  :group 'org-mobile
  :type 'boolean)

(defcustom org-mobile-action-alist
  '(("d" . (org-todo 'done))
    ("a" . (org-archive-subtree-default))
    ("d-a" . (progn (org-todo 'done) (org-archive-subtree-default))))
  "Alist with flags and actions for mobile sync.
When flagging an entry, MobileOrg will create entries that look like

  * F(action)  [[id:entry-id][entry title]]

This alist defines that the action in the parentheses of F() should mean,
i.e. what action should be taken.  The car of each elements of the alist
is an actions string.  The cdr is an Emacs Lisp form that will be evaluated
with the cursor on the headline of that entry."
  :group 'org-mobile
  :type '(repeat
	  (cons (string :tag "Action flag")
		(sexp   :tag "Action form"))))

(defvar org-mobile-pre-push-hook nil
  "Hook run before running `org-mobile-push'.
This could be used to clean up `org-mobile-directory', for example to
remove files that used to be included in the agenda but no longer are.
The presence of such files would not really be a problem, but after time
they may accumulate.")

(defvar org-mobile-post-push-hook nil
  "Hook run after running `org-mobile-push'.
If Emacs does not have direct write access to the WebDAV directory used
by the mobile device, this hook should be used to copy all files from the
local staging directory `org-mobile-directory' to the WebDAV directory,
for example using `rsync' or `scp'.")

(defvar org-mobile-pre-pull-hook nil
  "Hook run before executing `org-mobile-pull'.
If Emacs does not have direct write access to the WebDAV directory used
by the mobile device, this hook should be used to copy the capture file
`mobileorg.org' from the WebDAV location to the local staging
directory `org-mobile-directory'.")

(defvar org-mobile-post-pull-hook nil
  "Hook run after running `org-mobile-pull'.
If Emacs does not have direct write access to the WebDAV directory used
by the mobile device, this hook should be used to copy the emptied
capture file `mobileorg.org' back to the WebDAV directory, for example
using `rsync' or `scp'.")

(defvar org-mobile-last-flagged-files nil
  "List of files containing entreis flagged in the latest pull.")

;;;###autoload
(defun org-mobile-push ()
  "Push the current state of Org affairs to the WebDAV directory.
This will create the index file, copy all agenda files there, and also
create all custom agenda views, for upload to the mobile phone."
  (interactive)
  (org-mobile-check-setup)
  (run-hooks 'org-mobile-pre-push-hook)
  (org-mobile-create-sumo-agenda)
  (org-save-all-org-buffers) ; to save any IDs created by this process
  (org-mobile-copy-agenda-files)
  (org-mobile-create-index-file)
  (org-mobile-write-checksums)
  (run-hooks 'org-mobile-post-push-hook)
  (message "Files for mobile viewer staged"))

;;;###autoload
(defun org-mobile-pull ()
  "Pull the contents of `org-mobile-capture-file' and integrate them.
Apply all flagged actions, flag entries to be flagged and then call an
agenda view showing the flagged items."
  (interactive)
  (org-mobile-check-setup)
  (run-hooks 'org-mobile-pre-pull-hook)
  (let ((insertion-marker (org-mobile-move-capture)))
    (if (not (markerp insertion-marker))
	(message "No new items")
      (org-with-point-at insertion-marker
	(org-mobile-apply-flags (point) (point-max)))
      (move-marker insertion-marker nil)
      (run-hooks 'org-mobile-post-pull-hook)
      (when org-mobile-last-flagged-files
	;; Make an agenda view of flagged entries, but only in the files
	;; where stuff has been added.
	(put 'org-agenda-files 'org-restrict org-mobile-last-flagged-files)
	(let ((org-agenda-keep-restriced-file-list t))
	  (org-agenda nil "?"))))))

(defun org-mobile-check-setup ()
  "Check if org-mobile-directory has been set up."
  (when (or (not org-mobile-directory)
	    (not (stringp org-mobile-directory))
	    (not (string-match "\\S-" org-mobile-directory))
	    (not (file-exists-p org-mobile-directory))
	    (not (file-directory-p org-mobile-directory)))
    (error
     "Variable `org-mobile-directory' must point to an existing directory"))
  (when (or (not org-mobile-inbox-for-pull)
	    (not (stringp org-mobile-inbox-for-pull))
	    (not (string-match "\\S-" org-mobile-inbox-for-pull))
	    (not (file-exists-p
		  (file-name-directory org-mobile-inbox-for-pull))))
    (error
     "Variable `org-mobile-inbox-for-pull' must point to a file in an existing directory")))

(defun org-mobile-create-index-file ()
  "Write the index file in the WebDAV directory."
  (interactive)
  (let ((files (org-agenda-files t))
	file todo-kwds done-kwds drawers)
    (org-prepare-agenda-buffers (org-agenda-files t))
    (setq done-kwds (org-uniquify org-done-keywords-for-agenda))
    (setq todo-kwds (org-delete-all
		     done-kwds
		     (org-uniquify org-todo-keywords-for-agenda)))
    (setq drawers (org-uniquify org-drawers-for-agenda))
    (with-temp-file
	(expand-file-name org-mobile-index-file org-mobile-directory)
      (insert "#+TODO: " (mapconcat 'identity todo-kwds " ") " | "
	      (mapconcat 'identity done-kwds " ") "\n"
	      "#+DRAWERS: " (mapconcat 'identity drawers " ") "\n")
      (insert "* [[file:agendas.org][Agenda Views]]\n")
      (while (setq file (pop files))
	(insert (format "* [[file:%s][%s]]\n"
			(file-name-nondirectory file)
			(capitalize
			 (file-name-sans-extension
			  (file-name-nondirectory file))))))
      (insert (format "* [[file:%s][Captured before last sync]]\n"
		      org-mobile-capture-file)))))

(defun org-mobile-copy-agenda-files ()
  "Copy all agenda files to the stage or WebDAV directory."
  (let ((files (org-agenda-files t)) file buf)
    (while (setq file (pop files))
      (if (file-exists-p file)
	  (copy-file file (expand-file-name (file-name-nondirectory file)
					    org-mobile-directory)
		     'ok-if-exists)))
    (setq file (expand-file-name org-mobile-capture-file
				 org-mobile-directory))
    (unless (file-exists-p file)
      (save-excursion
	(setq buf (find-file file))
	(insert "\n")
	(save-buffer)
      (kill-buffer buf)))))

(defun org-mobile-write-checksums ()
  "Create checksums for all files in `org-mobile-directory'.
The table of checksums is written to the file mobile-checksums."
  (let ((cmd (cond ((executable-find "shasum"))
		   ((executable-find "sha1sum"))
		   ((executable-find "md5sum"))
		   ((executable-find "md5")))))
    (if (not cmd)
	(message "Checksums could not be generated: no executable")
      (with-temp-buffer
	(cd org-mobile-directory)
	(if (equal 0 (shell-command (concat cmd " *.org > checksums.dat")))
	    (message "Checksums written")
	  (message "Checksums could not be generated"))))))

(defun org-mobile-sumo-agenda-command ()
  "Return an agenda custom command that comprises all custom commands."
  (let ((custom-list
	 ;; normalize different versions
	 (delq nil
	       (mapcar
		(lambda (x)
		  (cond ((stringp (cdr x)) nil)
			((stringp (nth 1 x)) x)
			((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
			(t (cons (car x) (cons "" (cdr x))))))
		org-agenda-custom-commands)))
	new e key desc type match settings cmds gkey gdesc gsettings cnt)
    (while (setq e (pop custom-list))
      (cond
       ((stringp (cdr e))
	;; this is a description entry - skip it
	)
       ((eq (nth 2 e) 'search)
	;; Search view is interactive, skip
	)
       ((memq (nth 2 e) '(todo-tree tags-tree occur-tree))
	;; These are trees, not really agenda commands
	)
       ((memq (nth 2 e) '(agenda todo tags))
	;; a normal command
	(setq key (car e) desc (nth 1 e) type (nth 2 e) match (nth 3 e)
	      settings (nth 4 e))
	(setq settings
	      (cons (list 'org-agenda-title-append
			  (concat "<break>KEYS=" key " TITLE: "
				  (if (and (stringp desc) (> (length desc) 0))
				      desc (symbol-name type))
				  " " match))
		    settings))
	(push (list type match settings) new))
       ((symbolp (nth 2 e))
	;; A user-defined function, not sure how to handle that yet
	)
       (t
	;; a block agenda
	(setq gkey (car e) gdesc (nth 1 e) gsettings (nth 3 e) cmds (nth 2 e))
	(setq cnt 0)
	(while (setq e (pop cmds))
	  (setq type (car e) match (nth 1 e) settings (nth 2 e))
	  (setq settings (append gsettings settings))
	  (setq settings
		(cons (list 'org-agenda-title-append
			    (concat "<break>KEYS=" gkey "#" (number-to-string
						      (setq cnt (1+ cnt)))
				    " TITLE: " gdesc " " match))
		      settings))
	  (push (list type match settings) new)))))
    (list "X" "SUMO" (reverse new) nil)))

;;;###autoload
(defun org-mobile-create-sumo-agenda ()
  "Create a file that contains all custom agenda views."
  (interactive)
  (let* ((file (expand-file-name "agendas.org"
				 org-mobile-directory))
	 (org-agenda-custom-commands
	  (list (append (org-mobile-sumo-agenda-command)
			(list (list file))))))
    (unless (file-writable-p file)
      (error "Cannot write to file %s" file))
    (org-batch-store-agenda-views)))

(defun org-mobile-move-capture ()
  "Move the contents of the capture file to the inbox file.
Return a marker to the location where the new content has been added.
If nothing new has beed added, return nil."
  (interactive)
  (let ((inbox-buffer (find-file-noselect org-mobile-inbox-for-pull))
	(capture-buffer (find-file-noselect
			 (expand-file-name org-mobile-capture-file
					   org-mobile-directory)))
	(insertion-point (make-marker))
	not-empty content)
    (save-excursion
      (set-buffer capture-buffer)
      (setq content (buffer-string))
      (setq not-empty (string-match "\\S-" content))
      (when not-empty
	(set-buffer inbox-buffer)
	(widen)
	(goto-char (point-max))
	(or (bolp) (newline))
	(move-marker insertion-point
		     (prog1 (point) (insert content)))
	(save-buffer)
	(set-buffer capture-buffer)
	(erase-buffer)
	(save-buffer)))
    (kill-buffer capture-buffer)
    (if not-empty insertion-point)))

(defun org-mobile-apply-flags (&optional beg end)
  "Apply all flags in the current buffer.
If BEG and END are given, only do this in that region."
  (interactive)
  (require 'org-archive)
  (setq org-mobile-last-flagged-files nil)
  (setq beg (or beg (point-min)) end (or end (point-max)))
  (goto-char beg)
  (let ((marker (make-marker))
	(end (move-marker (make-marker) end))
	action id id-pos cmd text)
    (while (re-search-forward
	    "^\\*+[ \t]+F(\\([^()\n]*\\))[ \t]+\\[\\[id:\\([^]\n ]+\\)" end t)
      (goto-char (- (match-beginning 1) 2))
      (catch 'next
	(setq action (match-string 1)
	      id (match-string 2)
	      cmd (if (equal action "")
		      '(progn
			 (org-toggle-tag "FLAGGED" 'on)
			 (and text (org-entry-put nil "THEFLAGGINGNOTE" text)))
		    (cdr (assoc action org-mobile-action-alist)))
	      text (org-trim (buffer-substring (1+ (point-at-eol))
					       (save-excursion
						 (org-end-of-subtree t))))
	      id-pos (org-id-find id 'marker))
	(if (> (length text) 0)
	    ;; Make TEXT into a single line, to fit into a property
	    (setq text (mapconcat 'identity
				  (org-split-string text "\n")
				  "\\n"))
	  (setq text nil))
	(unless id-pos
	  (insert "BAD ID REFERENCE ")
	  (throw 'next t))
	(unless cmd
	  (insert "BAD FLAG ")
	  (throw 'next t))
	(move-marker marker (point))
	(save-excursion
	  (condition-case nil
	      (org-with-point-at id-pos
		(progn
		  (eval cmd)
		  (if (member "FLAGGED" (org-get-tags))
		      (add-to-list 'org-mobile-last-flagged-files
				   (buffer-file-name (current-buffer))))))
	    (error
	     (progn
	       (switch-to-buffer (marker-buffer marker))
	       (goto-char marker)
	       (insert "EXECUTION FAILED ")
	       (throw 'next t)))))
	;; If we get here, the action has been applied successfully
	;; So remove the entry
	(org-back-to-heading t)
	(delete-region (point) (org-end-of-subtree t t))))
    (move-marker marker nil)
    (move-marker end nil)))

(defun org-mobile-smart-read ()
  "Parse the entry at point for shortcuts and expand them.
These shortcuts are meant for fast and easy typing on the limited
keyboards of a mobile device.  Below we show a list of the shortcuts
currently implemented.

The entry is expected to contain an inactive time stamp indicating when
the entry was created.  When setting dates and
times (for example for deadlines), the time strings are interpreted
relative to that creation date.
Abbreviations are expected to take up entire lines, jst because it is so
easy to type RET on a mobile device.  Abbreviations start with one or two
letters, followed immediately by a dot and then additional information.
Generally the entire shortcut line is removed after action have been taken.
Time stamps will be constructed using `org-read-date'.  So for example a
line \"dd. 2tue\" will set a deadline on the second Tuesday after the
creation date.

Here are the shortcuts currently implemented:

dd. string             set deadline
ss. string             set scheduling
tt. string             set time tamp, here.
ti. string             set inactive time

tg. tag1 tag2 tag3     set all these tags, change case where necessary
td. kwd                set this todo keyword, change case where necessary

FIXME: Hmmm, not sure if we can make his work against the
auto-correction feature.  Needs a bit more thinking.  So this function
is currently a noop.")

(provide 'org-mobile)

;; arch-tag: ace0e26c-58f2-4309-8a61-05ec1535f658

;;; org-mobile.el ends here

