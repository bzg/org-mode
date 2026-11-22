;;; org-log-note.el --- Org notes and logging                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements adding log notes to Org headings.

;;; Code:

(require 'org-property)

(defvar org-log-post-message)
(defvar org-log-note-purpose)
(defvar org-log-note-how nil)
(defvar org-log-note-extra)
(defvar org-log-setup nil)

(defvar org-inhibit-startup)

(defcustom org-log-buffer-setup-hook nil
  "Hook that is run after an Org log buffer is created."
  :group 'org
  :version "24.1"
  :type 'hook)

(defcustom org-after-note-stored-hook nil
  "Hook triggered after a note is stored.
The point is at the stored note when the hook is executed."
  :group 'org-progress
  :type 'hook
  :package-version '(Org . "9.7"))

(defcustom org-log-note-headings
  '((done .  "CLOSING NOTE %t")
    (state . "State %-12s from %-12S %t")
    (note .  "Note taken on %t")
    (reschedule .  "Rescheduled from %S on %t")
    (delschedule .  "Not scheduled, was %S on %t")
    (redeadline .  "New deadline from %S on %t")
    (deldeadline .  "Removed deadline, was %S on %t")
    (refile . "Refiled on %t")
    (clock-out . ""))
  "Headings for notes added to entries.

The value is an alist, with the car being a symbol indicating the
note context, and the cdr is the heading to be used.  The heading
may also be the empty string.  The following placeholders can be
used:

  %t  a time stamp.
  %T  an active time stamp instead the default inactive one
  %d  a short-format time stamp.
  %D  an active short-format time stamp.
  %s  the new TODO state or time stamp (inactive), in double quotes.
  %S  the old TODO state or time stamp (inactive), in double quotes.
  %u  the user name.
  %U  full user name.

In fact, it is not a good idea to change the `state' entry,
because Agenda Log mode depends on the format of these entries."
  :group  'org-todo
  :group  'org-progress
  :type '(list :greedy t
	       (cons (const :tag "Heading when closing an item" done) string)
	       (cons (const :tag
			    "Heading when changing todo state (todo sequence only)"
			    state) string)
	       (cons (const :tag "Heading when just taking a note" note) string)
	       (cons (const :tag "Heading when rescheduling" reschedule) string)
	       (cons (const :tag "Heading when an item is no longer scheduled" delschedule) string)
	       (cons (const :tag "Heading when changing deadline"  redeadline) string)
	       (cons (const :tag "Heading when deleting a deadline" deldeadline) string)
	       (cons (const :tag "Heading when refiling" refile) string)
	       (cons (const :tag "Heading when clocking out" clock-out) string)))

(unless (assq 'note org-log-note-headings)
  (push '(note . "%t") org-log-note-headings))

(defvaralias 'org-log-state-notes-into-drawer 'org-log-into-drawer)
(defcustom org-log-into-drawer nil
  "Non-nil means insert state change notes and time stamps into a drawer.
When nil, state changes notes will be inserted after the headline and
any scheduling and clock lines, but not inside a drawer.

The value of this variable should be the name of the drawer to use.
LOGBOOK is proposed as the default drawer for this purpose, you can
also set this to a string to define the drawer of your choice.

A value of t is also allowed, representing \"LOGBOOK\".

A value of t or nil can also be set with on a per-file-basis with

   #+STARTUP: logdrawer
   #+STARTUP: nologdrawer

If this variable is set, `org-log-state-notes-insert-after-drawers'
will be ignored.

You can set the property LOG_INTO_DRAWER to overrule this setting for
a subtree.

Do not check directly this variable in a Lisp program.  Call
function `org-log-into-drawer' instead."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "Not into a drawer" nil)
	  (const :tag "LOGBOOK" t)
	  (string :tag "Other")))

(defcustom org-log-state-notes-insert-after-drawers nil
  "Non-nil means insert state change notes after any drawers in entry.
Only the drawers that *immediately* follow the headline and the
deadline/scheduled line are skipped.
When nil, insert notes right after the heading and perhaps the line
with deadline/scheduling if present.

This variable will have no effect if `org-log-into-drawer' is
set."
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-states-order-reversed t
  "Non-nil means the latest state note will be directly after heading.
When nil, the state change notes will be ordered according to time.

This option can also be set with on a per-file-basis with

   #+STARTUP: logstatesreversed
   #+STARTUP: nologstatesreversed"
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-reverse-note-order nil
  "Non-nil means store new notes at the beginning of a file or entry.
When nil, new notes will be filed to the end of a file or entry.
This can also be a list with cons cells of regular expressions that
are matched against file names, and values."
  :group 'org-capture
  :group 'org-refile
  :type '(choice
	  (const :tag "Reverse always" t)
	  (const :tag "Reverse never" nil)
	  (repeat :tag "By file name regexp"
		  (cons regexp boolean))))

(defun org-log-into-drawer ()
  "Name of the log drawer, as a string, or nil.
This is the value of `org-log-into-drawer'.  However, if the
current entry has or inherits a LOG_INTO_DRAWER property, it will
be used instead of the default value."
  (let ((p (org-entry-get nil "LOG_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") "LOGBOOK")
	  ((stringp p) p)
	  (p "LOGBOOK")
	  ((stringp org-log-into-drawer) org-log-into-drawer)
	  (org-log-into-drawer "LOGBOOK"))))

(defvar org-log-note-marker (make-marker)
  "Marker pointing at the entry where the note is to be inserted.")
(defvar org-log-note-purpose nil)
(defvar org-log-note-state nil)
(defvar org-log-note-previous-state nil)
(defvar org-log-note-extra nil)
(defvar org-log-note-frame nil)
(defvar org-log-note-window-configuration nil)
(defvar org-log-note-popup-window-configuration nil)
(defvar org-log-note-return-to (make-marker))
(defvar org-log-note-effective-time nil
  "Remembered current time.
So that dynamically scoped `org-extend-today-until' affects
timestamps in state change log.")
(defvar org-log-note-this-command
  "`this-command' when `org-add-log-setup' is called.")
(defvar org-log-note-recursion-depth
  "`recursion-depth' when `org-add-log-setup' is called.")

(defvar org-log-post-message nil
  "Message to be displayed after a log note has been stored.
The auto-repeater uses this.")

(defun org-add-note ()
  "Add a note to the current entry.
This is done in the same way as adding a state change note."
  (interactive nil org-mode)
  (org-add-log-setup 'note))

(defun org-log-beginning (&optional create)
  "Return expected start of log notes in current entry.
When optional argument CREATE is non-nil, the function creates
a drawer to store notes, if necessary.  Returned position ignores
narrowing."
  (org-with-wide-buffer
   (let ((drawer (org-log-into-drawer)))
     (cond
      (drawer
       ;; This either moves past planning and property drawer, to
       ;; first line below heading, or to `eob' (if heading is the
       ;; last heading in buffer without contents).
       (org-end-of-meta-data)
       (let ((regexp (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$"))
	     (end (if (org-at-heading-p) (point)
		    (save-excursion (outline-next-heading) (point))))
	     (case-fold-search t))
	 (catch 'exit
	   ;; Try to find existing drawer.
	   (while (re-search-forward regexp end t)
	     (let ((element (org-element-at-point)))
	       (when (org-element-type-p element 'drawer)
		 (let ((cend  (org-element-contents-end element)))
		   (when (and (not org-log-states-order-reversed) cend)
		     (goto-char cend)))
		 (throw 'exit nil))))
	   ;; No drawer found.  Create one, if permitted.
	   (when create
             ;; `org-end-of-meta-data' ended up at next heading
             ;; * Heading to insert drawer<maybe folded>
             ;; * Another heading
             ;;
             ;; Unless current heading is the last heading in buffer
             ;; and does not have a newline, `org-end-of-meta-data'
             ;; can move us to the next heading.
             ;; Avoid situation when we insert drawer right before
             ;; first "*".  Otherwise, if the heading is folded, we
             ;; are inserting after visible newline at the end of the
             ;; fold, thus breaking the fold continuity.
             (unless (eobp)
               (when (org-at-heading-p) (backward-char)))
             (org-fold-core-ignore-modifications
               (let (;; Heading
                     ;; <point>
                     ;; Text
                     (at-blank-line? (looking-at-p "^[ \t]*$"))
                     ;; Heading
                     ;; <point>Text
                     (at-beginning-of-non-blank-line?
                      (and (bolp) (not (eolp)))))
	         (unless (bolp)
                   ;; Heading<point> (see `backward-char' branch above)
                   (insert-and-inherit "\n"))
	         (let ((beg (point)) cbeg)
	           (insert-and-inherit ":" drawer ":")
                   (setq cbeg (point-marker))
                   (insert-and-inherit "\n:END:")
                   (cond
                    (at-blank-line?
                     ;; Heading
                     ;; :LOGBOOK:
                     ;; :END:
                     ;;
                     ;; Text
                     (insert "\n")
                     (backward-char))
                    (at-beginning-of-non-blank-line?
                     ;; Heading
                     ;; :LOGBOOK:
                     ;; :END:
                     ;; Text
                     (insert "\n")
                     (backward-char)))
	           (org-indent-region beg (point))
	           (org-fold-region cbeg (point) t 'drawer)
                   (move-marker cbeg nil)))))
	   (end-of-line 0))))
      (t
       (org-end-of-meta-data org-log-state-notes-insert-after-drawers)
       (let ((endpos (point)))
         (skip-chars-forward " \t\n")
         (forward-line 0)
         (unless org-log-states-order-reversed
	   (org-skip-over-state-notes)
	   (skip-chars-backward " \t\n")
	   (forward-line 1))
         ;; When current headline is at the end of buffer and does not
         ;; end with trailing newline the above can move to the
         ;; beginning of the headline.
         (when (< (point) endpos) (goto-char endpos))))))
   (if (bolp) (point) (line-beginning-position 2))))

(defun org-add-log-setup (&optional purpose state prev-state how extra)
  "Set up the post command hook to take a note.
If this is about to TODO state change, the new state is expected in STATE.
HOW is an indicator what kind of note should be created.
EXTRA is additional text that will be inserted into the notes buffer."
  (move-marker org-log-note-marker (point))
  (setq org-log-note-purpose purpose
	org-log-note-state state
	org-log-note-previous-state prev-state
	org-log-note-how how
	org-log-note-extra extra
	org-log-note-effective-time (org-current-effective-time)
        org-log-note-this-command this-command
        org-log-note-recursion-depth (recursion-depth)
        org-log-setup t)
  (add-hook 'post-command-hook 'org-add-log-note 'append))

(defun org--log-note-format-regexp (format)
  "Return a regexp matching log note FORMAT."
  (replace-regexp-in-string
   " +" " +"
   (org-replace-escapes
    (regexp-quote format)
    `(("%d" . ,org-ts-regexp-inactive)
      ("%D" . ,org-ts-regexp)
      ("%s" . "\\(?:\"\\S-+\"\\)?")
      ("%S" . "\\(?:\"\\S-+\"\\)?")
      ("%t" . ,org-ts-regexp-inactive)
      ("%T" . ,org-ts-regexp)
      ("%u" . ".*?")
      ("%U" . ".*?")))))

(defun org-skip-over-state-notes ()
  "Skip past the list of State notes in an entry.
The point is assumed to be on a list of State notes, each matching
`org-log-note-headings'.  The function moves point to the first list
item that is not a State note or to the end of the list if all the
items are State notes."
  (when (ignore-errors (goto-char (org-in-item-p)))
    (let* ((struct (org-list-struct))
	   (prevs (org-list-prevs-alist struct))
	   (regexp
	    (concat "[ \t]*- +"
		    (org--log-note-format-regexp
                     (cdr (assq 'state org-log-note-headings))))))
      (while (looking-at-p regexp)
	(goto-char (or (org-list-get-next-item (point) struct prevs)
		       (org-list-get-item-end (point) struct)))))))

(defun org-add-log-note (&optional _purpose)
  "Pop up a window for taking a note, and add this note later."
  (when (and (equal org-log-note-this-command this-command)
             (= org-log-note-recursion-depth (recursion-depth)))
    (remove-hook 'post-command-hook 'org-add-log-note)
    (setq org-log-note-frame (selected-frame))
    (setq org-log-setup nil)
    (setq org-log-note-window-configuration (current-window-configuration))
    (move-marker org-log-note-return-to (point))
    (pop-to-buffer (marker-buffer org-log-note-marker) '(org-display-buffer-full-frame))
    (setq org-log-note-popup-window-configuration (current-window-configuration))
    (goto-char org-log-note-marker)
    (pop-to-buffer "*Org Note*" '(org-display-buffer-split))
    (erase-buffer)
    (if (memq org-log-note-how '(time state))
        (org-store-log-note)
      (let ((org-inhibit-startup t)) (org-mode))
      (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
                      (cl-case org-log-note-purpose
                        (clock-out "stopped clock")
                        (done  "closed todo item")
                        (reschedule "rescheduling")
                        (delschedule "no longer scheduled")
                        (redeadline "changing deadline")
                        (deldeadline "removing deadline")
                        (refile "refiling")
                        (note "this entry")
                        (state
                         (format "state change from \"%s\" to \"%s\""
                                 (or org-log-note-previous-state "")
                                 (or org-log-note-state "")))
                        (t (error "This should not happen")))))
      (when org-log-note-extra (insert org-log-note-extra))
      (setq-local org-finish-function 'org-store-log-note)
      (run-hooks 'org-log-buffer-setup-hook))))

(defvar org-note-abort nil) ; dynamically scoped
(defun org-store-log-note ()
  "Finish taking a log note, and insert it to where it belongs."
  (let ((txt (prog1 (buffer-string)
	       (kill-buffer)))
	(note (cdr (assq org-log-note-purpose org-log-note-headings)))
	lines)
    (while (string-match "\\`# .*\n[ \t\n]*" txt)
      (setq txt (replace-match "" t t txt)))
    (when (string-match "\\s-+\\'" txt)
      (setq txt (replace-match "" t t txt)))
    (setq lines (and (not (equal "" txt)) (org-split-string txt "\n")))
    (when (org-string-nw-p note)
      (setq note
	    (org-replace-escapes
	     note
	     (list (cons "%u" (user-login-name))
		   (cons "%U" user-full-name)
		   (cons "%t" (format-time-string
			       (org-time-stamp-format 'long 'inactive)
			       org-log-note-effective-time))
		   (cons "%T" (format-time-string
			       (org-time-stamp-format 'long nil)
			       org-log-note-effective-time))
		   (cons "%d" (format-time-string
			       (org-time-stamp-format nil 'inactive)
			       org-log-note-effective-time))
		   (cons "%D" (format-time-string
			       (org-time-stamp-format nil nil)
			       org-log-note-effective-time))
		   (cons "%s" (cond
			       ((not org-log-note-state) "")
			       ((string-match-p org-ts-regexp
						org-log-note-state)
				(format "\"[%s]\""
					(substring org-log-note-state 1 -1)))
			       (t (format "\"%s\"" org-log-note-state))))
		   (cons "%S"
			 (cond
			  ((not org-log-note-previous-state) "")
			  ((string-match-p org-ts-regexp
					   org-log-note-previous-state)
			   (format "\"[%s]\""
				   (substring
				    org-log-note-previous-state 1 -1)))
			  (t (format "\"%s\""
				     org-log-note-previous-state)))))))
      (when lines (setq note (concat note " \\\\")))
      (push note lines))
    (when (and lines (not org-note-abort))
      (with-current-buffer (marker-buffer org-log-note-marker)
        (org-fold-core-ignore-modifications
	  (org-with-wide-buffer
	   ;; Find location for the new note.
	   (goto-char org-log-note-marker)
	   (set-marker org-log-note-marker nil)
	   ;; Note associated to a clock is to be located right after
	   ;; the clock.  Do not move point.
	   (unless (eq org-log-note-purpose 'clock-out)
	     (goto-char (org-log-beginning t)))
	   ;; Make sure point is at the beginning of an empty line.
	   (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert-and-inherit "\n")))
	         ((looking-at "[ \t]*\\S-") (save-excursion (insert-and-inherit "\n"))))
	   ;; In an existing list, add a new item at the top level.
	   ;; Otherwise, indent line like a regular one.
	   (let ((itemp (org-in-item-p)))
	     (if itemp
	         (indent-line-to
		  (let ((struct (save-excursion
				  (goto-char itemp) (org-list-struct))))
		    (org-list-get-ind (org-list-get-top-point struct) struct)))
	       (org-indent-line)))
	   (insert-and-inherit (org-list-bullet-string "-") (pop lines))
	   (let ((ind (org-list-item-body-column (line-beginning-position))))
	     (dolist (line lines)
	       (insert-and-inherit "\n")
               (unless (string-empty-p line)
	         (indent-line-to ind)
	         (insert-and-inherit line))))
           (run-hooks 'org-after-note-stored-hook)
	   (message "Note stored")
	   (org-back-to-heading t))))))
  ;; Don't add undo information when called from `org-agenda-todo'.
  (if (eq org-log-note-frame (selected-frame))
      (set-window-configuration org-log-note-window-configuration)
    (set-window-configuration org-log-note-popup-window-configuration)
    (when (frame-live-p org-log-note-frame)
      (select-frame-set-input-focus org-log-note-frame)
      (set-window-configuration org-log-note-window-configuration)))
  (with-current-buffer (marker-buffer org-log-note-return-to)
    (goto-char org-log-note-return-to))
  (move-marker org-log-note-return-to nil)
  (when org-log-post-message (message "%s" org-log-post-message)))

(defun org-notes-order-reversed-p ()
  "Check if the current file should receive notes in reversed order."
  (cond
   ((not org-reverse-note-order) nil)
   ((listp org-reverse-note-order)
    (catch 'exit
      (dolist (entry org-reverse-note-order)
        (when (string-match (car entry) buffer-file-name)
	  (throw 'exit (cdr entry))))))
   (t org-reverse-note-order)))

(provide 'org-log-note)

;;; org-log-note.el ends here
