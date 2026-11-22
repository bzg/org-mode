;;; org-read-date.el --- Date and time selection dialogue         -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements Org mode's `org-read-date' command - a
;; handy UI to select date and time.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-time)
(require 'org-element-timestamp)
(require 'org-keys)

(defvar org-time-stamp-rounding-minutes)
(declare-function calendar-iso-to-absolute "cal-iso" (date))

(defcustom org-read-date-prefer-future t
  "Non-nil means assume future for incomplete date input from user.
This affects the following situations:
1. The user gives a month but not a year.
   For example, if it is April and you enter \"feb 2\", this will be read
   as Feb 2, *next* year.  \"May 5\", however, will be this year.
2. The user gives a day, but no month.
   For example, if today is the 15th, and you enter \"3\", Org will read
   this as the third of *next* month.  However, if you enter \"17\",
   it will be considered as *this* month.

If you set this variable to the symbol `time', then also the following
will work:

3. If the user gives a time.
   If the time is before now, it will be interpreted as tomorrow.

Currently none of this works for ISO week specifications.

When this option is nil, the current day, month and year will always be
used as defaults.

See also `org-agenda-jump-prefer-future'."
  :group 'org-time
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Check month and day" t)
	  (const :tag "Check month, day, and time" time)))

(defcustom org-read-date-force-compatible-dates t
  "Should date/time prompt force dates that are guaranteed to work in Emacs?

Depending on the system Emacs is running on, certain dates cannot
be represented with the type used internally to represent time.
Dates between 1970-1-1 and 2038-1-1 can always be represented
correctly.  Some systems allow for earlier dates, some for later,
some for both.  One way to find out is to insert any date into an
Org buffer, putting the cursor on the year and hitting S-up and
S-down to test the range.

When this variable is set to t, the date/time prompt will not let
you specify dates outside the 1970-2037 range, so it is certain that
these dates will work in whatever version of Emacs you are
running, and also that you can move a file from one Emacs implementation
to another.  Whenever Org is forcing the year for you, it will display
a message and beep.

When this variable is nil, Org will check if the date is
representable in the specific Emacs implementation you are using.
If not, it will force a year, usually the current year, and beep
to remind you.  Currently this setting is not recommended because
the likelihood that you will open your Org files in an Emacs that
has limited date range is not negligible.

A workaround for this problem is to use diary sexp dates for time
stamps outside of this range."
  :group 'org-time
  :version "24.1"
  :type 'boolean)

(defcustom org-read-date-display-live t
  "Non-nil means display current interpretation of date prompt live.
This display will be in an overlay, in the minibuffer.  Note that
live display is only active when `org-read-date-popup-calendar'
is non-nil."
  :group 'org-time
  :type 'boolean)

(defvaralias 'org-popup-calendar-for-date-prompt
  'org-read-date-popup-calendar)

(defcustom org-read-date-popup-calendar t
  "Non-nil means pop up a calendar when prompting for a date.
In the calendar, the date can be selected with \\`mouse-1'.  However, the
minibuffer will also be active, and you can simply enter the date as well.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'boolean)

(defvar org-date-ovl (make-overlay 1 1))
(overlay-put org-date-ovl 'face 'org-date-selected)
(delete-overlay org-date-ovl)

(defvar org-ans1) ; dynamically scoped parameter
(defvar org-ans2) ; dynamically scoped parameter

(defvar org-time-was-given)
(defvar org-end-time-was-given)
(defvar org-overriding-default-time nil) ; dynamically scoped
(defvar org-read-date-overlay nil)
(defvar org-read-date-history nil)
(defvar org-read-date-final-answer nil)
(defvar org-read-date-analyze-futurep nil)
(defvar org-read-date-analyze-forced-year nil)
(defvar org-read-date-inactive)
(defvar org-def)
(defvar org-defdecode)
(defvar org-with-time)

(defvar org-plain-time-of-day-regexp) ; defined below

(defun org-get-compact-tod (s)
  "Transform time range S into compact form suitable for `org-read-date'.
For example, 12:00-13:00 will be transformed to 12:00+1.
Return nil when S is not a time range."
  (when (string-match "\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\(-\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\)?" s)
    (let* ((t1 (match-string 1 s))
	   (h1 (string-to-number (match-string 2 s)))
	   (m1 (string-to-number (match-string 3 s)))
	   (t2 (and (match-end 4) (match-string 5 s)))
	   (h2 (and t2 (string-to-number (match-string 6 s))))
	   (m2 (and t2 (string-to-number (match-string 7 s))))
	   dh dm)
      (if (not t2)
	  t1
	(setq dh (- h2 h1) dm (- m2 m1))
	(when (< dm 0) (setq dm (+ dm 60) dh (1- dh)))
	(concat t1 "+" (number-to-string dh)
		(and (/= 0 dm) (format ":%02d" dm)))))))

(defvar calendar-setup)			; Dynamically scoped.
(defun org-read-date (&optional with-time to-time from-string prompt
				default-time default-input inactive)
  "Read a date, possibly a time, and make things smooth for the user.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month, year,
hour and minute.  If this command is called to replace a timestamp at point,
or to enter the second timestamp of a range, the default time is taken
from the existing stamp.  Furthermore, the command prefers the future,
so if you are giving a date where the year is not given, and the day-month
combination is already past in the current year, it will assume you
mean next year.  For details, see the manual.  A few examples:

  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  2/15          --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  12            --> currentyear-currentmonth-12
  Fri           --> nearest Friday after today
  -Tue          --> last Tuesday
  etc.

Furthermore you can specify a relative date by giving, as the *first* thing
in the input:  a plus/minus sign, a number and a letter [hdwmy] to indicate
change in days weeks, months, years.
With a single plus or minus, the date is relative to today.  With a double
plus or minus, it is relative to the date in DEFAULT-TIME.  E.g.
  +4d           --> four days from today
  +4            --> same as above
  +2w           --> two weeks from today
  ++5           --> five days from default date

The function understands only English month and weekday abbreviations.

While prompting, a calendar is popped up - you can also select the
date with the mouse (button 1).  The calendar shows a period of three
months.  To scroll it to other months, use the keys `>' and `<'.
There are many other calendar navigation commands available, see
Info node `(org) The date/time prompt' for a full list.

If you don't like the calendar, turn it off with
       (setq org-read-date-popup-calendar nil)

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument WITH-TIME, the prompt will suggest to
also insert a time.  Note that when WITH-TIME is not set, you can
still enter a time, and this function will inform the calling routine
about this change.  The calling routine may then choose to change the
format used to insert the time stamp into the buffer to include the time.
With optional argument FROM-STRING, read from this string instead from
the user.  PROMPT can overwrite the default prompt.  DEFAULT-TIME is
the time/date that is used for everything that is not specified by the
user."
  (require 'parse-time)
  (let* ((org-with-time with-time)
	 (org-time-stamp-rounding-minutes
	  (if (equal org-with-time '(16))
	      '(0 0)
	    org-time-stamp-rounding-minutes))
	 (ct (org-current-time))
	 (org-def (or org-overriding-default-time default-time ct))
	 (org-defdecode (decode-time org-def))
         (cur-frame (selected-frame))
	 (mouse-autoselect-window nil)	; Don't let the mouse jump
	 (calendar-setup
	  (and (eq calendar-setup 'calendar-only) 'calendar-only))
	 (calendar-move-hook nil)
	 (calendar-view-diary-initially-flag nil)
	 (calendar-view-holidays-initially-flag nil)
	 ans (org-ans0 "") org-ans1 org-ans2 final cal-frame)
    ;; Rationalize `org-def' and `org-defdecode', if required.
    ;; Only consider `org-extend-today-until' when explicit reference
    ;; time is not given.
    (when (and (not default-time)
               (not org-overriding-default-time)
               (< (decoded-time-hour org-defdecode) org-extend-today-until))
      (setf (decoded-time-hour org-defdecode) -1)
      (setf (decoded-time-minute org-defdecode) 59)
      (setq org-def (org-encode-time org-defdecode))
      (setq org-defdecode (decode-time org-def)))
    (let* ((timestr (format-time-string
		     (if org-with-time "%Y-%m-%d %H:%M" "%Y-%m-%d")
		     org-def))
	   (prompt (concat (if prompt (concat prompt " ") "")
			   (format "Date+time [%s]: " timestr))))
      (cond
       (from-string (setq ans from-string))
       (org-read-date-popup-calendar
	(save-excursion
	  (save-window-excursion
	    (calendar)
	    (when (eq calendar-setup 'calendar-only)
	      (setq cal-frame
		    (window-frame (get-buffer-window calendar-buffer 'visible)))
	      (select-frame cal-frame))
	    ;; FIXME: Not sure we need `with-current-buffer' but I couldn't
            ;; convince myself that we're always in `calendar-buffer' after
            ;; the call to `calendar'.
	    (with-current-buffer calendar-buffer (setq cursor-type nil))
	    (unwind-protect
		(let ((days (- (time-to-days org-def)
			       (calendar-absolute-from-gregorian
				(calendar-current-date)))))
		  (org-funcall-in-calendar #'calendar-forward-day t days)
		  (let* ((old-map (current-local-map))
			 (map (copy-keymap calendar-mode-map))
			 (minibuffer-local-map
			  (copy-keymap org-read-date-minibuffer-local-map)))
		    (org-defkey map (kbd "RET") 'org-calendar-select)
		    (org-defkey map [mouse-1] 'org-calendar-select-mouse)
		    (org-defkey map [mouse-2] 'org-calendar-select-mouse)
		    (unwind-protect
			(progn
			  (use-local-map map)
			  (setq org-read-date-inactive inactive)
			  (add-hook 'post-command-hook 'org-read-date-display)
			  (setq org-ans0
				(read-string prompt
                                             (and default-input
                                                  (or (org-get-compact-tod default-input)
					              default-input))
					     'org-read-date-history
					     nil))
			  ;; org-ans0: from prompt
			  ;; org-ans1: from mouse click
			  ;; org-ans2: from calendar motion
			  (setq ans
				(concat org-ans0 " " (or org-ans1 org-ans2))))
		      (remove-hook 'post-command-hook 'org-read-date-display)
		      (use-local-map old-map)
		      (when org-read-date-overlay
			(delete-overlay org-read-date-overlay)
			(setq org-read-date-overlay nil)))))
	      (bury-buffer calendar-buffer)
	      (when cal-frame
		(delete-frame cal-frame)
		(select-frame-set-input-focus cur-frame))))))

       (t				; Naked prompt only
	(unwind-protect
	    (setq ans (read-string prompt default-input
				   'org-read-date-history timestr))
	  (when org-read-date-overlay
	    (delete-overlay org-read-date-overlay)
	    (setq org-read-date-overlay nil))))))

    (setq final (org-read-date-analyze ans org-def org-defdecode))

    (when org-read-date-analyze-forced-year
      (message "Year was forced into %s"
	       (if org-read-date-force-compatible-dates
		   "compatible range (1970-2037)"
		 "range representable on this machine"))
      (ding))

    (setq final (org-encode-time final))

    (setq org-read-date-final-answer ans)

    (if to-time
	final
      ;; This round-trip gets rid of 34th of August and stuff like that....
      (setq final (decode-time final))
      (if (and (boundp 'org-time-was-given) org-time-was-given)
	  (format "%04d-%02d-%02d %02d:%02d"
		  (decoded-time-year final) (decoded-time-month final)
                  (decoded-time-day final) (decoded-time-hour final)
                  (decoded-time-minute final))
	(format "%04d-%02d-%02d" (decoded-time-year final)
                (decoded-time-month final) (decoded-time-day final))))))

(defun org-read-date-display ()
  "Display the current date prompt interpretation in the minibuffer."
  (when org-read-date-display-live
    (when org-read-date-overlay
      (delete-overlay org-read-date-overlay))
    (when (minibufferp (current-buffer))
      (save-excursion
	(end-of-line 1)
	(while (not (equal (buffer-substring
			    (max (point-min) (- (point) 4)) (point))
			   "    "))
	  (insert " ")))
      (let* ((ans (concat (buffer-substring (line-beginning-position)
                                            (point-max))
			  " " (or org-ans1 org-ans2)))
	     (org-end-time-was-given nil)
	     (f (org-read-date-analyze ans org-def org-defdecode))
	     (fmt (org-time-stamp-format
                   (or org-with-time
                       (and (boundp 'org-time-was-given) org-time-was-given))
                   org-read-date-inactive
                   org-display-custom-times))
	     (txt (format-time-string fmt (org-encode-time f)))
	     (txt (concat "=> " txt)))
	(when (and org-end-time-was-given
		   (string-match org-plain-time-of-day-regexp txt))
	  (setq txt (concat (substring txt 0 (match-end 0)) "-"
			    org-end-time-was-given
			    (substring txt (match-end 0)))))
	(when org-read-date-analyze-futurep
	  (setq txt (concat txt " (=>F)")))
	(setq org-read-date-overlay
              (make-overlay (1- (line-end-position)) (line-end-position)))
        ;; Avoid priority race with overlay used by calendar.el.
        ;; See bug#69271.
        (overlay-put org-read-date-overlay 'priority 1)
	(org-overlay-display org-read-date-overlay txt 'secondary-selection)))))

(defun org-read-date-analyze (ans def defdecode)
  "Analyze the combined answer of the date prompt."
  ;; FIXME: cleanup and comment
  (let ((org-def def)
	(org-defdecode defdecode)
	(nowdecode (decode-time))
	delta deltan deltaw deltadef year month day
	hour minute second wday pm h2 m2 tl wday1
	iso-year iso-weekday iso-week iso-date futurep kill-year)
    (setq org-read-date-analyze-futurep nil
	  org-read-date-analyze-forced-year nil)
    (when (string-match "\\`[ \t]*\\.[ \t]*\\'" ans)
      (setq ans "+0"))

    (when (setq delta (org-read-date-get-relative ans nil org-def))
      (setq ans (replace-match "" t t ans)
	    deltan (car delta)
	    deltaw (nth 1 delta)
	    deltadef (nth 2 delta)))

    ;; Check if there is an iso week date in there.  If yes, store the
    ;; info and postpone interpreting it until the rest of the parsing
    ;; is done.
    (when (string-match "\\<\\(?:\\([0-9]+\\)-\\)?[wW]\\([0-9]\\{1,2\\}\\)\\(?:-\\([0-6]\\)\\)?\\([ \t]\\|$\\)" ans)
      (setq iso-year (when (match-end 1)
		       (org-small-year-to-year
			(string-to-number (match-string 1 ans))))
	    iso-weekday (when (match-end 3)
			  (string-to-number (match-string 3 ans)))
	    iso-week (string-to-number (match-string 2 ans)))
      (setq ans (replace-match "" t t ans)))

    ;; Help matching ISO dates with single digit month or day, like 2006-8-11.
    (when (string-match
	   "^ *\\(\\([0-9]+\\)-\\)?\\([0-1]?[0-9]\\)-\\([0-3]?[0-9]\\)\\([^-0-9]\\|$\\)" ans)
      (setq year (if (match-end 2)
		     (string-to-number (match-string 2 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    month (string-to-number (match-string 3 ans))
	    day (string-to-number (match-string 4 ans)))
      (setq year (org-small-year-to-year year))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))

    ;; Help matching dotted european dates
    (when (string-match
	   "^ *\\(3[01]\\|0?[1-9]\\|[12][0-9]\\)\\. ?\\(0?[1-9]\\|1[012]\\)\\.\\( ?[1-9][0-9]\\{3\\}\\)?" ans)
      (setq year (if (match-end 3) (string-to-number (match-string 3 ans))
		   (setq kill-year t)
		   (string-to-number (format-time-string "%Y")))
	    day (string-to-number (match-string 1 ans))
	    month (string-to-number (match-string 2 ans))
	    ans (replace-match (format "%04d-%02d-%02d" year month day)
			       t nil ans)))

    ;; Help matching american dates, like 5/30 or 5/30/7
    (when (string-match
	   "^ *\\(0?[1-9]\\|1[012]\\)/\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\(/\\([0-9]+\\)\\)?\\([^/0-9]\\|$\\)" ans)
      (setq year (if (match-end 4)
		     (string-to-number (match-string 4 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    month (string-to-number (match-string 1 ans))
	    day (string-to-number (match-string 2 ans)))
      (setq year (org-small-year-to-year year))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))
    ;; Help matching am/pm times, because `parse-time-string' does not do that.
    ;; If there is a time with am/pm, and *no* time without it, we convert
    ;; so that matching will be successful.
    (cl-loop for i from 1 to 2 do	; twice, for end time as well
	     (when (and (not (string-match "\\(\\`\\|[^+]\\)[012]?[0-9]:[0-9][0-9]\\([ \t\n]\\|$\\)" ans))
			(string-match "\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\(am\\|AM\\|pm\\|PM\\)\\>" ans))
	       (setq hour (string-to-number (match-string 1 ans))
		     minute (if (match-end 3)
				(string-to-number (match-string 3 ans))
			      0)
		     pm (equal ?p
			       (string-to-char (downcase (match-string 4 ans)))))
	       (if (and (= hour 12) (not pm))
		   (setq hour 0)
		 (when (and pm (< hour 12)) (setq hour (+ 12 hour))))
	       (setq ans (replace-match (format "%02d:%02d" hour minute)
					t t ans))))

    ;; Help matching HHhMM times, similarly as for am/pm times.
    (cl-loop for i from 1 to 2 do	; twice, for end time as well
	     (when (and (not (string-match "\\(\\`\\|[^+]\\)[012]?[0-9]:[0-9][0-9]\\([ \t\n]\\|$\\)" ans))
			(string-match "\\(?:\\(?1:[012]?[0-9]\\)?h\\(?2:[0-5][0-9]\\)\\)\\|\\(?:\\(?1:[012]?[0-9]\\)h\\(?2:[0-5][0-9]\\)?\\)\\>" ans))
	       (setq hour (if (match-end 1)
			      (string-to-number (match-string 1 ans))
			    0)
		     minute (if (match-end 2)
				(string-to-number (match-string 2 ans))
			      0))
	       (setq ans (replace-match (format "%02d:%02d" hour minute)
					t t ans))))

    ;; Check if a time range is given as a duration
    (when (string-match "\\([012]?[0-9]\\):\\([0-6][0-9]\\)\\+\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?" ans)
      (setq hour (string-to-number (match-string 1 ans))
	    h2 (+ hour (string-to-number (match-string 3 ans)))
	    minute (string-to-number (match-string 2 ans))
	    m2 (+ minute (if (match-end 5) (string-to-number
					    (match-string 5 ans))0)))
      (when (>= m2 60) (setq h2 (1+ h2) m2 (- m2 60)))
      (setq ans (replace-match (format "%02d:%02d-%02d:%02d" hour minute h2 m2)
			       t t ans)))

    ;; Check if there is a time range
    (when (boundp 'org-end-time-was-given)
      (setq org-time-was-given nil)
      (when (and (string-match org-plain-time-of-day-regexp ans)
		 (match-end 8))
	(setq org-end-time-was-given (match-string 8 ans))
	(setq ans (concat (substring ans 0 (match-beginning 7))
			  (substring ans (match-end 7))))))

    (setq tl (condition-case _
                 (parse-time-string ans)
               (error (parse-time-string "")))
	  day (or (decoded-time-day tl) (decoded-time-day org-defdecode))
	  month
	  (cond ((decoded-time-month tl))
		((not org-read-date-prefer-future) (decoded-time-month org-defdecode))
		;; Day was specified.  Make sure DAY+MONTH
		;; combination happens in the future.
		((decoded-time-day tl)
		 (setq futurep t)
		 (if (< day (decoded-time-day nowdecode)) (1+ (decoded-time-month nowdecode))
		   (decoded-time-month nowdecode)))
		(t (decoded-time-month org-defdecode)))
	  year
	  (cond ((and (not kill-year) (decoded-time-year tl)))
		((not org-read-date-prefer-future) (decoded-time-year org-defdecode))
		;; Month was guessed in the future and is at least
		;; equal to NOWDECODE's.  Fix year accordingly.
		(futurep
		 (if (or (> month (decoded-time-month nowdecode))
			 (>= day (decoded-time-day nowdecode)))
		     (decoded-time-year nowdecode)
		   (1+ (decoded-time-year nowdecode))))
		;; Month was specified.  Make sure MONTH+YEAR
		;; combination happens in the future.
		((decoded-time-month tl)
		 (setq futurep t)
		 (cond ((> month (decoded-time-month nowdecode)) (decoded-time-year nowdecode))
		       ((< month (decoded-time-month nowdecode)) (1+ (decoded-time-year nowdecode)))
		       ((< day (decoded-time-day nowdecode)) (1+ (decoded-time-year nowdecode)))
		       (t (decoded-time-year nowdecode))))
		(t (decoded-time-year org-defdecode)))
	  hour (or (decoded-time-hour tl) (decoded-time-hour org-defdecode))
	  minute (or (decoded-time-minute tl) (decoded-time-minute org-defdecode))
	  second (or (decoded-time-second tl) 0)
	  wday (decoded-time-weekday tl))

    (when (and (eq org-read-date-prefer-future 'time)
	       (not (decoded-time-day tl)) (not (decoded-time-month tl)) (not (decoded-time-year tl))
	       (equal day (decoded-time-day nowdecode))
	       (equal month (decoded-time-month nowdecode))
	       (equal year (decoded-time-year nowdecode))
	       (decoded-time-hour tl)
	       (or (< (decoded-time-hour tl) (decoded-time-hour nowdecode))
		   (and (= (decoded-time-hour tl) (decoded-time-hour nowdecode))
			(decoded-time-minute tl)
			(< (decoded-time-minute tl) (decoded-time-minute nowdecode)))))
      (setq day (1+ day)
	    futurep t))

    ;; Special date definitions below
    (cond
     (iso-week
      ;; There was an iso week
      (require 'cal-iso)
      (setq futurep nil)
      (setq year (or iso-year year)
	    day (or iso-weekday wday 1)
	    wday nil ; to make sure that the trigger below does not match
	    iso-date (calendar-gregorian-from-absolute
		      (calendar-iso-to-absolute
		       (list iso-week day year))))
					; FIXME:  Should we also push ISO weeks into the future?
					;      (when (and org-read-date-prefer-future
					;		 (not iso-year)
					;		 (< (calendar-absolute-from-gregorian iso-date)
					;		    (time-to-days nil)))
					;	(setq year (1+ year)
					;	      iso-date (calendar-gregorian-from-absolute
					;			(calendar-iso-to-absolute
					;			 (list iso-week day year)))))
      (setq month (calendar-extract-month iso-date)
	    year (calendar-extract-year iso-date)
	    day (calendar-extract-day iso-date)))
     (deltan
      (setq futurep nil)
      (unless deltadef
	(let ((now (decode-time)))
	  (setq day (decoded-time-day now)
                month (decoded-time-month now)
                year (decoded-time-year now))))
      ;; FIXME: Duplicated value in ‘cond’: ""
      (cond ((member deltaw '("h" ""))
             (when (boundp 'org-time-was-given)
               (setq org-time-was-given t))
             (setq hour (+ hour deltan)))
            ((member deltaw '("d" "")) (setq day (+ day deltan)))
            ((equal deltaw "w") (setq day (+ day (* 7 deltan))))
            ((equal deltaw "m") (setq month (+ month deltan)))
            ((equal deltaw "y") (setq year (+ year deltan)))))
     ((and wday (not (decoded-time-day tl)))
      ;; Weekday was given, but no day, so pick that day in the week
      ;; on or after the derived date.
      (setq wday1 (decoded-time-weekday (decode-time (org-encode-time 0 0 0 day month year))))
      (unless (equal wday wday1)
	(setq day (+ day (% (- wday wday1 -7) 7))))))
    (when (and (boundp 'org-time-was-given)
	       (decoded-time-hour tl))
      (setq org-time-was-given t))
    (when (< year 100) (setq year (+ 2000 year)))
    ;; Check of the date is representable
    (if org-read-date-force-compatible-dates
	(progn
	  (when (< year 1970)
	    (setq year 1970 org-read-date-analyze-forced-year t))
	  (when (> year 2037)
	    (setq year 2037 org-read-date-analyze-forced-year t)))
      (condition-case nil
	  (ignore (org-encode-time second minute hour day month year))
	(error
	 (setq year (decoded-time-year org-defdecode))
	 (setq org-read-date-analyze-forced-year t))))
    (setq org-read-date-analyze-futurep futurep)
    (list second minute hour day month year nil -1 nil)))

(defvar parse-time-weekdays)
(defun org-read-date-get-relative (s today default)
  "Check string S for special relative date string.
TODAY and DEFAULT are internal times, for today and for a default.
Return shift list (N what def-flag)
WHAT       is \"d\", \"w\", \"m\", or \"y\" for day, week, month, year.
N          is the number of WHATs to shift.
DEF-FLAG   is t when a double ++ or -- indicates shift relative to
           the DEFAULT date rather than TODAY."
  (require 'parse-time)
  (when (and
         ;; Force case-insensitive.
         (let ((case-fold-search t))
	   (string-match
	    (concat
	     "\\`[ \t]*\\([-+]\\{0,2\\}\\)"
	     "\\([0-9]+\\)?"
	     "\\([hdwmy]\\|\\(" (mapconcat #'car parse-time-weekdays "\\|") "\\)\\)?"
	     "\\([ \t]\\|$\\)") s))
	 (or (> (match-end 1) (match-beginning 1)) (match-end 4)))
    (let* ((dir (if (> (match-end 1) (match-beginning 1))
		    (string-to-char (substring (match-string 1 s) -1))
		  ?+))
	   (rel (and (match-end 1) (= 2 (- (match-end 1) (match-beginning 1)))))
	   (n (if (match-end 2) (string-to-number (match-string 2 s)) 1))
	   (what (if (match-end 3) (match-string 3 s) "d"))
	   (wday1 (cdr (assoc (downcase what) parse-time-weekdays)))
	   (date (if rel default today))
	   (wday (decoded-time-weekday (decode-time date)))
	   delta)
      (if wday1
	  (progn
	    (setq delta (mod (+ 7 (- wday1 wday)) 7))
	    (when (= delta 0) (setq delta 7))
	    (when (= dir ?-)
	      (setq delta (- delta 7))
	      (when (= delta 0) (setq delta -7)))
	    (when (> n 1) (setq delta (+ delta (* (1- n) (if (= dir ?-) -7 7)))))
	    (list delta "d" rel))
	(list (* n (if (= dir ?-) -1 1)) what rel)))))

(defun org-order-calendar-date-args (arg1 arg2 arg3)
  "Turn a user-specified date into the internal representation.
The internal representation needed by the calendar is (month day year).
This is a wrapper to handle the brain-dead convention in calendar that
user function argument order change dependent on argument order."
  (pcase calendar-date-style
    (`american (list arg1 arg2 arg3))
    (`european (list arg2 arg1 arg3))
    (`iso (list arg2 arg3 arg1))))

(defun org-funcall-in-calendar (func &optional keepdate &rest args)
  "Call FUNC in the calendar window and return to current window.
Unless KEEPDATE is non-nil, update `org-ans2' to the cursor date."
  (with-selected-window (get-buffer-window calendar-buffer t)
    (apply func args)
    (when (and (not keepdate) (calendar-cursor-to-date))
      (let* ((date (calendar-cursor-to-date))
	     (time (org-encode-time
                    0 0 0
                    (calendar-extract-day date)
                    (calendar-extract-month date)
                    (calendar-extract-year date))))
	(setq org-ans2 (format-time-string "%Y-%m-%d" time))))
    (move-overlay org-date-ovl (1- (point)) (1+ (point)) (current-buffer))))

(defun org-eval-in-calendar (form &optional keepdate)
  (declare (obsolete org-funcall-in-calendar "2024"))
  (org-funcall-in-calendar (lambda () (eval form t)) keepdate))

(defun org-calendar-goto-today-or-insert-dot ()
  "Go to the current date, or insert a dot.

If at the beginning of the prompt, behave as `org-calendar-goto-today' else
insert \".\"."
  (interactive)
  ;; Are we at the beginning of the prompt?
  (if (looking-back "^[^:]+: "
		    (let ((inhibit-field-text-motion t))
		      (line-beginning-position)))
      (org-funcall-in-calendar #'calendar-goto-today)
    (insert ".")))

(defun org-calendar-goto-today ()
  "Reposition the calendar window so the current date is visible."
  (interactive)
  (org-funcall-in-calendar #'calendar-goto-today))

(defun org-calendar-backward-month ()
  "Move the cursor backward by one month."
  (interactive)
  (org-funcall-in-calendar #'calendar-backward-month nil 1))

(defun org-calendar-forward-month ()
  "Move the cursor forward by one month."
  (interactive)
  (org-funcall-in-calendar #'calendar-forward-month nil 1))

(defun org-calendar-backward-year ()
  "Move the cursor backward by one year."
  (interactive)
  (org-funcall-in-calendar #'calendar-backward-year nil 1))

(defun org-calendar-forward-year ()
  "Move the cursor forward by one year."
  (interactive)
  (org-funcall-in-calendar #'calendar-forward-year nil 1))

(defun org-calendar-backward-week ()
  "Move the cursor backward by one week."
  (interactive)
  (org-funcall-in-calendar #'calendar-backward-week nil 1))

(defun org-calendar-forward-week ()
  "Move the cursor forward by one week."
  (interactive)
  (org-funcall-in-calendar #'calendar-forward-week nil 1))

(defun org-calendar-backward-day ()
  "Move the cursor backward by one day."
  (interactive)
  (org-funcall-in-calendar #'calendar-backward-day nil 1))

(defun org-calendar-forward-day ()
  "Move the cursor forward by one day."
  (interactive)
  (org-funcall-in-calendar #'calendar-forward-day nil 1))

(defun org-calendar-view-entries ()
  "Prepare and display a buffer with diary entries."
  (interactive)
  (org-funcall-in-calendar #'diary-view-entries)
  (message ""))

(defun org-calendar-scroll-month-left ()
  "Scroll the displayed calendar left by one month."
  (interactive)
  (org-funcall-in-calendar #'calendar-scroll-left nil 1))

(defun org-calendar-scroll-month-right ()
  "Scroll the displayed calendar right by one month."
  (interactive)
  (org-funcall-in-calendar #'calendar-scroll-right nil 1))

;; FIXME: Since Emacs 31, calendar.el has `calendar-total-months'.
;; So, we are not neccesarily scrolling "three" months.
;; Maybe rename.
(defun org-calendar-scroll-three-months-left ()
  "Scroll the displayed calendar left by three months."
  (interactive)
  (org-funcall-in-calendar
   (if (fboundp 'calendar-scroll-calendar-left)
       #'calendar-scroll-calendar-left
     (with-suppressed-warnings ((obsolete calendar-scroll-left-three-months))
       #'calendar-scroll-left-three-months))
   nil 1))

(defun org-calendar-scroll-three-months-right ()
  "Scroll the displayed calendar right by three months."
  (interactive)
  (org-funcall-in-calendar
   (if (fboundp 'calendar-scroll-calendar-right)
       #'calendar-scroll-calendar-right
     (with-suppressed-warnings ((obsolete calendar-scroll-right-three-months))
       #'calendar-scroll-right-three-months))
   nil 1))

(defun org-calendar-select ()
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (org-encode-time
                  0 0 0
                  (calendar-extract-day date)
                  (calendar-extract-month date)
                  (calendar-extract-year date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (when (active-minibuffer-window) (exit-minibuffer))))

(defun org-calendar-select-mouse (ev)
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive "e")
  (mouse-set-point ev)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (org-encode-time
                  0 0 0
                  (calendar-extract-day date)
                  (calendar-extract-month date)
                  (calendar-extract-year date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (when (active-minibuffer-window) (exit-minibuffer))))

(provide 'org-read-date)

;;; org-read-date.el ends here
