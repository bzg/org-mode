;;; org-timestamp.el --- Commands to work with timestamps         -*- lexical-binding: t; -*-

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
;; This library implements timestamp-related commands for Org mode.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'time-date)

(require 'org-element)
(require 'org-time)
(require 'org-read-date)
(require 'org-move)

(declare-function org-todo "org")
(declare-function org-at-date-range-p "org")
(declare-function org-get-date-from-calendar "org")
(declare-function org-clock-update-time-maybe "org-clock")
(declare-function org-get-heading "org")
(declare-function org-recenter-calendar "org")

(defvaralias 'org-time-stamp-rounding-minutes 'org-timestamp-rounding-minutes)
(defcustom org-timestamp-rounding-minutes '(0 5)
  "Number of minutes to round time stamps to.
\\<org-mode-map>\
These are two values, the first applies when first creating a time stamp.
The second applies when changing it with the commands `S-up' and `S-down'.
When changing the time stamp, this means that it will change in steps
of N minutes, as given by the second value.

When a setting is 0 or 1, insert the time unmodified.  Useful rounding
numbers should be factors of 60, so for example 5, 10, 15.

When this is larger than 1, you can still force an exact time stamp by using
a double prefix argument to a time stamp command like \
`\\[org-timestamp]' or `\\[org-timestamp-inactive],
and by using a prefix arg to `S-up/down' to specify the exact number
of minutes to shift."
  :group 'org-time
  :get (lambda (var) ; Make sure both elements are there
	 (if (integerp (default-value var))
	     (list (default-value var) 5)
	   (default-value var)))
  :type '(list
	  (integer :tag "when inserting times")
	  (integer :tag "when modifying times")))

;; Normalize old customizations of this variable.
(when (integerp org-time-stamp-rounding-minutes)
  (setq org-time-stamp-rounding-minutes
	(list org-time-stamp-rounding-minutes
	      org-time-stamp-rounding-minutes)))

(defcustom org-edit-timestamp-down-means-later nil
  "Non-nil means S-down will increase the time in a time stamp.
When nil, S-up will increase."
  :group 'org-time
  :type 'boolean)

(defcustom org-calendar-follow-timestamp-change t
  "Non-nil means make the calendar window follow timestamp changes.
When a timestamp is modified and the calendar window is visible, it will be
moved to the new date."
  :group 'org-time
  :type 'boolean)

(defvar org-last-changed-timestamp nil)
(defvar org-last-inserted-timestamp nil
  "The last time stamp inserted with `org-insert-timestamp'.")

(defvar org-time-was-given) ; dynamically scoped parameter
(defvar org-end-time-was-given) ; dynamically scoped parameter

(defalias 'org-time-stamp #'org-timestamp)
(defun org-timestamp (arg &optional inactive)
  "Prompt for a date/time and insert a time stamp.

If the user specifies a time like HH:MM or if this command is
called with at least one prefix argument, the time stamp contains
the date and the time.  Otherwise, only the date is included.

All parts of a date not specified by the user are filled in from
the timestamp at point, if any, or the current date/time
otherwise.

If there is already a timestamp at the cursor, it is replaced.

With two universal prefix arguments, insert an active timestamp
with the current time without prompting the user.

When called from Lisp, the timestamp is inactive if INACTIVE is
non-nil."
  (interactive "P" org-mode)
  (let* ((ts (cond
	      ((org-at-date-range-p t)
	       (match-string (if (< (point) (- (match-beginning 2) 2)) 1 2)))
	      ((org-at-timestamp-p 'lax) (match-string 0))))
	 ;; Default time is either the timestamp at point or today.
	 ;; When entering a range, only the range start is considered.
         (default-time (and ts (org-time-string-to-time ts)))
         (default-input (and ts (org-get-compact-tod ts)))
         (repeater (and ts
			(string-match "\\([.+-]+[0-9]+[hdwmy] ?\\)+" ts)
			(match-string 0 ts)))
	 org-time-was-given
	 org-end-time-was-given
	 (time
	  (if (equal arg '(16)) (current-time)
	    ;; Preserve `this-command' and `last-command'.
	    (let ((this-command this-command)
		  (last-command last-command))
	      (org-read-date
	       arg 'totime nil nil default-time default-input
	       inactive)))))
    (cond
     ((and ts
           (memq last-command '( org-time-stamp org-time-stamp-inactive
                                 org-timestamp org-timestamp-inactive))
           (memq this-command '( org-time-stamp org-time-stamp-inactive
                                 org-timestamp org-timestamp-inactive)))
      (insert "--")
      (org-insert-timestamp time (or org-time-was-given arg) inactive))
     (ts
      ;; Make sure we're on a timestamp.  When in the middle of a date
      ;; range, move arbitrarily to range end.
      (unless (org-at-timestamp-p 'lax)
	(skip-chars-forward "-")
	(org-at-timestamp-p 'lax))
      (replace-match "")
      (setq org-last-changed-timestamp
	    (org-insert-timestamp
	     time (or org-time-was-given arg)
	     inactive nil nil (list org-end-time-was-given)))
      (when repeater
	(backward-char)
	(insert " " repeater)
	(setq org-last-changed-timestamp
	      (concat (substring org-last-inserted-timestamp 0 -1)
		      " " repeater ">")))
      (message "Timestamp updated"))
     ((equal arg '(16)) (org-insert-timestamp time t inactive))
     (t (org-insert-timestamp
	 time (or org-time-was-given arg) inactive nil nil
	 (list org-end-time-was-given))))))

;; FIXME: can we use this for something else, like computing time differences?
(defalias 'org-time-stamp-inactive #'org-timestamp-inactive)
(defun org-timestamp-inactive (&optional arg)
  "Insert an inactive time stamp.

An inactive time stamp is enclosed in square brackets instead of
angle brackets.  It is inactive in the sense that it does not
trigger agenda entries.  So these are more for recording a
certain time/date.

If the user specifies a time like HH:MM or if this command is called with
at least one prefix argument, the time stamp contains the date and the time.
Otherwise, only the date is included.

When called with two universal prefix arguments, insert an inactive time stamp
with the current time without prompting the user."
  (interactive "P" org-mode)
  (org-timestamp arg 'inactive))

(defalias 'org-insert-time-stamp #'org-insert-timestamp)
(defun org-insert-timestamp (time &optional with-hm inactive pre post extra)
  "Insert a date stamp for the date given by the internal TIME.
See `format-time-string' for the format of TIME.
WITH-HM means use the stamp format that includes the time of the day.
INACTIVE means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda.
PRE and POST are optional strings to be inserted before and after the
stamp.
The command returns the inserted time stamp."
  (org-fold-core-ignore-modifications
    (let ((fmt (org-time-stamp-format with-hm inactive))
	  stamp)
      (insert-before-markers-and-inherit (or pre ""))
      (when (listp extra)
        (setq extra (car extra))
        (if (and (stringp extra)
	         (string-match "\\([0-9]+\\):\\([0-9]+\\)" extra))
	    (setq extra (format "-%02d:%02d"
			        (string-to-number (match-string 1 extra))
			        (string-to-number (match-string 2 extra))))
	  (setq extra nil)))
      (when extra
        (setq fmt (concat (substring fmt 0 -1) extra (substring fmt -1))))
      (insert-before-markers-and-inherit (setq stamp (format-time-string fmt time)))
      (insert-before-markers-and-inherit (or post ""))
      (setq org-last-inserted-timestamp stamp))))

(defun org-timestamp-up (&optional arg)
  "Increase the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month,
the day or the time, change that.  If the cursor is on the enclosing
bracket, change the timestamp type.
With prefix ARG, change by that many units."
  (interactive "p" org-mode)
  (org-timestamp-change (prefix-numeric-value arg) nil 'updown))

(defun org-timestamp-down (&optional arg)
  "Decrease the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month,
the day or the time, change that.  If the cursor is on the enclosing
bracket, change the timestamp type.
With prefix ARG, change by that many units."
  (interactive "p" org-mode)
  (org-timestamp-change (- (prefix-numeric-value arg)) nil 'updown))

(defun org-timestamp-up-day (&optional arg)
  "Increase the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p" org-mode)
  (if (and (not (org-at-timestamp-p 'lax))
	   (org-at-heading-p))
      (org-todo 'up)
    (org-timestamp-change (prefix-numeric-value arg) 'day 'updown)))

(defun org-timestamp-down-day (&optional arg)
  "Decrease the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p" org-mode)
  (if (and (not (org-at-timestamp-p 'lax))
	   (org-at-heading-p))
      (org-todo 'down)
    (org-timestamp-change (- (prefix-numeric-value arg)) 'day) 'updown))

(defun org-toggle-timestamp-type ()
  "Toggle the type (<active> or [inactive]) of a time stamp."
  (interactive nil org-mode)
  (when (org-at-timestamp-p 'lax)
    (let ((beg (match-beginning 0)) (end (match-end 0))
	  (map '((?\[ . "<") (?\] . ">") (?< . "[") (?> . "]"))))
      (save-excursion
	(goto-char beg)
	(while (re-search-forward "[][<>]" end t)
	  (replace-match (cdr (assoc (char-after (match-beginning 0)) map))
			 t t)))
      (message "Timestamp is now %sactive"
	       (if (equal (char-after beg) ?<) "" "in")))))

(defvar org-clock-history)                     ; defined in org-clock.el
(defvar org-clock-adjust-closest nil)          ; defined in org-clock.el
(defun org-timestamp-change (n &optional what updown suppress-tmp-delay)
  "Change the date in the time stamp at point.

The date is changed by N times WHAT.  WHAT can be `day', `month',
`year', `hour', or `minute'.  If WHAT is not given, the cursor
position in the timestamp determines what is changed.

When optional argument UPDOWN is non-nil, minutes are rounded
according to `org-time-stamp-rounding-minutes'.

When SUPPRESS-TMP-DELAY is non-nil, suppress delays like
\"--2d\"."
  (let ((origin (point))
	(timestamp? (org-at-timestamp-p 'lax))
	origin-cat
	with-hm inactive
	(dm (max (nth 1 org-time-stamp-rounding-minutes) 1))
	extra rem
	ts time time-original time0 fixnext clrgx)
    (unless timestamp? (user-error "Not at a timestamp"))
    (if (and (not what) (eq timestamp? 'bracket))
	(org-toggle-timestamp-type)
      ;; Point isn't on brackets.  Remember the part of the timestamp
      ;; the point was in.  Indeed, size of timestamps may change,
      ;; but point must be kept in the same category nonetheless.
      (setq origin-cat timestamp?)
      (when (and (not what) (not (eq timestamp? 'day))
		 org-display-custom-times
		 (get-text-property (point) 'display)
		 (not (get-text-property (1- (point)) 'display)))
	(setq timestamp? 'day))
      (setq timestamp? (or what timestamp?)
	    inactive (= (char-after (match-beginning 0)) ?\[)
	    ts (match-string 0))
      ;; FIXME: Instead of deleting everything and then inserting
      ;; later, we should make use of `replace-match', which preserves
      ;; markers.  The current implementation suffers from
      ;; `save-excursion' not preserving point inside the timestamp
      ;; once we delete the timestamp here.  The point moves to the
      ;; updated timestamp end.
      (replace-match "")
      (when (string-match
	     "\\(\\(-[012][0-9]:[0-5][0-9]\\)?\\( +[.+]?-?[-+][0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)*\\)[]>]"
	     ts)
	(setq extra (match-string 1 ts))
	(when suppress-tmp-delay
	  (setq extra (replace-regexp-in-string " --[0-9]+[hdwmy]" "" extra))))
      (when (string-match "^.\\{10\\}.*?[0-9]+:[0-9][0-9]" ts)
	(setq with-hm t))
      (setq time0 (org-parse-time-string ts))
      ;; Capture the original timestamp for direction validation later
      (setq time-original (org-encode-time time0))
      (let ((increment n))
        (if (and updown
	         (eq timestamp? 'minute)
	         (not current-prefix-arg))
	    ;; This looks like s-up and s-down.  Change by one rounding step.
            (progn
              (setq increment (* dm (cond ((> n 0) 1) ((< n 0) -1) (t 0))))
              (unless (= 0 (setq rem (% (decoded-time-minute time0) dm)))
                (setf (decoded-time-minute time0)
                      (+ (decoded-time-minute time0)
                         (if (> n 0) (- rem) (- dm rem))))))
          ;; Do not round anything in `org-modify-ts-extra' when prefix
          ;; argument is supplied - just use whatever is provided by the
          ;; prefix argument.
          (setq dm 1))
        (setq time
	      (org-encode-time
               (if-let* ((unit
                          (cl-case timestamp?
                            (minute :minute)
                            (hour :hour)
                            (day :day)
                            (month :month)
                            (year :year))))
                   (org-decoded-time-add
                    time0
                    (make-decoded-time unit increment))
                 time0))))
      ;; Validation if we're modifying hour or minute fields
      (when (and with-hm
                 (memq timestamp? '(hour minute))
                 (not (zerop n)))
        ;; Use time-less-p to compare the original timestamp to the
        ;; new one so that we ensure that the direction was
        ;; respected. In the case of the DST gap, normalization of the
        ;; timestamp post-shift results in wrapping that does not
        ;; match the intended direction e.g. 3:00 on the DST date
        ;; shifted down by 5 minutes results in 3:55
        (unless (if (> n 0)
                    (time-less-p time-original time)
                  (time-less-p time time-original))
          (insert ts)
          (user-error "Cannot shift %s into the DST gap (according to current timezone '%s')"
                      ts
                      (cadr (current-time-zone time-original)))))
      (when (and (memq timestamp? '(hour minute))
		 extra
		 (string-match "-\\([012][0-9]\\):\\([0-5][0-9]\\)" extra))
        ;; When modifying the start time in HH:MM-HH:MM range, update
        ;; end time as well.
	(setq extra (org-modify-ts-extra
		     extra ;; -HH:MM ...
                     ;; Fake position in EXTRA to force changing hours
                     ;; or minutes as needed.
		     (if (eq timestamp? 'hour)
                         2 ;; -H<H>:MM
                       5) ;; -HH:M<M>
		     n dm)))
      (when (integerp timestamp?)
	(setq extra (org-modify-ts-extra extra timestamp? n dm)))
      (when (eq what 'calendar)
	(let ((cal-date (org-get-date-from-calendar)))
          (setq time (org-encode-time
                      (decoded-time-set-defaults
                       (make-decoded-time
                        :month (calendar-extract-month cal-date)
                        :day   (calendar-extract-day   cal-date)
                        :year  (calendar-extract-year  cal-date)
                        :second (decoded-time-second time0)
                        :minute (decoded-time-minute time0)
                        :hour   (decoded-time-hour   time0)))))))
      ;; Insert the new timestamp, and ensure point stays in the same
      ;; category as before (i.e. not after the last position in that
      ;; category).
      (let ((pos (point)))
	;; Stay before inserted string. `save-excursion' is of no use.
	(setq org-last-changed-timestamp
	      (org-insert-timestamp time with-hm inactive nil nil extra))
	(goto-char pos))
      (save-match-data
	(looking-at org-ts-regexp3)
	(goto-char
	 (pcase origin-cat
	   ;; `day' category ends at the end of the weekday name if
	   ;; any (group 5), or before `hour' if any (group 7), or at
	   ;; the end of the timestamp (group 1).
	   (`day (min (cond ((match-end 5) (1- (match-end 5)))
                            ((match-beginning 7))
                            (t (1- (match-end 1))))
                      origin))
	   (`hour (min (match-end 7) origin))
	   (`minute (min (1- (match-end 8)) origin))
	   ((pred integerp) (min (1- (match-end 0)) origin))
	   ;; Point was right after the timestamp.  However, the
	   ;; timestamp length might have changed, so refer to
	   ;; (match-end 0) instead.
	   (`after (match-end 0))
	   ;; `year' and `month' have both fixed size: point couldn't
	   ;; have moved into another part.
	   (_ origin))))
      ;; Update clock if on a CLOCK line.
      (org-clock-update-time-maybe)
      ;; Maybe adjust the closest clock in `org-clock-history'
      (when org-clock-adjust-closest
	(if (not (and (org-at-clock-log-p)
		    (< 1 (length (delq nil (mapcar #'marker-position
						 org-clock-history))))))
	    (message "No clock to adjust")
	  (cond ((save-excursion	; fix previous clock?
		   (re-search-backward org-ts-regexp0 nil t)
		   (looking-back (concat org-clock-string " \\[")
				 (line-beginning-position)))
		 (setq fixnext 1 clrgx (concat org-ts-regexp0 "\\] =>.*$")))
		((save-excursion	; fix next clock?
		   (re-search-backward org-ts-regexp0 nil t)
		   (looking-at (concat org-ts-regexp0 "\\] =>")))
		 (setq fixnext -1 clrgx (concat org-clock-string " \\[" org-ts-regexp0))))
	  (save-window-excursion
	    ;; Find closest clock to point, adjust the previous/next one in history
	    (let* ((p (save-excursion (org-back-to-heading t)))
		   (cl (mapcar (lambda(c) (abs (- (marker-position c) p))) org-clock-history))
		   (clfixnth
		    (+ fixnext (- (length cl) (or (length (member (apply #'min cl) cl)) 100))))
		   (clfixpos (unless (> 0 clfixnth) (nth clfixnth org-clock-history))))
	      (if (not clfixpos)
		  (message "No clock to adjust")
		(save-excursion
		  (org-goto-marker-or-bmk clfixpos)
		  (org-fold-show-subtree)
		  (when (re-search-forward clrgx nil t)
		    (goto-char (match-beginning 1))
		    (let (org-clock-adjust-closest)
		      (org-timestamp-change n timestamp? updown))
		    (message "Clock adjusted in %s for heading: %s"
			     (file-name-nondirectory (buffer-file-name))
			     (org-get-heading t t)))))))))
      ;; Try to recenter the calendar window, if any.
      (when (and org-calendar-follow-timestamp-change
		 (get-buffer-window calendar-buffer t)
		 (memq timestamp? '(day month year)))
	(org-recenter-calendar (time-to-days time))))))

(defun org-modify-ts-extra (ts-string pos nincrements increment-step)
  "Change the lead-time/repeat fields at POS in timestamp string TS-STRING.
POS is the position in the timestamp string to be changed.
NINCREMENTS is the number of increments/decrements.

INCREMENT-STEP is step used for a single increment when POS in on
minutes.  Before incrementing minutes, they are rounded to
INCREMENT-STEP divisor."
  (let (;; increment order for dwmy: d-1=d; d+1=w; w+1=m; m+1=y; y+1=y.
        (idx '(("d" . 0) ("w" . 1) ("m" . 2) ("y" . 3) ("d" . -1) ("y" . 4)))
	pos-match-group hour minute new rem)
    (when (string-match "\\(-\\([012][0-9]\\):\\([0-5][0-9]\\)\\)?\\( +\\+\\([0-9]+\\)\\([dmwy]\\)\\)?\\( +-\\([0-9]+\\)\\([dmwy]\\)\\)?" ts-string)
      (cond
       ((or (org-pos-in-match-range pos 2) ;; POS in end hours
	    (org-pos-in-match-range pos 3)) ;; POS in end minutes
	(setq minute (string-to-number (match-string 3 ts-string))
	      hour (string-to-number (match-string 2 ts-string)))
	(if (org-pos-in-match-range pos 2) ;; POS in end hours
            ;; INCREMENT-STEP is only applicable to MINUTE.
	    (setq hour (+ hour nincrements))
	  (setq nincrements (* increment-step nincrements))
	  (unless (= 0 (setq rem (% minute increment-step)))
            ;; Round the MINUTE to INCREMENT-STEP.
	    (setq minute (+ minute (if (> nincrements 0) (- rem) (- increment-step rem)))))
	  (setq minute (+ minute nincrements)))
	(when (< minute 0) (setq minute (+ minute 60) hour (1- hour)))
	(when (> minute 59) (setq minute (- minute 60) hour (1+ hour)))
	(setq hour (mod hour 24))
	(setq pos-match-group 1
              new (format "-%02d:%02d" hour minute)))

       ((org-pos-in-match-range pos 6) ;; POS on "dmwy" repeater char.
	(setq pos-match-group 6
              new (car (rassoc (+ nincrements (cdr (assoc (match-string 6 ts-string) idx))) idx))))

       ((org-pos-in-match-range pos 5) ;; POS on X in "Xd" repeater.
	(setq pos-match-group 5
              ;; Never drop below X=1.
              new (format "%d" (max 1 (+ nincrements (string-to-number (match-string 5 ts-string)))))))

       ((org-pos-in-match-range pos 9) ;; POS on "dmwy" repeater in warning interval.
	(setq pos-match-group 9
              new (car (rassoc (+ nincrements (cdr (assoc (match-string 9 ts-string) idx))) idx))))

       ((org-pos-in-match-range pos 8) ;; POS on X in "Xd" in warning interval.
	(setq pos-match-group 8
              ;; Never drop below X=0.
              new (format "%d" (max 0 (+ nincrements (string-to-number (match-string 8 ts-string))))))))

      (when pos-match-group
	(setq ts-string (concat
		         (substring ts-string 0 (match-beginning pos-match-group))
		         new
		         (substring ts-string (match-end pos-match-group))))))
    ts-string))

(provide 'org-timestamp)

;;; org-timestamp.el ends here


